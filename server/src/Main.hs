{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main
  ( main
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Function
import qualified Data.IntMap as IM
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.WebSockets
import System.Random.MWC
import System.Random.MWC.Distributions (uniformShuffle)
import Text.Read (readMaybe)
import Types
import qualified WordLists

--------------------------------------------------------------------------------
-- Boring stuff
--------------------------------------------------------------------------------

main :: IO ()
main = do
  st <- newTVarIO initialServerState
  runSettings settings (logStdout (app st))

initialServerState :: ServerState
initialServerState = IM.empty

settings :: Settings
settings = setHost "0.0.0.0" (setPort 8080 defaultSettings)

wsOptions :: ConnectionOptions
wsOptions =
  defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate
  , connectionStrictUnicode = True
  , connectionFramePayloadSizeLimit = SizeLimit 1048576
  , connectionMessageDataSizeLimit = SizeLimit 8388608
  }

app :: TVar ServerState -> Application
app st req =
  (>>=) $
  case requestMethod req of
    "GET" ->
      case pathInfo req of
        [] -> respond status301 ""
        ["index.html"] -> respond status200 "Use /new-room or /join-room."
        ["new-room"] ->
          case getParam "nickname" of
            Nothing -> respond status400 "Please provide nickname."
            Just nick -> withSystemRandom . asGenIO $ \rng -> do
              rid <- uniformR (1000000, 9999999) rng
              cid <- uniform rng
              case WaiWS.websocketsApp wsOptions (newRoom st rid cid) req of
                Nothing -> respond status403 "WebSocket expected."
                Just resp ->
                  atomically $ do
                    modifyTVar'
                      st
                      (IM.insert
                         rid
                         Room {rJoinable = True, rClients = IM.singleton cid Client {cNickname = nick, cConn = Nothing}})
                    pure resp
        ["join-room"] ->
          case (getParam "room_id" >>= readMaybe . T.unpack, getParam "nickname") of
            (Just rid, Just nick) -> withSystemRandom . asGenIO $ \rng -> do
              cid <- uniform rng
              case WaiWS.websocketsApp wsOptions (joinRoom st rid cid) req of
                Nothing -> respond status403 "WebSocket expected."
                Just resp ->
                  atomically $ do
                    rooms <- readTVar st
                    case IM.lookup rid rooms of
                      Just room
                        | rJoinable room -> do
                          let newClient = Client {cNickname = nick, cConn = Nothing}
                          writeTVar st (IM.insert rid room {rClients = IM.insert cid newClient (rClients room)} rooms)
                          pure resp
                      _ -> respond status404 "No such room or not joinable."
            _ -> respond status400 "Please provide room_id and nickname."
        _ -> respond status404 "404: Not Found"
    _ -> respond status405 "405: Method Not Allowed"
  where
    getParam q = decodeUtf8 <$> join (List.lookup q (queryString req))
    respond status = pure . responseLBS status [("content-type", "text/plain")]

--------------------------------------------------------------------------------
-- Business logic
--------------------------------------------------------------------------------

data Client = Client
  { cNickname :: T.Text
  , cConn :: Maybe Connection
  }

data Room = Room
  { rClients :: IM.IntMap Client
  , rJoinable :: Bool
  }

type ServerState = IM.IntMap Room

beginConnection ::
     (Connection -> TVar ServerState -> Int -> IO b)
  -> TVar ServerState
  -> IM.Key
  -> IM.Key
  -> PendingConnection
  -> IO b
beginConnection cont st rid cid pending = do
  conn <- acceptRequest pending
  forkPingThread conn 30
  atomically $
    modifyTVar'
      st
      (IM.adjust
         (\Room {..} -> Room {rClients = IM.adjust (\Client {..} -> Client {cConn = Just conn, ..}) cid rClients, ..})
         rid)
  cont conn st rid

newRoom :: TVar ServerState -> Int -> Int -> ServerApp
newRoom = beginConnection $ \conn st rid -> do
  sendTextData conn (TellRoomId rid)
  d <- receiveData conn
  case d of
    ToldStartGame -> do
      atomically $ modifyTVar' st (IM.adjust (\Room{..} -> Room {rJoinable = False, ..}) rid)
      beginGame st rid
    _ -> throwIO (ErrorCall "Malicious client: State violation.")

joinRoom :: TVar ServerState -> Int -> Int -> ServerApp
joinRoom = beginConnection $ \_ st rid ->
  atomically $ do
    rooms <- readTVar st
    when (IM.member rid rooms) retry -- TODO make it be blocked on an MVar/TMVar for performance

raceSTM :: STM a -> STM b -> STM (Either a b)
raceSTM a b = (Left <$> a) `orElse` (Right <$> b)

beginGame :: TVar ServerState -> Int -> IO ()
beginGame st rid = withSystemRandom . asGenIO $ \rng -> do
  clients <- (IM.toList . rClients . (IM.! rid)) <$> readTVarIO st
  wordlist <- uniformShuffle WordLists.animals rng

  -- We assume it is safe to call `receiveData` and `sendTextData` at the same
  -- time. The main design complication is that `receiveData` cannot be
  -- interrupted, therefore we cannot simply use `race` such as `race
  -- (receiveData conn) (atomically (readTChan chan))`. So in terms of design,
  -- we will have a thread to read from each of the clients, and then
  -- communicate with this over a channel.

  clients' <- forM clients $ \(cid, Client name conn) -> do
    receiveQueue <- newTQueueIO
    receiver <- async $ forever $ receiveData (fromJust conn) >>= atomically . writeTQueue receiveQueue
    pure (cid, (name, receiveQueue, receiver, fromJust conn))

  let broadcastTo who msg = forM_ who $ \(_, (_, _, _, conn)) ->  sendTextData conn msg
      broadcast = broadcastTo clients'

  forM_ (zip [0 ..] clients') $ \(roundNo, roundDrawer@(_, (drawerName, drawerQueue, _, drawerConn))) -> do
    let guessers = List.deleteBy ((==) `on` fst) roundDrawer clients'
        broadcastGuessers = broadcastTo guessers

    broadcast (AnnounceRound roundNo drawerName)
    let wd = wordlist V.! (roundNo `mod` V.length wordlist)
    ss <- atomically (readTQueue drawerQueue)
    case ss of
      ToldStartRound -> pure ()
      _ -> throwIO (ErrorCall "Malicious client: State violation.")

    sendTextData drawerConn (TellDrawerWord wd)
    broadcastGuessers (AnnounceWordLength (T.length wd))

    let timerThread =
          forM_ (enumFromThenTo 8 7 0) $ \decaSeconds -> do
            threadDelay 10000000
            broadcast (AnnounceTimeLeft (decaSeconds * 10))

    r <- race timerThread $ do
      -- Within the time limit of 90 seconds, we either try to receive a drawing
      -- command from the drawer or a guess from the guessers.
      drawingChan <- newBroadcastTChanIO
      winner <- newEmptyTMVarIO

      let drawerThread conn = do
            d <- atomically (raceSTM (readTQueue drawerQueue) (readTMVar winner))
            case d of
              Right _ -> pure ()
              Left (GotDrawingCmd t) -> atomically (writeTChan drawingChan t) >> drawerThread conn
              _ -> throwIO (ErrorCall "Malicious client: State violation.")

          guesserThread name readQueue conn = do
            chan <- atomically $ dupTChan drawingChan
            -- A triple race between: (a) winner found, (b) got message from the drawing chan, (c) got message from the read queue
            d <- atomically (raceSTM (raceSTM (readTQueue readQueue) (readTChan chan)) (readTMVar winner))
            case d of
              Right _ -> pure ()
              Left (Right t) -> sendTextData conn (RelayDrawingCmd t) >> guesserThread name readQueue conn
              Left (Left (GotGuess w)) | w == wd -> atomically (putTMVar winner name)
                                       | otherwise -> sendTextData conn ReplyGuessIncorrect
              _ -> throwIO (ErrorCall "Malicious client: State violation.")

          actions = drawerThread drawerConn : map (\(_, (name, readQueue, _, conn)) -> guesserThread name readQueue conn) guessers

      mapConcurrently_ id actions
      atomically (readTMVar winner)
    case r of
      Left _ -> broadcast EndRoundWithoutWinner
      Right w -> broadcast (EndRoundWithWinner w)
    forM_ clients' $ \(_, (_, queue, _, _)) -> atomically (void (flushTQueue queue))
  atomically $ modifyTVar' st (IM.delete rid)
  forConcurrently_ clients' $ \(_, (_, _, receiver, conn)) -> do
    sendClose conn ("Goodbye!" :: T.Text)
    threadDelay 2000000
    cancel receiver
