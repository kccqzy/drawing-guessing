{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main
  ( main
  ) where

import Control.Applicative
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
     (Connection -> TVar ServerState -> IM.Key -> IO b)
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
joinRoom = beginConnection $ \_ st rid -> atomically $ do
  rooms <- readTVar st
  when (IM.member rid rooms) retry -- TODO make it be blocked on an MVar/TMVar for performance

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ p f = go
  where go = do
          x <- p
          if x then f >> go else pure ()

beginGame :: TVar ServerState -> Int -> IO ()
beginGame st rid = withSystemRandom . asGenIO $ \rng -> do
  clients <- (IM.toList . rClients . (IM.! rid)) <$> readTVarIO st
  wordlist <- uniformShuffle WordLists.animals rng

  let broadcastTo who msg = forM_ who $ \(_, Client _ conn) -> maybe (pure ()) (`sendTextData` msg) conn
      broadcast = broadcastTo clients

  forM_ (zip [0 ..] clients) $ \(roundNo, roundDrawer@(_, Client drawerName drawerConn)) -> do
    let guessers = List.deleteBy ((==) `on` fst) roundDrawer clients
        broadcastGuessers = broadcastTo guessers

    broadcast (AnnounceRound roundNo drawerName)
    let wd = wordlist V.! (roundNo `mod` V.length wordlist)
    sendTextData (fromJust drawerConn) (TellDrawerWord wd)
    broadcastGuessers (AnnounceWordLength (T.length wd))

    timerExpiration <- newEmptyTMVarIO
    let timerThread = do
          forM_ (enumFromThenTo 8 7 0) $ \decaSeconds -> do
            threadDelay 10000000
            broadcast (AnnounceTimeLeft (decaSeconds * 10))
          atomically $ putTMVar timerExpiration ()

    r <- withAsync timerThread $ \timer -> do
      -- Within the time limit of 90 seconds, we either try to receive a drawing
      -- command from the drawer or a guess from the guessers.
      drawingChan <- newBroadcastTChanIO
      winner <- newEmptyTMVarIO

      let shouldContinue = atomically $ liftA2 (&&) (isEmptyTMVar winner) (isEmptyTMVar timerExpiration)

          drawerThread :: Connection -> IO ()
          drawerThread conn = whileM_ shouldContinue $ do
            d <- receiveData conn
            case d of
              GotDrawingCmd t -> atomically $ writeTChan drawingChan t
              GotRefresh -> pure () -- Okay. This is because we can't interrupt 'receiveData'.
              _ -> throwIO (ErrorCall "Malicious client: State violation.")

          guesserThread :: T.Text -> Connection -> IO ()
          guesserThread name conn = whileM_ shouldContinue $ do
            chan <- atomically $ dupTChan drawingChan
            rcvdBuf <- newEmptyTMVarIO
            let relayThread = do
                  tt <- atomically ((Right <$> readTChan chan) `orElse` (Left () <$ readTMVar winner) `orElse` (Left <$> readTMVar timerExpiration))
                  case tt of
                    Right t -> sendTextData conn (RelayDrawingCmd t) >> relayThread
                    Left _ -> pure ()

                readerThread = whileM_ shouldContinue $ do
                  -- Wait till the rcvdBuf is empty
                  atomically $ do
                    e <- isEmptyTMVar rcvdBuf
                    if e
                      then pure ()
                      else retry
                  d <- receiveData conn
                  case d of
                    GotGuess w | w == wd -> atomically (putTMVar winner name)
                               | otherwise -> sendTextData conn ReplyGuessIncorrect
                    GotRefresh -> pure () -- Okay. This is because we can't interrupt 'receiveData'.
                    _ -> throwIO (ErrorCall "Malicious client: State violation.")

            concurrently_ relayThread readerThread

          actions = drawerThread (fromJust drawerConn) : map (\(_, Client name conn) -> guesserThread name (fromJust conn)) guessers

      mapConcurrently_ id actions
      w <- atomically ((Right <$> readTMVar winner) `orElse` (Left <$> takeTMVar timerExpiration))
      cancel timer
      pure w
    case r of
      Left _ -> broadcast EndRoundWithoutWinner
      Right w -> broadcast (EndRoundWithWinner w)
  atomically $ modifyTVar' st (IM.delete rid)
