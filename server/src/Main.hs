{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Foldable
import qualified Data.IntMap.Strict as IM
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
              case WaiWS.websocketsApp wsOptions (newRoom st rid nick) req of
                Nothing -> respond status403 "WebSocket expected."
                Just resp -> pure resp
        ["join-room"] ->
          case (getParam "room_id" >>= readMaybe . T.unpack, getParam "nickname") of
            (Just rid, Just nick) ->
              case WaiWS.websocketsApp wsOptions (joinRoom st rid nick) req of
                Nothing -> respond status403 "WebSocket expected."
                Just resp -> pure resp
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
  , cConn :: Connection
  }

type Clients = IM.IntMap Client

data Room = Room
  { rClients :: Clients
  , rJoinable :: Bool
  }

type ServerState = IM.IntMap Room

beginConnection ::
     (Connection -> TVar ServerState -> Int -> T.Text -> IO b)
  -> TVar ServerState
  -> Int
  -> T.Text
  -> PendingConnection
  -> IO b
beginConnection cont st rid nick pending = do
  conn <- acceptRequest pending
  forkPingThread conn 30
  cont conn st rid nick

announceNewPlayers :: TVar ServerState -> Int -> Clients -> Connection -> IO ()
announceNewPlayers st rid initialClients conn = do
    let findNewPlayers previousClients = do
          sendTextData conn (AnnouncePlayers (toList (cNickname <$> previousClients)))
          newClients <- atomically $ do
            currentClients <- (rClients . (IM.! rid)) <$> readTVar st
            if IM.size currentClients == IM.size previousClients then retry else pure currentClients
          findNewPlayers newClients
    findNewPlayers initialClients

newRoom :: TVar ServerState -> Int -> T.Text -> ServerApp
newRoom =
  beginConnection $ \conn st rid nick -> do
    let clients = IM.singleton 0 Client {cNickname = nick, cConn = conn}
    atomically $ modifyTVar' st (IM.insert rid Room {rJoinable = True, rClients = clients})
    sendTextData conn (TellRoomId rid)
    playerListAnnouncer <- async $ announceNewPlayers st rid clients conn
    d <- receiveData conn
    case d of
      ToldStartGame rounds ->
        join $
        atomically $ do
          clientsCount <- (IM.size . rClients . (IM.! rid)) <$> readTVar st
          if clientsCount >= 2
            then modifyTVar' st (IM.adjust (\Room {..} -> Room {rJoinable = False, ..}) rid) >>
                 pure (cancel playerListAnnouncer >> beginGame st rid rounds)
            else retry
      _ -> throwIO (ErrorCall "Malicious client: State violation.")

(|>) :: IM.IntMap a -> a -> IM.IntMap a
m |> v | not (IM.null m), (mi, _) <- IM.findMax m = IM.insert (1+mi) v m
       | otherwise = IM.singleton 0 v

joinRoom :: TVar ServerState -> Int -> T.Text -> ServerApp
joinRoom =
  beginConnection $ \conn st rid nick -> do
    initialClients <-
      join $
      atomically $ do
        rooms <- readTVar st
        case IM.lookup rid rooms of
          Just room
            | rJoinable room -> do
              let newClient = Client {cNickname = nick, cConn = conn}
                  newClients = rClients room |> newClient
              writeTVar st (IM.insert rid room {rClients = newClients} rooms)
              pure (pure newClients)
          _ -> pure $ sendClose conn ("Room does not exist." :: T.Text) >> pure IM.empty
    unless (IM.null initialClients) $ do
      race_ (announceNewPlayers st rid initialClients conn) $
        atomically $ do
          joinable <- (rJoinable . (IM.! rid)) <$> readTVar st
          when joinable retry -- wait till the room is no longer joinable
      atomically $ do
        rooms <- readTVar st
        when (IM.member rid rooms) retry -- TODO make it be blocked on an MVar/TMVar for performance

raceSTM :: STM a -> STM b -> STM (Either a b)
raceSTM a b = (Left <$> a) `orElse` (Right <$> b)

data ClientConn = ClientConn
  { receiveQueue :: TQueue Msg
  , receiver :: Async ()
  , sendQueue :: TQueue Msg
  , sender :: Async ()
  , conn :: Connection
  , name :: T.Text
  }

beginGame :: TVar ServerState -> Int -> Int -> IO ()
beginGame st rid rounds = withSystemRandom . asGenIO $ \rng -> do
  clients <- (rClients . (IM.! rid)) <$> readTVarIO st
  wordlist <- uniformShuffle WordLists.animals rng

  -- We assume it is safe to call `receiveData` and `sendTextData` at the same
  -- time. The main design complication is that `receiveData` cannot be
  -- interrupted, therefore we cannot simply use `race` such as `race
  -- (receiveData conn) (atomically (readTChan chan))`. So in terms of design,
  -- we will have a thread to read from each of the clients, and then
  -- communicate with this over a channel.

  clients' <- forM clients $ \(Client name conn) -> do
    receiveQueue <- newTQueueIO
    receiver <- async $ forever $ receiveData conn >>= atomically . writeTQueue receiveQueue
    sendQueue <- newTQueueIO
    sender <- async $ forever $ atomically (readTQueue sendQueue) >>= sendTextData conn
    pure ClientConn{..}

  let broadcastTo who msg = atomically $ forM_ who $ \ClientConn{sendQueue} -> writeTQueue sendQueue msg
      broadcast = broadcastTo clients'

  let clients'' = zip [0..] (take rounds (cycle (IM.toList clients')))

  let isClientDead cc = do
        receiverResult <- pollSTM (receiver cc)
        senderResult <- pollSTM (sender cc)
        -- We consider this client dead if either the receiver or sender is dead.
        pure (isJust receiverResult || isJust senderResult)


  result <- forM clients'' $ \(roundNo, (drawerIndex, drawer)) -> do
   -- Is this drawer dead? If so, skip him.
   isDead <- atomically $ isClientDead drawer
   if isDead then pure Nothing else do
    let guessers = IM.delete drawerIndex clients'
        broadcastGuessers = broadcastTo guessers
        sendDrawer = atomically . writeTQueue (sendQueue drawer)

    broadcast (AnnounceRound roundNo (name drawer))
    sendDrawer TellMayStartRound
    let wd = wordlist V.! (roundNo `mod` V.length wordlist)
    atomically (readTQueue (receiveQueue drawer)) >>= \case
      ToldStartRound -> pure ()
      _ -> throwIO (ErrorCall "Malicious client: State violation.")

    sendDrawer (TellDrawerWord wd)
    broadcastGuessers (AnnounceWordLength (T.length wd))

    let timerThread =
          forM_ (enumFromThenTo 89 88 0) $ \seconds -> do
            threadDelay 1000000
            broadcast (AnnounceTimeLeft seconds)

    r <- race timerThread $ do
      -- Within the time limit of 90 seconds, we either try to receive a drawing
      -- command from the drawer or a guess from the guessers.
      winner <- newEmptyTMVarIO

      let drawerThread = do
            d <- atomically (raceSTM (readTQueue (receiveQueue drawer)) (readTMVar winner))
            case d of
              Right _ -> pure ()
              Left (GotDrawingCmd t) -> broadcastGuessers (RelayDrawingCmd t) >> drawerThread
              _ -> throwIO (ErrorCall "Malicious client: State violation.")

          guesserThread ClientConn{..} = do
            let loop =
                  atomically (raceSTM (readTQueue receiveQueue) (readTMVar winner)) >>= \case
                    Right _ -> pure ()
                    Left (GotGuess w) | w == wd -> atomically (putTMVar winner name)
                                      | otherwise -> atomically (writeTQueue sendQueue (ReplyGuessIncorrect w)) >> loop
                    _ -> throwIO (ErrorCall "Malicious client: State violation.")
            loop
          actions = drawerThread : map guesserThread (IM.elems guessers)

      mapConcurrently_ id actions
      atomically (readTMVar winner)
    case r of
      Left _ -> broadcast EndRoundWithoutWinner
      Right w -> broadcast (EndRoundWithWinner w)
    atomically (readTQueue (receiveQueue drawer)) >>= \case
      ToldNextRound -> pure ()
      _ -> throwIO (ErrorCall "Malicious client: State violation.")
    case r of
      Left _ -> pure Nothing
      Right w -> pure (Just w)
  broadcast (EndGameWithTally (toList result))

  forConcurrently_ clients' $ \cc@ClientConn{..} -> do
  -- There is no guarantee that right after the broadcast attempt, the sender
  -- threads have a chance to actually send it out. Therefore we wait. We first
  -- wait for the sender to finish taking the item from the queue. Of course
  -- there is also no guarantee that there won't be a context switch to our
  -- thread right before the sender actually sends it. We hope for the best use
  -- a threadDelay. TODO in the future when we build a full ACK infrastructure
  -- on top of WebSocket, we can simply check the ACK number before killing the
  -- sender.
    atomically $ do
      isDead <- isClientDead cc
      isSenderQueueEmpty <- isEmptyTQueue sendQueue
      if isDead || isSenderQueueEmpty then pure () else retry
    threadDelay 1000000
    cancel sender
    threadDelay 2000000
    closer <- async $ sendClose conn ("Goodbye!" :: T.Text)
    threadDelay 4000000
    cancel receiver
    cancel closer
  atomically $ modifyTVar' st (IM.delete rid)
