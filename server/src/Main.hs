{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
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

data Msg = TellRoomId Int
         | ToldStartGame
         | AnnounceRound Int T.Text -- ^ Round number and drawer
         | TellDrawerWord T.Text
         | AnnounceWordLength Int
         | GotDrawingCmd T.Text
         | RelayDrawingCmd T.Text
         | GotGuess T.Text
         | ReplyGuessIncorrect
         | EndRoundWithWinner T.Text
         | EndRoundWithoutWinner
         | AnnounceTimeLeft Int

instance WebSocketsData Msg where
  toLazyByteString = toLazyByteString . encode
  fromLazyByteString bs = fromMaybe (error "unparsable message") (decode bs)
  fromDataMessage (Binary bs) = fromMaybe (error "unparsable message") (decode bs)
  fromDataMessage (Text bs _) = fromMaybe (error "unparsable message") (decode bs)

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
joinRoom = beginConnection $ \_ _ _ -> pure ()

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

    let timer = forM_ (enumFromThenTo 9 8 1) $ \decaSeconds -> do
          threadDelay 10000000
          broadcast (AnnounceTimeLeft (decaSeconds * 10))
    r <- race timer $ do
      -- Within the time limit of 90 seconds, we either try to receive a drawing
      -- command from the drawer or a guess from the guessers.
      drawingChan <- newBroadcastTChanIO
      winner <- newEmptyTMVarIO
      let drawerThread conn = whileM_ (atomically $ isEmptyTMVar winner) $ do
            d <- receiveData conn
            case d of
              GotDrawingCmd t -> atomically $ writeTChan drawingChan t
              _ -> throwIO (ErrorCall "Malicious client: State violation.")
          guesserThread name conn = do
            chan <- atomically $ dupTChan drawingChan
            whileM_ (atomically $ isEmptyTMVar winner) $ do
              ed <- race (receiveData conn) (atomically (readTChan chan))
              case ed of
                Right t -> sendTextData conn (RelayDrawingCmd t)
                Left (GotGuess w) | w == wd -> atomically (putTMVar winner name)
                                  | otherwise -> sendTextData conn ReplyGuessIncorrect
                Left _ -> throwIO (ErrorCall "Malicious client: State violation.")

          actions = drawerThread (fromJust drawerConn) : map (\(_, Client name conn) -> guesserThread name (fromJust conn)) guessers

      mapConcurrently_ id actions
      w <- atomically (readTMVar winner)
      pure w
    case r of
      Left _ -> broadcast EndRoundWithoutWinner
      Right w -> broadcast (EndRoundWithWinner w)

$(deriveJSON defaultOptions ''Msg)
