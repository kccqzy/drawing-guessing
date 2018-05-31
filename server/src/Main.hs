{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Network.WebSockets as WS
import System.Random (randomIO, randomRIO)
import Text.Read (readMaybe)

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

wsOptions :: WS.ConnectionOptions
wsOptions =
  WS.defaultConnectionOptions
  { WS.connectionCompressionOptions = WS.PermessageDeflateCompression WS.defaultPermessageDeflate
  , WS.connectionStrictUnicode = True
  , WS.connectionFramePayloadSizeLimit = WS.SizeLimit 1048576
  , WS.connectionMessageDataSizeLimit = WS.SizeLimit 8388608
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
            Just nick -> do
              rid <- randomRIO (1000000, 9999999)
              cid <- randomIO
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
            (Just rid, Just nick) -> do
              cid <- randomIO
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
  , cConn :: Maybe WS.Connection
  }

data Room = Room
  { rClients :: IM.IntMap Client
  , rJoinable :: Bool
  }

type ServerState = IM.IntMap Room

newRoom :: TVar ServerState -> Int -> Int -> WS.ServerApp
newRoom st rid cid = undefined

joinRoom :: TVar ServerState -> Int -> Int -> WS.ServerApp
joinRoom st rid cid = undefined
