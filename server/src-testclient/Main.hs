{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  ) where

import Control.Concurrent.Async
import Control.Monad
import Network.URI
import Network.WebSockets
import System.Environment
import System.IO
import Text.Read (readMaybe)
import Types

clientApp :: ClientApp ()
clientApp conn =
  race_
    (forever $ do
       l <- getLine
       let msg = readMaybe l :: Maybe Msg
       case msg of
         Just m -> sendTextData conn m
         Nothing -> hPutStrLn stderr "Invalid command.") $
  forever $ do
    (msg :: Msg) <- receiveData conn
    print msg


main :: IO ()
main = do
  args <- getArgs
  case args of
    [nick] ->
      runClient "127.0.0.1" 8080 ("/new-room?nickname=" ++ escapeURIString isUnescapedInURIComponent nick) clientApp
    [nick, pinStr]
      | Just pin <- readMaybe pinStr
      , minPin <= pin && pin <= maxPin ->
        runClient
          "127.0.0.1"
          8080
          ("/join-room?nickname=" ++ escapeURIString isUnescapedInURIComponent nick ++ "&room_id=" ++ show pin)
          clientApp
    _ -> hPutStrLn stderr "usage: dg-client NICK [PIN]"
  where
    minPin, maxPin :: Int
    minPin = 1000000
    maxPin = 9999999
