{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Maybe
import qualified Data.Text as T
import Network.WebSockets

data Msg
  = TellRoomId Int
  | ToldStartGame
  | AnnounceRound Int
                  T.Text -- ^ Round number and drawer
  | TellDrawerWord T.Text
  | AnnounceWordLength Int
  | GotDrawingCmd T.Text
  | RelayDrawingCmd T.Text
  | GotGuess T.Text
  | ReplyGuessIncorrect
  | EndRoundWithWinner T.Text
  | EndRoundWithoutWinner
  | AnnounceTimeLeft Int

$(deriveJSON defaultOptions ''Msg)

instance WebSocketsData Msg where
  toLazyByteString = toLazyByteString . encode
  fromLazyByteString bs = fromMaybe (error "unparsable message") (decode bs)
  fromDataMessage (Binary bs) = fromMaybe (error "unparsable message") (decode bs)
  fromDataMessage (Text bs _) = fromMaybe (error "unparsable message") (decode bs)