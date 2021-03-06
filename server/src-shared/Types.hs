{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Types (Msg(..))where

import Data.Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Maybe
import qualified Data.Text as T
import Network.WebSockets

-- | The messages that are exchanged between the server and the client. When a
-- new client creates a game, the 'TellRoomId' message is sent, followed by
-- 'AnnouncePlayers'. When others join a game, all existing players get the
-- 'AnnouncePlayers' message. When the game master sends 'ToldStartGame', the
-- message 'AnnounceRound' is sent to everyone. The message 'TellMayStartRound'
-- is additionally sent to the current round drawer. The drawer is expected to
-- reply with 'ToldStartRound', upon which the round begins and the clock
-- starts. The drawer can send 'GotDrawingCmd' upon whose receipt
-- 'RelayDrawingCmd' is sent. The guessers can send 'GotGuess' and the server
-- will either reply this client with 'ReplyGuessIncorrect' or with
-- 'EndRound'.
data Msg
  = TellRoomId Int
  | AnnouncePlayers [T.Text]
  | ToldStartGame Int -- ^ Number of rounds
  | TellMayStartRound
  | ToldStartRound
  | AnnounceRound Int
                  T.Text -- ^ Round number and drawer
  | TellDrawerWord T.Text
  | TellGuessersMaskedWord T.Text
  | GotDrawingCmd T.Text
  | RelayDrawingCmd T.Text
  | GotGuess T.Text
  | ReplyGuessIncorrect T.Text
  | ReplyGuessCorrect
  | EndRound
  | ToldNextRound
  | AnnounceTimeLeft Int
  | AnnounceScores [(T.Text, Int)]
  | EndGame
  deriving (Show, Read)

$(deriveJSON defaultOptions ''Msg)

instance WebSocketsData Msg where
  toLazyByteString = toLazyByteString . encode
  fromLazyByteString bs = fromMaybe (error "unparsable message") (decode bs)
  fromDataMessage (Binary bs) = fromMaybe (error "unparsable message") (decode bs)
  fromDataMessage (Text bs _) = fromMaybe (error "unparsable message") (decode bs)
