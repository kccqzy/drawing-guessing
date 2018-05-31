{-# LANGUAGE OverloadedStrings #-}
module WordLists (animals) where

import qualified Data.Text as T
import qualified Data.Vector as V

animals :: V.Vector T.Text
animals =
  V.fromList
    [ "alligator"
    , "ant"
    , "antelope"
    , "ape"
    , "donkey"
    , "baboon"
    , "bat"
    , "bear"
    , "beaver"
    , "bee"
    , "buffalo"
    , "butterfly"
    , "camel"
    , "cat"
    , "cheetah"
    , "chimpanzee"
    , "cicada"
    , "cockroach"
    , "cod"
    , "coyote"
    , "crab"
    , "raven"
    , "deer"
    , "dinosaur"
    , "dog"
    , "dolphin"
    , "duck"
    , "eel"
    , "elephant"
    , "elk"
    , "ferret"
    , "fish"
    , "fox"
    , "frog"
    , "giraffe"
    , "goat"
    , "goldfish"
    , "gorilla"
    , "grasshopper"
    , "hamster"
    , "hare"
    , "hedgehog"
    , "hippopotamus"
    , "hornet"
    , "horse"
    , "hyena"
    , "jellyfish"
    , "koala"
    , "leopard"
    , "lion"
    , "lizard"
    , "monkey"
    , "moose"
    , "mosquito"
    , "mouse"
    , "otter"
    , "ox"
    , "oyster"
    , "panda"
    , "pig"
    , "rabbit"
    , "raccoon"
    , "reindeer"
    , "salmon"
    , "sardine"
    , "scorpion"
    , "shark"
    , "sheep"
    , "skunk"
    , "snail"
    , "spider"
    , "squirrel"
    , "swan"
    , "tiger"
    , "wasp"
    , "weasel"
    , "whale"
    , "wolf"
    , "wombat"
    , "worm"
    , "zebra"
    ]