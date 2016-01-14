{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.PlayingCards where

import Control.Monad.State.Strict
import qualified Data.Vector as V
import System.Random

import Jatek.Interact
import Jatek.Random

data Suit = Diamond | Spade | Heart | Club deriving (Show, Eq, Enum, Read)
newtype Rank = Rank {unRank :: Int} deriving (Eq, Ord, Num, Enum, Read)

ace   = Rank 14
king  = Rank 13
queen = Rank 12
jack  = Rank 11

showRank :: Rank -> String
showRank (Rank 14) = "A"
showRank (Rank 13) = "K"
showRank (Rank 12) = "Q"
showRank (Rank 11) = "J"
showRank (Rank 10) = "T"
showRank (Rank n)  = show n

showSuit :: Suit -> String
showSuit Diamond = "d"
showSuit Spade   = "s"
showSuit Heart   = "h"
showSuit Club    = "c"

instance Show Rank where
  show = showRank

data Card = Card {rank :: Rank, suit :: Suit} deriving Eq

showCard :: Card -> String
showCard (Card r s) = showRank r ++ showSuit s

instance Show Card where
  show = showCard

-- complicated because it deals with crappy input from humans... 
readCard :: String -> Maybe Card
readCard str =
  case (ranks, suits) of
    ([r], [s]) -> Just $ Card r s
    _          -> Nothing
  where ranks = collect $ map (\c -> lookup c rankChars) str
        suits = collect $ map (\c -> lookup c suitChars) str
        collect maybes = foldl (\lst xMay ->
                                 case xMay of
                                   Just x  -> x:lst
                                   Nothing -> lst) [] maybes 
        suitChars = [('d', Diamond), ('D', Diamond),
                     ('s', Spade)  , ('S', Spade),
                     ('h', Heart)  , ('H', Heart),
                     ('c', Club)   , ('C', Club)]
        rankChars = [('a', ace)    , ('A', ace),
                     ('k', king)   , ('K', king),
                     ('q', queen)  , ('Q', queen),
                     ('j', jack)   , ('J', jack),
                     ('t', 10)     , ('T', 10),
                     ('0', 10),  -- we treat '1' in "10c" as inert
                     ('9', 9),
                     ('8', 8),
                     ('7', 7),
                     ('6', 6),
                     ('5', 5),
                     ('4', 4),
                     ('3', 3),
                     ('2', 2)]

instance Read Card where
  readsPrec _ str =
    case readCard str of
      Just card -> [(card, "")]
      Nothing   -> []

fullDeck :: [Card]
fullDeck = [Card r s | r <- [2..14], s <- [Diamond, Heart, Spade, Club]]
