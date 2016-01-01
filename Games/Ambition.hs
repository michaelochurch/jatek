{-# LANGUAGE DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, RecordWildCards #-}

module Games.Ambition where

import Control.Lens
import Control.Monad (join)
import Control.Monad.State.Strict
import Data.Monoid (Sum(..), getSum)
import Data.Maybe (fromJust)

import Jatek.Core
import Jatek.Interact

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

-- compllicated because it deals with crappy input from humans... 
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

pointValue :: Card -> Int
pointValue (Card r s) =
  case s of
    Club    -> if r == king then 18 else 0
    Heart   -> if r == 2 then 10 else if r > 10 then 3 else 1
    Spade   -> if r > 10 then 6 else 2
    Diamond -> if r > 10 then 3 else 1

firstTrickValue :: Int
firstTrickValue = 9

fullDeck :: [Card]
fullDeck = [Card r s | r <- [2..14], s <- [Diamond, Heart, Spade, Club]]

nextPos :: Int -> Int
nextPos n = (n + 1) `mod` 4

-- Perhaps this should have been done w/ "real" dependent types.
-- investigate Nat type... lots of ugly reinvention here.
newtype Tup4 a = Tup4 {fromTup4 :: (a, a, a, a)} deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

tup4Lens :: Lens (Tup4 a) (Tup4 a) (a, a, a, a) (a, a, a, a)
tup4Lens = lens fromTup4 (\_ x -> Tup4 x)

instance Field1 (Tup4 a) (Tup4 a) a a where
  _1 = tup4Lens . _1

instance Field2 (Tup4 a) (Tup4 a) a a where
  _2 = tup4Lens . _2

instance Field3 (Tup4 a) (Tup4 a) a a where
  _3 = tup4Lens . _3

instance Field4 (Tup4 a) (Tup4 a) a a where
  _4 = tup4Lens . _4

data TrickState = TrickState {tsTrickNum :: Int,
                              tsLeadPos :: Int,
                              tsHands   :: Tup4 [Card],
                              tsTable   :: Tup4 (Maybe Card)} deriving (Eq, Show)

data TrickView = TrickView {tvTrickNum :: Int,
                            tvLeadPos  :: Int,
                            tvHand     :: [Card],
                            tvTable    :: Tup4 (Maybe Card)} deriving (Eq, Show)

tup4 :: Int -> Lens' (Tup4 a) a
tup4 n =
  case (n `mod` 4) of
    0 -> _1
    1 -> _2
    2 -> _3
    3 -> _4
    _ -> undefined

class HasTrickView a where
  trickNum :: Lens' a Int
  leadPos  :: Lens' a Int
  table    :: Lens' a (Tup4 (Maybe Card))

instance HasTrickView TrickState where
  trickNum = lens tsTrickNum (\x y -> x {tsTrickNum = y})
  leadPos  = lens tsLeadPos  (\x y -> x {tsLeadPos = y})
  table    = lens tsTable    (\x y -> x {tsTable = y})

instance HasTrickView TrickView where
  trickNum = lens tvTrickNum (\x y -> x {tvTrickNum = y})
  leadPos  = lens tvLeadPos  (\x y -> x {tvLeadPos = y})
  table    = lens tvTable    (\x y -> x {tvTable = y})

tablePos :: (HasTrickView a) => Int -> Lens' a (Maybe Card)
tablePos pos =
  table . (tup4 pos)

ledSuit :: (HasTrickView a) => a -> Maybe Suit
ledSuit obj =
  if obj ^. trickNum == 1 then Just Diamond
  else fmap suit $ obj ^. (tablePos $ obj ^. leadPos)

-- Ambition always has exactly 4 players.
all4Players :: [Int]
all4Players = [0..3]

-- Find the first player who hasn't played to the trick.
trickActive :: TrickState -> [Int]
trickActive ts =
  case filter check (rotateL theLeadPos [0..3]) of
    []     -> []
    (x:_)  -> [x]
  where check n = ts ^. (tablePos n) == Nothing
        theLeadPos = ts ^. leadPos
        rotateL n list = (drop n list) ++ (take n list)

hasSuit :: [Card] -> Suit -> Bool
hasSuit cards s = any ((== s) . suit) cards

trickLegal :: TrickView -> Int -> Card -> Bool
trickLegal tv pos card =
  case ledSuit tv of
    Just ls ->
      if hasSuit (tvHand tv) ls
      then (suit card) == ls
      else True
    Nothing -> True

data TrickResult = TrickResult {cardsPlayed :: Tup4 Card,
                                whoWon      :: Int,
                                points      :: Int} deriving (Eq, Show)

trickMakeView :: TrickState -> Int -> TrickView
trickMakeView ts@TrickState {..} n =
  TrickView {tvTrickNum = tsTrickNum,
             tvTable    = tsTable,
             tvLeadPos  = tsLeadPos,
             tvHand     = tsHands ^. (tup4 n)}

trickUpdate' :: TrickState -> Int -> Card -> State RNGState TrickState
trickUpdate' ts pos card =
  return $ ts & table . (tup4 pos) .~ (Just card)

trickUpdate :: TrickState -> [Int] -> [Card] -> State RNGState TrickState
trickUpdate ts [pos] [card] = trickUpdate' ts pos card
trickUpdate _ _ _ = error "trick play is strictly one-at-a-time"

-- TODO: implement a zipTraversable and bypass the list conversion.
maxIndex :: (Ord b, Traversable t) => (a -> b) -> t a -> Int
maxIndex f xs =
  snd $ maximum $ zip (map f (foldr (:) [] xs)) [0..]

winnerOfTrick :: Suit -> Tup4 Card -> Int
winnerOfTrick ledSuit cards =
  maxIndex score cards
  where honor = any (\(Card r s) -> r >= jack && s == ledSuit) cards
        score c = if (suit c) == ledSuit
                  then if honor && (rank c) == 2 then 100 else unRank (rank c)
                  else -100

pointValueOfTrick :: Bool -> Tup4 Card -> Int
pointValueOfTrick isFirst cards =
  (if isFirst then firstTrickValue else 0) + base
  where base = getSum $ foldMap (Sum . pointValue) cards

trickTerminal :: TrickState -> Maybe TrickResult
trickTerminal ts =
  if all (/= Nothing) (ts ^. table)
  then let cards = fmap fromJust $ ts ^. table
           (Just theLedSuit) = ledSuit ts in
       Just $ TrickResult {cardsPlayed = cards,
                           whoWon      = winnerOfTrick theLedSuit cards,
                           points      = pointValueOfTrick (ts ^. trickNum == 1) cards}
  else Nothing

trick :: Game Int TrickState TrickView Card TrickResult
trick = Game {allPlayers = const all4Players,
              makeView   = trickMakeView,
              active     = trickActive,
              legal      = trickLegal,
              update     = trickUpdate,
              terminal   = trickTerminal}
