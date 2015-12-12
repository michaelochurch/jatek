{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module Games.Nim where

import Jatek.Core
import System.Random (randomRIO)

data NimGame = NimGame deriving (Show, Eq)

data NimPlayerId = First | Second deriving (Show, Eq)

other :: NimPlayerId -> NimPlayerId
other Second = First
other First = Second

data RandomNimPlayer = RandomNimPlayer

nimLimit :: Int
nimLimit = 3

instance Player IO NimGame RandomNimPlayer where
  play _ _ (n, id) myId =
    if myId == id then do
      let maxPlay = min nimLimit n
      play <- randomRIO (1, maxPlay)
      return (Just play)
    else return Nothing

nimLegality :: State NimGame -> NimPlayerId -> Maybe Int -> Bool
nimLegality (left, id) playerId Nothing  = id /= playerId
nimLegality (left, id) playerId (Just n) = id == playerId && n <= left

nimUpdate :: State NimGame -> [(PlayerId NimGame, Action NimGame)] -> State NimGame
nimUpdate (n, _) [(id, action)] =
  (n - action, other id)
nimUpdate _ _ = error "Illegal actions (exactly 1 active player required)."

instance Game NimGame where
  type Options NimGame   =  Int
  type State NimGame     = (Int, NimPlayerId)
  type PlayerId NimGame  =  NimPlayerId
  type Action NimGame    =  Int
  type Result NimGame    =  NimPlayerId
  type View NimGame      = (Int, NimPlayerId)  -- perfect information

  new _ n             = (n, First)

  terminal _ (0, player)  = Just (other player)
  terminal _ _            = Nothing

  allPlayers _ _ = [First, Second]

  view _ state _ = state

  isLegal _ = nimLegality
  update _  = nimUpdate
