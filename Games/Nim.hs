{-# LANGUAGE TemplateHaskell #-}

module Games.Nim where

import Control.Lens
import qualified Data.Map as M

import Jatek.Interact
import Jatek.Mechanic
import Jatek.Game

data NimPlayerId = First | Second deriving (Eq, Ord, Show, Read)

nextPlayer :: NimPlayerId -> NimPlayerId
nextPlayer First  = Second
nextPlayer Second = First

type NimState = (Int, NimPlayerId)

type NimView = NimState -- perfect information game

data NimAction = Take Int deriving (Eq, Show, Read)

type NimResult = NimPlayerId -- who won

nimMaxTake :: Int
nimMaxTake = 4

nimLegal :: NimView -> NimPlayerId -> NimAction -> Bool
nimLegal (nLeft, activePId) pId (Take n) =
  activePId == pId && 0 < n && n <= (min nLeft nimMaxTake)

nimUpdate :: NimState -> [NimPlayerId] -> [NimAction] -> NimState
nimUpdate (tokens, pId) [pId'] [(Take n)] | pId == pId' =
  (tokens - n, nextPlayer pId)
nimUpdate _ _ _ = error "Illegal action configuration."

nimTerminal (0, pId) = Just $ nextPlayer pId
nimTerminal _        = Nothing

nim :: Mechanic NimPlayerId NimState NimAction NimView NimResult
nim = Mechanic {players  = const [First, Second],
                makeView = \st _ -> st,
                active   = \(_, pId) -> [pId],
                legal    = nimLegal,
                update   = nimUpdate,
                terminal = nimTerminal}

data NimTo3State = NimTo3State {_scores       :: M.Map NimPlayerId Int,
                                _currentRound :: NimState} deriving (Eq, Show)

$(makeLenses ''NimTo3State)

type NimTo3View   = NimTo3State
type NimTo3Result = M.Map NimPlayerId Int

nimTo3 :: Game NimPlayerId NimTo3State NimAction NimTo3View NimTo3Result
nimTo3 = Game initState pickFn
  where reset            = (16, First)
        initState        = NimTo3State (M.fromList [(First, 0), (Second, 0)]) reset
        finished         = M.fold (\a b -> b || (a >= 3)) False
        fuseFn st winner = st & scores %~ M.unionWith (+) (M.singleton winner 1)
                              & currentRound .~ reset
        pickFn st        =
          if finished (st ^. scores)
          then Finished $ st ^. scores
          else Continue (Continuation {stateLens = currentRound,
                                       clientFn  = id,
                                       serverFn  = NimTo3State (st ^. scores),
                                       subgame   = nim,
                                       fuse      = fuseFn})

