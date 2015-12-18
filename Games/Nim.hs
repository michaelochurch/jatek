module Games.Nim where

import Control.Monad.State.Strict
import System.Random

import Jatek.Core

data NimPlayerId = First | Second deriving (Eq, Ord, Show, Read)

nextPlayer :: NimPlayerId -> NimPlayerId
nextPlayer First  = Second
nextPlayer Second = First

type NimState = (Int, NimPlayerId)

type NimView = NimState -- perfect information

data NimAction = Take Int deriving (Eq, Show, Read)

type NimResult = NimPlayerId -- who won

nimMaxTake :: Int
nimMaxTake = 4

nimLegal :: NimView -> NimPlayerId -> NimAction -> Bool
nimLegal (nLeft, activePId) pId (Take n) =
  activePId == pId && n <= (min nLeft nimMaxTake)

nimUpdate :: NimState -> [NimPlayerId] -> [NimAction] -> State RNGState NimState
nimUpdate (tokens, pId) [pId'] [(Take n)] | pId == pId' =
  return $ (tokens - n, nextPlayer pId)
nimUpdate _ _ _ = error "Illegal action configuration."

nimTerminal (0, pId) = Just $ nextPlayer pId
nimTerminal _        = Nothing

nim :: Game NimPlayerId NimState NimView NimAction NimResult
nim = Game {allPlayers    = const [First, Second],
            makeView      = \st _ -> st,

            active        = \(_, pId) ->  [pId],
            legal         = nimLegal,
            
            update        = nimUpdate,
            terminal      = nimTerminal}
