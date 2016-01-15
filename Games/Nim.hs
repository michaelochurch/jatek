module Games.Nim where

import Jatek.Interact
import Jatek.Mechanic

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

nim :: Mechanic NimPlayerId NimState NimView NimAction NimResult
nim = Mechanic {players  = const [First, Second],
                makeView = \st _ -> st,
                active   = \(_, pId) -> [pId],
                legal    = nimLegal,
                update   = nimUpdate,
                terminal = nimTerminal}
