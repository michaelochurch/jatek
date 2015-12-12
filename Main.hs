{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

import Control.Monad (foldM, when)
import System.Random (randomRIO)

class Game a where
  type Options a
  type State a
  type View a
  type PlayerId a
  type Result a
  type Action a
  
  new :: a -> Options a    -> State a
  terminal :: a -> State a -> Maybe (Result a)
  allPlayers :: a -> State a -> [PlayerId a]
  view :: a -> State a -> PlayerId a -> View a
  isLegal :: a -> View a -> PlayerId a -> Maybe (Action a) -> Bool

  -- players must know legality of an Action based on the View they have.
  -- TODO: spell the contract b/w isLegal, view, and isLegalSt out formally.
  
  isLegalSt :: a -> State a -> PlayerId a -> Maybe (Action a) -> Bool
  isLegalSt g s id a = isLegal g (view g s id) id a
  
  update :: a -> State a -> [(PlayerId a, Action a)] -> State a

type Turn a    = [(PlayerId a, Action a)]
type History a = [(State a, Turn a)]

legalityCheck :: (Game g, Eq (PlayerId g)) =>
                 g -> (State g) -> [(PlayerId g, Action g)] -> Bool
legalityCheck game state actions =
  all check (allPlayers game state)
  where check playerId = isLegalSt game state playerId (lookup playerId actions)

class (Game g, Monad m) => Player m g a where
  play :: a -> g -> View g -> PlayerId g -> m (Maybe (Action g))

collectActions :: (Game g, Monad m, Player m g p) => g -> State g -> [(PlayerId g, p)] -> m [(PlayerId g, Action g)]
collectActions game state players =
  foldM f [] players
  where f actions (playerId, player) = do
          let v = view game state playerId
          action <- play player game v playerId
          case action of
            Nothing -> return actions
            Just a  -> return ((playerId, a):actions)

fullGame :: (Game g, Monad m, Player m g p, Eq (PlayerId g)) =>
            g -> Options g -> [(PlayerId g, p)] -> Bool -> m (Result g, History g)
fullGame game opts players checkLegality =
  loop [] initState
  where initState = new game opts
        loop acc state =
          case terminal game state of
            Just result -> return (result, acc)
            Nothing -> do
              actions <- collectActions game state players
              when checkLegality $
                if legalityCheck game state actions
                then return ()
                else error "illegal action"
              let newState = update game state actions
                  newAcc   = (state,actions):acc
              loop newAcc newState



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



-- Play a nim game
main :: IO ()
main = do
  let players = [(First, RandomNimPlayer), (Second, RandomNimPlayer)]
  (result, history) <- fullGame NimGame 20 players False
  putStrLn $ "Result: " ++ show result
  putStrLn $ "History: " ++ show history
  
