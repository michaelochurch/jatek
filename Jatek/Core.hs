{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}

module Jatek.Core where

import Control.Arrow (first)
import Control.Monad (foldM, forM, forM_, join, when)
import Control.Monad.State.Strict
import Control.Monad.Trans
import qualified Data.Map as M
import System.Random

-- InteractT is "a gamelike process, not necessarily tied to a specific game."
-- Can send Events, await Actions. Has an RNG. Has user state. Lives in a monad. 

-- i : player ID type (often Int).
-- s : user state (game state). Keep general if possible.
-- v : view. An associated type representing what a player can see. (Not all
-- state is visible, except in perfect information games.)
-- t : action. What players can do / send back to the game.
-- m : monad being transformed. Typically IO or Identity. Prefer user state in s
-- rather than a StateT. 
-- a : what the interaction returns. 

type RNGState = StdGen

data InteractT i s v t m a =
  Terminal a |
  Send v [i] (InteractT i s v t m a) |
  Await [i] ([t] -> InteractT i s v t m a) |
  Random (RNGState -> ((InteractT i s v t m a), RNGState)) |
  Stateful (s -> ((InteractT i s v t m a), s)) | 
  M (m (InteractT i s v t m a))

instance (Monad m) => Functor (InteractT i s v t m) where
  fmap = liftM

instance (Monad m) => Applicative (InteractT i s v t m) where
  pure = return
  (<*>) = ap

magic :: (Monad m) => (s -> (m a, s)) -> (a -> m b) -> s -> (m b, s)
magic cont k s =
  let (ma, s1) = cont s in (ma >>= k, s1)

instance (Monad m) => Monad (InteractT i s v t m) where 
  return = Terminal
  (Terminal a)     >>= k = k a
  (Send v ps cont) >>= k = Send v ps (cont >>= k)
  (Await ps cont)  >>= k = Await ps (\ts -> (cont ts) >>= k)
  (Random cont)    >>= k = Random $ magic cont k
  (Stateful cont)  >>= k = Stateful $ magic cont k
  (M ma)           >>= k = M $ fmap (flip (>>=) k) ma

rollDie :: Int -> InteractT i s v t m Int
rollDie sides =
  Random (\rng -> let (a, rng1) = randomR (1, sides) rng in (Terminal a, rng1))

data Player i v t m =
  Player {playerId     :: i,
          handleView   :: v -> m (),
          chooseAction :: m t}

type Players i v t m = M.Map i (Player i v t m)

runInteractT :: (Ord i, Show i, Monad m) => InteractT i s v t m a -> StdGen -> s -> (Players i v t m) -> m a
runInteractT remt r0 s0 players =
  case remt of
    Terminal a -> return a
    Send view ps cont -> do
      forM_ ps $ \i ->
        case M.lookup i players of
          Just player -> handleView player view
          Nothing     -> badPlayerId i
      runInteractT cont r0 s0 players
    Await ps cont -> do
      actions <- forM ps $ \i ->
        case M.lookup i players of
          Just player -> chooseAction player
          Nothing     -> badPlayerId i
      runInteractT (cont actions) r0 s0 players
    Random cont ->
      let (remt1, r1) = cont r0 in
      runInteractT remt1 r1 s0 players
    Stateful cont ->
      let (remt1, s1) = cont s0 in
      runInteractT remt1 r0 s1 players
    M cont ->
      cont >>= (\x -> runInteractT x r0 s0 players)
    where badPlayerId i = error $ "given bad player ID of " ++ (show i)
  
instance (Monad m) => MonadState s (InteractT i s v t m) where
  get     = Stateful $ \s -> (Terminal s, s)
  put s   = Stateful $ \_ -> (Terminal (), s)
  state f = Stateful $ \s -> first Terminal (f s)

instance MonadTrans (InteractT i s v t) where
  lift ma = M $ Terminal <$> ma

instance MonadIO (InteractT i s v t IO) where
  liftIO = lift

send :: v -> [i] -> InteractT i s v t m ()
send view ids = Send view ids (Terminal ())

getActions :: (Monad m) => [i] -> InteractT i s v t m [t]
getActions ids = Await ids return

-- det :: (s -> [i] -> [t]) -> (s -> [i] -> [t] -> RNGState -> (s, RNGState))
-- det f s0 is as rng =  ((f s0 is as), rng)

type Actions i t = M.Map i t

-- --data PostUpdate s = Ready s | AwaitRandom s

data Game i s v t m a =
  Game {init       :: RNGState -> s,
        allPlayers :: s -> [i],
        makeView   :: s -> i -> v,
        
        active     :: s -> [i],
        legal      :: v -> i -> t -> Bool,

        update     :: s -> [i] -> [t] -> RNGState -> (s, RNGState),
        terminal   :: s -> Maybe a,
        logIt      :: String -> m ()}

legalSt :: Game i s v t m a -> s -> i -> t -> Bool
legalSt game@(Game {..}) st i act = (legal (makeView st i) i act)

sendViews :: (Monad m) => Game i s v t m a -> s -> InteractT i s v t m ()
sendViews game@(Game {..}) st =
  forM_ (allPlayers st) $ \i -> send (makeView st i) [i]

pollPlayers :: (Monad m) => Game i s v t m a -> s -> InteractT i s v t m [t]
pollPlayers game@(Game {..}) st = do
  let is = active st
  as <- getActions is
  if all (\(i, a) -> legalSt game st i a) (zip is as)
    then return as
    else do -- Someone did something illegal. Poll again.
      lift . logIt $ "Illegal action from client!!" 
      pollPlayers game st

ignore :: Monad m => a -> m ()
ignore = const (return ())



-- interpretGame :: (Eq v, Monad m, Show v) => Game i s v t m a -> InteractT i s v t m a
-- interpretGame game@(Game {..}) = do
--   st <- get
--   case terminal st of
--     Just result -> (logIt $ "Finished with result " ++ result) >> return result
--     Nothing     -> do
--       sendViews st (active st)
--       undefined


-- -- interpretGame :: (Monad m) => Game s v e t m a -> PlayT s e t m a
-- -- interpretGame game@(Game {..}) = do
-- --   st <- getUserState
-- --   case terminal st of
-- --     Just result  -> return result
-- --     Nothing      -> undefined

-- -- case terminal st of
--   --   Just a -> return a
--   --   Nothing -> undefined

-- -- case terminal st of
--   --   Just a -> M (onLoggable $ LResult a) >> (return a)
--   --   Nothing -> undefined
  
  

-- type NimState = (Int, PlayerId)

-- other :: PlayerId -> PlayerId
-- other n = (n + 1) `rem` 2

-- type NimView = NimState

-- data NimEvent = Change NimView | Winner PlayerId

-- data NimAction = Take Int

-- nimGame :: (Monad m) => Int -> Game NimState NimView NimEvent NimAction m PlayerId
-- nimGame n = Game {init = return (n, 0)}





-- class Game a where
--   type Options a
--   type State a
--   type View a
--   type PlayerId a
--   type Result a
--   type Action a
  
--   new :: a -> Options a    -> State a
--   terminal :: a -> State a -> Maybe (Result a)
--   allPlayers :: a -> State a -> [PlayerId a]
--   view :: a -> State a -> PlayerId a -> View a
--   isLegal :: a -> View a -> PlayerId a -> Maybe (Action a) -> Bool

--   -- players must know legality of an Action based on the View they have.
--   -- TODO: spell the contract b/w isLegal, view, and isLegalSt out formally.
  
--   isLegalSt :: a -> State a -> PlayerId a -> Maybe (Action a) -> Bool
--   isLegalSt g s id a = isLegal g (view g s id) id a
  
--   update :: a -> State a -> [(PlayerId a, Action a)] -> State a

-- type Turn a    = [(PlayerId a, Action a)]
-- type History a = [(State a, Turn a)]

-- legalityCheck :: (Game g, Eq (PlayerId g)) =>
--                  g -> (State g) -> [(PlayerId g, Action g)] -> Bool
-- legalityCheck game state actions =
--   all check (allPlayers game state)
--   where check playerId = isLegalSt game state playerId (lookup playerId actions)

-- class (Game g, Monad m) => Player m g a where
--   play :: a -> g -> View g -> PlayerId g -> m (Maybe (Action g))

-- collectActions :: (Game g, Monad m, Player m g p) => g -> State g -> [(PlayerId g, p)] -> m [(PlayerId g, Action g)]
-- collectActions game state players =
--   foldM f [] players
--   where f actions (playerId, player) = do
--           let v = view game state playerId
--           action <- play player game v playerId
--           case action of
--             Nothing -> return actions
--             Just a  -> return ((playerId, a):actions)

-- fullGame :: (Game g, Monad m, Player m g p, Eq (PlayerId g)) =>
--             g -> Options g -> [(PlayerId g, p)] -> Bool -> m (Result g, History g)
-- fullGame game opts players checkLegality =
--   loop [] initState
--   where initState = new game opts
--         loop acc state =
--           case terminal game state of
--             Just result -> return (result, acc)
--             Nothing -> do
--               actions <- collectActions game state players
--               when checkLegality $
--                 if legalityCheck game state actions
--                 then return ()
--                 else error "illegal action"
--               let newState = update game state actions
--                   newAcc   = (state,actions):acc
--               loop newAcc newState

