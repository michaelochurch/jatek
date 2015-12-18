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

getRand :: InteractT i s v t m RNGState
getRand = Random $ \r -> (Terminal r, r)

setRand :: RNGState -> InteractT i s v t m ()
setRand r = Random $ \_ -> (Terminal (), r)

instance MonadTrans (InteractT i s v t) where
  lift ma = M $ Terminal <$> ma

instance MonadIO (InteractT i s v t IO) where
  liftIO = lift

send :: v -> [i] -> InteractT i s v t m ()
send view ids = Send view ids (Terminal ())

getActions :: (Monad m) => [i] -> InteractT i s v t m [t]
getActions ids = Await ids return

data Game i s v t a =
  Game {allPlayers :: s -> [i],
        makeView   :: s -> i -> v,
        
        active     :: s -> [i],
        legal      :: v -> i -> t -> Bool,

        update     :: s -> [i] -> [t] -> (State RNGState s),
        terminal   :: s -> Maybe a}

legalSt :: Game i s v t a -> s -> i -> t -> Bool
legalSt game@(Game {..}) st i act = (legal (makeView st i) i act)

sendViews :: (Monad m) => Game i s v t a -> s -> InteractT i s v t m ()
sendViews game@(Game {..}) st =
  forM_ (allPlayers st) $ \i -> send (makeView st i) [i]

-- pollPlayers handles the case where it gets illegal actions, but good-faith
-- clients should check for legality.

pollPlayers :: (Monad m) => Game i s v t a -> s -> InteractT i s v t m [t]
pollPlayers game@(Game {..}) st = do
  let is = active st
  as <- getActions is
  if all (\(i, a) -> legalSt game st i a) (zip is as)
    then return as
    else do -- Someone did something illegal. Poll again.
      --lift . logIt $ "Illegal action from client!!" 
      pollPlayers game st

interpretGame :: (Eq v, Monad m, Show v, Show a) => Game i s v t a -> InteractT i s v t m a
interpretGame game@(Game {..}) = do
  st <- get
  case terminal st of
    Just result -> return result
    Nothing     -> do
      let playerIds = active st
      actions   <- pollPlayers game st
      oldRand   <- getRand
      let (newSt, newRand) = (update st playerIds actions) `runState` oldRand
      setRand newRand
      put newSt
      sendViews game newSt
      interpretGame game

ioPlayerPrompt :: (Show i, Read t) => i -> IO t
ioPlayerPrompt pId = do
  putStr $ "[" ++ (show pId) ++ "] Action? "
  out <- read <$> getLine
  putStrLn ""
  return out

ioPlayer :: (Show v, Show i, Read t) => i -> Player i v t IO
ioPlayer pId = Player {playerId     = pId,
                       handleView   = print,
                       chooseAction = ioPlayerPrompt pId}

mkIoPlayers :: (Show v, Ord i, Show i, Read t) => [i] -> Players i v t IO
mkIoPlayers pIds = M.fromList $ map (\i -> (i, ioPlayer i)) pIds
