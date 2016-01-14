{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

module Jatek.Interact where

import Control.Lens
import Control.Arrow (first, second)
import Control.Monad (ap, liftM)
import Control.Monad.State.Strict
import System.IO (hFlush, stdout)
import System.Random

import Jatek.Actor

-- InteractT is a monad transformer that includes two categories of state: RNG
-- state (Random) and user state (Stateful). This is to improve usability; it
-- doesn't give capabilities that a regular StateT wouldn't.

-- Its purpose is to represent, "a gamelike process, not necessarily tied to a
-- specific game". The "holy grail" is to be able to compose games together and
-- build larger games out of subgames ("mechanics").

-- i : player ID type (often Int).

-- u : user state. This shouldn't be folded into the monad m, because the
-- function liftI won't "lens" the state of a subgame into a larger game.

-- s : server message (e.g. a changing view from the game).
-- c : client message (e.g. a player's action).

-- m : monad being transformed. 
-- a : what the interaction returns. 

-- Related concept: every game has an associated State type and a View type. The
-- View type is what part of the game each player can see. A simple InteractT
-- use case might have the server sending views directly.

data InteractT i u c s m a =
  Terminal a |
  Talk [(i, s)] ([(i, c)] -> InteractT i u c s m a) |
  Stateful (u -> (InteractT i u c s m a, u)) |
  M (m (InteractT i u c s m a))

instance (Monad m, Show a, Show s, Show i) => Show (InteractT i u c s m a) where
  show (Terminal a) = "Terminal " ++ show a
  show (Talk msgs cont) = "Talk " ++ (show msgs) ++ " (λ)"
  show (Stateful cont) = "Stateful (λ)"
  show (M _) = "M (..)"

instance (Monad m) => Functor (InteractT i u c s m) where
  fmap = liftM

instance (Monad m) => Applicative (InteractT i u c s m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (InteractT i u c s m) where 
  return = Terminal
  (Terminal a)     >>= k = k a
  (Talk msgs cont) >>= k = Talk msgs (\cs -> (cont cs) >>= k)
  (Stateful cont)  >>= k = Stateful (\u -> first (flip (>>=) k) $ cont u)
  (M ma)           >>= k = M $ fmap (flip (>>=) k) ma

instance (Monad m) => MonadState u (InteractT i u c s m) where
  get    = Stateful (\u -> (Terminal u , u))
  put u  = Stateful (\_ -> (Terminal (), u))

instance MonadTrans (InteractT i u c s) where
  lift ma = M $ Terminal <$> ma

instance MonadIO (InteractT i u c s IO) where
  liftIO = lift

talk :: [(i, s)] -> ([(i, c)] -> InteractT i u c s m a) -> InteractT i u c s m a
talk = Talk

procure :: [i] -> (i -> s) -> (i -> c -> a) -> InteractT i u c s m [(i, a)]
procure ids sf cf =
  Talk (map (\i -> (i, sf i)) ids)
       (\ics -> Terminal $ map (\(i, c) -> (i, cf i c)) ics)

send :: [(i, s)] -> InteractT i u c s m ()
send msgs = Talk msgs (\_ -> Terminal ())

-- WARNING: In stateful monads, e.g. m = IO, this *can* side-effect the System.
-- That's by design. 
runInteractT :: (Monad m, Ord i) => InteractT i u c s m a -> System i m s c -> u -> m (a, u)
runInteractT intx system st =
   case intx of
     Terminal a -> return (a, st)
     Talk send cont ->
       sync system send >>= (\(intx1, sys1) -> runInteractT (cont intx1) sys1 st)
     Stateful cont ->
       let (intx1, st1) = cont st in runInteractT intx1 system st1
     M cont ->
       cont >>= (\intx1 -> runInteractT intx1 system st)

liftInteractT :: (Monad m, Ord i) =>
                 (c' -> c) -> (s -> s') -> Lens' u' u ->
                 InteractT i u c s m a -> InteractT i u' c' s' m a

liftInteractT cf sf uLens intx =
  case intx of
    Terminal a     -> Terminal a
    Talk send cont -> Talk (map (second sf) send)
                           (liftInteractT cf sf uLens . cont . map (second cf))
    Stateful cont  -> Stateful (\u' -> let (intx1, u1) = cont (view uLens u') in
                                 (liftInteractT cf sf uLens intx1, set uLens u1 u'))
    M cont         -> M $ fmap (liftInteractT cf sf uLens) cont
