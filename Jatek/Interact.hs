{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

module Jatek.Interact where

import Control.Lens
import Control.Arrow (first)
import Control.Monad (ap, liftM)
import Control.Monad.State.Strict
import System.IO (hFlush, stdout)
import System.Random

import Jatek.Actor

type RNGState = StdGen

-- InteractT is a monad transformer that includes two categories of state: RNG
-- state (Random) and user state (Stateful). This is to improve usability; it
-- doesn't give capabilities that a regular StateT wouldn't.

-- Its purpose is to represent, "a gamelike process, not necessarily tied to a
-- specific game". The "holy grail" is to be able to compose games together and
-- build larger games out of subgames ("mechanics").

-- i : player ID type (often Int).

-- s : server message (e.g. a changing view from the game).
-- c : client message (e.g. a player's action).

-- m : monad being transformed. 
-- a : what the interaction returns. 

-- Related concept: every game has an associated State type and a View type. The
-- View type is what part of the game each player can see. A simple InteractT
-- use case might have the server sending views directly.

data InteractT i c s m a =
  Terminal a |
  Talk [(i, s)] ([(i, c)] -> InteractT i c s m a) |
  M (m (InteractT i c s m a))

instance (Monad m, Show a, Show s, Show i) => Show (InteractT i c s m a) where
  show (Terminal a) = "Terminal " ++ show a
  show (Talk msgs cont) = "Talk " ++ (show msgs) ++ " (λ)"
  show (M _) = "M (..)"

instance (Monad m) => Functor (InteractT i c s m) where
  fmap = liftM

instance (Monad m) => Applicative (InteractT i c s m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (InteractT i c s m) where 
  return = Terminal
  (Terminal a)     >>= k = k a
  (Talk msgs cont) >>= k = Talk msgs (\cs -> (cont cs) >>= k)
  (M ma)           >>= k = M $ fmap (flip (>>=) k) ma

instance MonadTrans (InteractT i c s) where
  lift ma = M $ Terminal <$> ma

instance MonadIO (InteractT i c s IO) where
  liftIO = lift

runInteractT :: (Monad m, Eq i) => InteractT i c s m a -> System i m s c -> m a
runInteractT intx system =
   case intx of
     Terminal a -> return a
     Talk send cont ->
       sync system send >>= (\intx1 -> runInteractT (cont intx1) system)
     M cont ->
       cont >>= (\intx1 -> runInteractT intx1 system)



-- testInteractT :: (Read c, Show c, Show s, Show i) =>
--                  InteractT i c s IO a -> IO a
-- testInteractT remt = runInteractT remt consoleClient
