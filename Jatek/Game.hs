{-# LANGUAGE ExistentialQuantification, KindSignatures, RankNTypes, RecordWildCards #-}

module Jatek.Game where

import Control.Lens
import Control.Monad.State
import Jatek.Interact
import Jatek.Mechanic
import Jatek.Message

class GameLike (g :: * -> * -> * -> * -> * -> *) where
  runGameLike :: (Monad m, Ord i, Eq c, Eq s) => g i u c s a ->
                 InteractT i u (ClientMessage c) (ServerMessage s) m a

instance GameLike Mechanic where
  runGameLike = runMechanic

instance GameLike Game where
  runGameLike = runGame

data Continuation i u c s =
  forall u' s' c' a' g . (GameLike g, Eq s', Eq c') =>
  Continuation {stateLens :: Lens' u u',
                clientFn  :: c -> c',
                serverFn  :: s' -> s,
                subgame   :: g i u' c' s' a',
                fuse      :: u -> a' -> u}

data Next i u c s a = Finished a | Continue (Continuation i u c s)

data Game i u c s a =
  Game {initial :: u,
        pick    :: u -> Next i u c s a}

runGame :: (Monad m, Ord i) => Game i u c s a ->
                        InteractT i u (ClientMessage c) (ServerMessage s) m a
runGame game@(Game {..}) = do
  u <- get
  case pick u of
    Finished a         -> return a
    Continue c@(Continuation {..}) -> do
      a1 <- liftInteractT (fmap clientFn) (fmap serverFn)
                          stateLens (runGameLike subgame)
      u1 <- get
      put $ fuse u1 a1
      runGame game
