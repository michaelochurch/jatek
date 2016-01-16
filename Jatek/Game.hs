{-# LANGUAGE ExistentialQuantification, KindSignatures, RankNTypes, RecordWildCards #-}

module Jatek.Game where

import Control.Lens
import Jatek.Interact
import Jatek.Mechanic

class GameLike (g :: * -> * -> * -> * -> * -> *)

instance GameLike Mechanic
instance GameLike Game

data Continuation i u c s =
  forall u' s' c' a' g . (GameLike g) =>
  Continuation {stateLens :: Lens' u u',
                clientFn  :: c -> c',
                serverFn  :: s' -> s,
                subgame   :: g i u' c' s' a',
                fuse      :: u -> a' -> u}

data Next i u c s a = Finished a | Continue (Continuation i u c s)

data Game i u c s a =
  Game {initial :: u,
        pick    :: u -> Next i u c s a}

data GameServerMsg s
data GameClientMsg c

runGame :: (Monad m) => Game i u c s a ->
                        InteractT i u (GameClientMsg c) (GameServerMsg s) m a
runGame game@(Game {..}) =
  loop initial
  where loop st =
          case pick st of
            Finished a     -> return a
            Continue cont  -> undefined
