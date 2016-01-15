{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Jatek.Game where

import Control.Lens
import Jatek.Mechanic

data Continuation i u c s = forall u' s' c' a' . Continuation {
  stateLens :: Lens' u u',
  clientFn  :: c' -> c,
  serverFn  :: s -> s',
  mechanic  :: Mechanic i u' s' c' a',
  fuse      :: u -> a' -> u
}

data Next i u c s a = Finished a | Continue (Continuation i u c s)

data Game i u c s a =
  Game {pick :: u -> Next i u c s a}
