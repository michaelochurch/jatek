{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}

module Jatek.Core where

import Control.Arrow (first)
import Control.Monad (foldM, forM, forM_, join, when)
import Control.Monad.State.Strict
import Control.Monad.Trans
import qualified Data.Map as M
import System.IO (hFlush, stdout)
import System.Random

import Jatek.Interact

-- note: the function `legal` takes a view rather than a state, because a player
-- needs to know what actions are legal _without_ access to the (not visible to
-- that player) entire game state.

data Game i s v t a =
  Game {allPlayers :: s -> [i],
        makeView   :: s -> i -> v,
        
        active     :: s -> [i],
        legal      :: v -> i -> t -> Bool,

        update     :: s -> [i] -> [t] -> (State RNGState s),
        terminal   :: s -> Maybe a}

legalSt :: Game i s v t a -> s -> i -> t -> Bool
legalSt game@(Game {..}) st i act = (legal (makeView st i) i act)

sendViews :: (Monad m) => Game i s v t a -> s -> InteractT i s t v m ()
sendViews game@(Game {..}) st =
  forM_ (allPlayers st) $ \i -> send (makeView st i) [i]

-- pollPlayers handles the case where it gets illegal actions, but good-faith
-- clients should check for legality.

pollPlayers :: (Monad m) => Game i s v t a -> s -> InteractT i s t v m [t]
pollPlayers game@(Game {..}) st = do
  let is = active st
  as <- getActions is
  if all (\(i, a) -> legalSt game st i a) (zip is as)
    then return as
    else do -- Someone did something illegal. Poll again.
      pollPlayers game st

interpretGame :: (Eq v, Monad m, Show v, Show a) => Game i s v t a -> InteractT i s t v m a
interpretGame game@(Game {..}) = do
  st <- get
  sendViews game st
  case terminal st of
    Just result -> return result
    Nothing     -> do
      let playerIds = active st
      actions   <- pollPlayers game st
      oldRand   <- getRand
      let (newSt, newRand) = (update st playerIds actions) `runState` oldRand
      putRand newRand
      put newSt
      interpretGame game
