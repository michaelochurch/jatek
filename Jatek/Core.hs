{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module Jatek.Core where

import Control.Monad (foldM, when)

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

