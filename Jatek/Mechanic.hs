{-# LANGUAGE RecordWildCards #-}

module Jatek.Mechanic where

import Control.Monad.State
import qualified Data.Map as M

import Jatek.Actor
import Jatek.Interact

data ServerMessage v t = ViewChanged v | NeedAction v | LegalActionIs t |
                         AcceptAction | RejectAction deriving Show

data ClientMessage t = TryAction t | WantLegalAction deriving Show

data Mechanic i s v t a =
  Mechanic {players  :: s -> [i],
            makeView :: s -> i -> v,

            active   :: s -> [i],
            legal    :: v -> i -> t -> Bool,
            legal1   :: v -> i -> t,

            update   :: s -> [i] -> [t] -> s,
            terminal :: s -> Maybe a}

collect :: [Maybe a] -> [a]
collect = foldr (\xOpt acc -> maybe acc (:acc) xOpt) []

handleClients :: (Monad m, Ord i) =>
                 Mechanic i s v t a -> s ->
                 InteractT i u (ClientMessage t) (ServerMessage v t) m (M.Map i t)
handleClients mx@(Mechanic {..}) s =
  loop (M.empty)
  where involved = active s
        n        = length involved
        loop acc =
          if M.size acc == n
          then return acc
          else do
            let needActionIds = filter (not . flip M.member acc) involved
            clientMsgs <- procure needActionIds
                            (NeedAction . makeView s) (\_i c -> c)
            let outcomes = map (\(i, cm) ->
                                  case cm of
                                    WantLegalAction ->
                                      let act = legal1 (makeView s i) i
                                      in (i, LegalActionIs act, act) 
                                    TryAction t ->
                                      if legal (makeView s i) i t
                                      then (i, AcceptAction, t)
                                      else (i, RejectAction, t)) clientMsgs
                h (i, AcceptAction, t) = Just (i, t)
                h (i, _           , t) = Nothing
                newAcc    = M.union acc $ M.fromList (collect $ map h outcomes)
                outMsgs   = map (\(i, r, _) -> (i, r)) outcomes
            send outMsgs
            loop newAcc

sendUpdates :: (Eq v) => Mechanic i s v t a -> s -> s ->
               InteractT i s (ClientMessage t) (ServerMessage v t) m ()
sendUpdates mx@(Mechanic {..}) sOld sNew = send msgs
  where msgs = collect $ map f (players sOld)
        f i  = let vNew = makeView sNew i
               in if (vNew /=  makeView sOld i) then Just (i, ViewChanged vNew)
                  else Nothing

runMechanic :: (Monad m, Ord i, Eq v) => Mechanic i s v t a ->
               InteractT i s (ClientMessage t) (ServerMessage v t) m a
runMechanic mx = do
  u <- get
  case terminal mx u of
    Just a  -> return a
    Nothing -> do
      actions <- handleClients mx u
      let u1 = update mx u (M.keys actions) (M.elems actions)
      sendUpdates mx u u1 
      put u1    
      runMechanic mx
