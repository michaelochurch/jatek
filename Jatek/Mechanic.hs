{-# LANGUAGE RecordWildCards #-}

module Jatek.Mechanic where

import Control.Monad.State
import qualified Data.Map as M
import System.IO

import Jatek.Actor
import Jatek.Interact
import Jatek.Message

data Mechanic i s t v a =
  Mechanic {players  :: s -> [i],
            makeView :: s -> i -> v,

            active   :: s -> [i],
            legal    :: v -> i -> t -> Bool,

            update   :: s -> [i] -> [t] -> s,
            terminal :: s -> Maybe a}

collect :: [Maybe a] -> [a]
collect = foldr (\xOpt acc -> maybe acc (:acc) xOpt) []

handleClients :: (Monad m, Ord i) =>
                 Mechanic i s t v a -> s ->
                 InteractT i u (ClientMessage t) (ServerMessage v) m (M.Map i t)
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
            let outcomes = map (\(i, (TryAction t)) ->
                                 if legal (makeView s i) i t
                                 then (i, AcceptAction, t)
                                 else (i, RejectAction, t)) clientMsgs
                h (i, AcceptAction, t) = Just (i, t)
                h (i, _           , t) = Nothing
                newAcc    = M.union acc $ M.fromList (collect $ map h outcomes)
                outMsgs   = map (\(i, r, _) -> (i, r)) outcomes
            send outMsgs
            loop newAcc

sendUpdates :: (Eq v) => Mechanic i s t v a -> s -> s ->
               InteractT i s (ClientMessage t) (ServerMessage v) m ()
sendUpdates mx@(Mechanic {..}) sOld sNew = send msgs
  where msgs = collect $ map f (players sOld)
        f i  = let vNew = makeView sNew i
               in if (vNew /=  makeView sOld i) then Just (i, ViewChanged vNew)
                  else Nothing

runMechanic :: (Monad m, Ord i, Eq v) => Mechanic i s t v a ->
               InteractT i s (ClientMessage t) (ServerMessage v) m a
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

mkConsoleClient :: (Show v) => (String -> Maybe t) -> String -> SomeActor IO (ServerMessage v) (ClientMessage t)
mkConsoleClient parser tag =
  SomeActor $ standardIOActor goConsole
  where goConsole serverMsg = do
          putStrLn $ "[" ++ tag ++ "] " ++ (show serverMsg)
          hFlush stdout
          case serverMsg of
            NeedAction _ ->
              let loop = do
                           line <- getLine
                           case parser line of
                             Just t  -> return $ TryAction t
                             Nothing -> do
                               putStrLn "I can't parse that."
                               loop
              in loop
            RejectAction -> putStrLn "That's not a legal move." >> return Ok
            _            -> return Ok

mkConsoleSystem :: (Show v, Ord i) =>
                   (String -> Maybe t) -> [i] -> (i -> String) ->
                   System i IO (ServerMessage v) (ClientMessage t)
mkConsoleSystem parser ids tag =
  System $ M.fromList (map (\i -> (i, mkConsoleClient parser (tag i))) ids)
