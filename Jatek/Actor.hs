{-# LANGUAGE ExistentialQuantification, FlexibleInstances, FunctionalDependencies, KindSignatures, MultiParamTypeClasses, TupleSections #-}

module Jatek.Actor where

-- NOTE: this is not intended as a general-purpose Actor library. Its main
-- purpose is to reify (through System) abstractions like InteractT. 

import Control.Monad
import Data.IORef
import qualified Data.Map as M
import System.Random

-- NOTE: Actors return an updated copy of themselves, in order to support
-- stateless actors (i.e. actors that don't need m to be a stateful monad) but
-- they shouldn't change their impl. as a general rule. This is OOPy stuff that
-- exists to serve a very specific purpose and shouldn't be stretched beyond
-- that.

class Actor m a b self | self -> a, self -> b, self -> m where
   act :: self -> a -> m (b, (SomeActor m a b))

data SomeActor m a b = forall self . Actor m a b self => SomeActor self

instance Actor m a b (SomeActor m a b) where
  act (SomeActor actor) = act actor

data PureActor (m :: * -> *) a b = PureActor (a -> b)

instance (Monad m) => Actor m a b (PureActor m a b) where
  act self@(PureActor f) a = return $ (f a, SomeActor self)

data IOActor a b = IOActor (a -> IO (b, IOActor a b))

instance Actor IO a b (IOActor a b) where
  act (IOActor f) a = do
    (b, self) <- f a
    return $ (b, SomeActor self)

data ConsoleActor (m :: * -> *) a b = ConsoleActor

instance (Show a, Read b) => Actor IO a b (ConsoleActor IO a b) where
  act self@ConsoleActor a =
    let loop = do
          putStrLn $ show a
          input <- getLine
          case reads input of
            [(b, "")] -> return (b, SomeActor self)
            _ -> (putStrLn "Invalid Input!") >> loop
    in loop

data RandomIOActor = RandomIOActor (IORef StdGen)

instance Actor IO Int Int RandomIOActor where
  act self@(RandomIOActor cell) n = do
    gen <- readIORef cell
    let (a, gen1) = randomR (0, (n - 1)) gen
    writeIORef cell gen1
    return (a, SomeActor self)

data System i m a b = System (M.Map i (SomeActor m a b))

sendMessage :: (Monad m, Ord i) => System i m a b -> i -> a -> m (b, System i m a b)
sendMessage (System sys) i a =
  case M.lookup i sys of
    Just actor -> do
      (b, actor') <- act actor a
      return (b, System $ M.insert i actor' sys)
    Nothing    -> error "No actor with given id."

purely :: (Monad m) => (a -> b) -> SomeActor m a b
purely f = SomeActor $ PureActor f

console :: (Show a, Read b) => SomeActor IO a b
console = SomeActor $ (ConsoleActor :: ConsoleActor IO a b)

randActor :: Int -> IO (SomeActor IO Int Int)
randActor seed = do
  let gen = mkStdGen seed
  cell <- newIORef gen
  return $ SomeActor $ (RandomIOActor cell)

-- TODO: parallel version of sync (when we have long sys calls for networked
-- server stuff).
sync :: (Monad m, Ord i) => System i m a b -> [(i, a)] -> m ([(i, b)], System i m a b)
sync system msgs = loop [] system msgs
  where loop acc sys []             = return (acc, sys)
        loop acc sys ((i, a):msgs') = do
          (b, sys') <- sendMessage sys i a
          loop (acc ++ [(i, b)]) sys' msgs' 

sys0 :: IO (System String IO Int Int)
sys0 = do
  rand <- randActor 1337
  return $ System $ M.fromList [("console", console),
                                ("doubler", purely (*2)),
                                ("random",  rand)]

actorDemo :: IO ()
actorDemo = do
  sys <- sys0
  (res1, sys1) <- sync sys [("console", 1), ("doubler", 4), ("random", 20)]
  (res2, sys2) <- sync sys1 [("doubler", 21), ("random", 20)]
  print res1
  print res2
