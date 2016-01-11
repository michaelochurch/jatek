{-# LANGUAGE ExistentialQuantification, FlexibleInstances, FunctionalDependencies, KindSignatures, MultiParamTypeClasses, TupleSections #-}

module Jatek.Actor where

-- NOTE: this is not intended as a general-purpose Actor library. Its main
-- purpose is to reify (through System) abstractions like InteractT. 

import Control.Monad
import Data.IORef
import System.Random

class Actor m a b self | self -> a, self -> b, self -> m where
   act :: self -> a -> m b

data SomeActor m a b = forall self . Actor m a b self => SomeActor self

instance Actor m a b (SomeActor m a b) where
  act (SomeActor actor) = act actor

data PureActor (m :: * -> *) a b = PureActor (a -> b)

instance (Monad m) => Actor m a b (PureActor m a b) where
  act (PureActor f) a = return $ f a

data IOActor a b = IOActor (a -> IO b)

instance Actor IO a b (IOActor a b) where
  act (IOActor f) a = f a

data ConsoleActor (m :: * -> *) a b = ConsoleActor

instance (Show a, Read b) => Actor IO a b (ConsoleActor IO a b) where
  act ConsoleActor a =
    let loop = do
          putStrLn $ show a
          input <- getLine
          case reads input of
            [(b, "")] -> return b
            _ -> (putStrLn "Invalid Input!") >> loop
    in loop

data RandomIOActor = RandomIOActor (IORef StdGen)

instance Actor IO Int Int RandomIOActor where
  act (RandomIOActor cell) n = do
    gen <- readIORef cell
    let (a, gen1) = randomR (0, (n - 1)) gen
    writeIORef cell gen1
    return a

data System i m a b = System [(i, SomeActor m a b)]

sendMessage :: (Eq i) => System i m a b -> i -> a -> m b
sendMessage (System sys) i a =
  case lookup i sys of
    Just actor -> act actor a
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
sync :: (Monad m, Eq i) => System i m a b -> [(i, a)] -> m [(i, b)]
sync system msgs =
  forM msgs $ \(i, a) -> fmap (i,) (sendMessage system i a)

sys0 :: IO (System String IO Int Int)
sys0 = do
  rand <- randActor 1337
  return $ System [("console", console),
                   ("doubler", purely (*2)),
                   ("random",  rand)]

actorDemo = do
  sys <- sys0
  sync sys [("console", 1), ("doubler", 4), ("random", 20)] >>= print
  sync sys [("doubler", 21), ("random", 20)] >>= print

