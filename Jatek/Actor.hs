{-# LANGUAGE ExistentialQuantification, FlexibleInstances, FunctionalDependencies, KindSignatures, MultiParamTypeClasses #-}

import Data.Typeable

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

data System i m a b = System [(i, SomeActor m a b)]

send :: (Eq i) => System i m a b -> i -> a -> m b
send (System sys) i a =
  case lookup i sys of
    Just actor -> act actor a
    Nothing    -> error "No actor with given id."

purely :: (Monad m) => (a -> b) -> SomeActor m a b
purely f = SomeActor $ PureActor f

console :: (Show a, Read b) => SomeActor IO a b
console = SomeActor $ (ConsoleActor :: ConsoleActor IO a b)

sys0 :: System String IO Int Int
sys0 = System [("console", console),
               ("doubler", purely (*2))]

