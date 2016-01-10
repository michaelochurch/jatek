{-# LANGUAGE ExistentialQuantification, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}

class Actor m a b self where
   act :: self -> a -> m b

data SomeActor m a b = forall self . Actor m a b self => SomeActor self

data PureActor (m :: * -> *) a b = PureActor (a -> b)

instance (Monad m) => Actor m a b (PureActor m a b) where
  act (PureActor f) a = return $ f a
