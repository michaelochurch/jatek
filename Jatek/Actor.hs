{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}

class Actor m a b self where
   act :: self -> a -> m b

data SomeActor m a b = forall self . Actor m a b self => SomeActor self
