module Jatek.Interact where

import Control.Monad (ap, liftM)
import System.Random

type RNGState = StdGen

-- InteractT is a monad transformer that includes two categories of state: RNG
-- state (Random) and user state (Stateful). This is to improve usability; it
-- doesn't give capabilities that a regular StateT wouldn't.

-- Its purpose is to represent, "a gamelike process, not necessarily tied to a
-- specific game". The "holy grail" is to be able to compose games together and
-- build larger games out of subgames ("mechanics").

-- i : player ID type (often Int).
-- s : user state (game state). For convenience, RNG state is separate.
-- v : view. An associated type representing what a player can see. (Not all
-- state is visible, except in perfect information games.)
-- t : action. What players can do / send back to the game.
-- m : monad being transformed. Typically IO or Identity. Prefer user state in s
-- rather than a StateT. 
-- a : what the interaction returns. 

data InteractT i s v t m a =
  Terminal a |
  Send v [i] (InteractT i s v t m a) |
  Await [i] ([t] -> InteractT i s v t m a) |
  Random (RNGState -> ((InteractT i s v t m a), RNGState)) |
  Stateful (s -> ((InteractT i s v t m a), s)) | 
  M (m (InteractT i s v t m a))

instance (Monad m) => Functor (InteractT i s v t m) where
  fmap = liftM

instance (Monad m) => Applicative (InteractT i s v t m) where
  pure = return
  (<*>) = ap

bindStatelike :: (Monad m) => (s -> (m a, s)) -> (a -> m b) -> s -> (m b, s)
bindStatelike cont k s =
  let (ma, s1) = cont s in (ma >>= k, s1)

instance (Monad m) => Monad (InteractT i s v t m) where 
  return = Terminal
  (Terminal a)     >>= k = k a
  (Send v ps cont) >>= k = Send v ps (cont >>= k)
  (Await ps cont)  >>= k = Await ps (\ts -> (cont ts) >>= k)
  (Random cont)    >>= k = Random $ bindStatelike cont k
  (Stateful cont)  >>= k = Stateful $ bindStatelike cont k
  (M ma)           >>= k = M $ fmap (flip (>>=) k) ma
