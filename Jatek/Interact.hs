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
-- u : user state (game state). For convenience, RNG state is separate.

-- s : server message (e.g. a changing view from the game).
-- c : client message (e.g. a player's action).

-- m : monad being transformed. Typically IO or Identity. Prefer user state in s
-- rather than a StateT. 
-- a : what the interaction returns. 

-- Related concept: every game has an associated State type and a View type. The
-- View type is what part of the game each player can see. A simple InteractT
-- use case might have the server sending views directly.

data InteractT i u s c m a =
  Terminal a |
  Send s [i] (InteractT i u s c m a) |
  Await [i] ([c] -> InteractT i u s c m a) |
  Random (RNGState -> ((InteractT i u s c m a), RNGState)) |
  Stateful (u -> ((InteractT i u s c m a), u)) | 
  M (m (InteractT i u s c m a))

instance (Monad m) => Functor (InteractT i u s c m) where
  fmap = liftM

instance (Monad m) => Applicative (InteractT i u s c m) where
  pure = return
  (<*>) = ap

bindStatelike :: (Monad m) => (s -> (m a, s)) -> (a -> m b) -> s -> (m b, s)
bindStatelike cont k s =
  let (ma, s1) = cont s in (ma >>= k, s1)

instance (Monad m) => Monad (InteractT i u s c m) where 
  return = Terminal
  (Terminal a)     >>= k = k a
  (Send v ps cont) >>= k = Send v ps (cont >>= k)
  (Await ps cont)  >>= k = Await ps (\ts -> (cont ts) >>= k)
  (Random cont)    >>= k = Random $ bindStatelike cont k
  (Stateful cont)  >>= k = Stateful $ bindStatelike cont k
  (M ma)           >>= k = M $ fmap (flip (>>=) k) ma
