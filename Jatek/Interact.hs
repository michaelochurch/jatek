{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}

module Jatek.Interact where

import Control.Lens
import Control.Arrow (first)
import Control.Monad (ap, liftM)
import Control.Monad.State.Strict
import System.IO (hFlush, stdout)
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

data InteractT i u c s m a =
  Terminal a |
  Send s [i] (InteractT i u c s m a) |
  Await [i] ([c] -> InteractT i u c s m a) |
  Random (RNGState -> ((InteractT i u c s m a), RNGState)) |
  Stateful (u -> ((InteractT i u c s m a), u)) | 
  M (m (InteractT i u c s m a))

instance (Monad m, Show a, Show s, Show i, Show c) => Show (InteractT i u c s m a) where
  show (Terminal a) = "Terminal " ++ show a
  show (Send s [i] _) = "Send " ++ (show s) ++ " " ++ (show [i]) ++ " (λ)"
  show (Await [i] _) = "Await " ++ (show [i]) ++ " (λ)"
  show (Random _) = "Random (λ)"
  show (Stateful _) = "Stateful (λ)"
  show (M _) = "M (..)"

instance (Monad m) => Functor (InteractT i u c s m) where
  fmap = liftM

instance (Monad m) => Applicative (InteractT i u c s m) where
  pure = return
  (<*>) = ap

bindStatelike :: (Monad m) => (s -> (m a, s)) -> (a -> m b) -> s -> (m b, s)
bindStatelike cont k s =
  let (ma, s1) = cont s in (ma >>= k, s1)

instance (Monad m) => Monad (InteractT i u c s m) where 
  return = Terminal
  (Terminal a)     >>= k = k a
  (Send v ps cont) >>= k = Send v ps (cont >>= k)
  (Await ps cont)  >>= k = Await ps (\ts -> (cont ts) >>= k)
  (Random cont)    >>= k = Random $ bindStatelike cont k
  (Stateful cont)  >>= k = Stateful $ bindStatelike cont k
  (M ma)           >>= k = M $ fmap (flip (>>=) k) ma

instance (Monad m) => MonadState u (InteractT i u s c m) where
  get     = Stateful $ \u -> (Terminal u, u)
  put u   = Stateful $ \_ -> (Terminal (), u)
  state f = Stateful $ \u -> first Terminal (f u)

instance MonadTrans (InteractT i u c s) where
  lift ma = M $ Terminal <$> ma

instance MonadIO (InteractT i u c s IO) where
  liftIO = lift

putRand :: RNGState -> InteractT i u s c m ()
putRand r = Random $ (\_ -> (Terminal (), r))

getRand :: InteractT i u s c m RNGState
getRand = Random $ (\r -> (Terminal r, r))

send :: s -> [i] -> InteractT i u c s m ()
send view ids = Send view ids (Terminal ())

getActions :: (Monad m) => [i] -> InteractT i u c s m [c]
getActions ids = Await ids return

data Client s c m =
  Client {handleMsg :: s -> m (),
          choose    :: m c}

consoleClientHandleMsg :: (Show s, Show i) => i -> s -> IO ()
consoleClientHandleMsg pId msg = do
   putStrLn $ "[" ++ (show pId) ++ "] Received server message " ++ (show msg)


-- TODO: make this more informative on failure about what type of message it expects. 
consoleClientChoose :: (Show i, Read c, Show c) => i -> IO c
consoleClientChoose pId =
  loop where
    loop = do
      putStr $ "[" ++ (show pId) ++ "] Action? "
      hFlush stdout
      res <- reads <$> getLine
      putStrLn ""
      case res of
        [(out, "")] -> return out
        _           -> putStrLn ("Illegal action! " ++ (show res)) >> loop

consoleClient :: (Show i, Show c, Show s, Read c) => i -> Client s c IO
consoleClient cId = Client (consoleClientHandleMsg cId) (consoleClientChoose cId)

runInteractT :: (Monad m) =>
                InteractT i u c s m a -> StdGen -> u -> (i -> Client s c m) -> m (a, StdGen, u)
runInteractT remt r0 s0 clients =
  case remt of
    Terminal a -> return (a, r0, s0)
    Send msg ids cont -> do
      forM_ ids $ \i ->
        handleMsg (clients i) msg
      runInteractT cont r0 s0 clients
    Await ids cont -> do
      actions <- forM ids $ \i ->
        choose (clients i)
      runInteractT (cont actions) r0 s0 clients
    Random cont ->
      let (remt1, r1) = cont r0 in
      runInteractT remt1 r1 s0 clients
    Stateful cont ->
      let (remt1, s1) = cont s0 in
      runInteractT remt1 r0 s1 clients
    M cont ->
      cont >>= (\remt1 -> runInteractT remt1 r0 s0 clients)

testInteractT :: (Read c, Show c, Show s, Show i) =>
                 InteractT i u c s IO a -> Int -> u -> IO (a, StdGen, u)
testInteractT remt seed s0 =
  runInteractT remt (mkStdGen seed) s0 consoleClient

liftI :: (Monad m) => (c' -> c) -> (s -> s') -> Lens' u' u -> InteractT i u c s m a ->
         InteractT i u' c' s' m a
liftI fClient fServer lUser intSt =
  case intSt of
    Terminal a        -> Terminal a
    Send msg ids cont -> Send (fServer msg) ids (go cont)
    Await ids cont    -> Await ids $ \msgs -> go $ cont (map fClient msgs)
    Random cont       -> Random   $ \r -> let (next, r1) = cont r in (go next, r1) 
    Stateful cont     -> Stateful $ \u ->
                            let (next, u1) = cont (u ^. lUser) in (go next, u & lUser .~ u1)
    M cont            -> M $ fmap go cont
  where go = liftI fClient fServer lUser
