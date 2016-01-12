module Jatek.Random where

import Control.Monad (replicateM)
import qualified Data.Vector as V
import System.Random

import Jatek.Actor

class Monad m => MonadRandom m where
  getInt    :: (Int, Int) -> m Int
  getDouble :: m Double

instance MonadRandom IO where
  getInt    = randomRIO
  getDouble = randomIO

die :: (MonadRandom m)  => Int -> m Int
die n = getInt (1, n)

dice :: (MonadRandom m) => Int -> Int -> m [Int]
dice k n = replicateM k (die n)

-- We're using Data.Vector here because Knuth-shuffling a linked list is
-- monstrously inefficient.
shuffle :: (MonadRandom m) => V.Vector a -> m (V.Vector a)
shuffle vec = loop 0 vec
  where len = V.length vec
        loop i vec =
          if i >= (len - 1)
          then return vec
          else do
            j <- getInt (i, len - 1)
            loop (i + 1) (swap vec i j)
        swap v i j = v V.// [(i, v V.! j), (j, v V.! i)]
