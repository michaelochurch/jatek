{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

import Control.Monad (foldM, when)
import System.Random (mkStdGen, randomRIO)

import Jatek.Core
import Games.Nim

-- Play a nim game
main :: IO ()
main = do
  result <-runInteractT (interpretGame nim) (mkStdGen 0) (16, First) players
  putStrLn $ "!! Winner was " ++ (show result)
  where players = mkIoPlayers [First, Second]
