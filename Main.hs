{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

import Control.Monad (foldM, when)
import System.Random (randomRIO)

import Jatek.Core
import Games.Nim

-- Play a nim game
main :: IO ()
main = do
  let players = [(First, RandomNimPlayer), (Second, RandomNimPlayer)]
  (result, history) <- fullGame NimGame 20 players False
  putStrLn $ "Result: " ++ show result
  putStrLn $ "History: " ++ show history
  
