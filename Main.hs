{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

import Control.Monad (foldM, when)
import System.Random (mkStdGen, randomRIO)

import Jatek.Interact
import Jatek.Mechanic
import Jatek.Game

import Games.Nim
import Games.Ambition

import Util.PlayingCards

playNim :: IO ()
playNim = do
  let system = mkConsoleSystem parseAct [First, Second] show
  (winner, finalState) <- runInteractT (runMechanic nim) system (16, First)
  putStrLn $ "!! Winner was " ++ (show winner)
  where parseAct str =
          case reads str of
            [(n, "")] -> Just (Take n)
            _         -> Nothing

playNim3 :: IO ()
playNim3 = do
  let system = mkConsoleSystem parseAct [First, Second] show
  (result, finalState) <- runInteractT (runGame nimTo3) system (initial nimTo3)
  putStrLn $ "Result: " ++ (show result)
  putStrLn $ "Final state: " ++ (show finalState)
  where parseAct str =
          case reads str of
            [(n, "")] -> Just (Take n)
            _         -> Nothing
  
ambition1Trick :: IO ()
ambition1Trick = do
  let system = mkConsoleSystem parseCard (map Player [0..3]) show
  (result, finalState) <- runInteractT (runMechanic trick) system initState
  putStrLn $ "Result was " ++ (show result)
  putStrLn $ "Final state is " ++ (show finalState)
  where
    parseCard str =
      case reads str of
        [(c, "")] -> Just c
        _         -> Nothing
    initState = TrickState 1 2 (Tup4 (hand1, hand2, hand3, hand4))
                (Tup4 (Nothing, Nothing, Nothing, Nothing))
    hand1 = [(Card 7 Heart), (Card 2 Heart), (Card king Club)]
    hand2 = [(Card 3 Spade), (Card 5 Diamond), (Card 2 Diamond)]
    hand3 = [(Card jack Diamond), (Card 3 Diamond), (Card 6 Spade)]
    hand4 = [(Card ace Diamond), (Card 7 Diamond), (Card 9 Heart)]

main :: IO ()
main = ambition1Trick
