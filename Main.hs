{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

import Control.Monad (foldM, when)
import System.Random (mkStdGen, randomRIO)

import Jatek.Core
import Jatek.Interact

import Games.Ambition
import Games.Nim

import Util.PlayingCards

playNim :: IO ()
playNim = do
  result <- testInteractT (interpretGame nim) 0 (16, First)
  putStrLn $ "!! Winner was " ++ (show result)

ambition1Trick :: IO ()
ambition1Trick = do
  result <- testInteractT (interpretGame trick) 0 initState
  putStrLn $ "!! Winner was " ++ (show result)
  where
    initState = TrickState 1 2 (Tup4 (hand1, hand2, hand3, hand4))
                (Tup4 (Nothing, Nothing, Nothing, Nothing))
    hand1 = [(Card 7 Heart), (Card king Club)]
    hand2 = [(Card 3 Spade), (Card 5 Diamond)]
    hand3 = [(Card jack Diamond), (Card 3 Diamond)]
    hand4 = [(Card ace Diamond), (Card 7 Diamond)]

main :: IO ()
main = ambition1Trick
