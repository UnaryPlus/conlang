{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Kalvaszti.Generate (genWord) where

import qualified Control.Monad.State as State
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Random (MonadRandom, getRandomR, evalRandIO)
import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty(..))

choose :: MonadRandom m => NonEmpty (a, Double) -> m a
choose list = let
  weight (_, w) = abs w
  total = foldr ((+) . weight) 0 list
  in flip getChoice list <$> getRandomR (0, total)

getChoice :: Double -> NonEmpty (a, Double) -> a
getChoice i = \case
  (x, _) :| [] -> x
  (x, w) :| y:ys ->
    if i <= abs w then x
      else getChoice (i - abs w) (y :| ys)

consonantFront :: NonEmpty (String, Double)
consonantFront =
  [ ("m" , 1)
  , ("n" , 1)
  , ("p" , 1)
  , ("t" , 1)
  , ("f" , 1)
  , ("s" , 1)
  , ("l" , 1)
  , ("r" , 1)
  ]

consonantO =
  consonantFront <>
  [ ("ŋʷ" , 1)
  , ("kʷ" , 1)
  , ("xʷ" , 1)
  , ("hʷ" , 1)
  ]

consonantA =
  consonantFront <>
  [ ("ŋ" , 1)
  , ("ŋʷ", 0.6)
  , ("k" , 1)
  , ("kʷ", 0.6)
  , ("x" , 1)
  , ("xʷ", 0.6)
  , ("h" , 1)
  , ("hʷ", 0.6)
  ]

vowel :: NonEmpty (String, Double)
vowel =
  [ ("a", 2)
  , ("e", 2)
  , ("i", 2)
  , ("o", 3)
  ]

genSyllableCV :: MonadRandom m => m String
genSyllableCV = do
  v <- choose vowel
  c <- choose if v == "o" then consonantO else consonantA
  return (c ++ v)

genSyllable :: (MonadRandom m, MonadState Bool m) => m String
genSyllable = do
  b <- State.get --was the last syllable a vowel?
  if b then State.put False >> genSyllableCV
  else do
    x <- getRandomR (0, 2)
    if (x :: Int) == 0
      then State.put True >> choose vowel
      else State.put False >> genSyllableCV

wordLength :: NonEmpty (Int, Double)
wordLength =
  [ (2, 1)
  , (3, 2)
  , (4, 0.75)
  , (5, 0.5)
  ]

genWord' :: (MonadState Bool m, MonadRandom m) => m String
genWord' = do
  n <- choose wordLength
  concat <$> replicateM n genSyllable

genWord :: IO String
genWord = evalRandIO (evalStateT genWord' True)
