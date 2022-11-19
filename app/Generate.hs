{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Generate (genWord) where

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
  [ ("m" , 2)
  , ("n" , 2)
  , ("p" , 1)
  , ("t" , 2)
  , ("f" , 2)
  , ("s" , 2)
  , ("l" , 2)
  , ("r" , 2)
  ]

consonantO =
  consonantFront <>
  [ ("ŋʷ" , 1)
  , ("kʷ" , 2)
  , ("xʷ" , 1)
  , ("hʷ" , 2)
  ]

consonantA =
  consonantFront <>
  [ ("ŋ" , 1)
  , ("ŋʷ", 0.5)
  , ("k" , 2)
  , ("kʷ", 1)
  , ("x" , 1)
  , ("xʷ", 0.5)
  , ("h" , 2)
  , ("hʷ", 1)
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
