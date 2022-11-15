{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module Generate where

import Control.Monad.Random (MonadRandom, getRandomR, replicateM, evalRandIO)
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
  [ ("m" , 3)
  , ("n" , 3)
  , ("p" , 2)
  , ("t" , 3)
  , ("f" , 3)
  , ("s" , 4)
  , ("l" , 3)
  , ("r" , 3)
  ]

consonantO =
  consonantFront <>
  [ ("ŋ" , 3)
  , ("k" , 3)
  , ("x" , 3)
  , ("h" , 3)
  ]

consonantA =
  consonantFront <>
  [ ("ŋ" , 2)
  , ("ŋʷ", 2)
  , ("k" , 2)
  , ("kʷ", 2)
  , ("x" , 2)
  , ("xʷ", 2)
  , ("h" , 2)
  , ("hʷ", 2)
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

genSyllable :: MonadRandom m => m String
genSyllable = do
  x <- getRandomR (0, 2)
  if (x :: Int) == 0
    then choose vowel
    else genSyllableCV

wordLength :: NonEmpty (Int, Double)
wordLength =
  [ (2, 1)
  , (3, 2)
  , (4, 0.75)
  , (5, 0.5)
  ]

genWord :: MonadRandom m => m String
genWord = do
  n <- choose wordLength
  concat <$> replicateM n genSyllable

genWordIO :: IO String
genWordIO = evalRandIO genWord
