{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Evolve (addStress, evolve, evolveTrace) where

import qualified Data.Set as Set
import qualified Data.List as List

import Language.Change
import Language.Change.Quote (pat, sim, spl)

addStress :: String -> String
addStress str =
  case List.break isVowel str of
    (left, []) -> left
    (left, x:right) -> left ++ x : '\'' : right

evolve :: String -> String
evolve = takeWhile (/= '#')
       . applyChanges stage3
       . contract
       . applyChanges stage2
       . vowelLoss
       . applyChanges stage1
       . (++ "#")

evolveTrace :: String -> [String]
evolveTrace wd = let
  s1 = traceChanges stage1 (wd ++ "#")
  s2 = traceChanges stage2 (vowelLoss (last s1))
  s3 = traceChanges stage3 (contract (last s2))
  in s1 ++ s2 ++ s3

voicedC = Set.fromList "mnŋbdgvzʒɣlr"
voicelessC = Set.fromList "ptkfsʃxh"
setC = voicedC `Set.union` voicelessC
setV = Set.fromList "aeiəɨou"
setVoiced = voicedC `Set.union` setV

isVowel :: Char -> Bool
isVowel x = x `Set.member` setV

data VowelState
  = First
  | AfterVowel
  | Other
  deriving (Eq)

vowelLoss :: String -> String
vowelLoss xs = let
  strong = maybe True odd (stressedVowel xs)
  in vowelLoss' (strong, First) xs

vowelLoss' :: (Bool, VowelState) -> String -> String
vowelLoss' (strong, st) = \case
  [] -> []
  x:xs | isVowel x -> let
    strong' = case xs of { '\'':_ -> True; _ -> strong }
    beforeVowel = testPatterns xs [pat| '?V |]
    keep = strong' || beforeVowel || st == AfterVowel || st == First

    st' = if beforeVowel then AfterVowel else Other
    xs' = vowelLoss' (not strong', st') xs
    in if keep then x:xs' else xs'

    | otherwise -> x : vowelLoss' (strong, st) xs

--how many vowels are before the '?
stressedVowel :: String -> Maybe Int
stressedVowel = \case
  [] -> Nothing
  '\'':_ -> Just 0
  x:xs | isVowel x -> (1+) <$> stressedVowel xs
  _:xs -> stressedVowel xs

stage1 :: [Change Char]
stage1 =
  [ [sim| o > u / _V!*{ie} |]
  , [sim| s > ʃ / _{iu} |]
  ]

stage2 :: [Change Char]
stage2 =
  [ [sim| i > ɨ, e > ə, a > o / _'?Cʷ{C#} |]
  , [sim| ʷ > / _{C#} |]
  , [spl|
      ɨ > u / ʷ_
        > i / iV!*_
        > ə / _
    |]
  , [sim| ŋ > ŋʷ, k > kʷ, x > xʷ, h > hʷ / _{ou} |]
  , [spl|
      {m n ŋ}
        > m / _{mp}
        > n / _{nt}
        > ŋ / _{ŋk}

      s > ʃ / _ʃ
      ʃ > s / _s

      h > f / _{pf}
        > rᵒ / _t
        > s / _s
        > ʃ / _ʃ
        > x / _{kx}

      p > f / _{nŋtk}
      t > rᵒ / _{mŋpk}
      k > x / _{mnpt}

      f > v / _{mnŋlr}
      s > z / _{mnŋlr}
      ʃ > ʒ / _{mnŋlr}

      p > b / _{lr}
      t > d / _{lr}
      k > g / _{lr}
    |]
  , [spl|
      x > h / {ŋk}_
      h > x / x_
      r > d / z_, ʒ_, l_
    |]
  , [sim| m > mp, n > nt, ŋ > ŋk / _{fsʃxh} |]
  , [sim| m > mb, n > nd, ŋ > ŋg / _{lr} |]
  , [sim|
      p > b, t > d, k > g,
      f > v, s > z, ʃ > ʒ, x > ɣ
      / [Voiced]_ʷ?V'!
    |]
  , [sim| ᵒ > / _ |]
  , [sim| ' > . / _V?V!*VV |]
  ]

contract :: Eq a => [a] -> [a]
contract = \case
  [] -> []
  x:xs -> x : contract (dropWhile (== x) xs)

stage3 :: [Change Char]
stage3 =
  [ [spl|
      a >   / _'?ə
      i > j / _'?V
      u > o / _'?V

      ə > a / {eu}'?_
      e > i / {au}'?_
      o > u / {ae}'?_
    |]
  ]
