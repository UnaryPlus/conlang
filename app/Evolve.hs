{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Evolve where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

import Language.Change
import Language.Change.Quote (sim, spl)

type Sound = Text

--TODO: update sound sets
setC, setV, setVoiced :: Set Char
setC = Set.fromList "mnŋptkfsxhlr"
setV = Set.fromList "aeiou"
setVoiced = undefined

isVowel :: Char -> Bool
isVowel x = x `Set.member` setV

stage1 :: [Change Char]
stage1 =
  [ [sim| o > u / _V!*{ie} |]
  , [sim| s > ʃ / _{iu} |]
  , [sim| ŋ > ŋʷ, k > kʷ, x > xʷ, h > hʷ / _{ou} |]
  ]

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
    beforeVowel = case xs of { v:_ | isVowel v -> True; _ -> False }
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

stage2 :: [Change Char]
stage2 =
  [ [sim| i > ɨ, e > ə, a > o / _'?Cʷ{C#} |]
  , [spl|
      ɨ > u / ʷ_
        > i / iV!*_
        > ə / _
    |]
  , [sim| ʷ > / _{ouC#} |]
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

      x > k / _{sʃ}

      f > v / _{mnŋlr}
      s > z / _{mnŋlr}
      ʃ > ʒ / _{mnŋlr}

      p > b / _{lr}
      t > d / _{lr}
      k > g / _{lr}
    |]
  , [spl|
      x > h / {ŋk}_
        > k / {sʃ}_
      h > x / x_

      r > d / z_, ʒ_, l_
      l > n / n_
    |]
  , [sim| m > mp, n > nt, ŋ > ŋk / _{fsʃxh} |]
  , [sim| m > mb, n > nd, ŋ > ŋg / _{lr} |]
  --TODO: voicing
  , [sim| ᵒ > / _ |]
  , [spl|
      a > a' / 'V!*_V
      e > e' / 'V!*_V
      ə > ə' / 'V!*_V
      i > i' / 'V!*_V
      o > o' / 'V!*_V
      u > u' / 'V!*_V

      ' > / _V!*VV
    |]

  ]

contract :: Eq a => [a] -> [a]
contract = \case
  [] -> []
  x:xs -> x : contract (dropWhile (== x) xs)

stage3 :: [Change Char]
stage3 =
    --TODO: account for stress
  [ [spl|
      a >   / _ə
      i > j / _V
      u > o / _V

      ə > a / {eu}_
      e > i / {au}_
      o > u / {ae}_
    |]
  ]
