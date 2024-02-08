{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module Kalvaszti.Evolve (evolve, evolveTrace) where

import Language.Change
import Language.Change.Quote (sim, spl)

evolve :: String -> String
evolve =
    takeWhile (/= '#')
  . applyChanges stage3
  . contract
  . applyChanges stage2
  . vowelLoss
  . applyChanges stage1
  . prepare
  where prepare str = "'" ++ str ++ "#"

evolveTrace :: String -> [String]
evolveTrace wd = let
  s1 = traceChanges stage1 ("'" ++ wd ++ "#")
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

vowelLoss :: String -> String
vowelLoss = vowelLoss' (True, False)

vowelLoss' :: (Bool, Bool) -> String -> String
vowelLoss' (strong, afterVowel) = \case
  [] -> []
  x:xs | isVowel x -> let
    beforeVowel = case xs of { [] -> False; v:_ -> isVowel v }
    keep = strong || beforeVowel || afterVowel
    xs' = vowelLoss' (not strong, True) xs
    in if keep then x:xs' else xs'

    | otherwise -> x : vowelLoss' (strong, False) xs

stage1 :: [Change Char]
stage1 =
  [ [sim| o > u / _V!*{ie} |]
  , [sim| s > ʃ / _{iu} |]
  ]

stage2 :: [Change Char]
stage2 =
  [ [sim| i > ɨ, e > ə, a > o / _Cʷ{C#} |]
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
      / [Voiced]_ʷ?V
    |]
  , [sim| ᵒ > / _ |]
  , [sim| ' > . / _V!*VV*V!*VV |]
  ]

contract :: Eq a => [a] -> [a]
contract = \case
  [] -> []
  x:xs -> x : contract (dropWhile (== x) xs)

stage3 :: [Change Char]
stage3 =
  [ [spl|
      a >   / _ə
      u > o / _V

      ə > a / {eu}_
      e > i / {au}_
      o > u / {ae}_
    |]
  ]
