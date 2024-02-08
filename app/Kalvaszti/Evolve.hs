{- TODO: write test suite -}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module Kalvaszti.Evolve (evolve, evolveTrace) where

import Language.Change (Change, applyChanges)
import Language.Change.Quote (chs)

import Data.Function ((&))

evolve :: String -> String
evolve s = s ++ "#"
  & applyChanges stage1
  & vowelLoss
  & applyChanges stage2
  & degemination
  & applyChanges stage3
  & takeWhile (/= '#')

evolveTrace :: String -> String
evolveTrace s = let
  part1 = traceChanges stage1 (s ++ "#")
  part2 = traceChanges stage2 (vowelLoss (last part1))
  part3 = traceChanges stage3 (degemination (last part2))
  in part1 ++ part2 ++ part3

setV = "aeiouʏɨə"

isVowel :: Char -> Bool
isVowel x = x `elem` setV

stage1 :: [Change Char]
stage1 = [chs|
  * o > ʏ / _V!*{ie}
  * s > ʃ / _{iʏ}
  |]

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

stage2 :: [Change Char]
stage2 = [chs|
  * ʏ > { o / _V ; u / _ }
  * { i > ɨ; e > ə; a > o } / _Cʷ{C#}
    ŋ > m / _ʷ{C#}
    ʷ > % / _{C#}
  * { ŋ > ŋʷ; k > kʷ; x > xʷ; h > hʷ } / _o
  * ɨ > { u / ʷ_; i / iV!*_; ə / _ }

  * ə > a / _{aeo}
    e > i / _{ao}
    o > u / _{ae}
  * { aa > a; ee > e; ii > i; oo > o } / _

  * l > { % / i_{sʃ}, {aeo}V_{sʃ}; i / {ou}_{sʃ}; u / {ae}_{sʃ} }
  * u > o / _i

  * ŋ > m / _ʷiV, _ʷeu
    ʷ > % / _iV, _eu
  * a > o / ʷ_u
    i > % / ʃ_V

  * x > { h / _ʷ{ou} ; w / _ʷV }
  * ʷ > % / w_

  * { p > b; t > d; k > g; 
      f > v; s > z; ʃ > ʒ; x > ɣ } / V_V
  
  * m, n, ŋ > { m / _p; n / _t; ŋ / _k; % / _{mnŋ} }
    s, ʃ > % / _{sʃ}
    h > % / _{fsʃx}
    { p > k; m > ŋ } / _{fw}, _hʷ
  
  * r > t / {lsʃ}_
    l > t / {tnsʃ}_

  * { p > b; t > d; k > g } / {mnŋlr}_ʷ!
    { f > v; s > z; ʃ > ʒ; x > ɣ } / {lr}_
    x > k / {kŋ}_
    w > hʷ / {ptk}_
    h > { % / {fsʃx}_ʷ!, {fsʃx}_ʷ{ou}; w / {fsʃx}_ }
    ʷ > % / {fsʃx}h_
  |]

degemination :: String -> String
degemination = \case
  [] -> []
  x:xs -> x : degemination (dropWhile (== x) xs)

stage3 :: [Change Char]
stage3 = [chs|
  * { p > f; t > r; k > x } / _{ptkmnŋ}
    h > { f / _p; r / _t; x / _k }
    { p > b; t > d; k > g } / _{lr}
    { f > v; s > z; ʃ > ʒ } / _{mnŋlr} 

  * h > x / {iu}_{C#}
  |]
