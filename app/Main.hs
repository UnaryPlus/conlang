{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

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

changes :: [Change Char]
changes =
  [ [sim| o > u / _C?ʷ?{ie} |]
  , [sim| s > ʃ / _{iu} |]
  , [sim| ŋ > ŋʷ, k > kʷ, x > xʷ, h > hʷ / _{ou} |]
  --TODO: vowel loss
  , [sim| i > ɨ, e > ə, a > o / _Cʷ{C#} |]
  , [spl|
      ɨ > u / ʷ_
        > i / iC?ʷ?C?_
        > ə / _
    |]
  , [sim| ŋʷ > ŋ, kʷ > k, xʷ > x, hʷ > h / _{ouC#} |]
  , [spl|
      m n ŋ
        > m / _{pm}
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
  --TODO: stress shift
  --TODO: voicing

  ]
