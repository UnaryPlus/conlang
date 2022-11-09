{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Void (Void)
import Data.Text (Text, singleton)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Control.Applicative (liftA2)

import qualified Data.Map as Map
import Data.Map (Map)

--VALID SOUNDS:
{-
m mˠ n ɲ ŋ ŋʷ
p pˠ t k kʷ
b bˠ d g gʷ
f s ʃ ç x xʷ h hʷ
v z ʒ ɣ ɣʷ
l lˠ r rᵒ j

a e i ɑ ɒ ə y o u
-}

type Nat = Word

data Sound = C Text | V Text Bool

replaceR :: (a -> [a] -> b) -> [a] -> [b]
replaceR _ [] = []
replaceR f (x:xs) = f x xs : replaceR f xs

replace :: ([a] -> a -> [a] -> b) -> [a] -> [b]
replace f = replace' []
  where
    replace' _ [] = []
    replace' left (x:right) = f left x right : replace' (x:left) right

isSingle :: [a] -> Bool
isSingle = \case
  [_] -> True
  _ -> False

oneOf, noneOf :: Eq a => [a] -> a -> Bool
oneOf = flip elem
noneOf = flip notElem

anything = const True

matchConsonant :: Nat -> [Sound] -> (Text -> Bool) -> Bool
matchConsonant n sounds test =
  case (n, sounds) of
    (_, []) -> False
    (0, C c : _) -> test c
    (0, _) -> False
    (n, _ : sounds') -> matchConsonant (n - 1) sounds' test

matchVowel :: Nat -> [Sound] -> (Text -> Bool) -> Bool
matchVowel n sounds test =
  case (n, sounds) of
    (_, []) -> False
    (0, V v _ : _) -> test v
    (0, _) -> False
    (n, _ : sounds') -> matchVowel (n - 1) sounds' test

umlaut :: [Sound] -> [Sound]
umlaut = replaceR \x right ->
  case x of
    V "o" b
      | matchVowel 0 right (oneOf [ "e", "i" ])
      || matchConsonant 0 right (noneOf [ "ŋʷ", "kʷ", "xʷ", "hʷ" ])
      && matchVowel 1 right (oneOf [ "e", "i" ])
      -> V "u" b
    _ -> x

palatalize :: [Sound] -> [Sound]
palatalize = replaceR \x right ->
  case x of
    C "s" | matchVowel 0 right (oneOf [ "i", "u" ]) -> C "ʃ"
    _ -> x

labialize :: [Sound] -> [Sound]
labialize = replaceR \x right ->
  if matchVowel 0 right (oneOf [ "o", "u" ])
    then case x of
      C "ŋ" -> C "ŋʷ"
      C "k" -> C "kʷ"
      C "x" -> C "xʷ"
      C "h" -> C "hʷ"
      _ -> x
    else x

vowelLoss :: [Sound] -> [Sound]
vowelLoss sounds =
  vowelLoss' FirstVowel strong sounds
  where
    strong = maybe True even (stressedVowel sounds)

data VowelType
  = FirstVowel
  | AfterVowel
  | Other
  deriving (Eq)

vowelLoss' :: VowelType -> Bool -> [Sound] -> [Sound]
vowelLoss' vtype strong = \case
  [] -> []
  C _ : xs -> vowelLoss' vtype strong xs

  vowel@(V _ stressed) : xs -> let
    strong' = strong || stressed
    beforeVowel = matchVowel 0 xs anything
    retain = or [ strong', beforeVowel, vtype == FirstVowel, vtype == AfterVowel ]

    vtypeNext = if beforeVowel then AfterVowel else Other
    strongNext = not strong'
    xs' = vowelLoss' vtypeNext strongNext xs
    in
    if retain then vowel : xs' else xs'

stressedVowel :: [Sound] -> Maybe Nat
stressedVowel = \case
  [] -> Nothing
  (C _ : xs) -> stressedVowel xs
  (V _ False : xs) -> (1 +) <$> stressedVowel xs
  (V _ True : _) -> Just 0

shiftRounding :: [Sound] -> [Sound]
shiftRounding = replaceR \x right ->
  case x of
    V v b
      | matchConsonant 0 right (oneOf [ "ŋʷ", "kʷ", "xʷ", "hʷ" ])
      && (isSingle right || matchConsonant 1 right anything)
      -> V (rounded v) b
    C c
      | null right || matchConsonant 0 right anything
      -> C (unrounded c)
    _ -> x
  where
    rounded = \case
      "a" -> "ɒ"
      "e" -> "ə"
      "i" -> "y"
      v -> v

    unrounded = \case
      "ŋʷ" -> "ŋ"
      "kʷ" -> "k"
      "xʷ" -> "x"
      "hʷ" -> "h"
      c -> c

delabialize :: [Sound] -> [Sound]
delabialize = replaceR \x right ->
  case x of
    C c
      | matchVowel 0 right (oneOf [ "o", "u" ])
      -> C (unrounded c)
    _ -> x
  where
    unrounded = \case
      "ŋʷ" -> "ŋ"
      "kʷ" -> "k"
      "xʷ" -> "x"
      "hʷ" -> "h"
      c -> c

simplify1 :: [Sound] -> [Sound]
simplify1 = replaceR \x right ->
  case (x, right) of
    (C c1, C c2 : _) -> C (simplify c1 c2)
    _ -> x

  where
    simplify c1 c2
      | c1 `elem` [ "m", "n", "ŋ" ] =
        if c2 `elem` [ "m", "p" ] then "m"
        else if c2 `elem` [ "n", "t" ] then "n"
        else if c2 `elem` [ "ŋ", "ŋʷ", "k", "kʷ"] then "ŋ"
        else c1

      | c1 == "p" =
        if c2 `elem` [ "n", "ŋ", "ŋʷ", "t", "k", "kʷ" ] then "f"
        else "p"

      | c1 == "t" = undefined

      | c1 `elem` [ "s", "ʃ" ] && c2 `elem` [ "s", "ʃ" ] = c2

      | c1 == "x" && c2 `elem` [ "s", "ʃ" ] = k

      | c1 == "h" && c2 `elem` [ "f", "s", "ʃ" "x", "" ] = undefined
      | otherwise = c1













main :: IO ()
main = putStrLn "Hello, Haskell!"
