module Main where

import Data.Void (Void)
import Data.Text (Text)
import qualified Text.Megaparsec as M
import qualified Replace.Megaparsec (streamEdit)

{-
m mˠ n ɲ ŋ ŋʷ
p pˠ t k kʷ
b bˠ d g gʷ
f s ʃ ç x xʷ h hʷ
v z ʒ ɣ ɣʷ
l lˠ r j

a e i ɑ ɒ ə y o u
-}

type Parser = M.Parsec Void Text




main :: IO ()
main = putStrLn "Hello, Haskell!"
