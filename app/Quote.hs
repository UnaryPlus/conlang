{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Quote where

import qualified Text.Megaparsec as M
import qualified Control.Monad.Combinators.NonEmpty as NE

import Text.Megaparsec ((<|>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Control.Monad (void)

import Data.Char

import qualified Language.Haskell.TH as TH

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (Q, Exp)

import Change

type Pairing a b = NonEmpty (a, b)

data CharS
  = Lit Char
  | AntiQ String

type SetS = NonEmpty CharS

data PatternS
  = OneS SetS
  | OptionalS SetS
  | ManyS SetS

data EnvS
  = EnvS [PatternS] [PatternS]

data ChangeS
  = SimpleS (Pairing SetS String) (NonEmpty EnvS)
  | SplitS (Pairing SetS (Pairing String (NonEmpty EnvS)))


type Parser = M.Parsec Void String

special = ">/,[]{}?*_"

isSound c = not (c `elem` special || isAsciiUpper c || isSpace c)

isIdentifier c = isAlphaNum c || c == '\'' || c == '_'

spaces :: Parser ()
spaces = void (M.many (M.satisfy isSpace))

symbol :: Char -> Parser ()
symbol c = M.single c >> spaces

replacement :: Parser String
replacement = M.many (M.satisfy isSound <* spaces)

charS :: Parser CharS
charS = lit <|> oneChar <|> multipleChar
  where
    lit = Lit <$> M.satisfy isSound <* spaces

    oneChar = AntiQ . pure <$> M.satisfy isAsciiUpper <* spaces

    multipleChar = AntiQ
      <$ symbol '['
      <*> M.some (M.satisfy isIdentifier)
      <* symbol ']'

setS :: Parser SetS
setS = NE.some charS

setS' :: Parser SetS
setS' = (:|[]) <$> charS
    <|> M.between (symbol '{') (symbol '}') setS

patternS :: Parser PatternS
patternS = do
  s <- setS'
  M.try (symbol '?' >> return (OptionalS s))
    <|> M.try (symbol '*' >> return (ManyS s))
    <|> return (OneS s)

envS :: Parser EnvS
envS = EnvS <$> M.many patternS <* symbol '_' <*> M.many patternS

simpleS :: Parser ChangeS
simpleS = SimpleS
  <$> NE.sepBy1 change (symbol ',')
  <* symbol '/'
  <*> NE.sepBy1 envS (symbol ',')
  where
    change = (,) <$> setS <* symbol '>' <*> replacement

splitS :: Parser ChangeS
splitS = SplitS <$> NE.sepEndBy1 clause (symbol ';')
  where
    clause = (,) <$> setS <*> NE.some change
    change = (,) <$ symbol '>' <*> replacement
      <* symbol '/' <*> NE.sepBy1 envS (symbol ',')

setLoc :: (Int, Int) -> Parser ()
setLoc (line, col) =
  M.updateParserState \state ->
    let posState = M.statePosState state
        sourcePos = M.pstateSourcePos posState
        sourcePos' = sourcePos
          { M.sourceLine = M.mkPos line
          , M.sourceColumn = M.mkPos col
          }
        posState' = posState { M.pstateSourcePos = sourcePos' }
    in state { M.statePosState = posState' }


total :: Parser a -> Parser a
total p = spaces >> p <* M.eof

sim :: QuasiQuoter
sim = QuasiQuoter { quoteExp = quote simpleS }

spl :: QuasiQuoter
spl = QuasiQuoter { quoteExp = quote splitS }

quote :: Parser ChangeS -> String -> Q Exp
quote p input = do
  loc <- TH.location
  let file = TH.loc_filename loc
      (line, col) = TH.loc_start loc
      p' = setLoc (line, col) >> total p
  case M.runParser p' file input of
    Left errors -> fail (M.errorBundlePretty errors)
    Right change -> quoteChange change

quoteChange :: ChangeS -> Q Exp
quoteChange = \case
  SimpleS cs envs -> let
    cs' = foldr cons [] cs
    in Simple (Map.fromList cs')

  where
    cons :: Foldable t => (t a, b) -> [(a, b)] -> [(a, b)]
    cons (xs, y) cs = foldr (\x -> ((x, y):)) cs xs
