module Main where

import Generate (genWord)
import Evolve (prepare, evolve)
import Control.Monad (replicateM)

pad :: Int -> a -> [a] -> [a]
pad n c str = str ++ replicate (n - length str) c

main :: IO ()
main = mapM_ display =<< replicateM 100 genWord
  where
    display wd =
      putStrLn (pad 16 ' ' wd ++ ">>  " ++ evolve (prepare wd))
