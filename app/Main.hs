module Main where

import Kalvaszti.Generate (genWord)
import Kalvaszti.Evolve (evolve, evolveTrace)
import Control.Monad (replicateM)

pad :: Int -> a -> [a] -> [a]
pad n c str = str ++ replicate (n - length str) c

main :: IO ()
main = mapM_ display =<< replicateM 100 genWord
  where
    display wd =
      putStrLn (pad 16 ' ' wd ++ ">>  " ++ evolve wd)
