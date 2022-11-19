module Main where

import Generate (genWord)
import Evolve (addStress, evolve, evolveTrace)
import Control.Monad (replicateM)

pad :: Int -> a -> [a] -> [a]
pad n c str = str ++ replicate (n - length str) c

main :: IO ()
main = mapM_ putStrLn $ evolveTrace (addStress "poi")
