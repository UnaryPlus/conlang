module Main where

import Generate (genWordIO)
import Control.Monad (replicateM)

main :: IO ()
main = mapM_ putStrLn =<< replicateM 100 genWordIO
