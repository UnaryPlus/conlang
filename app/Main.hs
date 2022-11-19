module Main where

import Generate (genWordIO)
import Evolve (stage1, vowelLoss)
import Control.Monad (replicateM)
import Language.Change (applyChange)

evolve :: String -> String
evolve = vowelLoss . s1
  where s1 x = foldr applyChange x stage1

main :: IO ()
main = mapM_ (putStrLn . evolve) =<< replicateM 100 genWordIO
