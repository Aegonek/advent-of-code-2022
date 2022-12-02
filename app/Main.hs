module Main where

import qualified Dailies.December01_1 as December01_1
import qualified Dailies.December01_2 as December01_2
import qualified Dailies.December02_1 as December02_1
import qualified Dailies.December02_2 as December02_2
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let task = head args
  solution <- case task of
    "1-1" -> December01_1.solution
    "1-2" -> December01_2.solution
    "2-1" -> December02_1.solution
    "2-2" -> December02_2.solution
  print solution