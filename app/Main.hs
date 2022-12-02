module Main where

import qualified Dailies.December01_1 as December01_1
import qualified Dailies.December01_2 as December01_2
import qualified Dailies.December02_1 as December02_1
import qualified Dailies.December02_2 as December02_2

main :: IO ()
main = print =<< December02_2.solution
