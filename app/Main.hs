module Main where

import qualified Dailies.December01_1 as December01_1
import qualified Dailies.December01_2 as December01_2

main :: IO ()
main = print =<< December01_2.impl01
