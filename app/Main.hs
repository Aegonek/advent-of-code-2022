module Main where

import qualified Dailies.January01 as January01
import qualified Dailies.January01_2 as January01_2

main :: IO ()
main = print =<< January01_2.impl01
