module Dailies.December03_1 where

import Data.Char (isLower, isUpper, ord)
import Paths_advent_of_code
import Data.Functor ((<&>))
import Data.List (intersect)

solution :: IO Int
solution = do
  lines' <- lines <$> (readFile =<< getDataFileName "data/input03.txt")
  let knapsacks =
        lines' <&>
            ( \line ->
                let (div', mod') = divMod (length line) 2
                 in if mod' == 0
                      then splitAt div' line
                      else error "Expecting even number of items in knapsack"
            )
      dups = knapsacks <&> \(x, y) -> head $ x `intersect` y
      result = sum $ map priority dups

  return result

priority :: Char -> Int
priority char | isLower char = ord char - 96
              | isUpper char = ord char - 38