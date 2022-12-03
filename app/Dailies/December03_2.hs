{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Dailies.December03_2 where

import Data.Char (isLower, isUpper, ord)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import Paths_advent_of_code

solution :: IO Int
solution = do
  lines' <- lines <$> (readFile =<< getDataFileName "data/input03.txt")
  let groups = groupsOf 3 lines'
      dups =
        groups <&> \[xs, ys, zs] ->
          xs
            & find (\x -> x `elem` ys && x `elem` zs)
            & fromMaybe (error "Exactly one duplicate item expected!")
      result = sum $ map priority dups
  return result

priority :: Char -> Int
priority char
  | isLower char = ord char - 96
  | isUpper char = ord char - 38

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = go [] xs
  where
    go acc [] = reverse acc
    go acc rem =
      let (taken, left) = splitAt n rem
       in go (taken : acc) left