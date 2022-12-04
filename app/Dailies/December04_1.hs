{-# LANGUAGE TypeApplications #-}

module Dailies.December04_1 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (findIndex, intersect)
import Data.Maybe (fromMaybe)
import Paths_advent_of_code

solution :: IO Int
solution = do
  lines_ <- lines <$> (readFile =<< getDataFileName "data/input04.txt")
  let ranges =
        lines_ <&> \str ->
          let ranges = HomoTuple $ splitByEq ',' str
              ranges' = fmap (splitByEq '-') ranges
              ranges'' = fmap (\(start, end) -> (read @Int start, read @Int end)) ranges'
           in ranges''
      overlapping =
        ranges
          & filter
            ( \ranges ->
                let HomoTuple (xs, ys) = fmap (uncurry enumFromTo) ranges
                    cross = xs `intersect` ys
                 in length cross == length xs
                      || length cross == length ys
            )
  return $ length overlapping

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy pred xs =
  let i = findIndex pred xs & fromMaybe (length xs)
      (xs', ys') = splitAt i xs
   in (xs', drop 1 ys')

splitByEq :: (Eq a) => a -> [a] -> ([a], [a])
splitByEq x = splitBy (== x)

newtype HomoTuple a = HomoTuple (a, a)

instance Functor HomoTuple where
  fmap f (HomoTuple (x, y)) = HomoTuple (f x, f y)