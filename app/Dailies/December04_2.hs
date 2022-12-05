{-# LANGUAGE TypeApplications #-}

module Dailies.December04_2 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (findIndex, intersect)
import Data.Maybe (fromMaybe)
import Data.Extra.List (splitByEq)
import Paths_advent_of_code

solution :: IO Int
solution = do
  lines_ <- lines <$> (readFile =<< getDataFileName "data/input04.txt")
  let ranges =
        lines_ <&> unHomo . \str ->
          let ranges = HomoTuple $ splitByEq ',' str
              ranges' = fmap (splitByEq '-') ranges
              ranges'' = fmap (\(start, end) -> (read @Int start, read @Int end)) ranges'
           in ranges''
      overlapping =
        ranges
          & filter
            ( \ranges ->
                let HomoTuple (xs, ys) = fmap (uncurry enumFromTo) (HomoTuple ranges)
                    cross = xs `intersect` ys
                 in not $ null cross
            )
  return $ length overlapping

newtype HomoTuple a = HomoTuple (a, a)

unHomo (HomoTuple tup) = tup

instance Functor HomoTuple where
  fmap f (HomoTuple (x, y)) = HomoTuple (f x, f y)