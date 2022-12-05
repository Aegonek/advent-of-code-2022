module Data.Extra.List where
   
import Data.List (findIndex)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable(..))

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy pred xs =
  let i = findIndex pred xs & fromMaybe (length xs)
      (xs', ys') = splitAt i xs
   in (xs', drop 1 ys')

splitByEq :: (Eq a) => a -> [a] -> ([a], [a])
splitByEq x = splitBy (== x)

findMap :: (Foldable f) => (a -> Maybe b) -> f a -> Maybe b
findMap f = foldl' step Nothing
   where
   step Nothing x = f x
   step sth _ = sth