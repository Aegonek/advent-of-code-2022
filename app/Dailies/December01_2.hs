{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Dailies.December01_2 where

import Control.Newtype (Newtype (pack), under, underF)
import Data.Foldable (Foldable (..), foldlM, for_)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), getDown)
import Paths_advent_of_code
import Text.Read (readMaybe)
import Prelude hiding (maximum, minimum)

data State = State {best :: [Int], curr :: Int}

initialState = State [0, 0, 0] 0

solution :: IO Int
solution = do
  lines <- lines <$> (readFile =<< getDataFileName "data/input01.txt")
  let State {best} =
        foldl'
          ( \acc@(State {best, curr}) next ->
              if null next
                then
                  let updated = take 3 $ underF Down (List.insert $ pack curr) best
                   in State {best = updated, curr = 0}
                else
                  let next' = parseInt next
                   in State {best, curr = curr + next'}
          )
          initialState
          lines
  pure $ sum best
  where
    parseInt str = fromMaybe (error "Not a valid number. Shouldn't happen on this input") (readMaybe @Int str)