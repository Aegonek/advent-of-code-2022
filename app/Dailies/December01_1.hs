{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Dailies.December01_1 where

import Data.Foldable (Foldable (foldl'), foldlM, for_)
import Data.Maybe (fromMaybe)
import Paths_advent_of_code
import Text.Read (readMaybe)
import Prelude hiding (maximum, minimum)

data State = State {maximum, curr :: Int}

initialState = State 0 0

solution :: IO Int
solution = do
  lines <- lines <$> (readFile =<< getDataFileName "data/input01.txt")
  let State {maximum} =
        foldl'
          ( \acc@(State {maximum, curr}) next ->
              if null next
                then State {maximum = max maximum curr, curr = 0}
                else
                  let next' = parseInt next
                   in State {maximum, curr = curr + next'}
          )
          initialState
          lines
  pure maximum
  where
    parseInt str = fromMaybe (error "Not a valid number. Shouldn't happen on this input") (readMaybe @Int str)
