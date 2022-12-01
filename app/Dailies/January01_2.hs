{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Dailies.January01_2 where

import Data.Foldable (foldlM, for_)
import Text.Read (readMaybe)
import Data.Ord (Down(..), getDown)
import qualified Data.List as List
import Prelude hiding (maximum, minimum)

data State = State {best :: [Down Int], curr :: Int}

initialState = State (map Down [0, 0, 0]) 0

impl01 :: IO Int
impl01 = do
  lines <- lines <$> readFile "E:\\advent-of-code\\data\\input"
  State {best} <-
    foldlM
      ( \acc@(State {best, curr}) next ->
          if null next
            then
              let updated = take 3 $ List.insert (Down curr) best
               in pure $ State {best = updated, curr = 0}
            else do
              next' <- parseNumOrThrow next
              pure $ State {best, curr = curr + next'}
      )
      initialState
      lines
  pure $ getDown $ sum best
  where
    parseNumOrThrow str = maybe (error "Not a valid number. Shouldn't happen on this input") pure (readMaybe @Int str)