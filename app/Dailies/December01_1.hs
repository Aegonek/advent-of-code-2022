{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Dailies.December01_1 where

import Data.Foldable (foldlM, for_)
import Text.Read (readMaybe)
import Prelude hiding (maximum, minimum)

data State = State {maximum, curr :: Int}

initialState = State 0 0

impl01 :: IO Int
impl01 = do
  lines <- lines <$> readFile "E:\\advent-of-code\\data\\input"
  State {maximum} <-
    foldlM
      ( \acc@(State {maximum, curr}) next ->
          if null next
            then pure $ State {maximum = max maximum curr, curr = 0}
            else do
              next' <- parseNumOrThrow next
              pure $ State {maximum, curr = curr + next'}
      )
      initialState
      lines
  pure maximum
  where
    parseNumOrThrow str = maybe (error "Not a valid number. Shouldn't happen on this input") pure (readMaybe @Int str)
