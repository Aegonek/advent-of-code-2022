{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Dailies.December02_2 where

import Data.Functor (($>), (<&>))
import Data.Maybe (mapMaybe, fromMaybe)
import Paths_advent_of_code
import qualified Text.ParserCombinators.ReadP as Parse
import Text.Read (readMaybe)
import Data.List (elemIndex)
import Data.Function ((&))

solution :: IO Int
solution = do
  lines' <- lines <$> (readFile =<< getDataFileName "data/input02.txt")
  let moves = mapMaybe (readMaybe @Move) lines'
      sum' = sum $ map rate moves
  return sum'

data Figure = Rock | Scissors | Paper deriving (Eq, Show, Enum)

data Outcome = Win | Draw | Lose deriving (Eq, Show)

data Move = Move {enemy :: Figure, outcome :: Outcome } deriving (Show)

instance Read Move where
  readsPrec _ = Parse.readP_to_S parser
    where
      parser = do
        enemy <-
          Parse.choice
            [ Parse.char 'A' $> Rock,
              Parse.char 'B' $> Paper,
              Parse.char 'C' $> Scissors
            ]
        _ <- Parse.char ' '
        outcome <-
          Parse.choice
            [ Parse.char 'X' $> Lose,
              Parse.char 'Y' $> Draw,
              Parse.char 'Z' $> Win
            ]
        return $ Move {enemy, outcome}

myFigure :: Move -> Figure
myFigure Move {enemy, outcome} =
  case (enemy, outcome) of
      (x, Win) -> next x
      (x, Lose) -> prev x
      (x, Draw) -> x
  where
  figures = [Rock, Paper, Scissors]
  next figure = 
    let i = elemIndex figure figures & fromMaybe (error "Unreachable, this list contains all cases!")
        i' = if i + 1 < length figures then i + 1 else 0
    in figures !! i'
  prev figure = 
    let i = elemIndex figure figures & fromMaybe (error "Unreachable, this list contains all cases!")
        i' = if i - 1 >= 0 then i - 1 else length figures - 1
    in figures !! (let i' = (i - 1) in if i' >= 0 then i' else 2)

rateFigure :: Figure -> Int
rateFigure = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

rateOutcome :: Outcome -> Int
rateOutcome = \case
  Lose -> 0
  Draw -> 3
  Win -> 6

rate :: Move -> Int
rate move@(Move {outcome}) = rateOutcome outcome + rateFigure (myFigure move)