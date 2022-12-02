{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Dailies.December02_1 where

import Data.Functor (($>), (<&>))
import Data.Maybe (mapMaybe)
import Paths_advent_of_code
import qualified Text.ParserCombinators.ReadP as Parse
import Text.Read (readMaybe)

solution :: IO Int
solution = do
  lines' <- lines <$> (readFile =<< getDataFileName "data/input02.txt")
  let moves = mapMaybe (readMaybe @Move) lines'
      sum' = sum $ map rate moves
  return sum'

data Figure = Rock | Paper | Scissors deriving (Eq, Show)

data Outcome = Win | Draw | Lose deriving (Eq, Show)

data Move = Move {enemy, me :: Figure} deriving (Show)

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
        me <-
          Parse.choice
            [ Parse.char 'X' $> Rock,
              Parse.char 'Y' $> Paper,
              Parse.char 'Z' $> Scissors
            ]
        return $ Move {me, enemy}

outcome :: Move -> Outcome
outcome Move {enemy, me} =
  case (me, enemy) of
    (Rock, Scissors) -> Win
    (Paper, Rock) -> Win
    (Scissors, Paper) -> Win
    (x, y) | x == y -> Draw
    (_, _) -> Lose

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
rate move@(Move {enemy, me}) = rateOutcome (outcome move) + rateFigure me