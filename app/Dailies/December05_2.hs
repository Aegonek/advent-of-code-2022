{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Dailies.December05_2 where

import Paths_advent_of_code
import Data.Extra.List (splitBy, findMap)
import qualified Data.List as List
import qualified Text.ParserCombinators.ReadP as Parse
import Data.Maybe (mapMaybe)
import Data.Function ((&))
import Data.Foldable (find, Foldable (..))
import Data.Char (isLetter)
import Data.Functor ((<&>))
import qualified Text.Extra.ReadP as Parse
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Extra.Monad ((<<))

solution :: IO String
solution = do
  lines_ <- lines <$> (readFile =<< getDataFileName "data/input05.txt")
  let (stack, moves) = splitBy null lines_
      cargo = parseCargo stack
      moves' = map (read @Move) moves
      Cargo {stacks} = foldl' (flip evalMove) cargo moves'
      tops = map head stacks
  return tops

type Stack = [Char]

newtype Cargo = Cargo 
  { stacks :: [Stack] -- ^ top item is first
  } deriving (Show)

parseCargo :: 
  [String] -- ^ raw task input
  -> Cargo
parseCargo rows =
  let cols = List.transpose rows
      chars = cols <&> filter isLetter -- from every column we retain only letters
      stacks = filter (not . null) chars -- and we discard columns that didn't have any letters
  in Cargo stacks

data Move = Move {quantity, from, to :: Int} deriving Show

evalMove :: Move -> Cargo -> Cargo
evalMove (Move {quantity, from, to}) (Cargo {stacks}) = runST $ do
  stacks' <- traverse newSTRef stacks
  let fromStackRef = stacks' !! (from - 1)
      toStackRef = stacks' !! (to - 1)

  fromStack <- readSTRef fromStackRef
  let (taken, left) = splitAt quantity fromStack
  writeSTRef fromStackRef left
  modifySTRef' toStackRef (taken<>)
  
  Cargo <$> traverse readSTRef stacks'

instance Read Move where
  readsPrec _ = 
    Parse.readP_to_S $ do
      quantity <- Parse.string "move " >> Parse.int << Parse.char ' '
      from <- Parse.string "from " >> Parse.int << Parse.char ' '
      to <- Parse.string "to " >> Parse.int
      return $ Move {quantity, from, to}