{-# LANGUAGE TypeApplications #-}
module Text.Extra.ReadP where

import Text.ParserCombinators.ReadP
import Data.Functor ((<&>))
import Data.Char (isDigit)

int :: ReadP Int
int = many (satisfy isDigit) <&> read @Int