module Main where

import qualified Dailies.December01_1 as December01_1
import qualified Dailies.December01_2 as December01_2
import qualified Dailies.December02_1 as December02_1
import qualified Dailies.December02_2 as December02_2
import qualified Dailies.December03_1 as December03_1
import qualified Dailies.December03_2 as December03_2
import qualified Dailies.December04_1 as December04_1
import qualified Dailies.December04_2 as December04_2
import qualified Dailies.December05_1 as December05_1
import qualified Dailies.December05_2 as December05_2
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let task = head args
  solution <- case task of
    "1-1" -> show <$> December01_1.solution
    "1-2" -> show <$> December01_2.solution
    "2-1" -> show <$> December02_1.solution
    "2-2" -> show <$> December02_2.solution
    "3-1" -> show <$> December03_1.solution
    "3-2" -> show <$> December03_2.solution
    "4-1" -> show <$> December04_1.solution
    "4-2" -> show <$> December04_2.solution
    "5-1" -> show <$> December05_1.solution
    "5-2" -> show <$> December05_2.solution
  print solution