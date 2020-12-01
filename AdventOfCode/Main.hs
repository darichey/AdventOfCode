{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import qualified Data.Map.Ordered as OMap
import Data.Maybe (fromMaybe)
import System.Console.CmdArgs (Data, Typeable, cmdArgs, def, help, opt, typ, (&=))
import Util (printSolutions)
import qualified Year2019.Day01
import qualified Year2019.Day02
import qualified Year2019.Day03
import qualified Year2019.Day04
import qualified Year2019.Day05
import qualified Year2019.Day06
import qualified Year2019.Day07
import qualified Year2019.Day08
import qualified Year2019.Day09
import qualified Year2019.Day10
import qualified Year2019.Day11
import qualified Year2019.Day12
import qualified Year2019.Day13
import qualified Year2019.Day14
import qualified Year2020.Day01

solutionsMap :: OMap.OMap [Char] (OMap.OMap [Char] (IO ()))
solutionsMap =
  OMap.fromList
    [ ( "2019",
        OMap.fromList
          [ ("1", printSolutions "Day 1" Year2019.Day01.solutions),
            ("2", printSolutions "Day 2" Year2019.Day02.solutions),
            ("3", printSolutions "Day 3" Year2019.Day03.solutions),
            ("4", printSolutions "Day 4" Year2019.Day04.solutions),
            ("5", printSolutions "Day 5" Year2019.Day05.solutions),
            ("6", printSolutions "Day 6" Year2019.Day06.solutions),
            ("7", printSolutions "Day 7" Year2019.Day07.solutions),
            ("8", printSolutions "Day 8" Year2019.Day08.solutions),
            ("9", printSolutions "Day 9" Year2019.Day09.solutions),
            ("10", printSolutions "Day 10" Year2019.Day10.solutions),
            ("12", printSolutions "Day 11" Year2019.Day11.solutions),
            ("12", printSolutions "Day 12" Year2019.Day12.solutions),
            ("13", printSolutions "Day 13" Year2019.Day13.solutions),
            ("14", printSolutions "Day 14" Year2019.Day14.solutions)
          ]
      ),
      ( "2020",
        OMap.fromList
          [ ("1", printSolutions "Day 1" Year2020.Day01.solutions)
          ]
      )
    ]

data Args = Args {year :: String, day :: String}
  deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs =
  Args
    { year = def &= help "The year to execute" &= typ "YEAR",
      day = "ALL" &= help "The day to execute" &= typ "DAY" &= opt "ALL"
    }

main :: IO ()
main = do
  Args {year = year, day = day} <- cmdArgs defaultArgs
  if day == "ALL"
    then
      let all = fmap (mapM_ snd . OMap.assocs) (OMap.lookup year solutionsMap)
       in fromMaybe (putStrLn $ "No such year " ++ year) all
    else
      let sol = OMap.lookup year solutionsMap >>= OMap.lookup day
       in fromMaybe (putStrLn $ "No solution for " ++ year ++ "." ++ day) sol