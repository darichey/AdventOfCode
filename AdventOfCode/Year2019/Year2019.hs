module Year2019.Year2019 (year2019) where

import Solution (Year (..), printSolution, todo)
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

year2019 :: Year
year2019 =
  Year
    [ printSolution Year2019.Day01.solution,
      printSolution Year2019.Day02.solution,
      printSolution Year2019.Day03.solution,
      printSolution Year2019.Day04.solution,
      printSolution Year2019.Day05.solution,
      printSolution Year2019.Day06.solution,
      printSolution Year2019.Day07.solution,
      printSolution Year2019.Day08.solution,
      printSolution Year2019.Day09.solution,
      printSolution Year2019.Day10.solution,
      printSolution Year2019.Day11.solution,
      printSolution Year2019.Day12.solution,
      printSolution Year2019.Day13.solution,
      printSolution Year2019.Day14.solution,
      printSolution $ todo "Day 15",
      printSolution $ todo "Day 16",
      printSolution $ todo "Day 18",
      printSolution $ todo "Day 19",
      printSolution $ todo "Day 20",
      printSolution $ todo "Day 21",
      printSolution $ todo "Day 22",
      printSolution $ todo "Day 23",
      printSolution $ todo "Day 24",
      printSolution $ todo "Day 25"
    ]
