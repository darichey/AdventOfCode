module Year2021.Year2021 (year2021) where

import Solution (Year (..), printSolution, todo)
import qualified Year2021.Day01
import qualified Year2021.Day02
import qualified Year2021.Day03
import qualified Year2021.Day04
import qualified Year2021.Day05
import qualified Year2021.Day06
import qualified Year2021.Day07
import qualified Year2021.Day08
import qualified Year2021.Day09

year2021 :: Year
year2021 =
  Year
    [ printSolution Year2021.Day01.solution,
      printSolution Year2021.Day02.solution,
      printSolution Year2021.Day03.solution,
      printSolution Year2021.Day04.solution,
      printSolution Year2021.Day05.solution,
      printSolution Year2021.Day06.solution,
      printSolution Year2021.Day07.solution,
      printSolution Year2021.Day08.solution,
      printSolution Year2021.Day09.solution,
      printSolution $ todo "Day 10",
      printSolution $ todo "Day 11",
      printSolution $ todo "Day 12",
      printSolution $ todo "Day 13",
      printSolution $ todo "Day 14",
      printSolution $ todo "Day 15",
      printSolution $ todo "Day 16",
      printSolution $ todo "Day 17",
      printSolution $ todo "Day 18",
      printSolution $ todo "Day 19",
      printSolution $ todo "Day 20",
      printSolution $ todo "Day 21",
      printSolution $ todo "Day 22",
      printSolution $ todo "Day 23",
      printSolution $ todo "Day 24",
      printSolution $ todo "Day 25"
    ]
