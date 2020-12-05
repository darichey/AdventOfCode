module Year2020.Year2020 (year2020) where

import Solution (Year (..), printSolution, todo)
import qualified Year2020.Day01
import qualified Year2020.Day02
import qualified Year2020.Day03
import qualified Year2020.Day04

year2020 :: Year
year2020 =
  Year
    [ printSolution Year2020.Day01.solution,
      printSolution Year2020.Day02.solution,
      printSolution Year2020.Day03.solution,
      printSolution Year2020.Day04.solution,
      printSolution $ todo "Day 5",
      printSolution $ todo "Day 6",
      printSolution $ todo "Day 7",
      printSolution $ todo "Day 8",
      printSolution $ todo "Day 9",
      printSolution $ todo "Day 10",
      printSolution $ todo "Day 11",
      printSolution $ todo "Day 12",
      printSolution $ todo "Day 13",
      printSolution $ todo "Day 14",
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
