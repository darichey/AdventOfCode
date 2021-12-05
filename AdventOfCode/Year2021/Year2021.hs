module Year2021.Year2021 (year2021) where

import Solution (Year (..), printSolution, todo)
import qualified Year2021.Day01
import qualified Year2021.Day02

year2021 :: Year
year2021 =
  Year
    [ printSolution Year2021.Day01.solution,
      printSolution Year2021.Day02.solution,
      printSolution $ todo "Day 2",
      printSolution $ todo "Day 3",
      printSolution $ todo "Day 4",
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
