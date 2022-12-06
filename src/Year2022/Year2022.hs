module Year2022.Year2022 (year2022) where

import Solution (Year (..), printSolution, todo)
import qualified Year2022.Day01
import qualified Year2022.Day02
import qualified Year2022.Day03
import qualified Year2022.Day04
import qualified Year2022.Day05

year2022 :: Year
year2022 =
  Year
    [ printSolution Year2022.Day01.solution,
      printSolution Year2022.Day02.solution,
      printSolution Year2022.Day03.solution,
      printSolution Year2022.Day04.solution,
      printSolution Year2022.Day05.solution,
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
