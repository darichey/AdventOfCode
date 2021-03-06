module Year2020.Year2020 (year2020) where

import Solution (Year (..), printSolution, todo)
import qualified Year2020.Day01
import qualified Year2020.Day02
import qualified Year2020.Day03
import qualified Year2020.Day04
import qualified Year2020.Day05
import qualified Year2020.Day06
import qualified Year2020.Day07
import qualified Year2020.Day08
import qualified Year2020.Day09
import qualified Year2020.Day10
import qualified Year2020.Day11
import qualified Year2020.Day12
import qualified Year2020.Day13
import qualified Year2020.Day14
import qualified Year2020.Day15
import qualified Year2020.Day16
import qualified Year2020.Day17
import qualified Year2020.Day18
import qualified Year2020.Day19

year2020 :: Year
year2020 =
  Year
    [ printSolution Year2020.Day01.solution,
      printSolution Year2020.Day02.solution,
      printSolution Year2020.Day03.solution,
      printSolution Year2020.Day04.solution,
      printSolution Year2020.Day05.solution,
      printSolution Year2020.Day06.solution,
      printSolution Year2020.Day07.solution,
      printSolution Year2020.Day08.solution,
      printSolution Year2020.Day09.solution,
      printSolution Year2020.Day10.solution,
      printSolution Year2020.Day11.solution,
      printSolution Year2020.Day12.solution,
      printSolution Year2020.Day13.solution,
      printSolution Year2020.Day14.solution,
      printSolution Year2020.Day15.solution,
      printSolution Year2020.Day16.solution,
      printSolution Year2020.Day17.solution,
      printSolution Year2020.Day18.solution,
      printSolution Year2020.Day19.solution,
      printSolution $ todo "Day 20",
      printSolution $ todo "Day 21",
      printSolution $ todo "Day 22",
      printSolution $ todo "Day 23",
      printSolution $ todo "Day 24",
      printSolution $ todo "Day 25"
    ]
