{-# LANGUAGE LambdaCase #-}

module Year2022.Day02 (solution) where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Solution (Solution (Solution))

data Shape = Rock | Paper | Scissors

data Outcome = Win | Loss | Tie

outcomeOfShapes :: Shape -> Shape -> Outcome
outcomeOfShapes Rock Paper = Win
outcomeOfShapes Rock Scissors = Loss
outcomeOfShapes Paper Scissors = Win
outcomeOfShapes Paper Rock = Loss
outcomeOfShapes Scissors Rock = Win
outcomeOfShapes Scissors Paper = Loss
outcomeOfShapes _ _ = Tie

scoreOfShape :: Shape -> Int
scoreOfShape = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

scoreOfOutcome :: Outcome -> Int
scoreOfOutcome = \case
  Win -> 6
  Loss -> 0
  Tie -> 3

shapeOfLetter :: Char -> Shape
shapeOfLetter = \case
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors

outcomeOfLetter :: Char -> Outcome
outcomeOfLetter = \case
  'X' -> Loss
  'Y' -> Tie
  'Z' -> Win

shapeToPlay :: Shape -> Outcome -> Shape
shapeToPlay Rock Win = Paper
shapeToPlay Rock Loss = Scissors
shapeToPlay Paper Win = Scissors
shapeToPlay Paper Loss = Rock
shapeToPlay Scissors Win = Rock
shapeToPlay Scissors Loss = Paper
shapeToPlay m Tie = m

parse :: String -> Maybe [(Char, Char)]
parse = Just . fmap ((\[opponent, me] -> (head opponent, head me)) . words) . lines

part1 :: [(Char, Char)] -> Int
part1 =
  sum
    . fmap
      ( ( \(opponent, me) ->
            scoreOfOutcome (outcomeOfShapes opponent me) + scoreOfShape me
        )
          . join (***) shapeOfLetter
      )

part2 :: [(Char, Char)] -> Int
part2 =
  sum
    . fmap
      ( ( \(opponent, outcome) ->
            scoreOfOutcome outcome + scoreOfShape (shapeToPlay opponent outcome)
        )
          . bimap shapeOfLetter outcomeOfLetter
      )

solution :: Solution [(Char, Char)] Int Int
solution = Solution "Day 2" "input/Year2022/day2.txt" parse part1 part2
