{-# LANGUAGE RecordWildCards #-}
module Year2021.Day02 (solution) where

import Solution (Solution (Solution))
import Util (Parser)
import qualified Text.Megaparsec as P
import Data.Either.Combinators (rightToMaybe)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec (sepBy, (<|>))
import qualified Text.Megaparsec.Char.Lexer as L

data Command = Forward Int | Down Int | Up Int

data Position = Position { horizontal :: Int, depth :: Int, aim :: Int }

parse :: String -> Maybe [Command]
parse = rightToMaybe . P.parse commands ""
  where
    commands :: Parser [Command]
    commands = command `sepBy` char '\n'

    command = (Forward <$> (string "forward " *> L.decimal)) <|> (Down <$> (string "down " *> L.decimal)) <|> (Up <$> (string "up " *> L.decimal))

solve :: (Position -> Command -> Position) -> [Command] -> Int
solve doCommand commands = horizontal finalPos * depth finalPos
  where
    finalPos = foldl doCommand (Position 0 0 0) commands

part1 :: [Command] -> Int
part1 = solve doCommand
  where
    doCommand ::  Position -> Command -> Position
    doCommand Position {..} command = case command of
      Forward n -> Position (horizontal + n) depth aim
      Down n -> Position horizontal (depth + n) aim
      Up n -> Position horizontal (depth - n) aim

part2 :: [Command] -> Int
part2 = solve doCommand
  where
    doCommand ::  Position -> Command -> Position
    doCommand Position {..} command = case command of
      Forward n -> Position (horizontal + n) (depth + aim * n) aim
      Down n -> Position horizontal depth (aim + n)
      Up n -> Position horizontal depth (aim - n)

solution :: Solution [Command] Int Int
solution = Solution "Day 2" "input/Year2021/day2.txt" parse part1 part2
