module Year2020.Day04 (solution) where

import Control.Monad (guard, void)
import Data.Either (fromRight)
import Data.Ix (Ix (inRange))
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Solution (Solution (Solution))
import Text.Megaparsec (choice, many, oneOf, sepBy, some, try, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser, count)

parse :: String -> Maybe [String]
parse = Just . splitOn "\n\n"

hasAllKeys :: String -> Bool
hasAllKeys input = and $ (`isInfixOf` input) <$> ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 :: [String] -> Int
part1 = count hasAllKeys

part2 :: [String] -> Int
part2 = count valid
  where
    valid input = fromRight False $ hasAllKeys . show <$> P.parse passport "" input

passport :: Parser [String]
passport = entry `sepBy` spaceChar

entry :: Parser String
entry = do
  key <- some alphaNumChar
  _ <- char ':'
  case key of
    "byr" -> L.decimal >>= guardInRange (1920, 2002)
    "iyr" -> L.decimal >>= guardInRange (2010, 2020)
    "eyr" -> L.decimal >>= guardInRange (2020, 2030)
    "hgt" -> cm <|> inch
    "hcl" -> char '#' *> many (oneOf "0123456789abcdef") `ofLength` 6
    "ecl" -> void $ choice (try . string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    "pid" -> many digitChar `ofLength` 9
    "cid" -> void $ many alphaNumChar
  return key
  where
    guardInRange range = guard . inRange range
    ofLength p n = p >>= (guard . (== n) . length)
    cm = try (L.decimal <* string "cm") >>= guardInRange (150, 193)
    inch = try (L.decimal <* string "in") >>= guardInRange (59, 76)

solution :: Solution [String] Int Int
solution = Solution "Day 4" "input/Year2020/day4.txt" parse part1 part2
