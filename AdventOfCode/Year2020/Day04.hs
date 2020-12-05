module Year2020.Day04 where

import Control.Monad (guard, void)
import Data.Either (fromRight)
import Data.Ix (Ix (inRange))
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Solution (Solution (Solution))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (nat)
import Util (count)
import Text.Parsec (space, alphaNum, many, sepBy, oneOf, char, (<|>), choice, try, string, digit)

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
passport = entry `sepBy` space

entry :: Parser String
entry = do
  key <- many alphaNum
  char ':'
  case key of
    "byr" -> nat >>= guardInRange (1920, 2002)
    "iyr" -> nat >>= guardInRange (2010, 2020)
    "eyr" -> nat >>= guardInRange (2020, 2030)
    "hgt" -> cm <|> inch
    "hcl" -> char '#' *> many (oneOf "0123456789abcdef") `ofLength` 6
    "ecl" -> void $ choice (try . string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    "pid" -> many digit `ofLength` 9
    "cid" -> void $ many alphaNum
  return key
  where
    guardInRange range = guard . inRange range
    ofLength p n = p >>= (guard . (== n) . length)
    cm = try (nat <* string "cm") >>= guardInRange (150, 193)
    inch = try (nat <* string "in") >>= guardInRange (59,76)

solution :: Solution [String] Int Int
solution = Solution "Day 4" "input/Year2020/day4.txt" parse part1 part2