module Year2020.Day02 where

import Data.Ix (inRange)
import Text.Parsec (alphaNum, anyChar, char, many, sepBy, space, string)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (nat)
import Util (count, occurrences, xor)
import Data.Either.Combinators (rightToMaybe)
import Solution (Solution(Solution))

data Password = Password {num1 :: Int, num2 :: Int, letter :: Char, str :: String}

parse :: String -> Maybe [Password]
parse input = rightToMaybe $ P.parse passwords "" input
  where
    passwords :: Parser [Password]
    passwords = password `sepBy` string "\n"

    password :: Parser Password
    password = do
      num1 <- nat
      char '-'
      num2 <- nat
      space
      letter <- anyChar
      string ": "
      str <- many alphaNum
      return $ Password num1 num2 letter str

part1 :: [Password] -> Int
part1 = count valid
  where
    valid p = inRange (num1 p, num2 p) (occurrences (letter p) (str p))

part2 :: [Password] -> Int
part2 = count valid
  where
    valid p = xor (validChar p num1) (validChar p num2)
    validChar p num = str p !! (num p - 1) == letter p

solution :: Solution [Password] Int Int
solution = Solution "Day 2" "input/Year2020/day2.txt" parse part1 part2