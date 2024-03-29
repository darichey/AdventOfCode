module Year2020.Day02 (solution) where

import Data.Bits (xor)
import Data.Either.Combinators (rightToMaybe)
import Data.Ix (inRange)
import Solution (Solution (Solution))
import Text.Megaparsec (many, sepBy)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser, count, occurrences)

data Password = Password {num1 :: Int, num2 :: Int, letter :: Char, str :: String}

parse :: String -> Maybe [Password]
parse = rightToMaybe . P.parse passwords ""
  where
    passwords :: Parser [Password]
    passwords = password `sepBy` string "\n"

    password :: Parser Password
    password = do
      num1 <- L.decimal
      _ <- char '-'
      num2 <- L.decimal
      _ <- spaceChar
      letter <- letterChar
      _ <- string ": "
      str <- many alphaNumChar
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
