{-# LANGUAGE NamedFieldPuns #-}

module Year2020.Day02 where

import Data.Either (fromRight)
import Data.Ix (inRange)
import Text.Parsec (alphaNum, anyChar, char, many, sepBy, space, string)
import Text.Parsec.String (Parser, parseFromFile)
import Text.ParserCombinators.Parsec.Number (nat)
import Util (count, occurrences, xor)

data Password = Password {num1 :: Int, num2 :: Int, letter :: Char, str :: String}

getInput :: IO [Password]
getInput = do
  parsed <- parseFromFile input "input/Year2020/day2.txt"
  return $ fromRight (error "parse error") parsed
  where
    input :: Parser [Password]
    input = password `sepBy` string "\n"

    password :: Parser Password
    password = do
      num1 <- nat
      char '-'
      num2 <- nat
      space
      letter <- anyChar
      string ": "
      str <- many alphaNum
      return Password {num1, num2, letter, str}

day01a :: [Password] -> Int
day01a = count valid
  where
    valid p = inRange (num1 p, num2 p) (occurrences (letter p) (str p))

day01b :: [Password] -> Int
day01b = count valid
  where
    valid p = xor (validChar p num1) (validChar p num2)
    validChar p num = str p !! (num p - 1) == letter p

solutions :: IO (Int, Int)
solutions = do
  input <- getInput
  return (day01a input, day01b input)