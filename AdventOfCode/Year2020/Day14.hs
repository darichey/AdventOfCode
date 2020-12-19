{-# LANGUAGE RecordWildCards #-}

module Year2020.Day14 (solution) where

import Data.Bits ((.&.), (.|.))
import Data.Char (digitToInt, intToDigit)
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Map as Map
import Numeric (readInt, showIntAtBase)
import Solution (Solution (Solution))
import Text.Megaparsec (many, sepBy, try, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (alphaNumChar, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser, occurrences)

data Instruction
  = SetMask String
  | WriteValue {dest :: Integer, value :: Integer}

readBin :: String -> Integer
readBin = fst . head . readInt 2 (`elem` "01") digitToInt

parse :: String -> Maybe [Instruction]
parse = rightToMaybe . P.parse (ins `sepBy` newline) ""
  where
    ins :: Parser Instruction
    ins = try setMask <|> try writeValue

    setMask :: Parser Instruction
    setMask = do
      string "mask = "
      mask <- many alphaNumChar
      return $ SetMask mask

    writeValue :: Parser Instruction
    writeValue = do
      string "mem["
      dest <- L.decimal
      string "] = "
      value <- L.decimal
      return $ WriteValue dest value

part1 :: [Instruction] -> Integer
part1 = sum . snd . foldl doIns ("", Map.empty)
  where
    doIns :: (String, Map.Map Integer Integer) -> Instruction -> (String, Map.Map Integer Integer)
    doIns (_, mem) (SetMask m) = (m, mem)
    doIns (mask, curMem) WriteValue {..} = (mask, Map.insert dest (applyMask value mask) curMem)

    applyMask :: Integer -> String -> Integer
    applyMask i mask = (i .&. andMask) .|. orMask
      where
        orMask = readBin $ fmap (\c -> if c == '1' then '1' else '0') mask
        andMask = readBin $ fmap (\c -> if c == '0' then '0' else '1') mask

part2 :: [Instruction] -> Integer
part2 = sum . snd . foldl doIns ("", Map.empty)
  where
    doIns :: (String, Map.Map Integer Integer) -> Instruction -> (String, Map.Map Integer Integer)
    doIns (_, mem) (SetMask m) = (m, mem)
    doIns (mask, curMem) WriteValue {..} = (mask, Map.union (newValues dest value mask) curMem)

    newValues :: Integer -> Integer -> String -> Map.Map Integer Integer
    newValues dest value mask = Map.fromList $ zip (allDests dest mask) (repeat value)

    allDests :: Integer -> String -> [Integer]
    allDests originalDest mask = fmap (\b -> readBin $ replaceXs masked (leftPad '0' numXs (showIntAtBase 2 intToDigit b ""))) [0 .. 2 ^ numXs - 1]
      where
        originalDestBin = leftPad '0' 36 (showIntAtBase 2 intToDigit originalDest "")
        masked = doMask mask originalDestBin
        numXs = occurrences 'X' masked

    doMask :: String -> String -> String
    doMask mask s = fmap f (zip s mask)
      where
        f (c, '0') = c
        f (_, '1') = '1'
        f (_, 'X') = 'X'

    leftPad :: Char -> Int -> String -> String
    leftPad c n s = replicate (n - length s) c ++ s

    replaceXs :: String -> String -> String
    replaceXs original replace = go original replace ""
      where
        go :: String -> String -> String -> String
        go original "" acc = acc ++ original
        go ('X' : os) (r : rs) acc = go os rs (acc ++ (r : ""))
        go (o : os) rs acc = go os rs (acc ++ (o : ""))

solution :: Solution [Instruction] Integer Integer
solution = Solution "Day 14" "input/Year2020/day14.txt" parse part1 part2
