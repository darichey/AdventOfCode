{-# LANGUAGE RecordWildCards #-}

module Year2020.Day19 where

import Data.Either.Combinators (fromRight')
import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import Solution (Solution (Solution))
import Text.Megaparsec (sepBy, try, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, letterChar, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Regex.PCRE ((=~))
import Util (Parser, occurrences)

data Rule = Str String | Many [Int] | Or [Int] [Int] | Star Int | Thing Int Int deriving (Show)

data Input = Input {rules :: IntMap.IntMap Rule, messages :: [String]}

parse :: String -> Maybe Input
parse input = Just $ Input (IntMap.fromList rulesP) messages
  where
    [rulesPart, stringsPart] = splitOn "\n\n" input

    rulesP :: [(Int, Rule)]
    rulesP = fromRight' $ P.parse (rule `sepBy` newline) "" rulesPart
      where
        rule :: Parser (Int, Rule)
        rule = do
          i <- L.decimal
          _ <- string ": "
          r <- try str <|> try many <|> try or
          return (i, r)

        str = Str <$> (char '\"' *> P.many letterChar <* char '\"')
        many = Many <$> (L.decimal `sepBy` char ' ')
        or = do
          a <- P.many (L.decimal <* char ' ')
          _ <- string "| "
          b <- L.decimal `sepBy` char ' '
          return $ Or a b

    messages = lines stringsPart

rulesToRegex :: IntMap.IntMap Rule -> String
rulesToRegex rules = "^" ++ go 0 ++ "$"
  where
    go :: Int -> String
    go rule = case rules IntMap.! rule of
      (Str x) -> "(" ++ x ++ ")"
      (Many xs) -> "(" ++ (xs >>= ((\f -> "(" ++ f ++ ")") . go)) ++ ")"
      (Or a b) -> "((" ++ (a >>= ((\f -> "(" ++ f ++ ")") . go)) ++ ")|(" ++ (b >>= ((\f -> "(" ++ f ++ ")") . go)) ++ "))"
      (Star x) -> "((" ++ go x ++ ")+)"
      (Thing a b) -> "(" ++ "(?'foo'(" ++ go a ++ ")(?&foo)?(" ++ go b ++ "))" ++ ")"

part1 :: Input -> Int
part1 Input {..} = occurrences True (fmap (=~ rulesToRegex rules) messages)

part2 :: Input -> Int
part2 Input {..} = occurrences True (fmap (=~ rulesToRegex fixed) messages)
  where
    fixed = IntMap.insert 11 (Thing 42 31) (IntMap.insert 8 (Star 42) rules)

solution :: Solution Input Int Int
solution = Solution "Day 19" "input/Year2020/day19.txt" parse part1 part2
