module Year2020.Day07 (solution) where

import Data.Either.Combinators (rightToMaybe)
import Data.Functor (($>))
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Solution (Solution (Solution))
import Text.Megaparsec (many, optional, sepBy, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, letterChar, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser, count)

type Rule = [(Int, String)]

type DAG = Map.Map String Rule

parse :: String -> Maybe DAG
parse = rightToMaybe . P.parse rules ""
  where
    rules :: Parser DAG
    rules = Map.fromList <$> rule `sepBy` char '\n'

    rule :: Parser (String, Rule)
    rule = do
      name <- bagName
      string " bags contain "
      children <- (hasContents <|> noContents) `sepBy` string ", "
      char '.'
      return (name, catMaybes children)

    bagName :: Parser String
    bagName = do
      w1 <- many letterChar
      spaceChar
      w2 <- many letterChar
      return $ w1 ++ " " ++ w2

    hasContents :: Parser (Maybe (Int, String))
    hasContents = do
      num <- L.decimal
      spaceChar
      name <- bagName
      string " bag"
      optional $ char 's'
      return $ Just (num, name)

    noContents :: Parser (Maybe (Int, String))
    noContents = string "no other bags" $> Nothing

part1 :: DAG -> Int
part1 graph = count (canReach graph "shiny gold") (Map.keys graph)
  where
    children :: DAG -> String -> [String]
    children graph bag = nub $ (snd <$> graph Map.! bag) >>= (\name -> name : children graph name)

    canReach :: DAG -> String -> String -> Bool
    canReach graph to from = to `elem` children graph from

part2 :: DAG -> Int
part2 graph = numBags (graph Map.! "shiny gold")
  where
    numBags :: Rule -> Int
    numBags = sum . fmap (\(n, bag) -> n + n * numBags (graph Map.! bag))

solution :: Solution DAG Int Int
solution = Solution "Day 7" "input/Year2020/day7.txt" parse part1 part2
