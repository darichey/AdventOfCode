module Year2019.Day14 (solution) where

import Data.Either.Combinators (rightToMaybe)
import qualified Data.Map as Map
import Solution (Solution (Solution))
import Text.Parsec (char, letter, many1, sepBy, space, string)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

data Chemical = Chemical String Int
  deriving (Show, Eq)

data Reaction = Reaction Chemical [Chemical]
  deriving (Show, Eq)

type Recipes = Map.Map String Reaction

type Leftovers = Map.Map String Int

multiplyChem :: Chemical -> Int -> Chemical
multiplyChem (Chemical name amt) n = Chemical name (amt * n)

takeLeftovers :: Chemical -> Leftovers -> (Chemical, Leftovers)
takeLeftovers (Chemical name amt) leftovers = (Chemical name stillNeeded, leftovers')
  where
    alreadyHave = Map.findWithDefault 0 name leftovers
    stillNeeded = amt - min amt alreadyHave
    leftovers' = Map.insert name (max 0 (alreadyHave - amt)) leftovers

oreRequired :: Recipes -> Chemical -> Int
oreRequired recipes c = snd $ go Map.empty c
  where
    go :: Leftovers -> Chemical -> (Leftovers, Int)
    go leftovers (Chemical _ 0) = (leftovers, 0)
    go leftovers c@(Chemical "ORE" i) = (leftovers, i)
    go leftovers c = foldl goAndCombine (finalLeftovers, 0) actualIngredients
      where
        (Chemical name needed, leftovers') = takeLeftovers c leftovers
        (Reaction (Chemical _ outputAmt) input) = Map.findWithDefault undefined name recipes

        timesToDoRecipe = ceiling $ fromIntegral needed / fromIntegral outputAmt
        leftoverOutput = (timesToDoRecipe * outputAmt) - needed

        finalLeftovers = Map.insertWith (+) name leftoverOutput leftovers'
        actualIngredients = fmap (`multiplyChem` timesToDoRecipe) input

    goAndCombine :: (Leftovers, Int) -> Chemical -> (Leftovers, Int)
    goAndCombine (leftovers, x) c = (leftovers', x + y)
      where
        (leftovers', y) = go leftovers c

binarySearch :: Recipes -> Int -> Int -> Int
binarySearch recipes low high
  | high < low = low
  | ore > (10 ^ 12) = binarySearch recipes low (mid - 1)
  | ore < (10 ^ 12) = binarySearch recipes (mid + 1) high
  where
    mid = low + ((high - low) `div` 2)
    ore = oreRequired recipes (Chemical "FUEL" mid)

parse :: String -> Maybe Recipes
parse = rightToMaybe . P.parse recipes ""
  where
    chemical :: Parser Chemical
    chemical = do
      amount <- int
      _ <- space
      name <- many1 letter
      return $ Chemical name amount

    reaction :: Parser Reaction
    reaction = do
      input <- chemical `sepBy` string ", "
      _ <- string " => "
      output <- chemical
      return $ Reaction output input

    recipes :: Parser Recipes
    recipes = do
      reactions <- reaction `sepBy` char '\n'
      return $ (Map.fromList . fmap (\r@(Reaction (Chemical name _) _) -> (name, r))) reactions

part1 :: Recipes -> Int
part1 recipes = oreRequired recipes (Chemical "FUEL" 1)

part2 :: Recipes -> Int
part2 recipes = binarySearch recipes 0 (10 ^ 12) - 1

solution :: Solution Recipes Int Int
solution = Solution "Day 14" "input/Year2019/day14.txt" parse part1 part2
