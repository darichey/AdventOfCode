module Year2019.Day06 (solution) where

import Control.Monad (msum)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Tree (Tree (..), levels, unfoldTree)
import Solution (Solution (Solution))

type Orbits = Tree String

parse :: String -> Maybe Orbits
parse content =
  let edges = fmap ((\[x, y] -> (x, y)) . splitOn ")") (lines content)
      adjList = M.fromListWith (++) (fmap (\(a, b) -> (a, [b])) edges)
   in Just $ unfoldTree (\n -> (n, fromMaybe [] (M.lookup n adjList))) "COM"

part1 :: Orbits -> Int
part1 = sum . fmap (uncurry (*)) . zip [0 ..] . fmap length . levels

part2 :: Orbits -> Int
part2 tree = lcaToYou + lcaToSan - 2
  where
    toYou = fromJust $ pathTo "YOU" tree
    toSan = fromJust $ pathTo "SAN" tree
    common = length $ takeWhile (uncurry (==)) (zip toYou toSan)
    lcaToYou = length toYou - common
    lcaToSan = length toSan - common

pathTo :: (Eq a) => a -> Tree a -> Maybe [a]
pathTo n Node {rootLabel = root, subForest = children}
  | root == n = Just [n]
  | null children = Nothing
  | otherwise = (root :) <$> msum (fmap (pathTo n) children)

solution :: Solution Orbits Int Int
solution = Solution "Day 6" "input/Year2019/day6.txt" parse part1 part2
