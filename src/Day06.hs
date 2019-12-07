module Day06 where

import Data.Tree
import Data.Maybe
import Data.List.Split
import Control.Monad
import qualified Data.Map as M

type Orbits = Tree String

getInput :: IO Orbits
getInput = do
    content <- readFile "input/day6.txt"
    let edges = fmap ((\[x,y] -> (x,y)) . splitOn ")") (lines content)
    let adjList = M.fromListWith (++) (fmap (\(a,b) -> (a,[b])) edges)

    return $ unfoldTree (\n -> (n, fromMaybe [] (M.lookup n adjList))) "COM"

day06a :: Orbits -> Int
day06a = sum . fmap (uncurry (*)) . zip [0..] . fmap length . levels

day06b :: Orbits -> Int
day06b tree = lcaToYou + lcaToSan - 2
    where
        toYou = fromJust $ pathTo "YOU" tree
        toSan = fromJust $ pathTo "SAN" tree
        common = length $ takeWhile (uncurry (==)) (zip toYou toSan)
        lcaToYou = (length toYou) - common
        lcaToSan = (length toSan) - common

pathTo :: (Eq a) => a -> Tree a -> Maybe [a]
pathTo n Node{rootLabel=root, subForest=children}
    | root == n     = Just [n]
    | null children = Nothing
    | otherwise     = (root:) <$> (msum $ fmap (pathTo n) children)

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day06a input, day06b input)