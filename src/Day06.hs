module Day06 where

import Data.List.Split
import Data.List
import qualified Data.Map as M
import Data.Function
import Data.Tuple
import Data.Maybe
import qualified Data.Set as S

type Graph n = M.Map n [n]
type Edge n = (n, n)

numOrbits :: Graph String -> Int
numOrbits orbits = sum $ allPaths
    where
        planets = S.toList $ S.fromList $ concat (M.elems orbits)
        allPaths = (fmap (\n -> shortestPath "COM" n orbits) planets)

getInput :: IO (Graph String)
getInput = do
    content <- readFile "input/day6.txt"
    let pairs = fmap ((\[x,y] -> (x,y)) . (splitOn ")")) (lines content)
    return $ foldl addEdge M.empty pairs

paths :: (Ord n, Eq n) => n -> n -> [n] -> Graph n -> [[n]]
paths from to path _ | from == to = [reverse path]
paths from to path graph = adjacentTo from graph
                         & filter (not . (flip elem) path)
                         >>= (\n -> paths n to (n:path) graph)

adjacentTo :: (Ord n) => n -> Graph n -> [n]
adjacentTo n graph = fromMaybe [] (M.lookup n graph)

addEdge :: (Ord n) => Graph n -> Edge n -> Graph n
addEdge graph p = addEdge' (addEdge' graph p) (swap p)
    where
        addEdge' graph (from,to) = M.insertWith (++) from [to] graph

shortestPath :: (Ord n) => n -> n -> Graph n -> Int
shortestPath from to graph = (minimum (fmap length (paths from to [] graph)))

day06a :: Graph String -> Int
day06a = numOrbits

day06b :: Graph String -> Int
day06b orbits = (shortestPath "YOU" "SAN" orbits) - 2

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day06a input, day06b input)