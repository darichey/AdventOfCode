module Day06 where

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.SP
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

type Orbits = (Gr String Int, M.Map String Int)

getInput :: IO Orbits
getInput = do
    content <- readFile "input/day6.txt"
    let pairs = fmap ((splitOn ")") . lines) content
    let nodes = zip (S.toList $ S.fromList $ (lines content) >>= (splitOn ")")) [0..]
    let nodeMap = M.fromList nodes
    let edges = (fmap (splitOn ")") (lines content)) >>= (\[x,y] -> [(fromJust $ M.lookup x nodeMap, fromJust $ M.lookup y nodeMap, 1), (fromJust $ M.lookup y nodeMap, fromJust $ M.lookup x nodeMap, 1)])

    return $ (G.mkGraph (fmap swap nodes) edges, nodeMap)

day06a :: Orbits -> Int
day06a (graph, labels) = fromJust $ do
    com <- M.lookup "COM" labels
    let shortestPaths = spTree com graph
    return $ sum $ fmap (snd . head . G.unLPath) shortestPaths

day06b :: Orbits -> Int
day06b (graph, labels) = (fromJust shortestPath) - 2
    where
        shortestPath = do
            you <- M.lookup "YOU" labels
            san <- M.lookup "SAN" labels
            length <- spLength you san graph
            return length

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day06a input, day06b input)