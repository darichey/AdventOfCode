{-# LANGUAGE TemplateHaskell #-}

module Year2020.Day12 (solution) where

import Control.Lens (makeLenses, (%~), (&), (+~), (-~), (^.))
import Linear ((^*))
import Linear.V2 (R1 (_x), R2 (_y), V2 (..), perp)
import Solution (Solution (Solution))

data State = State {_pos :: V2 Int, _facing :: V2 Int}

makeLenses ''State

type Move = (Char, Int)

parse :: String -> Maybe [Move]
parse = Just . fmap f . lines
  where
    f (c : xs) = (c, read xs)

rotate :: Int -> V2 Int -> V2 Int
rotate deg v = iterate perp v !! (deg `div` 90)

part1 :: [Move] -> Int
part1 = sum . fmap abs . _pos . foldl doMove (State (V2 0 0) (V2 1 0))
  where
    doMove :: State -> Move -> State
    doMove state ('N', dy) = state & pos . _y +~ dy
    doMove state ('S', dy) = state & pos . _y -~ dy
    doMove state ('E', dx) = state & pos . _x +~ dx
    doMove state ('W', dx) = state & pos . _x -~ dx
    doMove state ('L', deg) = state & facing %~ rotate deg
    doMove state ('R', deg) = state & facing %~ rotate (360 - deg)
    doMove state ('F', d) = state & pos +~ (state ^. facing * V2 d d)

part2 :: [Move] -> Int
part2 = sum . fmap abs . _pos . foldl doMove (State (V2 0 0) (V2 10 1))
  where
    doMove :: State -> Move -> State
    doMove state ('N', dy) = state & facing . _y +~ dy
    doMove state ('S', dy) = state & facing . _y -~ dy
    doMove state ('E', dx) = state & facing . _x +~ dx
    doMove state ('W', dx) = state & facing . _x -~ dx
    doMove state ('L', deg) = state & facing %~ rotate deg
    doMove state ('R', deg) = state & facing %~ rotate (360 - deg)
    doMove state ('F', d) = state & pos +~ (state ^. facing ^* d)

solution :: Solution [Move] Int Int
solution = Solution "Day 12" "input/Year2020/day12.txt" parse part1 part2
