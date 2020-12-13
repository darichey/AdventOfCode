{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Year2020.Day12 where

import Control.Lens (makeLenses, (%~), (&), (+~), (^.))
import Data.Either.Combinators (rightToMaybe)
import Data.Functor (($>))
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Solution (Solution (Solution))
import Text.Megaparsec (sepBy, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser)

data State = State {_pos :: V2 Int, _facing :: V2 Int}

data State' = State' {_pos' :: V2 Int, _waypoint :: V2 Int}

makeLenses ''State
makeLenses ''State'

data Move
  = North Int
  | South Int
  | East Int
  | West Int
  | TurnLeft Int
  | TurnRight Int
  | Forward Int

parse :: String -> Maybe [Move]
parse = rightToMaybe . P.parse (move `sepBy` newline) ""
  where
    move :: Parser Move
    move = do
      constructor <-
        char 'N' $> North
          <|> char 'S' $> South
          <|> char 'E' $> East
          <|> char 'W' $> West
          <|> char 'L' $> TurnLeft
          <|> char 'R' $> TurnRight
          <|> char 'F' $> Forward

      num <- L.decimal

      return $ constructor num

manhattanDistance :: V2 Int -> Int
manhattanDistance v = abs (v ^. _x) + abs (v ^. _y)

rotate :: Int -> V2 Int -> V2 Int
rotate 90 v = V2 (- v ^. _y) (v ^. _x)
rotate 180 v = V2 (- v ^. _x) (- v ^. _y)
rotate 270 v = V2 (v ^. _y) (- v ^. _x)

rotateAbout :: V2 Int -> Int -> V2 Int -> V2 Int
rotateAbout origin deg v = origin + rotate deg (v - origin)

part1 :: [Move] -> Int
part1 = manhattanDistance . _pos . finalPosition
  where
    doMove :: State -> Move -> State
    doMove state (North dy) = state & pos +~ V2 0 dy
    doMove state (South dy) = state & pos +~ V2 0 (- dy)
    doMove state (East dx) = state & pos +~ V2 dx 0
    doMove state (West dx) = state & pos +~ V2 (- dx) 0
    doMove state (TurnLeft deg) = state & facing %~ rotate deg
    doMove state (TurnRight deg) = state & facing %~ rotate (360 - deg)
    doMove state (Forward d) = state & pos +~ (state ^. facing * V2 d d)

    finalPosition :: [Move] -> State
    finalPosition = foldl doMove (State (V2 0 0) (V2 1 0))

part2 :: [Move] -> Int
part2 = manhattanDistance . _pos' . finalPosition
  where
    doMove :: State' -> Move -> State'
    doMove state (North dy) = state & waypoint +~ V2 0 dy
    doMove state (South dy) = state & waypoint +~ V2 0 (- dy)
    doMove state (East dx) = state & waypoint +~ V2 dx 0
    doMove state (West dx) = state & waypoint +~ V2 (- dx) 0
    doMove state (TurnLeft deg) = state & waypoint %~ rotateAbout (state ^. pos') deg
    doMove state (TurnRight deg) = state & waypoint %~ rotateAbout (state ^. pos') (360 - deg)
    doMove state (Forward d) =
      let toWaypoint = state ^. waypoint - state ^. pos'
       in (state & pos' +~ (toWaypoint * V2 d d) & waypoint +~ (toWaypoint * V2 d d))

    finalPosition :: [Move] -> State'
    finalPosition = foldl doMove (State' (V2 0 0) (V2 10 1))

solution :: Solution [Move] Int Int
solution = Solution "Day 12" "input/Year2020/day12.txt" parse part1 part2
