{-# LANGUAGE RecordWildCards #-}

module Year2020.Day08 (solution) where

import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (find)
import Data.Functor (($>))
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)
import qualified Data.Vector as Vector
import Solution (Solution (Solution))
import Text.Megaparsec (sepBy, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (newline, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser)

data Ins = Nop Int | Acc Int | Jmp Int

data Program = Program {acc :: Int, curIns :: Int, instructions :: Vector.Vector Ins}

data EndState = Finished Int | InfiniteLoop Int

getAcc :: EndState -> Int
getAcc (Finished x) = x
getAcc (InfiniteLoop x) = x

isFinished :: EndState -> Bool
isFinished (Finished _) = True
isFinished _ = False

parse :: String -> Maybe Program
parse = fmap (Program 0 0 . Vector.fromList) . (rightToMaybe . P.parse (ins `sepBy` newline) "")
  where
    ins :: Parser Ins
    ins = do
      constructor <-
        string "nop" $> Nop
          <|> string "acc" $> Acc
          <|> string "jmp" $> Jmp
      spaceChar
      num <- L.signed (pure ()) L.decimal
      return $ constructor num

run :: Program -> EndState
run = go IntSet.empty
  where
    go :: IntSet.IntSet -> Program -> EndState
    go visited Program {..}
      | curIns == Vector.length instructions = Finished acc
      | IntSet.member curIns visited = InfiniteLoop acc
      | otherwise = go (IntSet.insert curIns visited) (step $ instructions Vector.! curIns)
      where
        step (Nop _) = Program acc (curIns + 1) instructions
        step (Acc x) = Program (acc + x) (curIns + 1) instructions
        step (Jmp x) = Program acc (curIns + x) instructions

part1 :: Program -> Int
part1 = getAcc . run

part2 :: Program -> Int
part2 = getAcc . fromJust . find isFinished . fmap run . swapped
  where
    swapped :: Program -> [Program]
    swapped Program {..} = Vector.toList $ Program acc curIns <$> Vector.imap replace instructions
      where
        replace :: Int -> Ins -> Vector.Vector Ins
        replace i (Nop x) = instructions Vector.// [(i, Jmp x)]
        replace i (Jmp x) = instructions Vector.// [(i, Nop x)]
        replace _ _ = instructions

solution :: Solution Program Int Int
solution = Solution "Day 8" "input/Year2020/day8.txt" parse part1 part2