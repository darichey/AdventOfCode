module Year2021.Day04 (solution) where

import Solution (Solution (Solution))
import qualified Text.Megaparsec as P
import Data.Either.Combinators (rightToMaybe)
import Text.Megaparsec (sepBy, many, some, sepBy1, try, notFollowedBy)
import Text.Megaparsec.Char (char, string, newline)
import Util (Parser)
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShowId)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (transpose, find)

type InputSequence = [Int]
type Board = [[Int]]

parse :: String -> Maybe (InputSequence, [Board])
parse = rightToMaybe . P.parse input ""
  where
    input = (,) <$> inputSequence <*> (string "\n\n" *> (board `sepBy1` string "\n\n"))

    inputSequence :: Parser InputSequence
    inputSequence = L.decimal `sepBy` char ','

    board :: Parser Board
    board = (many (char ' ') *> L.decimal `sepBy1` some (char ' ')) `sepBy1` try (newline *> notFollowedBy newline)

won :: Set Int -> Board -> Bool
won marked board = any (`Set.isSubsetOf` marked) winningSets
  where
    winningSets = Set.fromList <$> board ++ transpose board

unmarkedSum :: (Num a, Ord a, Foldable t) => t [a] -> Set a -> a
unmarkedSum board marked = sum $ filter (\i -> not $ i `Set.member` marked) (concat board)

part1 :: (InputSequence, [Board]) -> Int
part1 (nums, boards) = unmarkedSum winningBoard finalMarked * lastMarked
  where
    (winningBoard, lastMarked, finalMarked) = go Set.empty 0

    go :: Set Int -> Int -> (Board, Int, Set Int)
    go marked i = case find (won marked) boards of
      Nothing -> go (Set.insert (nums !! i) marked) (i + 1)
      Just b -> (b, nums !! (i - 1), marked)

part2 :: (InputSequence, [Board]) -> Int
part2 (nums, boards) = unmarkedSum lastWinningBoard finalMarked * lastMarked
  where
    (lastWinningBoard, lastMarked, finalMarked) = traceShowId $ go Set.empty 0 boards

    go :: Set Int -> Int -> [Board] -> (Board, Int, Set Int)
    go marked i [b] = if won marked b then (b, nums !! (i - 1), marked) else go (Set.insert (nums !! i) marked) (i + 1) [b]
    go marked i remainingBoards = go (Set.insert (nums !! i) marked) (i + 1) (filter (not . won marked) remainingBoards)

solution :: Solution (InputSequence, [Board]) Int Int
solution = Solution "Day 4" "input/Year2021/day4.txt" parse part1 part2
