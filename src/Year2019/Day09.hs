module Year2019.Day09 (solution) where

import qualified Data.IntMap.Strict as M
import Data.List (unfoldr)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import Solution (Solution (Solution))

newtype IntCode = IntCode (M.IntMap Int) deriving (Show)

data Program = Program Int [Int] [Int] Int IntCode deriving (Show)

parse :: String -> Maybe Program
parse input =
  let ins = M.fromAscList $ zip [0 ..] (fmap read (splitOn "," input))
   in Just $ Program 0 [] [] 0 (IntCode ins)

valueOf :: Program -> Param -> Int
valueOf (Program _ _ _ _ code) (Pos x) = valueAt code x
valueOf _ (Imm x) = x
valueOf (Program _ _ _ relBase code) (Rel x) = valueAt code (relBase + x)

valueAt :: IntCode -> Int -> Int
valueAt (IntCode code) x = code M.! x

values :: Program -> [Param] -> [Int]
values = fmap . valueOf

update' :: Int -> Int -> IntCode -> IntCode
update' at value (IntCode code) = IntCode code'
  where
    code' = M.insert at value code

update :: Param -> Int -> Program -> IntCode
update (Pos z) xy (Program _ _ _ _ code) = update' z xy code
update (Rel z) xy (Program _ _ _ relBase code) = update' (relBase + z) xy code

combine :: (Int -> Int -> Int) -> Program -> Param -> Param -> Int
combine f _ (Imm x) (Imm y) = f x y
combine f prog (Imm x) y = f x (valueOf prog y)
combine f prog x (Imm y) = f (valueOf prog x) y
combine f prog x y = f (valueOf prog x) (valueOf prog y)

data Param
  = Pos Int
  | Imm Int
  | Rel Int
  deriving (Show)

data Ins
  = Add Param Param Param
  | Mul Param Param Param
  | In Param
  | Out Param
  | JumpT Param Param
  | JumpF Param Param
  | Less Param Param Param
  | Equals Param Param Param
  | SetRel Param
  | Terminate
  deriving (Show)

data EndState = NotStarted | Suspended | Terminated deriving (Show, Eq)

state :: Program -> EndState
state (Program ptr _ _ _ _) | ptr == -1 = Terminated
state prog@(Program _ input _ _ _) =
  case (nextIns prog, input) of
    (In _, []) -> Suspended
    _ -> NotStarted

nextIns :: Program -> Ins
nextIns (Program ptr _ _ _ code) =
  case op of
    1 -> Add (param 1 c) (param 2 b) (param 3 a)
    2 -> Mul (param 1 c) (param 2 b) (param 3 a)
    3 -> In (param 1 c)
    4 -> Out (param 1 c)
    5 -> JumpT (param 1 c) (param 2 b)
    6 -> JumpF (param 1 c) (param 2 b)
    7 -> Less (param 1 c) (param 2 b) (param 3 a)
    8 -> Equals (param 1 c) (param 2 b) (param 3 a)
    9 -> SetRel (param 1 c)
    99 -> Terminate
  where
    [e, d, c, b, a] = digits 5 (valueAt code ptr)
    op = d * 10 + e

    param :: Int -> Int -> Param
    param offset mode =
      case mode of
        0 -> Pos
        1 -> Imm
        2 -> Rel
        $ valueAt code (ptr + offset)

    digits :: Int -> Int -> [Int]
    digits n num = take n (unfoldr (\b -> Just $ swap (divMod b 10)) num)

withInput :: [Int] -> Program -> Program
withInput input (Program ptr _ output relBase code) = Program ptr input output relBase code

output :: Program -> [Int]
output (Program _ _ output _ _) = output

shouldStop :: Program -> Bool
shouldStop p = let s = state p in s == Terminated || s == Suspended

run :: Program -> Program
run = until shouldStop apply

apply :: Program -> Program
apply ps@(Program ptr input output relBase code) = case nextIns ps of
  (Add p1 p2 p3) -> Program (ptr + 4) input output relBase code'
    where
      code' = update p3 (combine (+) ps p1 p2) ps
  (Mul p1 p2 p3) -> Program (ptr + 4) input output relBase code'
    where
      code' = update p3 (combine (*) ps p1 p2) ps
  (In p1) -> Program (ptr + 2) input' output relBase code'
    where
      (i : input') = input
      code' = update p1 i ps
  (Out p1) -> Program (ptr + 2) input (x : output) relBase code
    where
      x = valueOf ps p1
  (JumpT p1 p2) -> Program ptr' input output relBase code
    where
      [x, y] = values ps [p1, p2]
      ptr' = if x /= 0 then y else ptr + 3
  (JumpF p1 p2) -> Program ptr' input output relBase code
    where
      [x, y] = values ps [p1, p2]
      ptr' = if x == 0 then y else ptr + 3
  (Less p1 p2 p3) -> Program (ptr + 4) input output relBase code'
    where
      [x, y] = values ps [p1, p2]
      code' = update p3 (if x < y then 1 else 0) ps
  (Equals p1 p2 p3) -> Program (ptr + 4) input output relBase code'
    where
      [x, y] = values ps [p1, p2]
      code' = update p3 (if x == y then 1 else 0) ps
  (SetRel p1) -> Program (ptr + 2) input output (relBase + offset) code
    where
      offset = valueOf ps p1
  Terminate -> Program (-1) input output relBase code

part1 :: Program -> Int
part1 = head . output . run . withInput [1]

part2 :: Program -> Int
part2 = head . output . run . withInput [2]

solution :: Solution Program Int Int
solution = Solution "Day 9" "input/Year2019/day9.txt" parse part1 part2
