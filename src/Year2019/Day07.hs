module Year2019.Day07 (solution) where

import Data.List (permutations, unfoldr)
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import qualified Data.Vector as V
import Solution (Solution (Solution))

newtype IntCode = IntCode (V.Vector Int) deriving (Show)

data Program = Program Int [Int] [Int] IntCode deriving (Show)

parse :: String -> Maybe Program
parse = Just . Program 0 [] [] . IntCode . V.fromList . fmap read . splitOn ","

valueOf :: IntCode -> Param -> Int
valueOf code (Pos x) = valueAt code x
valueOf _ (Imm x) = x

valueAt :: IntCode -> Int -> Int
valueAt (IntCode code) x = code V.! x

values :: IntCode -> [Param] -> [Int]
values = fmap . valueOf

update :: Param -> Int -> IntCode -> IntCode
update (Pos z) xy (IntCode code) = IntCode $ code V.// [(z, xy)]

combine :: (Int -> Int -> Int) -> IntCode -> Param -> Param -> Int
combine f _ (Imm x) (Imm y) = f x y
combine f code (Imm x) y = f x (valueOf code y)
combine f code x (Imm y) = f (valueOf code x) y
combine f code x y = f (valueOf code x) (valueOf code y)

data Param
  = Pos Int
  | Imm Int
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
  | Terminate
  deriving (Show)

data EndState = NotStarted | Suspended | Terminated deriving (Show, Eq)

state :: Program -> EndState
state (Program ptr _ _ _) | ptr == -1 = Terminated
state prog@(Program _ input _ _) =
  case (nextIns prog, input) of
    (In _, []) -> Suspended
    _ -> NotStarted

nextIns :: Program -> Ins
nextIns (Program ptr _ _ code) =
  case op of
    1 -> Add (param 1 c) (param 2 b) (param 3 a)
    2 -> Mul (param 1 c) (param 2 b) (param 3 a)
    3 -> In (param 1 c)
    4 -> Out (param 1 c)
    5 -> JumpT (param 1 c) (param 2 b)
    6 -> JumpF (param 1 c) (param 2 b)
    7 -> Less (param 1 c) (param 2 b) (param 3 a)
    8 -> Equals (param 1 c) (param 2 b) (param 3 a)
    99 -> Terminate
  where
    [e, d, c, b, a] = digits 5 (valueAt code ptr)
    op = d * 10 + e

    param :: Int -> Int -> Param
    param offset mode =
      case mode of
        0 -> Pos
        1 -> Imm
        $ valueAt code (ptr + offset)

digits :: Int -> Int -> [Int]
digits n num = take n (unfoldr (\b -> Just $ swap (divMod b 10)) num)

withInput :: [Int] -> Program -> Program
withInput input (Program ptr _ output code) = Program ptr input output code

output :: Program -> [Int]
output (Program _ _ output _) = output

run :: Program -> ([Int], Program)
run prog = (out, prog')
  where
    prog' = until (\p -> let s = state p in s == Terminated || s == Suspended) apply prog
    out = output prog'

apply :: Program -> Program
apply ps@(Program ptr input output code) = case nextIns ps of
  (Add p1 p2 p3) -> Program (ptr + 4) input output code'
    where
      code' = update p3 (combine (+) code p1 p2) code
  (Mul p1 p2 p3) -> Program (ptr + 4) input output code'
    where
      code' = update p3 (combine (*) code p1 p2) code
  (In p1) -> Program (ptr + 2) input' output code'
    where
      (i : input') = input
      code' = update p1 i code
  (Out p1) -> Program (ptr + 2) input (x : output) code
    where
      x = valueOf code p1
  (JumpT p1 p2) -> Program ptr' input output code
    where
      [x, y] = values code [p1, p2]
      ptr' = if x /= 0 then y else ptr + 3
  (JumpF p1 p2) -> Program ptr' input output code
    where
      [x, y] = values code [p1, p2]
      ptr' = if x == 0 then y else ptr + 3
  (Less p1 p2 p3) -> Program (ptr + 4) input output code'
    where
      [x, y] = values code [p1, p2]
      code' = update p3 (if x < y then 1 else 0) code
  (Equals p1 p2 p3) -> Program (ptr + 4) input output code'
    where
      [x, y] = values code [p1, p2]
      code' = update p3 (if x == y then 1 else 0) code
  Terminate -> Program (-1) input output code

part1 :: Program -> Int
part1 prog = maximum outputs
  where
    inputs = permutations [0, 1, 2, 3, 4]
    outputs = fmap (\[a, b, c, d, e] -> f prog a b c d e) inputs

f :: Program -> Int -> Int -> Int -> Int -> Int -> Int
f prog a b c d e =
  let outA = head $ fst $ run (withInput [a, 0] prog)
      outB = head $ fst $ run (withInput [b, outA] prog)
      outC = head $ fst $ run (withInput [c, outB] prog)
      outD = head $ fst $ run (withInput [d, outC] prog)
      outE = head $ fst $ run (withInput [e, outD] prog)
   in outE

part2 :: Program -> Int
part2 prog = maximum outputs
  where
    inputs = permutations [5, 6, 7, 8, 9]
    outputs :: [Int]
    outputs = fmap (\[a, b, c, d, e] -> h prog prog prog prog prog a b c d e) inputs

h :: Program -> Program -> Program -> Program -> Program -> Int -> Int -> Int -> Int -> Int -> Int
h progA progB progC progD progE a b c d e =
  let (outA, progA') = run (withInput [a, 0] progA)
      (outB, progB') = run (withInput [b, head outA] progB)
      (outC, progC') = run (withInput [c, head outB] progC)
      (outD, progD') = run (withInput [d, head outC] progD)
      (outE, progE') = run (withInput [e, head outD] progE)
   in g progA' progB' progC' progD' progE' (head outE)

g :: Program -> Program -> Program -> Program -> Program -> Int -> Int
g progA progB progC progD progE inA =
  let (outA, progA') = run (withInput [inA] progA)
      (outB, progB') = run (withInput [head outA] progB)
      (outC, progC') = run (withInput [head outB] progC)
      (outD, progD') = run (withInput [head outC] progD)
      (outE, progE') = run (withInput [head outD] progE)
   in if state progE' == Terminated
        then head outE
        else g progA' progB' progC' progD' progE' (head outE)

solution :: Solution Program Int Int
solution = Solution "Day 7" "input/Year2019/day7.txt" parse part1 part2
