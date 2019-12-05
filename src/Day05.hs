module Day05 where

import Data.Char
import Data.List.Split
import Debug.Trace
import qualified Data.Vector as V

type Program = V.Vector Int
data Op = Add | Mul | In | Out | JumpT | JumpF | Less | Equals | Terminate deriving (Show)
data PMode = Pos | Imm deriving (Show)
data Ins = Ins Op PMode PMode PMode deriving (Show)

leftPad :: Char -> String -> Int -> String
leftPad with str num = (replicate (num - length str) with) ++ str

parseIns :: Int -> Ins
parseIns ins = Ins (op de) (mode c) (mode b) (mode a)
    where
        (a:b:c:de) = leftPad '0' (show ins) 5
        op :: String -> Op
        op "01" = Add
        op "02" = Mul
        op "03" = In
        op "04" = Out
        op "05" = JumpT
        op "06" = JumpF
        op "07" = Less
        op "08" = Equals
        op "99" = Terminate
        mode :: Char -> PMode
        mode '0' = Pos
        mode '1' = Imm

run :: [Int] -> Program -> [Int]
run input prog = fst $ run' 0 input [] prog

run' :: Int -> [Int] -> [Int] -> Program -> ([Int], Program)
run' (-1) _ output prog = (output, prog)
run' ptr input output prog = let
    ins = traceShowId $ parseIns (prog V.! ptr)
    (ptr', input', output', prog') = apply ptr input ins prog

    in run' ptr' input' (output' ++ output) prog'

apply :: Int -> [Int] -> Ins -> Program -> (Int, [Int], [Int], Program)
apply ptr input (Ins Add mode1 mode2 mode3) prog = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, x + y)]

apply ptr input (Ins Mul mode1 mode2 mode3) prog = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, x * y)]

apply ptr input (Ins In mode1 mode2 mode3) prog = (ptr + 2, tail input, [], prog')
    where
        x = prog V.! (ptr + 1)
        prog' = prog V.// [(x, head input)]

apply ptr input (Ins Out mode1 mode2 mode3) prog = (ptr + 2, input, [x], prog)
    where
        x = param (ptr + 1) mode1 prog

apply ptr input (Ins JumpT mode1 mode2 mode3) prog = (ptr', input, [], prog)
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        ptr' = if x /= 0 then y else ptr + 3

apply ptr input (Ins JumpF mode1 mode2 mode3) prog = (ptr', input, [], prog)
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        ptr' = if x == 0 then y else ptr + 3

apply ptr input (Ins Less mode1 mode2 mode3) prog = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, if x < y then 1 else 0)]

apply ptr input (Ins Equals mode1 mode2 mode3) prog = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, if x == y then 1 else 0)]

apply ptr input (Ins Terminate _ _ _) prog = (-1, input, [], prog)

param :: Int -> PMode -> Program -> Int
param ptr Pos prog = prog V.! (prog V.! ptr)
param ptr Imm prog = prog V.! ptr

getInput :: IO Program
getInput = (V.fromList . (fmap read) . (splitOn ",")) <$> readFile "input/day5.txt"