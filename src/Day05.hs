module Day05 where

import Data.List.Split
import qualified Data.Vector as V

type Program = V.Vector Int
data Op = Add | Mul | In | Out | JumpT | JumpF | Less | Equals | Terminate deriving (Show)
data ParamMode = Pos | Imm deriving (Show)
data Ins = Ins Op ParamMode ParamMode ParamMode deriving (Show)

parseIns :: Int -> Ins
parseIns ins = Ins (op de) (mode c) (mode b) (mode a)
    where
        (a:b:c:de) = leftPad '0' (show ins) 5

        leftPad :: Char -> String -> Int -> String
        leftPad with str num = (replicate (num - length str) with) ++ str

        op :: String -> Op
        op s = case s of
            "01" -> Add
            "02" -> Mul
            "03" -> In
            "04" -> Out
            "05" -> JumpT
            "06" -> JumpF
            "07" -> Less
            "08" -> Equals
            "99" -> Terminate

        mode :: Char -> ParamMode
        mode c = case c of
            '0' -> Pos
            '1' -> Imm

run :: [Int] -> Program -> [Int]
run input = fst . run' 0 input []
    where
        run' :: Int -> [Int] -> [Int] -> Program -> ([Int], Program)
        run' (-1) _     output prog = (output, prog)
        run' ptr  input output prog = run' ptr' input' (output' ++ output) prog'
            where
                ins = parseIns $ prog V.! ptr
                (ptr', input', output', prog') = apply ptr input prog ins

apply :: Int -> [Int] -> Program -> Ins -> (Int, [Int], [Int], Program)
apply ptr input prog (Ins Add mode1 mode2 _) = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, x + y)]

apply ptr input prog (Ins Mul mode1 mode2 _) = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, x * y)]

apply ptr input prog (Ins In mode1 _ _) = (ptr + 2, tail input, [], prog')
    where
        x = prog V.! (ptr + 1)
        prog' = prog V.// [(x, head input)]

apply ptr input prog (Ins Out mode1 _ _) = (ptr + 2, input, [x], prog)
    where
        x = param (ptr + 1) mode1 prog

apply ptr input prog (Ins JumpT mode1 mode2 _) = (ptr', input, [], prog)
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        ptr' = if x /= 0 then y else ptr + 3

apply ptr input prog (Ins JumpF mode1 mode2 _) = (ptr', input, [], prog)
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        ptr' = if x == 0 then y else ptr + 3

apply ptr input prog (Ins Less mode1 mode2 _) = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, if x < y then 1 else 0)]

apply ptr input prog (Ins Equals mode1 mode2 _) = (ptr + 4, input, [], prog')
    where
        x = param (ptr + 1) mode1 prog
        y = param (ptr + 2) mode2 prog
        z = prog V.! (ptr + 3)
        prog' = prog V.// [(z, if x == y then 1 else 0)]

apply ptr input prog (Ins Terminate _ _ _) = (-1, input, [], prog)

param :: Int -> ParamMode -> Program -> Int
param ptr Pos prog = prog V.! (prog V.! ptr)
param ptr Imm prog = prog V.! ptr

getInput :: IO Program
getInput = (V.fromList . (fmap read) . (splitOn ",")) <$> readFile "input/day5.txt"

day05a :: Program -> Int
day05a  = head . run [1]

day05b :: Program -> Int
day05b = head . run [5]

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day05a input, day05b input)
