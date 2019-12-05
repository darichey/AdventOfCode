module Day05 where

import Data.List.Split
import qualified Data.Vector as V

data Program = Program (V.Vector Int) deriving Show

getInput :: IO Program
getInput = (Program . V.fromList . (fmap read) . (splitOn ",")) <$> readFile "input/day5.txt"

valueOf :: Param -> Program -> Int
valueOf (Pos x) prog = valueAt x prog
valueOf (Imm x) _    = x

valueAt :: Int -> Program -> Int
valueAt x (Program code) = code V.! x

update :: Param -> Int -> Program -> Program
update (Pos z) xy (Program code) = Program $ code V.// [(z, xy)]

data Param = Pos Int
           | Imm Int 
           deriving Show

data Ins = Add Param Param Param
         | Mul Param Param Param
         | In Param
         | Out Param
         | JumpT Param Param
         | JumpF Param Param
         | Less Param Param Param
         | Equals Param Param Param
         | Terminate
         deriving Show

parseIns :: Int -> Program -> Ins
parseIns ptr prog =
    case de of
        "01" -> Add    (param 1 c) (param 2 b) (param 3 a)
        "02" -> Mul    (param 1 c) (param 2 b) (param 3 a)
        "03" -> In     (param 1 c)
        "04" -> Out    (param 1 c)
        "05" -> JumpT  (param 1 c) (param 2 b)
        "06" -> JumpF  (param 1 c) (param 2 b)
        "07" -> Less   (param 1 c) (param 2 b) (param 3 a)
        "08" -> Equals (param 1 c) (param 2 b) (param 3 a)
        "99" -> Terminate
    where
        (a:b:c:de) = leftPad '0' (show (valueAt ptr prog)) 5

        leftPad :: Char -> String -> Int -> String
        leftPad with str num = (replicate (num - length str) with) ++ str

        param :: Int -> Char -> Param
        param offset mode = case mode of
            '0' -> Pos (valueAt (ptr + offset) prog)
            '1' -> Imm (valueAt (ptr + offset) prog)

run :: [Int] -> Program -> [Int]
run input = fst . run' 0 input []
    -- where
run' :: Int -> [Int] -> [Int] -> Program -> ([Int], Program)
run' (-1) _ output prog = (output, prog)
run' ptr input output prog = run' ptr' input' output' prog'
    where
        f = apply (parseIns ptr prog)
        (ptr', input', output', prog') = f ptr input output prog

apply :: Ins -> Int -> [Int] -> [Int] -> Program -> (Int, [Int], [Int], Program)
apply ins ptr input output prog = case ins of
    (Add p1 p2 p3) -> (ptr + 4, input, output, prog')
        where
            x = valueOf p1 prog
            y = valueOf p2 prog
            prog' = update p3 (x + y) prog

    (Mul p1 p2 p3) -> (ptr + 4, input, output, prog')
        where
            x = valueOf p1 prog
            y = valueOf p2 prog
            prog' = update p3 (x * y) prog

    (In p1) -> (ptr + 2, tail input, output, prog')
        where
            prog' = update p1 (head input) prog

    (Out p1) -> (ptr + 2, input, x:output, prog)
        where
            x = valueOf p1 prog

    (JumpT p1 p2) -> (ptr', input, output, prog)
        where
            x = valueOf p1 prog
            y = valueOf p2 prog
            ptr' = if x /= 0 then y else ptr + 3

    (JumpF p1 p2) -> (ptr', input, output, prog)
        where
            x = valueOf p1 prog
            y = valueOf p2 prog
            ptr' = if x == 0 then y else ptr + 3

    (Less p1 p2 p3) -> (ptr + 4, input, output, prog')
        where
            x = valueOf p1 prog
            y = valueOf p2 prog
            prog' = update p3 (if x < y then 1 else 0) prog

    (Equals p1 p2 p3) -> (ptr + 4, input, output, prog')
        where
            x = valueOf p1 prog
            y = valueOf p2 prog
            prog' = update p3 (if x == y then 1 else 0) prog

    Terminate -> (-1, input, output, prog)

day05a :: Program -> Int
day05a  = head . run [1]

day05b :: Program -> Int
day05b = head . run [5]

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day05a input, day05b input)
