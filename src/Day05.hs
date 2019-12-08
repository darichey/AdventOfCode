module Day05 where

import Data.List.Split
import Data.List
import Data.Tuple
import qualified Data.Vector as V

newtype IntCode = IntCode (V.Vector Int) deriving Show
data Program = Program Int [Int] [Int] IntCode deriving Show

getInput :: IO Program
getInput = Program 0 [] [] . IntCode . V.fromList . fmap read . splitOn "," <$> readFile "input/day5.txt"

valueOf :: IntCode -> Param -> Int
valueOf code (Pos x) = valueAt code x
valueOf _    (Imm x) = x

valueAt :: IntCode -> Int -> Int
valueAt (IntCode code) x = code V.! x

values :: IntCode -> [Param] -> [Int]
values = fmap . valueOf

update :: Param -> Int -> IntCode -> IntCode
update (Pos z) xy (IntCode code) = IntCode $ code V.// [(z, xy)]

combine :: (Int -> Int -> Int) -> IntCode -> Param -> Param -> Int
combine f _    (Imm x) (Imm y) = f x y
combine f code (Imm x) y       = f x (valueOf code y)
combine f code x      (Imm y)  = f (valueOf code x) y
combine f code x       y       = f (valueOf code x) (valueOf code y)

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

nextIns :: Program -> Ins
nextIns (Program ptr _ _ code) =
    case op of
        1 -> Add    (param 1 c) (param 2 b) (param 3 a)
        2 -> Mul    (param 1 c) (param 2 b) (param 3 a)
        3 -> In     (param 1 c)
        4 -> Out    (param 1 c)
        5 -> JumpT  (param 1 c) (param 2 b)
        6 -> JumpF  (param 1 c) (param 2 b)
        7 -> Less   (param 1 c) (param 2 b) (param 3 a)
        8 -> Equals (param 1 c) (param 2 b) (param 3 a)
        99 -> Terminate
    where
        [e,d,c,b,a] = digits 5 (valueAt code ptr)
        op = d * 10 + e

        param :: Int -> Int -> Param
        param offset mode = case mode of
            0 -> Pos 
            1 -> Imm
            $ valueAt code (ptr + offset)

        digits :: Int -> Int -> [Int]
        digits n num = take n (unfoldr (\b -> Just $ swap (divMod b 10)) num)

withInput :: [Int] -> Program -> Program
withInput input (Program ptr _ output code) = Program ptr input output code

terminated :: Program -> Bool
terminated (Program ptr _ _ _) = ptr == -1

output :: Program -> [Int]
output (Program _ _ output _) = output

run :: Program -> [Int]
run = output . until terminated apply

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
            (i:input') = input
            code' = update p1 i code

    (Out p1) -> Program (ptr + 2) input (x:output) code
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

day05a :: Program -> Int
day05a = head . run . withInput [1]

day05b :: Program -> Int
day05b = head . run . withInput [5]

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day05a input, day05b input)