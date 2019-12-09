module Day09 where

import Data.List.Split
import Data.List
import Data.Tuple
import qualified Data.Vector as V
import Debug.Trace

newtype IntCode = IntCode (V.Vector Int) deriving Show
data Program = Program Int [Int] [Int] Int IntCode deriving Show

getInput :: IO Program
-- getInput = Program 0 [] [] 0 . IntCode . (\xs -> xs ++ (V.replicate 1000000 0)) . V.fromList . (fmap read :: [Int]) . splitOn "," <$> readFile "input/day9.txt"
getInput = do
    input <- readFile "input/day9.txt"
    let ins = V.fromList (fmap read (splitOn "," input))
    let extended = ins V.++ V.replicate 100000 (0 :: Int)
    return $ Program 0 [] [] 0 (IntCode extended)


valueOf :: Program -> Param -> Int
valueOf (Program _ _ _ _ code)       (Pos x) = valueAt code x
valueOf _                            (Imm x) = x
valueOf (Program _ _ _ relBase code) (Rel x) = valueAt code (relBase + x)

valueAt :: IntCode -> Int -> Int
valueAt (IntCode code) x = code V.! x

values :: Program -> [Param] -> [Int]
values = fmap . valueOf

update :: Param -> Int -> Program -> IntCode
update (Pos z) xy (Program _ _ _ _ (IntCode code)) = IntCode $ code V.// [(z, xy)]
update (Rel z) xy (Program _ _ _ relBase (IntCode code)) = IntCode $ code V.// [(relBase + z, xy)]

combine :: (Int -> Int -> Int) -> Program -> Param -> Param -> Int
combine f _    (Imm x) (Imm y) = f x y
combine f prog (Imm x) y       = f x (valueOf prog y)
combine f prog x      (Imm y)  = f (valueOf prog x) y
combine f prog x       y       = f (valueOf prog x) (valueOf prog y)

data Param = Pos Int
           | Imm Int
           | Rel Int
           deriving Show

data Ins = Add Param Param Param
         | Mul Param Param Param
         | In Param
         | Out Param
         | JumpT Param Param
         | JumpF Param Param
         | Less Param Param Param
         | Equals Param Param Param
         | SetRel Param
         | Terminate
         deriving Show

data EndState = NotStarted | Suspended | Terminated deriving (Show, Eq)

state :: Program -> EndState
state (Program ptr _ _ _ _) | ptr == -1 = Terminated
state prog@(Program _ input _  _ code) =
    case (nextIns prog, input) of
        (In _, []) -> Suspended
        _          -> NotStarted

nextIns :: Program -> Ins
nextIns (Program ptr _ _ _ code) =
    case op of
        1 -> Add    (param 1 c) (param 2 b) (param 3 a)
        2 -> Mul    (param 1 c) (param 2 b) (param 3 a)
        3 -> In     (param 1 c)
        4 -> Out    (param 1 c)
        5 -> JumpT  (param 1 c) (param 2 b)
        6 -> JumpF  (param 1 c) (param 2 b)
        7 -> Less   (param 1 c) (param 2 b) (param 3 a)
        8 -> Equals (param 1 c) (param 2 b) (param 3 a)
        9 -> SetRel (param 1 c)
        99 -> Terminate
    where
        [e,d,c,b,a] = digits 5 (valueAt code ptr)
        op = d * 10 + e

        param :: Int -> Int -> Param
        param offset mode = case mode of
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

run :: Program -> ([Int], Program)
run prog = (out, prog')
    where
        prog' = until (\p -> let s = state p in s == Terminated || s == Suspended) apply prog
        out = output prog'

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
            (i:input') = input
            code' = update p1 i ps

    (Out p1) -> Program (ptr + 2) input (x:output) relBase code
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

day09a :: Program -> Int
day09a prog = head $ fst $ run $ withInput [1] prog

day09b :: Program -> Int
day09b prog = head $ fst $ run $ withInput [2] prog

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return $ (day09a input, day09b input)