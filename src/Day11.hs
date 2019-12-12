module Day11 (solutions) where

import Data.List.Split
import Data.List
import Data.Tuple
import qualified Data.IntMap.Strict as M
import qualified Data.Map as Map
import Debug.Trace

newtype IntCode = IntCode (M.IntMap Int) deriving Show
data Program = Program Int [Int] [Int] Int IntCode deriving Show

getInput :: IO Program
getInput = do
    input <- readFile "input/day11.txt"
    let ins = M.fromAscList $ zip [0..] (fmap read (splitOn "," input))
    return $ Program 0 [] [] 0 (IntCode ins)


valueOf :: Program -> Param -> Int
valueOf (Program _ _ _ _ code)       (Pos x) = valueAt code x
valueOf _                            (Imm x) = x
valueOf (Program _ _ _ relBase code) (Rel x) = valueAt code (relBase + x)

valueAt :: IntCode -> Int -> Int
-- valueAt (IntCode code) x = code M.! x
valueAt (IntCode code) x = M.findWithDefault 0 x code

values :: Program -> [Param] -> [Int]
values = fmap . valueOf

update' :: Int -> Int -> IntCode -> IntCode
update' at value (IntCode code) = IntCode code'
    where code' = M.insert at value code

update :: Param -> Int -> Program -> IntCode
update (Pos z) xy (Program _ _ _ _ code) = update' z xy code
update (Rel z) xy (Program _ _ _ relBase code) = update' (relBase + z) xy code

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

clearOutput :: Program -> Program
clearOutput (Program ptr input output relBase code) = Program ptr input [] relBase code

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

type Point = (Int, Int)
type Board = Map.Map Point Color
data Dir = L | R | U | D deriving (Show, Eq)
data Position = Position Dir Point deriving (Show, Eq)
data Color = B | W deriving (Show, Eq)

colorToChar :: Color -> Char
colorToChar B = '.'
colorToChar W = '#'

colorToInt :: Color -> Int
colorToInt B = 0
colorToInt W = 1

intToColor :: Int -> Color
intToColor 0 = B
intToColor 1 = W

turn :: Int -> Dir -> Dir
turn 0 = turnL
turn 1 = turnR

turnL :: Dir -> Dir
turnL L = D
turnL R = U
turnL U = L
turnL D = R

turnR :: Dir -> Dir
turnR L = U
turnR R = D
turnR U = R
turnR D = L

stepForward :: Position -> Position
stepForward (Position facing (x,y)) = case facing of
    L -> Position facing (x-1, y)
    R -> Position facing (x+1, y)
    U -> Position facing (x, y-1)
    D -> Position facing (x, y+1)

colorAt :: Point -> Board -> Color
colorAt = Map.findWithDefault B

runWithCurrentColor :: Position -> Board -> Program -> (Position, Board, Program)
runWithCurrentColor pos@(Position facing p) board prog = (pos', board', prog')
    where
        c = colorAt p board
        prog' = run (withInput [colorToInt c] prog)
        (toTurn:toPaint:_) = output prog'
        board' = Map.insert p (intToColor toPaint) board
        pos' = stepForward (Position (turn toTurn facing) p)

runRobot :: Position -> Board -> Program -> (Position, Board, Program)
runRobot pos board prog =
    if state prog == Terminated then
        (pos, board, prog)
    else
        runRobot pos' board' prog'
            where
                (pos', board', prog') = runWithCurrentColor pos board (clearOutput prog)

showBoard :: Board -> String
showBoard board = unlines $ transpose $ chunksOf 100 (fmap (\p -> colorToChar $ colorAt p board) [(x,y) | x <- [-50..49], y <- [-50..49]])

day11a :: Program -> Int
day11a prog = Map.size board'
    where
        pos = Position U (0,0)
        board = Map.empty
        (pos', board', prog') = runRobot pos board prog

day11b :: Program -> String
day11b prog = showBoard board'
    where
        pos = Position U (0,0)
        board = Map.singleton (0,0) W
        (pos', board', prog') = runRobot pos board prog

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day11a input, 0)