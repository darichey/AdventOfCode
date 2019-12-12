module Day12 (solutions) where

import qualified Data.Set as Set
import Linear.V3
import Linear.Vector
import Control.Lens ((^.))
import Text.Parsec (try, string, (<|>), many1, oneOf, sepBy)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number
import Data.Either

type Position = V3 Int
type Velocity = V3 Int
data Moon = Moon Position Velocity deriving (Eq, Show)
type Universe = [Moon]

addVelocity :: Moon -> Velocity -> Moon
addVelocity (Moon pos v) u = Moon pos (u ^+^ v)

-- The gravity on a due to b
gravityDueTo :: Moon -> Moon -> Velocity
gravityDueTo (Moon pos1 _) (Moon pos2 _) = liftI2 pull pos1 pos2
    where
        pull :: Int -> Int -> Int
        pull a b
            | a > b     = -1
            | a < b     =  1
            | otherwise =  0

gravityDueToAllOther :: Universe -> Moon -> Velocity
gravityDueToAllOther universe moon = sumV $ fmap (uncurry gravityDueTo) others
    where
        others = zip (repeat moon) [other | other <- universe, other /= moon]

applyGravity :: Universe -> Universe
applyGravity universe = fmap (uncurry addVelocity) (zip universe dvs)
    where
        dvs = fmap (gravityDueToAllOther universe) universe

move :: Universe -> Universe
move = fmap (\(Moon pos v) -> Moon (pos ^+^ v) v)

simulate :: Universe -> [Universe]
simulate = iterate (move . applyGravity)

energy :: Moon -> Int
energy (Moon pos vel) = pot * kin
    where
        absSum x y = abs x + abs y
        pot = foldl1 absSum pos
        kin = foldl1 absSum vel

period :: (V3 Int -> Int) -> [Universe] -> Int
period dim universes = firstRepeat $ states dim universes

states :: (V3 Int -> Int) -> [Universe] -> [[(Int, Int)]]
states dim = fmap (fmap (\(Moon pos vel) -> (dim pos, dim vel)))

firstRepeat :: (Ord a) => [a] -> Int
firstRepeat = firstRepeat' 0 Set.empty
    where
        firstRepeat' n seen (x:xs)
            | Set.member x seen = n
            | otherwise         = firstRepeat' (n + 1) (Set.insert x seen) xs

day12a :: Universe -> Int
day12a u = sum $ fmap energy (simulate u !! 1000)

day12b :: Universe -> Int
day12b universe = foldl1 lcm (fmap (`period` universes) [(^._x), (^._y), (^._z)])
    where
        universes = simulate universe

universe :: Parser Universe
universe = moon `sepBy` string "\n"
    where
        moon = do
            _ <- string "<x="
            x <- int
            _ <- string ", y="
            y <- int
            _ <- string ", z="
            z <- int
            _ <- string ">"
            return $ Moon (V3 x y z) (V3 0 0 0)

getInput :: IO Universe
getInput = do
    parsed <- parseFromFile universe "input/day12.txt"
    return $ fromRight [] parsed

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day12a input, day12b input)