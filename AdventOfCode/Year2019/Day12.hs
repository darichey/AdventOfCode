module Year2019.Day12 (solution) where

import Control.Lens ((^.))
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Set as Set
import Linear.V3 (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import Linear.Vector (Additive (liftI2, (^+^)), sumV)
import Solution (Solution (Solution))
import Text.Megaparsec (sepBy)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser)

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
      | a > b = -1
      | a < b = 1
      | otherwise = 0

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
    firstRepeat' n seen (x : xs)
      | Set.member x seen = n
      | otherwise = firstRepeat' (n + 1) (Set.insert x seen) xs

part1 :: Universe -> Int
part1 u = sum $ fmap energy (simulate u !! 1000)

part2 :: Universe -> Int
part2 universe = foldl1 lcm (fmap (`period` universes) [(^. _x), (^. _y), (^. _z)])
  where
    universes = simulate universe

parse :: String -> Maybe Universe
parse = rightToMaybe . P.parse universe ""
  where
    universe :: Parser Universe
    universe = moon `sepBy` string "\n"
      where
        moon = do
          string "<x="
          x <- L.decimal
          string ", y="
          y <- L.decimal
          string ", z="
          z <- L.decimal
          string ">"
          return $ Moon (V3 x y z) (V3 0 0 0)

solution :: Solution Universe Int Int
solution = Solution "Day 12" "input/Year2019/day12.txt" parse part1 part2
