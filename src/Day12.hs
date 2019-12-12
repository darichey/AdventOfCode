module Day12 where

import qualified Data.Set as Set

type V3 = (Int, Int, Int)
data Moon = Moon V3 V3 deriving (Show)
type Universe = (Moon, Moon, Moon, Moon)

withVelocity :: Moon -> V3 -> Moon
withVelocity (Moon pos _) v = (Moon pos v)

addVelocity :: Moon -> V3 -> Moon
addVelocity (Moon pos v) u = (Moon pos (add u v))

withPosition :: Moon -> V3 -> Moon
withPosition (Moon _ v) pos = Moon pos v

add :: V3 -> V3 -> V3
add (a, b, c) (x, y, z) = (a + x, b + y, c + z)

gravity :: Moon -> Moon -> (V3, V3)
gravity (Moon (x1, y1, z1) (vx1, vy1, vz1)) (Moon (x2, y2, z2) (vx2, vy2, vz2)) = (v1, v2)
    where
        (vx1', vx2') = gravity' x1 x2
        (vy1', vy2') = gravity' y1 y2
        (vz1', vz2') = gravity' z1 z2
        v1 = (vx1', vy1', vz1')
        v2 = (vx2', vy2', vz2')

gravity' :: Int -> Int -> (Int, Int)
gravity' a b
    | a > b     = (-1, 1)
    | a < b     = (1, -1)
    | otherwise = (0, 0)

applyAllGravity :: Universe -> Universe
applyAllGravity (a, b, c, d) = (a', b', c', d')
    where
        (a1, b1) = gravity a b
        (a2, c1) = gravity a c
        (a3, d1) = gravity a d
        (b2, c2) = gravity b c
        (b3, d2) = gravity b d
        (c3, d3) = gravity c d
        a' = addVelocity a (add a3 (add a1 a2))
        b' = addVelocity b (add b3 (add b1 b2))
        c' = addVelocity c (add c3 (add c1 c2))
        d' = addVelocity d (add d3 (add d1 d2))

move :: Moon -> Moon
move (Moon pos v) = Moon (add pos v) v

moveAll :: Universe -> Universe
moveAll (a, b, c, d) = (a', b', c', d')
    where
        a' = move a
        b' = move b
        c' = move c
        d' = move d

simulate :: Universe -> [Universe]
simulate = iterate (moveAll . applyAllGravity)

energy :: Moon -> Int
energy (Moon (x, y, z) (vx, vy, vz)) = pot * kin
    where
        pot = abs x + abs y + abs z
        kin = abs vx + abs vy + abs vz

dimensions :: [Universe] -> [[((Int, Int), (Int, Int), (Int, Int), (Int, Int))]]
dimensions states =
    [ fmap xs states
    , fmap ys states
    , fmap zs states
    ]
    where
        xs :: Universe -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
        xs (a,b,c,d) = (xs' a, xs' b, xs' c, xs' d)
        xs' :: Moon -> (Int, Int)
        xs' (Moon (x, _, _) (vx, _ ,_)) = (x, vx)

        ys :: Universe -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
        ys (a,b,c,d) = (ys' a, ys' b, ys' c, ys' d)
        ys' :: Moon -> (Int, Int)
        ys' (Moon (_, y, _) (_, vy ,_)) = (y, vy)

        zs :: Universe -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
        zs (a,b,c,d) = (zs' a, zs' b, zs' c, zs' d)
        zs' :: Moon -> (Int, Int)
        zs' (Moon (_, _, z) (_, _ ,vz)) = (z, vz)

firstRepeat :: (Ord a) => [a] -> Int
firstRepeat = firstRepeat' 0 Set.empty
    where
        firstRepeat' n seen (x:xs)
            | Set.member x seen = n
            | otherwise         = firstRepeat' (n + 1) (Set.insert x seen) xs

day12a :: Universe -> Int
day12a u = sum $ fmap energy [a',b',c',d']
    where
        (a',b',c',d') = simulate u !! 1000

day12b :: Universe -> Int
day12b = foldl1 lcm . fmap firstRepeat . dimensions . simulate

getInput :: IO Universe
getInput = do
    let a = Moon (-5,6,-11) (0, 0, 0)
        b = Moon (-8,-4,-2) (0, 0, 0)
        c = Moon (1,16,4) (0, 0, 0)
        d = Moon (11,11,-4) (0, 0, 0)
    return (a, b, c, d)

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day12a input, day12b input)