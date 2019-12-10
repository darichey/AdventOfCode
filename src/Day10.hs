module Day10 where

import Data.Maybe
import Data.Ratio
import Debug.Trace
import Data.List
import Data.Ord
import qualified Data.Map as M

type Point = (Int, Int)
data Polar = Polar Double Double
    deriving Eq
type LineSegment = (Point, Point)

instance Show Polar where
    show (Polar r theta) = "(" ++ (take 5 (show r)) ++ ", " ++ (take 5 (show theta)) ++ ")"

instance Ord Polar where
    compare (Polar r1 theta1) (Polar r2 theta2) =
        if r2 > r1 then LT
        else if r2 < r1 then GT
        else if theta2 > theta1 then LT
        else if theta2 < theta1 then GT
        else EQ

getInput :: IO [Point]
getInput = do
    contents <- readFile "input/day10.txt"
    let l = lines contents
    let points = [(x,y) | x <- [0..(length (head l)) - 1], y <- [0..(length l) -1]]
    let s = fmap (\(x,y) -> if ((l !! y) !! x) == '#' then Just (x, -y) else Nothing) points
    return $ catMaybes s

between (ax,ay) (bx,by) (cx,cy) = dot >= 0 && dot <= squaredLength
    where
        dot = (cx - ax) * (bx - ax) + (cy - ay) * (by - ay)
        squaredLength = (bx - ax)*(bx - ax) + (by - ay)*(by - ay)

onLine :: Point -> LineSegment -> Bool
onLine a@(x,y) (b@(x1,y1), c@(x2,y2)) | x1 == x2 = between b c a && x == x1
                                      | otherwise = between b c a && ((fromIntegral y) == m * (fromIntegral x) + int)
                                        where
                                            delY = (fromIntegral y2) - (fromIntegral y1)
                                            delX = (fromIntegral x2) - (fromIntegral x1)
                                            m = delY % delX
                                            int = (fromIntegral y1) - (m * (fromIntegral x1))

isVisibleFrom :: Point -> Point -> [Point] -> Bool
isVisibleFrom a b asteroids = not $ any (==True) inTheWay
    where
        l = (a, b)
        s = fmap (\p -> (p, onLine p l)) (removeItem b (removeItem a asteroids))
        inTheWay = fmap snd s

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

allVisibleFrom :: Point -> [Point] -> [Point]
allVisibleFrom a asteroids = filter (\b -> isVisibleFrom a b asteroids) (removeItem a asteroids)

polar :: Point -> Polar
polar (x,y) = Polar r theta
    where
        r = sqrt $ ((fromIntegral x) ** 2) + ((fromIntegral y) ** 2)
        theta' = atan2 (fromIntegral y) (fromIntegral x)
        theta = if theta' < 0 then theta' + (2 * pi) else theta'

cartesian :: Polar -> Point
cartesian (Polar r theta) = (x, y)
    where
        x = round $ r * cos theta
        y = round $ r * sin theta

-- nthDestroyed :: Int -> [Point] -> Point
-- nthDestroyed n asteroids = cartesian $ (tail $ sort $ fmap polar asteroids) !! n
toDestroy :: [Point] -> [Point]
toDestroy asteroids = fmap cartesian ys
    where
        inPolar :: [(Double, [Polar])]
        inPolar = fmap ( (\p@(Polar r theta) -> (theta, [p])) . polar) asteroids
        -- grouped = groupBy (\(Polar _ theta1) (Polar _ theta2) -> theta1 == theta2) inPolar
        grouped = fmap snd (M.toList $ M.fromListWith (++) inPolar)
        sortedGroups = sortOn (Down . \(Polar _ theta:_) -> theta) (fmap (sortOn (\(Polar r _) -> r)) grouped)
        longestLength = length $ maximumBy (comparing length) sortedGroups
        ys :: [Polar]
        ys = concatMap (`nths` sortedGroups) [0..longestLength-1]

nths :: Int -> [[a]] -> [a]
nths n = mapMaybe (\x -> nth n x)

nth :: Int -> [a] -> Maybe a
nth n xs | n >= length xs = Nothing
         | otherwise = Just $ xs !! n

shift :: Point -> [Point] -> [Point]
shift zero = fmap (shift1 zero)

shift1 :: Point -> Point -> Point
shift1 (x0,y0) (x,y) = (x-x0, y-y0)

rotate90clockwise :: Point -> Point
rotate90clockwise (x,y) = (y,-x)

rotate90counter :: Point -> Point
rotate90counter (x,y) = (-y,x)

day10a :: [Point] -> (Point, Int)
day10a asteroids = maximumBy (comparing snd) $ fmap (\p -> (p, length $ allVisibleFrom p asteroids)) asteroids

day10b :: Point -> [Point] -> Int
day10b stationLoc asteroids = x * 100 + y
-- day10b stationLoc asteroids = (x,y)
    where
        transformed = removeItem (0,0) $ fmap rotate90clockwise (shift stationLoc asteroids)
        untransformed = fmap (\p -> shift1 (shift1 stationLoc (0,0)) (rotate90counter p)) (toDestroy transformed)
        f = fmap (\(x,y) -> (x,-y)) untransformed
        (x,y) = f !! 198

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    let (stationLoc, numVisible) = day10a input
    return (numVisible, day10b stationLoc input)