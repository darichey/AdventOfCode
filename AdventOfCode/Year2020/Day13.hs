module Year2020.Day13 where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import Solution (Solution (Solution))
import Text.Read (readMaybe)

parse :: String -> Maybe (Integer, [Maybe Integer])
parse = Just . parseLines . lines
  where
    parseLines [l1, l2] = (read l1, fmap readMaybe (splitOn "," l2))

part1 :: (Integer, [Maybe Integer]) -> Integer
part1 (earliest, busses) = (depart - earliest) * id
  where
    (depart, id) = head [(depart, id) | depart <- [earliest ..], id <- catMaybes busses, depart `mod` id == 0]

part2 :: (Integer, [Maybe Integer]) -> Integer
part2 = solve . congruences
  where
    congruences :: (Integer, [Maybe Integer]) -> [(Integer, Integer)]
    congruences = mapMaybe f . zip [0,-1..] . snd

    f :: (Integer, Maybe Integer) -> Maybe (Integer, Integer)
    f (x, Just y) = Just (x, y)
    f _ = Nothing

    solve :: [(Integer, Integer)] -> Integer
    solve congs = sum (fmap g congs) `mod` moduliProd
      where
        moduliProd = product (fmap snd congs)

        g (a, m) = a * b * b'
          where
            b = moduliProd `div` m
            b' = modularInverse b m

    modularInverse :: Integer -> Integer -> Integer
    modularInverse a m = fst $ euclid a m
      where
        euclid :: Integer -> Integer -> (Integer, Integer)
        euclid _ 0 = (1, 0)
        euclid a b = (t, s - q * t)
          where (q, r) = quotRem a b
                (s, t) = euclid b r

solution :: Solution (Integer, [Maybe Integer]) Integer Integer
solution = Solution "Day 13" "input/Year2020/day13.txt" parse part1 part2
