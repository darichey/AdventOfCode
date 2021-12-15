{-# LANGUAGE ScopedTypeVariables #-}

module Util (count, occurrences, NoQuotes (NoQuotes), Parser, windows, twoSum, convolution) where

import Data.List (tails)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (Parsec)

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where show (NoQuotes str) = str

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count p = foldl (\acc x -> if p x then acc + 1 else acc) 0

occurrences :: (Foldable t, Eq a) => a -> t a -> Int
occurrences x = count (x ==)

type Parser = Parsec Void String

windows :: Int -> [a] -> [[a]]
windows n = filter ((n ==) . length) . fmap (take n) . tails

twoSum :: (Ord a, Num a) => a -> [a] -> Maybe (a, a)
twoSum target = go Set.empty
  where
    go _ [] = Nothing
    go set (x : xs) =
      if Set.member (target - x) set
        then Just (x, target - x)
        else go (Set.insert x set) xs

convolution :: forall a b. (a -> [a] -> b) -> [[a]] -> [[b]]
convolution f xs = [[f x ns | (x, col) <- zip (xs !! row) [0..],
                              let ns = neighbors row col] | row <- [0 .. length xs - 1]]
  where
    neighbors :: Int -> Int -> [a]
    neighbors row col = [(xs !! y) !! x | (y, x) <- [(row - 1, col), (row, col - 1), (row, col + 1), (row + 1, col)],
                                          y >= 0, x >= 0, y < length xs, x < length (xs !! row)]