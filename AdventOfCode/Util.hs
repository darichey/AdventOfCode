module Util (count, occurrences, NoQuotes (NoQuotes), Parser, windows, twoSum) where

import Data.List (tails)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (Parsec)

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where show (NoQuotes str) = str

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

occurrences :: Eq a => a -> [a] -> Int
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
