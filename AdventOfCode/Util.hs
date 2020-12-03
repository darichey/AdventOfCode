module Util (count, occurrences, xor, NoQuotes (NoQuotes)) where

import Control.Arrow ((>>>))

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where show (NoQuotes str) = str

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

occurrences :: Eq a => a -> [a] -> Int
occurrences x = count (x ==)

xor :: Bool -> Bool -> Bool
xor = (/=)
