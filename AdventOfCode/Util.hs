module Util (count, occurrences, xor) where

import Control.Arrow ((>>>))

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

occurrences :: Eq a => a -> [a] -> Int
occurrences x = count (x ==)

xor :: Bool -> Bool -> Bool
xor = (/=)