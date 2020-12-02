{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util (printSolutions, count, occurrences, xor) where

import Control.Arrow ((>>>))

class Solution a where
  showSol :: a -> String

instance {-# OVERLAPPING #-} Solution String where
  showSol = id

instance (Show a) => Solution a where
  showSol = show

printSolutions :: (Solution a, Solution b) => String -> IO (a, b) -> IO ()
printSolutions day solsM = do
  (part1, part2) <- solsM
  putStrLn $ " == " ++ day ++ " == "
  putStrLn $ showSol part1
  putStrLn $ showSol part2

count :: (a -> Bool) -> [a] -> Int
count p = filter p >>> length

occurrences :: Eq a => a -> [a] -> Int
occurrences x = count (x ==)

xor :: Bool -> Bool -> Bool
xor = (/=)