{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util (printSolutions) where

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