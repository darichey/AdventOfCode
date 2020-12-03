{-# LANGUAGE RecordWildCards #-}

module Solution (Solution (..), printSolution, Year (..), todo, printDay) where

data Solution i a b = Solution
  { name :: String,
    inputPath :: FilePath,
    parse :: String -> Maybe i,
    part1 :: i -> a,
    part2 :: i -> b
  }

data Year = Year {days :: [IO ()]}

printSolution :: (Show a, Show b) => Solution i a b -> IO ()
printSolution Solution {..} = do
  fileContent <- readFile inputPath
  case parse fileContent of
    Nothing -> putStrLn "Couldn't parse input"
    Just i -> do
      putStrLn $ "==" ++ name ++ "=="
      print $ part1 i
      print $ part2 i

todo :: String -> Solution () String String
todo name = Solution name "/dev/null" (Just . const ()) (const "TODO") (const "TODO")

printDay :: Int -> Year -> IO ()
printDay day Year{..} = days !! (day - 1)