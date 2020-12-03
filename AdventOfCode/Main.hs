{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

import Solution (printDay)
import System.Console.CmdArgs (Data, Typeable, cmdArgs, def, help, opt, typ, (&=))
import Year2020.Year2020 (year2020)

data Args = Args {year :: String, day :: String}
  deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs =
  Args
    { year = def &= help "The year to execute" &= typ "YEAR",
      day = "ALL" &= help "The day to execute" &= typ "DAY" &= opt "ALL"
    }

main :: IO ()
main = do
  Args {..} <- cmdArgs defaultArgs
  let dayInt = read day :: Int
   in if year == "2020"
        then printDay dayInt year2020
        else undefined

-- if day == "ALL"
--   then
--     let all = fmap (mapM_ snd . OMap.assocs) (OMap.lookup year undefined)
--      in fromMaybe (putStrLn $ "No such year " ++ year) all
--   else
--     let sol = OMap.lookup year undefined >>= OMap.lookup day
--      in fromMaybe (putStrLn $ "No solution for " ++ year ++ "." ++ day) sol