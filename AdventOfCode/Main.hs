{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

import qualified Data.Map as Map
import Solution (Year, days)
import System.Console.CmdArgs (Data, Typeable, cmdArgs, def, help, opt, typ, (&=))
import Year2015.Year2015 (year2015)
import Year2019.Year2019 (year2019)
import Year2020.Year2020 (year2020)

years :: Map.Map String Year
years = Map.fromList [("2015", year2015), ("2019", year2019), ("2020", year2020)]

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
  let y = years Map.! year
  if day == "ALL"
    then sequence_ $ days y
    else days y !! (read day - 1)
