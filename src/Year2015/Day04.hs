{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day04 (solution) where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as C
import Data.List (find)
import Data.Maybe (fromJust)
import Solution (Solution (Solution))

parse :: String -> Maybe String
parse = Just

findBeginsWith :: C.ByteString -> C.ByteString -> Int
findBeginsWith prefix key = fromJust $ find (C.isPrefixOf prefix . encode . MD5.hash . C.append key . C.pack . show) [0 ..]

part1 :: String -> Int
part1 = findBeginsWith "00000" . C.pack

part2 :: String -> Int
part2 = findBeginsWith "000000" . C.pack

solution :: Solution String Int Int
solution = Solution "Day 4" "input/Year2015/day4.txt" parse part1 part2
