module Util (count, occurrences, NoQuotes (NoQuotes), Parser) where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where show (NoQuotes str) = str

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

occurrences :: Eq a => a -> [a] -> Int
occurrences x = count (x ==)

type Parser = Parsec Void String