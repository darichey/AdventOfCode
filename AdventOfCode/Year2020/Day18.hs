module Year2020.Day18 where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Either.Combinators (fromRight')
import Solution (Solution (Solution))
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser)

parse :: String -> Maybe [String]
parse = Just . lines . filter (/= ' ')

binary :: P.MonadParsec e s m => P.Tokens s -> (a -> a -> a) -> Operator m a
binary name f = InfixL (f <$ string name)

eval :: [[Operator Parser Int]] -> String -> Int
eval operators = fromRight' . P.parse expr ""
  where
    expr = makeExprParser (L.decimal <|> parens) operators
    parens = char '(' *> (expr <* char ')')

part1 :: [String] -> Int
part1 = sum . fmap (eval [[binary "+" (+), binary "*" (*)]])

part2 :: [String] -> Int
part2 = sum . fmap (eval [[binary "+" (+)], [binary "*" (*)]])

solution :: Solution [String] Int Int
solution = Solution "Day 18" "input/Year2020/day18.txt" parse part1 part2
