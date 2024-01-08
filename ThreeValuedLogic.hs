-- https://www.codewars.com/kata/65579292e361e60e202906f4/haskell
-- Three-valued logic

module ThreeValuedLogic (threevl) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as TT
import qualified Text.Parsec.Language as Lang

import Data.List.Split (splitOn)

data Ternary = TrueT | FalseT | UnknownT deriving (Show, Eq)

type Parser a = Parsec String () a

symbol :: String -> Parser String
symbol = TT.symbol Lang.haskell

reservedOp :: String -> Parser ()
reservedOp = TT.reservedOp Lang.haskell

ternary :: Parser Ternary
ternary = (symbol "T" >> return TrueT)
      <|> (symbol "F" >> return FalseT)
      <|> (symbol "U" >> return UnknownT)

notExpr :: Parser (Ternary -> Ternary)
notExpr = reservedOp "not" >> return notT

expr :: Parser Ternary
expr = buildExpressionParser
       [[Prefix notExpr],
        [binary "and" andT AssocLeft],
        [binary "xor" xorT AssocLeft],
        [binary "or" orT AssocLeft]]
       term
  where
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc
    term = ternary <|> parens expr
    parens p = do
      symbol "("
      x <- p
      symbol ")"
      return x

notT :: Ternary -> Ternary
notT TrueT = FalseT
notT FalseT = TrueT
notT UnknownT = UnknownT

andT :: Ternary -> Ternary -> Ternary
andT TrueT TrueT = TrueT
andT _ FalseT = FalseT
andT FalseT _ = FalseT
andT _ _ = UnknownT

xorT :: Ternary -> Ternary -> Ternary
xorT TrueT TrueT = FalseT
xorT TrueT FalseT = TrueT
xorT FalseT TrueT = TrueT
xorT FalseT FalseT = FalseT
xorT _ _ = UnknownT

orT :: Ternary -> Ternary -> Ternary
orT FalseT FalseT = FalseT
orT TrueT _ = TrueT
orT _ TrueT = TrueT
orT _ _ = UnknownT

threevl :: String -> Int
threevl input = case parse expr "" (removeNotNot input) of
    Left err -> 9
    Right parsedExpr -> case parsedExpr of
        TrueT -> 1
        FalseT -> -1
        UnknownT -> 0

removeNotNot s = concat $ splitOn "not not " s

{-
３値論理の文字列を解釈して結果を返す。
True=1、False=-1、Unknown=0
演算子はnot,and,xor,orのみ。優先順位もこの順。
括弧を先に演算する。

ChatGPTを使って構文解析するプログラムを書いたところ、
サンプル問題は通るようになった。
しかし、ランダムテストでどうしても通らないところがある。
not not と、notが連続する部分があると、どうしても解析できない。

発想を変えて、notのnotは存在しないのと同じなので、
最初に"not not "を削除してしまうというやり方で解いた。
-}


