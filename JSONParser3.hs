-- https://www.codewars.com/kata/55aa170b54c32468c30000a9
-- JSON Parser
-- 失敗ケース

module JSON.Parser (parse) where

import Data.Char
import Data.List

-- import JSON.Parser.Preloaded (Value(..))

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null deriving Show

parse :: String -> Maybe Value
parse "null" = Just Null
parse "true" = Just (Boolean True)
parse "false" = Just (Boolean False)
parse ('"' : xs) = Just (String (init xs))
parse xs@(x:_)
    | x == '-' || isDigit x = Just (Number (read xs))
    | x == '[' = Array <$> parseArray (drop 1 $ init $ tail xs)
    | x == '{' = Object <$> parseObject (drop 1 $ init $ tail xs)
    | otherwise = Nothing

parseArray :: String -> Maybe [Value]
parseArray "" = Just []
parseArray s =
    case parse s of
        Nothing -> Nothing
        Just v -> 
            case uncons (dropWhile (/= ',') s) of
                Nothing -> Just [v]
                Just (',', s') -> (:) v <$> parseArray s'
                _ -> Nothing

parseObject :: String -> Maybe [(Value,Value)]
parseObject "" = Just []
parseObject s =
    case uncons (dropWhile (== ' ') s) of
        Nothing -> Nothing
        Just ('}', _) -> Just []
        Just _ -> do
            (key, rest) <- parseString s
            value <-
                case uncons (dropWhile (/= ':') rest) of
                    Just (':', s') -> parse (dropWhile (==' ') s')
                    _ -> Nothing
            (:) (key,value) <$> parseObject (dropWhile (/= ',') rest)

parseString :: String -> Maybe (Value, String)
parseString s =
    let (result, rest) = break (/= '"') (tail s)
    in Just (String result, drop 2 rest)


{-
string
      ""
      " chars "
chars
      char
      char chars
char
      any-character-except-"
number
      int
      int frac
int
      digit
      digit1-9 digits 
      - digit
      - digit1-9 digits
frac
      . digits
digits
      digit
      digit digits
----
object
      {}
      { members }
members
      pair
      pair , members
pair
      string : value
array
      []
      [ elements ]
elements
      value 
      value , elements
value
      string
      number
      object
      array
      true
      false
      null
-}