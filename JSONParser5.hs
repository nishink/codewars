-- https://www.codewars.com/kata/55aa170b54c32468c30000a9
-- JSON Parser
-- 失敗ケース

module JSON.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String

-- import JSON.Parser.Preloaded (Value(..))

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null deriving Show


-- 数値のパーサー
jsonNumber :: Parser Double
jsonNumber = do
  minus <- optionMaybe (char '-')
  int <- many1 digit
  frac <- optionMaybe $ char '.' *> many1 digit
  let numStr = case frac of
        Nothing -> int
        Just f  -> int ++ "." ++ f
      num = read numStr :: Double
  case minus of
    Nothing -> return num
    Just _  -> return $ -num

-- 文字列のパーサー
jsonString :: Parser String
jsonString = char '"' *> many (noneOf "\"") <* char '"'

-- 値のパーサー
jsonValue :: Parser Value
jsonValue = choice
  [ Number <$> jsonNumber
  , String <$> jsonString
  , Boolean True <$ string "true"
  , Boolean False <$ string "false"
  , Null <$ string "null"
  , Array <$> (char '[' *> jsonValue `sepBy` char ',' <* char ']')
  , Object <$> (char '{' *> jsonPair `sepBy` char ',' <* char '}')
  ]
  where
    jsonPair = do
      key <- jsonValue
      char ':'
      val <- jsonValue
      return (key, val)

-- JSONのパーサー
parse :: String -> Maybe Value
parse s = case P.parse jsonValue "" s of Right json -> Just json; _ -> Nothing
 
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