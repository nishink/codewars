-- https://www.codewars.com/kata/55aa170b54c32468c30000a9
-- JSON Parser
-- 失敗ケース

module JSON.Parser (parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String
--import Text.Parsec.Char
--import Text.Parsec.Language (emptyDef)
--import qualified Text.Parsec.Token as Tok
--import Control.Applicative ((<$>),(<*>))

import Data.Char
import Data.List

-- import JSON.Parser.Preloaded (Value(..))

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null deriving Show

jsonParser :: Parser Value
jsonParser =
      jsonNull
  <|> jsonBool
  <|> jsonNumber
  <|> jsonString
  <|> jsonArray
  <|> jsonObject

jsonNull :: Parser Value
jsonNull = string "null" >> return Null

jsonBool :: Parser Value
jsonBool =
      ( string "true" >> return (Boolean True) )
  <|> ( string "false" >> return (Boolean False) )

jsonNumber :: Parser Value
jsonNumber =
      ( fmap (Number . read) $ many1 digit )
  <|> ( fmap (Number . negate . read) $ char '-' >> many1 digit )

jsonString :: Parser Value
jsonString = fmap String $ char '"' >> manyTill anyChar (char '"')

jsonArray :: Parser Value
jsonArray = fmap Array $ between (char '[') (char ']') $ jsonParser `sepBy` char ','

jsonObject :: Parser Value
jsonObject = fmap Object $ between (char '{') (char '}') $ keyValue `sepBy` char ','
  where
    keyValue = do
      key <- jsonString
      char ':'
      val <- jsonParser
      return (case key of String str -> (key, val); _ -> error "key must be a string")

parse :: String -> Maybe Value
parse s = case P.parse jsonParser "" s of Right json -> Just json; _ -> Nothing
 
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