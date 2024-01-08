-- https://www.codewars.com/kata/55aa170b54c32468c30000a9
-- JSON Parser
-- 失敗ケース

module JSON.Parser (parse) where

import Text.Parsec hiding (parse)
import Text.Parsec.Char
import Text.Parsec.String
import Control.Applicative ((<$>),(<*>))

-- import JSON.Parser.Preloaded (Value(..))

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null deriving Show

data Expr
    = Val String
    | Sum [Expr]
    | Product [Expr]
    deriving Show

-- expr ::= Sum term ('+' term)*
expr :: Parsec String () Expr
expr = Sum <$> ((:) <$> term <*> many (char '+' *> term))

-- term ::= Product factor ('*' factor)*
term :: Parsec String () Expr
term = Product <$> ((:) <$> factor <*> many (char '*' *> factor))

-- factor ::= '(' expr ')' | Val (digit*)
factor :: Parsec String () Expr
factor = (char '(' *> expr <* char ')') <|> (Val <$> (many1 digit))


data Builder = Builder {
    integral  :: Parser (Maybe String),
    float :: Parser (Maybe String)
}
data Which
    = Both
    | LeftOnly
    | RightOnly
    | Neither

joinM :: (Monoid a) => Maybe a -> Maybe a -> (Which, Maybe a)
joinM Nothing Nothing   = (Neither, Nothing)
joinM x Nothing         = (LeftOnly, x)
joinM Nothing x         = (RightOnly, x)
joinM (Just x) (Just y) = (Both, Just (x <> y))

joinL :: Maybe a -> [a] -> Maybe [a]
joinL Nothing xs  = Just xs
joinL (Just x) xs = Just (x:xs)

-- string ::= String ( '"' chars '"' )
-- chars ::= char*
-- char ::= noneOf '"'
stringLiteral :: Parser String
stringLiteral = char '"' *> many (noneOf ['"']) <* char '"'

jsonString :: Parser Value
jsonString = String <$> stringLiteral

-- number ::= int | int frac
-- int ::= digit | digit1-9 digits | '-' digit | '-' digit1-9 digits
-- frac ::= '.' digits
-- digits ::= digit | oneOf[1..9] digits
intLiteral :: Parser String
intLiteral = (:) <$> oneToNine <*> many digit

oneToNine :: Parser Char
oneToNine = oneOf (concatMap show [1..9])

intPart :: Parser (Maybe String)
intPart = (joinL) <$> optionMaybe (char '-') <*> intLiteral

floatPart :: Parser (Maybe String)
floatPart = optionMaybe $ flp
    where
        flp = (:) <$> char '.' <*> many1 digit

builder :: Builder
builder = Builder intPart floatPart

jsonNumber :: Parser Value
jsonNumber = maybeNumber builder

maybeNumber :: Builder -> Parser Value
maybeNumber (Builder x y) = do
    num <- joinM <$> x <*> y
    case num of
        (Neither, Nothing)  -> Number <$> pure 0
        (LeftOnly, Just i)  -> Number <$> toInt i
        (RightOnly, Just f) -> Number <$> toFloat ('0':f)
        (Both, Just f)      -> Number <$> toFloat f

toInt :: String -> Parser Double
toInt i = pure $ read i

toFloat :: String -> Parser Double
toFloat f = pure $ read f

-- bool ::= "ture" | "false"
matchTrue :: Parser String
matchTrue = string "true"

matchFalse :: Parser String
matchFalse = string "false"

alwaysTrue :: Parser Bool
alwaysTrue = pure True

alwaysFalse :: Parser Bool
alwaysFalse = pure False

boolTrue :: Parser Bool
boolTrue = matchTrue *> alwaysTrue

boolFalse :: Parser Bool
boolFalse = matchFalse *> alwaysFalse

bool :: Parser Bool
bool = boolTrue <|> boolFalse

jsonBool :: Parser Value
jsonBool = Boolean <$> bool

-- null ::= "null"
matchNull :: Parser String
matchNull = string "null"

jsonNull :: Parser Value
jsonNull = Null <$> matchNull


jsonValue :: Parser Value
jsonValue
    = jsonBool
    <|> jsonString
    <|> jsonNumber
    <|> jsonArray
    <|> jsonObject
    <|> jsonNull

-- array ::= '[' elements ']'
arrayLiteral :: Parser [Value]
arrayLiteral
    = ( char '[' )
    *> (jsonValue `sepBy` ( char ',' ))
    <* ( char ']' )

jsonArray :: Parser Value
jsonArray = Array <$> arrayLiteral

objectEntry :: Parser (Value, Value)
objectEntry = do
    key <- jsonValue
    char ':'
    value <- jsonValue
    return (key, value)

jsonObject :: Parser Value
jsonObject = Object
    <$> (( char '{' )
    *> (objectEntry `sepBy` ( char ',' ))
    <* ( char '}' ))

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