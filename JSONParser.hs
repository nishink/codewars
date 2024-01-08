-- https://www.codewars.com/kata/55aa170b54c32468c30000a9
-- JSON Parser
-- 正解ケース

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

-- JSON = spaces [ Object | Array | String | Number | Bool | Null ] spaces
-- JSONは前後にスペース文字のある、Object、Array、String、Number、Boolean、Nullのいずれかである。
-- *>は左側にあるものを読み飛ばして右側の値を返す。<*は右側にあるものを読み飛ばして左側の値を返す。
-- <|>は、parseする条件における「または」
parseJSON :: Parser Value
parseJSON = spaces *> (parseObject <|> parseArray <|> parseString <|> parseNumber <|> parseBool <|> parseNull) <* spaces

-- Object = '{' spaces [ KV (',' spaces) ... ] '}'
-- Objectは、{と}に囲まれた、,で区切られたKVである。
-- sepByは区切り文字で区切った値のリストを返す。
-- <$>はValueをParser Valueにするためのもの。
parseObject :: Parser Value
parseObject = Object <$> (char '{' *> spaces *> sepBy parseKV (char ',' <* spaces) <* char '}')

-- KV = String spaces ':' spaces JSON
-- ここでも<$>を使っているが、左側に２つ引数を必要とする(,)がある。
-- この場合、<*>の左側が第１引数、右側が第２引数となるので、Parser (Value,Value)を得ることができる。
parseKV :: Parser (Value, Value)
parseKV = (,) <$> (parseString <* spaces <* char ':' <* spaces) <*> parseJSON

-- Array = '[' spaces [ JSON (',' spaces ) ] ']'
-- Arrayは、[と]に囲まれた、,で区切られたJSONである。
parseArray :: Parser Value
parseArray = Array <$> (char '[' *> spaces *> sepBy parseJSON (char ',' <* spaces) <* char ']')

-- String = '"' [ ^\" ]* '"'
-- Stringは、""に囲まれた、"を含まない０文字以上の文字列である。
parseString :: Parser Value
parseString = String <$> (char '"' *> many (noneOf "\"") <* char '"')

-- Number = [ '-' ] Int [ '.' ] digit+
-- Numberは、マイナス符号、整数部、小数部から成る。
-- 小数部がない場合は整数部のみ、小数部がある場合は、整数部＋小数点＋小数部をDouble型のnumとして取り出す。
-- マイナス符号がない場合はnumをそのまま返す。マイナス符号がある場合はnumにマイナスをつけて返す。
parseNumber :: Parser Value
parseNumber = do
  minus <- optionMaybe (char '-')
  int <- parseInt
  frac <- optionMaybe $ char '.' *> many1 digit
  let numStr = case frac of
        Nothing -> int
        Just f  -> int ++ "." ++ f
      num = read numStr :: Double
  case minus of
    Nothing -> return (Number num)
    Just _  -> return (Number (-num))

-- Int = "0" | [ '1'..'9' ] digit*
-- 整数部は、単体の0もしくは、0で始まらない数字の列である。
parseInt :: Parser String
parseInt = string "0" *> pure "0" <|> (:) <$> oneOf ['1'..'9'] <*> many digit

-- Bool = "true" | "fa;se"
-- Booleanは、trueまたはfalseである。
parseBool :: Parser Value
parseBool = string "true" *> pure (Boolean True) <|> string "false" *> pure (Boolean False)

-- Null = "null"
-- Nullは、nullである。
parseNull :: Parser Value
parseNull = string "null" *> pure Null


-- JSONのパーサー
-- <* eofをつけないと末尾に余計な文字があっても読み飛ばしてしまう。
-- Parsec.parseがEitherであるため、Maybeに変換している。
parse :: String -> Maybe Value
parse s = case P.parse (parseJSON <* eof) "" s of Right json -> Just json; _ -> Nothing
 
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