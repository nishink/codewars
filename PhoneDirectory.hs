-- https://www.codewars.com/kata/56baeae7022c16dd7400086e
-- Phone Directory

module Codewars.G964.Phonedir where

import Data.Char

phone :: String -> [Char] -> [Char]
phone strng num = let
    directory = map format $ lines strng
    profiles  = filter (\(p,_,_) -> p == num) directory
    in if profiles == [] then "Error => Not found: " ++ num
        else if length profiles >= 2 then "Error => Too many people: " ++ num
        else (let (_, name, address) = head profiles in 
            "Phone => " ++ num ++ ", Name => " ++ name ++ ", Address => " ++ address)

format :: String -> (String,String,String)
format s = let
    (phone, remain) = takePhone s
    (name, remain2) = takeName remain
    address = trimAddress remain2
    in (phone, name, address)

takePhone :: String -> (String,String)
takePhone s = matchPhone "" s
    where
        matchPhone :: String -> String -> (String,String)
        matchPhone s ('+':x:'-':a:b:c:'-':d:e:f:'-':g:h:i:j:xs) =
            ([x,'-',a,b,c,'-',d,e,f,'-',g,h,i,j], s ++ xs)
        matchPhone s ('+':x:y:'-':a:b:c:'-':d:e:f:'-':g:h:i:j:xs) =
            ([x,y,'-',a,b,c,'-',d,e,f,'-',g,h,i,j],s ++ xs)
        matchPhone s (x:xs) = matchPhone (s ++ [x]) xs

takeName :: String -> (String,String)
takeName s = matchName "" s
    where
        matchName :: String -> String -> (String,String)
        matchName s ('<':xs) = (takeWhile (/='>') xs, s ++ tail (dropWhile (/='>') xs))
        matchName s (x:xs) = matchName (s ++ [x]) xs

trimAddress :: String -> String
trimAddress s =
    unwords $ words [ if isAlphaNum c || c == '-' || c == '.' then c else ' ' | c <- s ]
{-
module Codewars.G964.PhonedirSpec where
import Codewars.G964.Phonedir

import Data.List
import Data.List.Split (splitOn, linesBy)
import Data.Char (isSpace)
import Data.Maybe (fromJust)

import Test.Hspec
import Test.QuickCheck
import Text.Printf (printf)

dr :: String
dr = "/+1-541-754-3010 156 Alphand_St. <J Steeve>\n 133, Green, Rd. <E Kustur> NY-56423 ;+1-541-914-3010;\n\
\+1-541-984-3012 <P Reed> /PO Box 530; Pollocksville, NC-28573\n :+1-321-512-2222 <Paul Dive> Sequoia Alley PQ-67209\n\
\+1-741-984-3090 <Peter Reedgrave> _Chicago\n :+1-921-333-2222 <Anna Stevens> Haramburu_Street AA-67209\n\
\+1-111-544-8973 <Peter Pan> LA\n +1-921-512-2222 <Wilfrid Stevens> Wild Street AA-67209\n\
\<Peter Gone> LA ?+1-121-544-8974 \n <R Steell> Quora Street AB-47209 +1-481-512-2222!\n\
\<Arthur Clarke> San Antonio $+1-121-504-8974 TT-45120\n <Ray Chandler> Teliman Pk. !+1-681-512-2222! AB-47209,\n\
\<Sophia Loren> +1-421-674-8974 Bern TP-46017\n <Peter O'Brien> High Street +1-908-512-2222; CC-47209\n\
\<Anastasia> +48-421-674-8974 Via Quirinal Roma\n <P Salinger> Main Street, +1-098-512-2222, Denver\n\
\<C Powel> *+19-421-674-8974 Chateau des Fosses Strasbourg F-68000\n <Bernard Deltheil> +1-498-512-2222; Mount Av.  Eldorado\n\
\+1-099-500-8000 <Peter Crush> Labrador Bd.\n +1-931-512-4855 <William Saurin> Bison Street CQ-23071\n\
\<P Salinge> Main Street, +1-098-512-2222, Denve\n"

testPhone :: String -> [Char] -> [Char] -> Spec
testPhone r z s = 
    it (printf "should return phone for num: %s " z) $
        phone r z `shouldBe` s

spec :: Spec
spec = do
    describe "phone" $ do
        testPhone dr "48-421-674-8974" "Phone => 48-421-674-8974, Name => Anastasia, Address => Via Quirinal Roma"
        testPhone dr "1-921-512-2222" "Phone => 1-921-512-2222, Name => Wilfrid Stevens, Address => Wild Street AA-67209"
        testPhone dr "1-908-512-2222" "Phone => 1-908-512-2222, Name => Peter O'Brien, Address => High Street CC-47209"
        testPhone dr "1-541-754-3010" "Phone => 1-541-754-3010, Name => J Steeve, Address => 156 Alphand St."
        testPhone dr "1-121-504-8974" "Phone => 1-121-504-8974, Name => Arthur Clarke, Address => San Antonio TT-45120"
        testPhone dr "1-498-512-2222" "Phone => 1-498-512-2222, Name => Bernard Deltheil, Address => Mount Av. Eldorado"
        testPhone dr "1-098-512-2222" "Error => Too many people: 1-098-512-2222"
        testPhone dr "5-555-555-5555" "Error => Not found: 5-555-555-5555"

/+1-541-754-3010 156 Alphand_St. <J Steeve>\n
 133, Green, Rd. <E Kustur> NY-56423 ;+1-541-914-3010;\n\
\+1-541-984-3012 <P Reed> /PO Box 530; Pollocksville, NC-28573\n
 :+1-321-512-2222 <Paul Dive> Sequoia Alley PQ-67209\n\
\+1-741-984-3090 <Peter Reedgrave> _Chicago\n
 :+1-921-333-2222 <Anna Stevens> Haramburu_Street AA-67209\n\
\+1-111-544-8973 <Peter Pan> LA\n
 +1-921-512-2222 <Wilfrid Stevens> Wild Street AA-67209\n\
\<Peter Gone> LA ?+1-121-544-8974 \n
 <R Steell> Quora Street AB-47209 +1-481-512-2222!\n\
\<Arthur Clarke> San Antonio $+1-121-504-8974 TT-45120\n
 <Ray Chandler> Teliman Pk. !+1-681-512-2222! AB-47209,\n\
\<Sophia Loren> +1-421-674-8974 Bern TP-46017\n
 <Peter O'Brien> High Street +1-908-512-2222; CC-47209\n\
\<Anastasia> +48-421-674-8974 Via Quirinal Roma\n
 <P Salinger> Main Street, +1-098-512-2222, Denver\n\
\<C Powel> *+19-421-674-8974 Chateau des Fosses Strasbourg F-68000\n
 <Bernard Deltheil> +1-498-512-2222; Mount Av.  Eldorado\n\
\+1-099-500-8000 <Peter Crush> Labrador Bd.\n
 +1-931-512-4855 <William Saurin> Bison Street CQ-23071\n\
\<P Salinge> Main Street, +1-098-512-2222, Denve\n

テストプログラムからアドレス帳の例を確認する。
一人分のアドレスは一行で表していることから、まずは行ごとに分解すればよい。
電話番号は+X-abc-def-ghijの形式のもの、名前は<>で囲まれたもの、住所はそれ以外を取り出せば良いのだが、
おそらくそのままではない部分がいくつか見受けられる。

電話番号と名前を取り除いた後の文字列から、いくつかの記号をスペースに置き換えているように見える。
置き換えていないのはハイフンとピリオドで、アンダーバーやスラッシュ、バックスラッシュ、カンマ、セミコロンなどはスペースに置き換えている。

まずは電話番号と名前を取り出す関数を考えてみる。
電話番号と名前を取り出した後の文字列から、余分な記号をスペースに置き換え、
さらに余分なスペースもトリムすれば良い。
これで一行分の処理はできる。

さらに全体を見た時に、同じ電話番号が重複していた場合は"Too many people"、
存在しない電話番号の場合は"Not found"を返す必要がある。

重複を検知する必要があることから、Setを使おうかとも思ったが、
単にリストに入れておいて条件に合致するものがいくつあるかで判定すればよい。





-}