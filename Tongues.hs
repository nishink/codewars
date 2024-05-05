-- https://www.codewars.com/kata/52763db7cffbc6fe8c0007f8
-- Tongues

module Codewars.Exercise.Tongues where

import Data.Char
import Data.List
import Data.Maybe

tongues :: String -> String
tongues = map decode

decode :: Char -> Char
decode c
    | isAlpha c = if isUpper c then toUpper (rotate $ toLower c) else rotate c
    | otherwise = c

vowel = "aiyeou"
consonants = "bkxznhdcwgpvjqtsrlmf"

rotate :: Char -> Char
rotate c
    | elem c vowel = lshift c vowel 3
    | otherwise    = lshift c consonants 10

lshift :: Char -> String -> Int -> Char
lshift c s i = let
    idx = fromJust $ elemIndex c s
    n = idx - i + (if idx < i then length s else 0)
    in s !! n

{-

ガンダルフの著作は長い間研究のために利用可能でしたが、
誰もまだ彼らがどの言語で書かれているかを理解していません。
最近、コードネームROT13でしか知られていないハッカーによるプログラミング作業により、
ガンダルフは単純な文字置換スキームしか使用せず、さらに、独自の逆であることがわかりました。
同じ操作がメッセージをスクランブルするのと同じようにスクランブルします。

この操作は、小文字（つまり、下部または上部）を維持しながら、
シーケンス'a' 'i' 'y' 'e' 'o' 'u'の母音を循環的に3進母音に置き換えることによって実行されます。

Similarly, consonants are replaced from the 
sequence 'b' 'k' 'x' 'z' 'n' 'h' 'd' 'c' 'w' 'g' 'p' 'v' 'j' 'q' 't' 's' 'r' 'l' 'm' 'f' by advancing ten letters.

例えば'One ring to rule them all.'というフレーズは、'Ita dotf ni dyca nsaw ecc.'

この変換の魅力的な点は、結果として得られる言語が発音可能な単語を生み出すことです。
この問題については、ガンダルフの原稿をプレーンテキストに翻訳するコードを書きます。

あなたの仕事は、ガンダルフの文章を解読する関数を書くことです。

---

母音は循環的に３個、子音は循環的に１０個ずらしているように見える。
したがって、その逆になるような操作をすれば良い。

与えられた文字列を一文字ずつ見ていき、アルファベットであれば、まず小文字に直して母音か子音かを判定、
母音なら３個、子音なら１０個ずらした文字に置き換える。
元が大文字だったら大文字に戻す。
アルファベット以外の文字はそのまま。

-}