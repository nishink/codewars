-- https://www.codewars.com/kata/581e014b55f2c52bb00000f8
-- Decipher this!

module Kata (decipherThis) where 

decipherThis :: String -> String
decipherThis message = unwords $ map decipherWord (words message)
  where
    decipherWord :: String -> String
    decipherWord [] = []
    decipherWord word =
      let (asciiStr, rest) = span (`elem` ['0'..'9']) word
          firstChar = toEnum (read asciiStr :: Int) :: Char
          swappedRest = swapSecondAndLast rest
      in firstChar : swappedRest

    swapSecondAndLast :: String -> String
    swapSecondAndLast [] = []
    swapSecondAndLast [x] = [x]
    swapSecondAndLast xs@(x:_:_) =
      let lastChar = last xs
          middle = init (tail xs)
      in lastChar : middle ++ [x]

{-
説明：1つの文字列を受け取り、各単語の最初の文字がASCIIコードでエンコードされているメッセージをデコードします。
各単語の最初の文字をASCIIコードから変換し、次に2番目と最後の文字を入れ替えます。
例：
decipherThis "72olle 103doo 100ya"  -->  "Hello good day"   
decipherThis "82yade 115te 103o"    -->  "Ready set go"
-}
