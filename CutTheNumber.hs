-- https://www.codewars.com/kata/59cf17a9a25c8c08f400000b
-- Simple Fun #359: Cut The Number

module CutTheNumber.Kata (cut) where

import Data.List
import Data.Char
import Debug.Trace

cut :: String -> Int -> Int
cut number target = kut (read (shorten number) :: Int ) target 0

kut :: Int -> Int -> Int -> Int
kut 0 _ acc = acc
kut number target acc = 
  let
    len = figure number
    cand = [ kut n target t | 
           i <- [1..(if len > 5 then 5 else len)], 
           let (n, a) = divMod number (10^i), let t = acc + a,
           t <= target ] 
  in if cand == [] then -1 else maximum cand

figure :: Int -> Int
figure n
  | n < 10 = 1
  | otherwise = 1 + figure (div n 10)

shorten :: String -> String
shorten number = concat $ map short $ group number
  where
    short s = if head s == '0' && length s > 5 then take 5 s else s

cat :: String -> Int -> Int -> Int
cat [] _ acc = acc
cat number target acc = 
  let
    len = length number
    cand = [ cat t target a | i <- [1..(if len > 5 then 5 else len)], 
             let (h, t) = splitAt i number, let a = acc + read h :: Int,
             a <= target ]
  in if cand == [] then -1 else maximum cand


cut' :: String -> Int -> Int
cut' s n = foldl (\x y -> if x < y && y <= n then y else x) (-1) $ map calc $ piece s
 --let
 --   elems = map words $ piece s
 --   values = map read elems
 --   target = [ sum ns | p <- piece s, let ns = map read (words p), filter (> n) ns == [] ]
 --   target = filter (<=n) $ map (sum . (map read)) $ map words $ piece s
 --   target = filter (<=n) $ map calc $ piece s
 --   target = [ c | p <- piece s, let c = calc p, c <= n ]
 -- in if target == [] then -1 else maximum target

calc :: String -> Int
calc s = parse s 0 0
  where
    parse []     cnt acc = acc + cnt
    parse (x:xs) cnt acc 
      | x == ' '  = parse xs 0 (acc + cnt)
      | otherwise = parse xs (cnt * 10 + (digitToInt x)) acc

piece :: String -> [String]
piece xs = let
    len = length xs
    bin = [ binary n (len-1) (""," ") | n <- [0..2^(len-1)-1] ]
    sep = separate xs
  in map concat $ map (interleave sep) bin

binary :: Int -> Int -> (String, String) -> [String]
binary _ 0 _ = []
binary val fig (zero, one) = 
  binary (val `div` 2) (fig - 1) (zero, one) ++ 
  if val `mod` 2 == 1 then [one] else [zero]

separate :: String -> [String]
separate xs = [ [x] | x <- xs ]

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave (x:xs) ys = x : interleave ys xs


{-
分割の全組み合わせを作ってから最大値を探すのだと、２０桁になった時に時間がかかりすぎる。
0 < number.length < 20
0 < target <= 10000
問題の条件として、targetは最大でも10000なので、
分割しようとしたときに10000を超えるものを作らないようにすればよい。

"123456789"

("1", cut "23456789"), ("12", cut "3456789"), ... ("1234", cut "56789")
("12345", cut "6789")は最初の区切りが10000を超えているので除去する。

さらに考えると、targetが指定されているのだから、
take (length (show target)) number <= target
としてやれば、targetを超える桁の区切りは行わないことになる。

よって、最初にtarget以内となる区切りのパターンを割り出す。

number = "123456789", target = 10000
 separate number target = [ ("1","23456789"), ("12","3456789"), ... ]

これを一つずつ取り出して、最初の区切りと、残りをまた区切ったものの組み合わせを解とするように定義する。

[ combinations h (func t target) | (h, t) <- separate number target ]

ここでfuncはどう定義すればよいか。
t :: String, target :: Intなので、
func :: String -> Int -> *
となる。
また、combinationsでhと組み合わせたいので、*の各要素は[String]にしたい。
よって、
func :: String -> Int -> [[String]]
となる。

separate number target = [ ["1", "23456789"], ...] となるように定義するなら、
func = separate :: String -> Int -> [[String]]となる。

[ combinations x (separate y target) | (x:y:[]) <- separate number target ]

combinations :: String -> [[String]] -> [[String]]
combinations s seps = map (s:) seps


もっとシンプルに考える。

cut number target 
cut "123456789" 10000 = 
  maximum $ filter (\x -> 0 < x && x <= target) 
    [cut "2345678" (10000-1), cut "3456789" (10000-12), ...]
  = maximum [ cut n t | i <- [1..len number], 
      let n = drop i number, let t = target - read (take i number) :: Int,
      0 < t, t < target ]

一見、いい案のようにも見えるが、これでは加算していった数が見当たらない。

cut number target acc =
cut "123456789" 10000 0 =
  let cand =
    [ cut n t (acc+a) | i <- [1..len number], 
      let n = drop i number, let a = read (take i number) :: Int, let t = target - a,
      0 < t, t < target ]
  in if cand == [] then -1 else maximum cand
cut [] _ acc = acc

なんと、これでもまだ遅いらしい。

ならば、最初から全部数字として処理したらどうか。
cut number target acc =
cut 123456789 10000 0 =
	[ cut n target t | 
	  i <- [1..5], let (n, a) = divMod number (10^i), let t = acc + a,
	  t <= target ] 

これでもまだ遅いらしいので、最後の手段で０が５個以上連続したら０を短くする処理を入れてなんとか通った。


賢い人の答えは下記。
targetを徐々に減らしていけばよかったらしい。

-}

cut2 number target
 | read number <= target = read number
 | True = maximum $ map y [l,(l - 1)..1]
  where
   l = length number
   y n = if l <= target then let u = cut2 r (target - l) in if 0 <= u then l + u else -1 else -1
    where
     l = read $ take n number
     r = drop n number


