-- https://www.codewars.com/kata/537e18b6147aa838f600001b
-- Text align justify

module TextAlignJustify where

justify :: String -> Int -> String
justify text width = f (words text) width
    where
        f ws w = let
            (line, remain) = breakWords ws w
            in if null remain then unwords line else fillSpace line w ++ "\n" ++ f remain w


breakWords :: [String] -> Int -> ([String], [String])
breakWords [] _ = ([],[])
breakWords (x:xs) w = f xs [x]
    where
        f [] acc = (acc, [])
        f (x:xs) acc = 
            if length (unwords (acc ++ [x])) <= w 
                then f xs (acc ++ [x]) else (acc, (x:xs))

fillSpace :: [String] -> Int -> String
fillSpace ws w = let
    totalLen = sum (map length ws)
    spaceCnt = if w < totalLen then 0 else w - totalLen
    wordSpacePair = zip ws $ (allocSpace spaceCnt (length ws))
    in concat $ map (\(s,i) -> s ++ replicate i ' ') wordSpacePair

allocSpace :: Int -> Int -> [Int]
allocSpace _ 1 = [0]
allocSpace s w = let
    (d,m) = divMod s (w-1)
    base = replicate (w-1) d
    in f base m ++ [0]
    where
        f xs 0 = xs
        f (x:xs) n = (x+1) : f xs (n-1)


{-
文章を指定した幅で整える。
文章を単語で区切り、単語間のスペースの数を調節する。
スペースの数は、行の先頭に近い方が多くなる。

123456789012345678901234567890
Lorem  ipsum  dolor  sit amet,
指定した幅に改行文字は含まれない。

単語を１つずつ取り出して、１行分の幅を超えそうなら一旦そこで切る処理を作る。
切った単語の長さの合計を指定幅から引いた残りをスペースの数とする。
スペースの数を先頭多めに割り振る処理を作る。

最後の行だけ、スペースは１つでよい。
幅に合わせて調節する必要がないので。


Lorem  ipsum  dolor  sit amet,
consectetur  adipiscing  elit.
Vestibulum    sagittis   dolor
mauris,  at  elementum  ligula
tempor  eget.  In quis rhoncus
nunc,  at  aliquet orci. Fusce
at   dolor   sit   amet  felis
suscipit   tristique.   Nam  a
imperdiet   tellus.  Nulla  eu
vestibulum    urna.    Vivamus
tincidunt  suscipit  enim, nec
ultrices   nisi  volutpat  ac.
Maecenas   sit   amet  lacinia
arcu,  non dictum justo. Donec
sed  quam  vel  risus faucibus
euismod.  Suspendisse  rhoncus
rhoncus  felis  at  fermentum.
Donec lorem magna, ultricies a
nunc    sit    amet,   blandit
fringilla  nunc. In vestibulum
velit    ac    felis   rhoncus
pellentesque. Mauris at tellus
enim.  Aliquam eleifend tempus
dapibus. Pellentesque commodo,
nisi    sit   amet   hendrerit
fringilla,   ante  odio  porta
lacus,   ut   elementum  justo
nulla et dolor.

Lorem  ipsum  dolor  sit amet,
consectetur  adipiscing  elit.
Vestibulum    sagittis   dolor
mauris,  at  elementum  ligula
tempor  eget.  In quis rhoncus
nunc,  at  aliquet orci. Fusce
at   dolor   sit   amet  felis
suscipit   tristique.   Nam  a
imperdiet   tellus.  Nulla  eu
vestibulum    urna.    Vivamus
tincidunt  suscipit  enim, nec
ultrices   nisi  volutpat  ac.
Maecenas   sit   amet  lacinia
arcu,  non dictum justo. Donec
sed  quam  vel  risus faucibus
euismod.  Suspendisse  rhoncus
rhoncus  felis  at  fermentum.
Donec  lorem  magna, ultricies
a   nunc   sit  amet,  blandit
fringilla  nunc. In vestibulum
velit    ac    felis   rhoncus
pellentesque.     Mauris    at
tellus  enim. Aliquam eleifend
tempus  dapibus.  Pellentesque
commodo,    nisi    sit   amet
hendrerit    fringilla,   ante
odio     porta    lacus,    ut
elementum   justo   nulla   et
dolor.
-}