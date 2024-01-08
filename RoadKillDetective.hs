-- https://www.codewars.com/kata/58e18c5434a3022d270000f2
-- The Road-Kill Detective

module RoadKillDetective (roadKill) where

import Data.List

--import RoadKillDetective.Preloaded (animals)
animals = ["aardvark","alligator","armadillo","antelope","baboon","bear","bobcat","butterfly","cat","camel","cow","chameleon","dog","dolphin","duck","dragonfly","eagle","elephant","emu","echidna","fish","frog","flamingo","fox","goat","giraffe","gibbon","gecko","hyena","hippopotamus","horse","hamster","insect","impala","iguana","ibis","jackal","jaguar","jellyfish","kangaroo","kiwi","koala","killerwhale","lemur","leopard","llama","lion","monkey","mouse","moose","meerkat","numbat","newt","ostrich","otter","octopus","orangutan","penguin","panther","parrot","pig","quail","quokka","quoll","rat","rhinoceros","racoon","reindeer","rabbit","snake","squirrel","sheep","seal","turtle","tiger","turkey","tapir","unicorn","vampirebat","vulture","wombat","walrus","wildebeast","wallaby","yak","zebra"]

roadKill :: String -> String
roadKill photo = let
    target = convert $ filter ('='/=) photo
    result = [ name | 
        name <- animals, 
        detect target (convert name) || detect (reverse target) (convert name) ]
    in if null result then "??" else head result

convert :: String -> [(Char,Int)]
convert s = [ (head e, length e) | e <- group s ]

detect :: [(Char,Int)] -> [(Char,Int)] -> Bool
detect [] [] = True
detect target@(x:xs) known@(y:ys) =
    if fst x == fst y && snd x >= snd y then detect xs ys else False
detect _ _ = False


{-
動物の轢死体の写真から動物を判定する。
車に轢かれた動物が、引き延ばされたような状態になっているので、それを戻す。
"==========h===yyyyyy===eeee=n==a========"
・まず、余計な'='を取り除く。
・連続する同じ文字は、１つまたは２つに集約する。（ここがミソ）
　候補となる動物は、同じ文字は連続して最大２つまでしかないため。
・それぞれの文字を組み合わせて単語を作る。
　同じ文字が２つある場合、１つの場合と２つの場合をそれぞれ組み合わせる。
・組み合わせた結果が、候補の中にあれば、それが答え。
　候補の中になければ"??"が答え。
・例題によると、轢死体は逆順になっている場合がある。

・ただ、単語が長いと組み合わせが膨大になりそうなので、ちょっと工夫が必要そうである。
　連続する同じ文字は、文字とその数のペアに集約するのはどうだろうか。
　候補の動物も同様に、文字とその数のペアに集約する。
　"aardvark" -> [('a',2),('r',1),...,('k',1)]といった具合だ。
　先頭の文字から比較していって、文字が違えば候補から除外、
　文字が同じでも数が足りなければ候補から除外、
　文字がおなじで数が足りていれば次の文字へ、
　というのを繰り返すのはどうか。

-}