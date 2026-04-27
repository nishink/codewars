-- https://www.codewars.com/kata/69bf57993a0061ef6d03c095
-- Cancellable Item Transactions

module ItemTransactions (calculate) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (isDigit)

-- 型定義
type Transaction = (Int, Char) -- (quantity, item)

calculate :: Map Char Int -> String -> Int
calculate priceList transactions = 
    -- 取引文字列を解析して、数量とアイテムのリストを生成し、取引を処理して合計金額を計算する
    let (total, _) = foldl processTransaction (0, []) (parseTransactions transactions)
    in total
    where
        -- 取引文字列を解析して、数量とアイテムのリストを生成する
        parseTransactions :: String -> [Transaction]
        parseTransactions [] = []
        parseTransactions (x:xs)
            -- 数量が負の場合は、'-'の後に続く数字を読み取る
            | x == '-' = let (qtyStr, rest) = span isDigit xs
                         in if null qtyStr
                            then (0, x) : parseTransactions xs
                            else let item = head rest
                                 in (read ('-':qtyStr), item) : parseTransactions (tail rest)
            -- 数量が正の場合は、数字を読み取る
            | isDigit x = let (qty, rest) = span isDigit (x:xs)
                              item = head rest
                          in (read qty, item) : parseTransactions (tail rest)
            -- アイテムだけの場合、数量は0とみなす
            | otherwise = (0, x) : parseTransactions xs

        -- 取引を処理する関数
        processTransaction :: (Int, [(Int, Char)]) -> Transaction -> (Int, [(Int, Char)])
        processTransaction (total, history) (0, item) = -- 数量が0の場合はキャンセル取引とみなす
            let (newTotal, newHistory) = cancelTransaction total history item
            in (newTotal, newHistory)
        processTransaction (total, history) (qty, item) = -- 通常の取引を処理する
            let price = Map.findWithDefault 0 item priceList -- アイテムの価格を取得（存在しない場合は0）
            in (total + qty * price, (qty, item) : history) -- 合計金額を加算し、取引を履歴の先頭に追加する

        -- キャンセル取引を処理する関数
        cancelTransaction :: Int -> [(Int, Char)] -> Char -> (Int, [(Int, Char)])
        cancelTransaction total history item =
            case break (\(_, i) -> i == item) history of -- breakは最初に条件に合致した要素以降と、それ以前で分割する
                (before, (qty, _):after) -> (total - qty * Map.findWithDefault 0 item priceList, before ++ after) -- 合致した数量を減算することでキャンセルする
                _ -> (total, history) -- キャンセルする取引がない場合は、合計と履歴をそのまま返す



{-
概要

1.取引ログ

アイテム取引の文字列が与えられます。例えば：

2A3B4C1A2A
5X6X1Y2Z1Y8X
各取引は、数字の後に文字が続き、項目の数量を示します。

例えば、「4A」は項目Aの4単位を意味します。

文字列は左から右へ処理すべきです。

2.キャンセル規則

時々、文字が単独で現れることがあります。これは、その項目の最新の取引が存在する場合、削除されることを意味します。

その項目に最近の取引がない場合、操作は効果がありません。

3.価格表

価格表（ハッシュマップ/辞書）も提供され、各商品を単価にマッピングします。

タスク

取引文字列と価格表を使用して、合計金額を計算することがあなたの課題です。

制約

連続したキャンセルが複数発生する可能性があります
負の量がテストされます
複数桁の数量はテストされます
例

price_dict = {"A":1,"B":3,"C":2}
transaction = "1A2BA3AC4CA"

Steps (processed from left to right):
  - "1A" -> 1*1 = 1
  - "2B" -> 2*3 = 6
  -  "A" -> removes the most recent transaction of A (1A)
  - "3A" -> 3*1 = 3
  -  "C" -> no previous transaction of item C (nothing to remove)
  - "4C" -> 4*2 = 8
  - "A" -> removes the most recent transaction of A (3A)

  
  Total: 14
-}