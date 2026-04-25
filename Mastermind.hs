-- https://www.codewars.com/kata/58a848258a6909dd35000003
-- Mastermind

module Mastermind where

--import Preloaded (Code, Game, GuessResult(..), Color(..))
-- ** 以下の定義はPreloadedからのものです ** 
-- | ゲームアクションを表す新しい型。@runGame game@はアクションの前に残っている
--   推測の数を取り、アクションの後に残っている推測の数と返される値をペアにして返します。
-- 
--   コンストラクタはエクスポートされません。@Game@アクションはその@Monad@インスタンスを
--   使用して構築することができます。
newtype Game a = Game {runGame :: Int -> (a,Int)}

instance Functor Game where
  fmap f g = Game $ \s -> let (a, s') = runGame g s in (f a, s')

instance Applicative Game where
  pure a = Game $ \s -> (a, s)
  f <*> g = Game $ \s -> let (a, s') = runGame f s; (b, s'') = runGame g s' in (a b, s'')

instance Monad Game where
  return = pure
  g >>= f = Game $ \s -> let (a, s') = runGame g s; h = f a in runGame h s

-- | 秘密のコードに現れる可能性のある色。
data Color
  = Red
  | Blue
  | Green
  | Orange
  | Purple
  | Yellow
  deriving (Show, Eq, Enum)
  
-- | 秘密のコードの型シノニム。
type Code = (Color, Color, Color, Color)

-- | 提供されたチェック関数を呼び出した結果の可能性をカプセル化するデータ型。
data GuessResult
  = Won
  | Result !Int !Int -- ^ @Result b w@ は@b@個の黒と@w@個の白を表します
  | TooManyGuesses
  deriving (Show, Eq)

-- | すべての可能なコードのリストを生成する関数
allCodes :: [Code]
allCodes = [(a, b, c, d) | a <- colors, b <- colors, c <- colors, d <- colors]
  where colors = [Red .. Yellow]


-- ヒントをもとに、次の推測候補を絞り込む関数
filterCandidates :: Code -> (Int, Int) -> [Code] -> [Code]
filterCandidates guess (b, w) = filter (\c -> evaluate guess c == (b, w))

-- 正解との差を評価（何個の `black` / `white` を持つか）
evaluate :: Code -> Code -> (Int, Int)
evaluate secret guess = (black, white)
  where
    black = length $ filter id $ zipWith (==) (codeToList secret) (codeToList guess)
    unmatchedSecret = [s | (s, g) <- zip (codeToList secret) (codeToList guess), s /= g]
    unmatchedGuess  = [g | (s, g) <- zip (codeToList secret) (codeToList guess), s /= g]
    white = length $ filter (`elem` unmatchedSecret) unmatchedGuess
    codeToList (x1, x2, x3, x4) = [x1, x2, x3, x4]


-- マスターマインドの解答ロジック
mastermind :: (Code -> Game GuessResult) -> Game Code
mastermind check = go allCodes
  where
    go [] = error "No possible solutions!"
    go (c:cs) = do
      result <- check c
      case result of
        Won -> return c  -- 正解なら終了
        Result b w -> go (filterCandidates c (b, w) cs)  -- 候補を絞る
        TooManyGuesses -> error "Exceeded max guesses!"

-- 秘密のコード
secretCode :: Code
secretCode = (Red, Blue, Green, Yellow)

-- 推測したコードをチェックする関数
check :: Code -> Game GuessResult
check guess = Game $ \s ->
  if guess == secretCode
  then (Won, s - 1)
  else (Result (countBlacks guess) (countWhites guess), s - 1)
  where
    countBlacks (a, b, c, d) = length $ filter id [a == fst secretCode, b == snd secretCode, c == thd secretCode, d == fth secretCode]
    countWhites (a, b, c, d) = length (filter (`elem` codeToList secretCode) [a, b, c, d]) - countBlacks (a, b, c, d)
    fst (x, _, _, _) = x
    snd (_, x, _, _) = x
    thd (_, _, x, _) = x
    fth (_, _, _, x) = x
    codeToList (x1, x2, x3, x4) = [x1, x2, x3, x4]

-- メイン関数
main :: IO ()
main = do
  let (result, _) = runGame (mastermind check) 10 -- 10回の推測を許可
  print result
