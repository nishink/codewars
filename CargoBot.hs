-- https://www.codewars.com/kata/616a585e6814de0007f037a7
-- Cargo-Bot Interpreter

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CargoBot where

-- import Preloaded

-- | The following are defined in Preloaded

-- クレート（木箱）の色を表すデータ型
data Color = Red | Yellow | Green | Blue deriving (Show, Eq, Enum)

-- クレート（木箱）を表すデータ型
newtype Crate = Crate { color :: Color } deriving (Show, Eq, Enum)

-- 爪の動作を表すデータ型
data Action
  = ClawLeft -- 爪を左に移動
  | ClawRight -- 爪を右に移動
  | ClawDown -- 状態により動作が変わる
             -- 爪が空で下に木箱がなければ何もしない
             -- 爪が空で下に木箱がある場合、爪は一番上の木箱を拾う
             -- 爪が木箱を持っている場合、下のスタックの一番上に木箱を置く
  | Prog1 | Prog2 | Prog3 | Prog4 -- プログラムを実行
  deriving (Show, Eq, Enum)
  
-- 条件付きフラグ：特定の条件の時だけアクションを実行する
data Flag
  = None -- 爪が空の時だけアクションを実行
  | Any -- 爪が木箱を持っている時だけアクションを実行
  | IsColor Color -- 爪が特定の色の木箱を持っている時だけアクションを実行
  deriving (Show, Eq)

-- コマンドを表すデータ型
data Command = Command
  { command :: Action -- アクション
  , flag    :: Maybe Flag -- 条件付きフラグ、条件なしの場合はNothing
  } deriving (Show, Eq)

-- プログラムを表すデータ型
type Program = [Command]

-- | cargoBot関数
cargobot :: [[Crate]] -> (Program, Program, Program, Program) -> Int -> [[Crate]]
cargobot crates progs maxSteps =
  stacks $ runProgram 1 initialState progs
  where
    initialState = State
      { stacks = crates -- スタックの状態
      , clawPos = 0 -- 爪の位置
      , holding = Nothing -- 爪が木箱を持っているか
      , stepsLeft = maxSteps -- 残りのステップ数
      }

-- プログラムを実行する関数
runProgram :: Int -> State -> (Program, Program, Program, Program) -> State
runProgram _ state _ | stepsLeft state <= 0 = state -- 残りのステップ数が0以下の場合はそのまま返す
runProgram n state progs = execCommands (getProgram n progs) state progs -- プログラムnからコマンドを取り出して実行

-- プログラムを取得する関数
getProgram :: Int -> (Program, Program, Program, Program) -> Program
getProgram 1 (p, _, _, _) = p
getProgram 2 (_, p, _, _) = p
getProgram 3 (_, _, p, _) = p
getProgram 4 (_, _, _, p) = p
getProgram _ _ = []

-- コマンドを実行する関数
execCommands :: [Command] -> State -> (Program, Program, Program, Program) -> State
execCommands [] st _ = st
execCommands _ st _ | stepsLeft st <= 0 = st
execCommands (Command act cond : rest) st progs
    | not (flagMatches cond (holding st)) =
        execCommands rest st { stepsLeft = stepsLeft st - 1 } progs
    | otherwise =
        execCommands rest (execAction act st progs) progs

-- アクションを実行する関数
execAction :: Action -> State -> (Program, Program, Program, Program) -> State
execAction _ st _ | stepsLeft st <= 0 = st
execAction ClawLeft st@(State stacks pos hold steps) _
    | pos > 0 = st { clawPos = pos - 1, stepsLeft = steps - 1 }
    | otherwise = st { stepsLeft = steps - 1 }
execAction ClawRight st@(State stacks pos hold steps) _
    | pos < length stacks - 1 = st { clawPos = pos + 1, stepsLeft = steps - 1 }
    | otherwise = st { stepsLeft = steps - 1 }
execAction ClawDown st@(State stacks pos hold steps) _ =
    case hold of
    Nothing ->
        case stacks !! pos of
        [] -> st { stepsLeft = steps - 1 } -- nothing to pick up
        c:cs -> st { stacks = replaceNth pos cs stacks, holding = Just c, stepsLeft = steps - 1 }
    Just c ->
        st { stacks = replaceNth pos (c : stacks !! pos) stacks, holding = Nothing, stepsLeft = steps - 1 }
execAction Prog1 st progs = runProgram 1 st { stepsLeft = stepsLeft st - 1 } progs
execAction Prog2 st progs = runProgram 2 st { stepsLeft = stepsLeft st - 1 } progs
execAction Prog3 st progs = runProgram 3 st { stepsLeft = stepsLeft st - 1 } progs
execAction Prog4 st progs = runProgram 4 st { stepsLeft = stepsLeft st - 1 } progs

-- 条件付きフラグが一致するかを判定
flagMatches :: Maybe Flag -> Maybe Crate -> Bool
flagMatches Nothing _ = True -- 条件がない場合は常に一致
flagMatches (Just None) Nothing = True -- 爪が空の場合に一致
flagMatches (Just None) _ = False -- 爪が空でない場合は不一致
flagMatches (Just Any) (Just _) = True -- 爪が何かを持っている場合に一致
flagMatches (Just Any) Nothing = False -- 爪が空の場合は不一致
flagMatches (Just (IsColor c)) (Just (Crate col)) = c == col -- 特定の色が一致する場合
flagMatches (Just (IsColor _)) _ = False -- 色が一致しない場合は不一致

-- ヘルパー：リストの n 番目を差し替える
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth 0 y (_:xs) = y : xs
replaceNth n y (x:xs) = x : replaceNth (n - 1) y xs

-- 内部状態の定義
data State = State
  { stacks     :: [[Crate]]
  , clawPos    :: Int
  , holding    :: Maybe Crate
  , stepsLeft  :: Int
  } deriving Show

-- テスト用フラグ指定
withFlag :: Action -> Flag -> Command
withFlag a = Command a . Just

action :: Action -> Command
action a = Command a Nothing

{-
cargobot [[Crate Red, Crate Yellow], [Crate Blue], []] ([action ClawDown, action ClawRight, action ClawDown], [], [], []) 3

cargobot [ [Crate Green,Crate Green,Crate Green,Crate Green,Crate Green,Crate Green],[],[],[],[],[] ]  ( [Command {command = ClawDown, flag = Nothing},Command {command = Prog2, flag = Just (IsColor Green)},Command {command = ClawRight, flag = Nothing},Command {command = Prog2, flag = Nothing},Command {command = ClawDown, flag = Nothing},Command {command = ClawRight, flag = Nothing},Command {command = Prog1, flag = Nothing}],    [Command {command = ClawRight, flag = Just (IsColor Green)},Command {command = ClawDown, flag = Nothing},Command {command = ClawLeft, flag = Nothing},Command {command = Prog1, flag = Just None}],    [],    [] )  173

-}
