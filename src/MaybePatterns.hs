module MaybePatterns
  ( Example(..)
  , examples
  , findExample
  ) where

import Data.Maybe (isJust, isNothing, maybeToList, listToMaybe, fromMaybe, catMaybes, mapMaybe)
import Data.List (find)
import Control.Applicative (liftA2, (<|>))
import Control.Monad (guard, mfilter, (>=>))
import Data.Monoid (First(..), Last(..), getFirst, getLast)
import Text.Read (readMaybe)

-- | A single runnable example showcasing a concrete Maybe pattern.
--   'explain' is shown as bullet points.
--   'runText' contains a pre-rendered demonstration block.
--   Keep examples minimal and focused.
data Example = Example
  { exampleId :: String
  , title     :: String
  , explain   :: [String]
  , runText   :: String
  } deriving (Eq, Show)

findExample :: String -> Maybe Example
findExample key = find ((== key) . exampleId) examples

-- Helpers used by multiple examples
halfEven :: Int -> Maybe Int
halfEven n = if even n then Just (n `div` 2) else Nothing


-- Use guard to keep only even values inside Maybe
guardEven :: Maybe Int -> Maybe Int
guardEven mx = do
  x <- mx
  guard (even x)
  pure x

lookupEmailByName :: String -> Maybe String
lookupEmailByName name = do
  uid <- lookup name userIdByName
  lookup uid emailById
  where
    userIdByName = [("alice", 1 :: Int), ("bob", 2)]
    emailById    = [(1, "alice@example.com"), (2, "bob@example.com")]

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

examples :: [Example]
examples =
  [ Example "basic-fmap" "fmapで安全に写像"
      [ "コンテナ内の値だけ変換し、Nothingはそのまま。Functorの基本。" ]
      (unlines
        [ "fmap (+1) (Just 3) => " ++ show (fmap (+1) (Just (3 :: Int)))
        , "fmap (+1) Nothing  => " ++ show (fmap (+1) (Nothing :: Maybe Int))
        ])
  , Example "basic-bind" ">>= で安全に合成"
      [ "失敗（Nothing）を自動伝播。中身がなければ以降の計算は実行されない。" ]
      (unlines
        [ "Just 10 >>= halfEven => " ++ show (Just (10 :: Int) >>= halfEven)
        , "Just 5  >>= halfEven => "  ++ show (Just (5 :: Int)  >>= halfEven)
        , "Nothing  >>= halfEven => " ++ show ((Nothing :: Maybe Int) >>= halfEven)
        ])
  , Example "basic-do" "do記法で段階的に失敗伝播"
      [ "段階的に取り出していき、どこかで失敗すれば全体がNothing。" ]
      (unlines
        [ "do {x<-Just 20; y<-halfEven x; z<-halfEven y; pure z} => "
            ++ show (do { x <- Just (20 :: Int); y <- halfEven x; z <- halfEven y; pure z })
        , "do {x<-Just 10; y<-halfEven x; z<-halfEven y; pure z} => "
            ++ show (do { x <- Just (10 :: Int); y <- halfEven x; z <- halfEven y; pure z })
        ])
  , Example "applicative-ap" "<*> で関数コンテナを適用"
      [ "関数を包んだMaybeを値に適用。いずれかがNothingなら結果もNothing。" ]
      (unlines
        [ "Just (+1) <*> Just 3 => " ++ show (Just ((+1) :: Int -> Int) <*> Just 3)
        , "Just (+)  <*> Just 2 <*> Just 3 => "
            ++ show (Just ((+) :: Int -> Int -> Int) <*> Just 2 <*> Just 3)
        , "Nothing   <*> Just 3 => " ++ show ((Nothing :: Maybe (Int -> Int)) <*> Just 3)
        ])
  , Example "applicative-liftA2" "liftA2 で二項関数適用"
      [ "二つのMaybeを二項関数で合成。" ]
      (unlines
        [ "liftA2 (+) (Just 2) (Just 3) => "
            ++ show (liftA2 ((+) :: Int -> Int -> Int) (Just 2) (Just 3))
        , "liftA2 (+) (Just 2) Nothing  => "
            ++ show (liftA2 ((+) :: Int -> Int -> Int) (Just 2) (Nothing :: Maybe Int))
        ])
  , Example "maybe-fn" "maybe で安全に取り出し"
      [ "maybe def f m: Nothingならdef、Just xならf x。" ]
      (unlines
        [ "maybe \"n/a\" show (Just 42) => " ++ show (maybe "n/a" show (Just (42 :: Int)))
        , "maybe \"n/a\" show Nothing  => " ++ show (maybe "n/a" show (Nothing :: Maybe Int))
        ])
  , Example "fromMaybe" "デフォルトでアンラップ"
      [ "fromMaybe def m: Nothingならdef、Just xならx。" ]
      (unlines
        [ "fromMaybe 0 (Just 5) => " ++ show (fromMaybe (0 :: Int) (Just 5))
        , "fromMaybe 0 Nothing  => " ++ show (fromMaybe (0 :: Int) (Nothing :: Maybe Int))
        ])
  , Example "isJust-isNothing" "状態判定"
      [ "中身の有無を判定する述語。" ]
      (unlines
        [ "isJust (Just 1)   => " ++ show (isJust (Just (1 :: Int)))
        , "isNothing Nothing => " ++ show (isNothing (Nothing :: Maybe Int))
        ])
  , Example "maybeToList" "リストへ安全変換"
      [ "Just xは[x]に、Nothingは[]に。" ]
      (unlines
        [ "maybeToList (Just 1) => " ++ show (maybeToList (Just (1 :: Int)))
        , "maybeToList Nothing  => " ++ show (maybeToList (Nothing :: Maybe Int))
        ])
  , Example "listToMaybe" "安全な先頭取得"
      [ "空リストでも例外にならない安全なhead。" ]
      (unlines
        [ "listToMaybe [1,2,3] => " ++ show (listToMaybe ([1,2,3] :: [Int]))
        , "listToMaybe []      => " ++ show (listToMaybe ([] :: [Int]))
        ])
  , Example "catMaybes" "Justだけ抽出"
      [ "Maybeのリストから値だけを取り出す。" ]
      (unlines
        [ "catMaybes [Just 1, Nothing, Just 3] => " ++ show (catMaybes [Just (1 :: Int), Nothing, Just 3])
        ])
  , Example "mapMaybe-parseInt" "失敗するパースを混ぜた変換"
      [ "mapMaybeで失敗要素を落としつつ変換。" ]
      (unlines
        [ "mapMaybe readMaybe [\"10\",\"x\",\"20\"] :: [Int] => "
            ++ show (mapMaybe readMaybe (["10","x","20"] :: [String]) :: [Int])
        ])
  , Example "guard-even" "guardで条件フィルタ"
      [ "条件を満たさない場合、計算をNothingにする。" ]
      (unlines
        [ "guardEven (Just 4) => " ++ show (guardEven (Just (4 :: Int)))
        , "guardEven (Just 3) => " ++ show (guardEven (Just (3 :: Int)))
        ])
  , Example "alternative-fallback" "<|> でフォールバック"
      [ "左がNothingなら右を返す。連鎖可能。" ]
      (unlines
        [ "Just 1 <|> Just 2 => " ++ show ((Just (1 :: Int)) <|> Just 2)
        , "Nothing <|> Just 2 => " ++ show ((Nothing :: Maybe Int) <|> Just 2)
        , "Nothing <|> Nothing => " ++ show ((Nothing :: Maybe Int) <|> (Nothing :: Maybe Int))
        ])
  , Example "mfilter" "中身を述語でフィルタ"
      [ "Just xで述語falseならNothing。Nothingはそのまま。" ]
      (unlines
        [ "mfilter even (Just 4) => " ++ show (mfilter even (Just (4 :: Int)) :: Maybe Int)
        , "mfilter even (Just 3) => " ++ show (mfilter even (Just (3 :: Int)) :: Maybe Int)
        , "mfilter even Nothing  => " ++ show (mfilter even (Nothing :: Maybe Int) :: Maybe Int)
        ])
  , Example "safeHead" "部分関数を安全化"
      [ "headの代わりにMaybeを返す安全版。" ]
      (unlines
        [ "safeHead [1,2,3] => " ++ show (safeHead ([1,2,3] :: [Int]))
        , "safeHead []      => " ++ show (safeHead ([] :: [Int]))
        ])
  , Example "nested-lookup" "lookupの段階的合成"
      [ "二段階のlookupをKleisli合成で表現。" ]
      (unlines
        [ "lookupEmailByName \"bob\" => " ++ show (lookupEmailByName "bob")
        , "lookupEmailByName \"carol\" => " ++ show (lookupEmailByName "carol")
        ])
  , Example "sequence-list" "sequenceでまとめる"
      [ "[Maybe a] -> Maybe [a] へ。どれかがNothingなら全体がNothing。" ]
      (unlines
        [ "sequence [Just 1, Just 2, Just 3] => "
            ++ show (sequence [Just (1 :: Int), Just 2, Just 3] :: Maybe [Int])
        , "sequence [Just 1, Nothing, Just 3] => "
            ++ show (sequence [Just (1 :: Int), Nothing, Just 3] :: Maybe [Int])
        ])
  , Example "traverse-parseInts" "traverseで一括パース"
      [ "readMaybeを各要素に適用し、全体をMaybeでまとめる。" ]
      (unlines
        [ "traverse readMaybe [\"1\",\"2\",\"3\"] :: Maybe [Int] => "
            ++ show (traverse readMaybe (["1","2","3"] :: [String]) :: Maybe [Int])
        , "traverse readMaybe [\"1\",\"x\",\"3\"] :: Maybe [Int] => "
            ++ show (traverse readMaybe (["1","x","3"] :: [String]) :: Maybe [Int])
        ])
  , Example "kleisli-compose" "(>=>) で関数合成"
      [ "失敗しうる関数を安全に合成。" ]
      (unlines
        [ "(halfEven >=> halfEven) 8 => " ++ show (((halfEven >=> halfEven) 8))
        , "(halfEven >=> halfEven) 3 => " ++ show (((halfEven >=> halfEven) 3))
        ])
  , Example "first-last-monoid" "First/Last でモノイド結合"
      [ "候補列から最初のJust、または最後のJustを抽出。" ]
      (unlines
        [ "getFirst (mconcat [First Nothing, First (Just 2), First (Just 3)]) => "
            ++ show (getFirst (mconcat [First (Nothing :: Maybe Int), First (Just 2), First (Just 3)]))
        , "getLast  (mconcat [Last Nothing, Last (Just 2), Last (Just 3)]) => "
            ++ show (getLast (mconcat [Last (Nothing :: Maybe Int), Last (Just 2), Last (Just 3)]))
        ])
  ]
