# Maybeモナド活用パターン CLI（Stack）

最小実装で Maybe モナドの“多様な活用パターン”を横断的に確認できるサンプル CLI です。21 パターンを収録し、`list`/`all`/`<id>` の3モードで出力します。

## 特徴

- base のみ（追加依存なし）
- 読みやすい整形出力（各パターン: タイトル/要点/デモ）
- 実装は小さく、パターン理解に集中

## インストール/実行

```bash
# ビルド
stack build

# 収録パターン一覧
stack run list

# 全パターン連続実行
stack run all

# 単一パターン実行
stack run basic-fmap
```

## CLI 仕様

- `stack run list`: ID とタイトルの一覧
- `stack run all`: 全パターンを順に実行
- `stack run <id>`: 指定 ID のみ実行（例: `basic-fmap`）

## 収録パターン（21）

- basic-fmap: `fmap`で`Just`/`Nothing`を変換
- basic-bind: `>>=`で安全に合成
- basic-do: do記法で段階的に失敗伝播
- applicative-ap: `<*>`で関数コンテナを適用
- applicative-liftA2: `liftA2`で二項関数適用
- maybe-fn: `maybe def f m`で安全に取り出し
- fromMaybe: デフォルト値でアンラップ
- isJust-isNothing: 状態判定
- maybeToList: `Maybe -> [a]`に変換
- listToMaybe: `[]`から安全に先頭取得
- catMaybes: `Maybe`リストから値だけ抽出
- mapMaybe-parseInt: 失敗するパースを混ぜた変換
- guard-even: `guard`で条件フィルタ
- alternative-fallback: `<|>`でフォールバック
- mfilter: 述語で`Maybe`の中身を条件フィルタ
- safeHead: 部分関数を安全化
- nested-lookup: `lookup`の段階的合成
- sequence-list: `sequence [Maybe a] -> Maybe [a]`
- traverse-parseInts: `traverse readMaybe`で一括パース
- kleisli-compose: `(>=>)`で失敗する関数を合成
- first-last-monoid: `First`/`Last`でモノイド結合

## サンプル出力（抜粋）

```text
[#] basic-fmap - fmapで安全に写像
  - コンテナ内の値だけ変換し、Nothingはそのまま。Functorの基本。

  Demo:
    fmap (+1) (Just 3) => Just 4
    fmap (+1) Nothing  => Nothing
----------------------------------------
```

## 実際のソース抜粋

### CLI のエントリポイント（`src/Main.hs`）

```haskell
-- 引数で list/all/<id> を切り替える
case args of
  ["list"] -> listExamples
  ["all"]  -> runAll
  [exId]    -> runOne exId
  _         -> putStrLn usage
```

### パターン登録の構造（`src/MaybePatterns.hs`）

```haskell
data Example = Example
  { exampleId :: String
  , title     :: String
  , explain   :: [String]
  , runText   :: String
  }

examples :: [Example]
examples =
  [ Example "basic-fmap" "fmapで安全に写像"
      ["コンテナ内の値だけ変換し、Nothingはそのまま。Functorの基本。"]
      (unlines
        [ "fmap (+1) (Just 3) => " ++ show (fmap (+1) (Just (3 :: Int)))
        , "fmap (+1) Nothing  => " ++ show (fmap (+1) (Nothing :: Maybe Int))
        ])
  -- ... 他パターンも同様に登録
  ]
```

## 拡張の仕方

- 新しいパターンは `src/MaybePatterns.hs` の `examples` に `Example` を1件追加
- `exampleId` は `stack run <id>` の引数に使われます
