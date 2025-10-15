# このCLIでの Maybe 活用ガイド（Deep Dive）

本ドキュメントは、本CLIの各サブコマンドの実装内部で `Maybe` がどのように使われているかを、最小コードとともに解説します。依存は `base` のみです。

---

## 前提と直観

- `Maybe a = Nothing | Just a`
- 「値が無い／失敗しうる」状況を例外なしに表現
- 代表的な道具:
  - `fromMaybe :: a -> Maybe a -> a`（デフォルトで取り出す）
  - `listToMaybe :: [a] -> Maybe a`（安全な先頭取得）
  - `mapMaybe :: (a -> Maybe b) -> [a] -> [b]`（失敗要素を落としつつ変換）
  - `traverse :: Applicative f => (a -> f b) -> t a -> f (t b)`（全要素成功時のみ成功）
  - `(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b`（失敗伝播）
  - `(<|>) :: Alternative f => f a -> f a -> f a`（フォールバック）

---

## greet: 環境変数/引数の安全な選択

狙い: ユーザー名を「引数 > 環境変数 `$USER` > デフォルト 'stranger'」の順で決定。空文字は無効扱い。

ポイント:

- `listToMaybe` で「最初の非フラグ引数」を `Maybe` で取得
- `mfilter (not . null)` で空文字を弾く
- `fromMaybe` で最終的なデフォルト値へフォールバック

最小抜粋（概念）:

```haskell
nameArg  = listToMaybe positionalArgs               -- Maybe String
chosen   = fromMaybe "stranger" $                   -- String
             mfilter (not . null) nameArg <|>
             mfilter (not . null) envUser
```

性質/考え方:

- データが「無い」状態を例外ではなく `Nothing` として自然に合成
- デフォルト値は最後に `fromMaybe` で一括指定

---

## sum: strict/lenient の二段パース

狙い: 与えられたトークン列を数値に変換して合計。

- strict: 1つでも不正があれば `Either` で理由を保持して失敗（例: "invalid number: x"）
- lenient: 不正トークンは落として合計

最小抜粋:

```haskell
-- strict (Either)
case traverse readEitherInt tokens of
  Right xs -> print (sum xs)
  Left err -> putStrLn ("Error: " ++ err)

-- lenient (Maybe)
let xs = mapMaybe readMaybe tokens in print (sum xs)
```

性質/考え方:

- `traverse readMaybe :: [String] -> Maybe [Int]` は「全部成功なら Just、どれか失敗なら Nothing」
- `mapMaybe readMaybe` は「成功要素だけを抽出」。部分的成功を許可
- 最初の不正トークン検出には `listToMaybe [t | t <- tokens, readMaybe t == Nothing]`

---

## head: ファイル先頭行の安全取得

狙い: 空ファイルでも例外にせず、メッセージにフォールバック。

最小抜粋:

```haskell
let mFirst = listToMaybe (lines content)  -- Maybe String
putStrLn (maybe "Empty file" id mFirst)
```

性質/考え方:

- 部分関数 `head` の代わりに `listToMaybe` を使い、空でも安全
- 表示は `maybe def id` で `Nothing` をデフォルトへ

---

## email: 二段階 lookup とフォールバック（原因の区別）

狙い: `name -> userId -> email` を安全に段階解決。大文字小文字の差異も許容（フォールバック）。

最小抜粋:

```haskell
data EmailErr = NoSuchUser | EmailMissing

lookupEmail :: String -> Either EmailErr String
lookupEmail name = do
  uid <- maybe (Left NoSuchUser) Right
         (lookup name userIdByName <|> lookup (map toLower name) userIdByNameLower)
  maybe (Left EmailMissing) Right (lookup uid emailById)

case lookupEmail name of
  Right e -> putStrLn e
  Left NoSuchUser   -> putStrLn "Error: user not found"
  Left EmailMissing -> putStrLn "Error: email not registered"
```

性質/考え方:

- `(>>=)` で段階的に失敗を伝播（どこかが失敗したら全体 `Nothing`）
- `<|>` で別の検索戦略（小文字化）にフォールバック
- 失敗原因は `Either` で保持し、ユーザーに分かるメッセージを出し分け

---

## まとめ: このCLIで見える Maybe の実践パターン

- **安全な選択/フォールバック**: 引数/環境変数/デフォルトの合成（`mfilter`, `<|>`, `fromMaybe`）
- **安全な先頭取得**: `listToMaybe` で例外のない head
- **二段パースの設計**: strict は `traverse`、lenient は `mapMaybe`
- **段階的合成**: `lookup` チェーンを `(>>=)` で自然に表現

参考実装: `src/Main.hs` を参照してください。
