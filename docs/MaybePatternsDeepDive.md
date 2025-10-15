# Maybe モナド活用 Deep Dive

本ドキュメントは、本プロジェクトに収録された Maybe 活用パターンを、背景・用途・小さな性質（等式）とともに解説します。原則として base のみで完結します。

## 前提

- データ型: `data Maybe a = Nothing | Just a`
- 直観: 「失敗しうる or 欠損しうる」値を表現。`Nothing` は失敗/欠損、`Just x` は成功/存在。
- 代表的インスタンス:
  - Functor: `fmap :: (a -> b) -> Maybe a -> Maybe b`
  - Applicative: `(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b`
  - Monad: `(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b`
  - Alternative: `(<|>) :: Maybe a -> Maybe a -> Maybe a`
  - Monoid (`First a`, `Last a`): `mconcat` 等で候補列から「最初/最後の Just」を拾う

---

## 各パターン解説（主要抜粋）

### 1. basic-fmap（Functor）

- 狙い: 中身がある時だけ関数を適用。`Nothing` はそのまま。
- 等式: `fmap id = id`、`fmap (g . f) = fmap g . fmap f`

```haskell
fmap (+1) (Just 3) == Just 4
fmap (+1) Nothing  == Nothing
```

### 2. basic-bind（Monad: >>=）

- 狙い: 失敗を自動伝播させながら関数を合成。
- 用途: validation の段階実行、逐次的なルックアップ。

```haskell
Just 10 >>= halfEven == Just 5
Just 5  >>= halfEven == Nothing
```

### 3. basic-do（Monad: do記法）

- 狙い: `(>>=)` の糖衣。可読性向上。

```haskell
do { x <- Just 20; y <- halfEven x; z <- halfEven y; pure z } == Just 5
```

### 4. applicative-ap（Applicative: <*>）

- 狙い: 関数側も `Maybe` の時に適用。部分適用も自然に書ける。

```haskell
Just (+) <*> Just 2 <*> Just 3 == Just 5
```

### 5. applicative-liftA2

- 狙い: 二項関数と2つの `Maybe` を直接合成。

```haskell
liftA2 (+) (Just 2) (Just 3) == Just 5
```

### 6. maybe-fn（`maybe`）

- 狙い: デフォルトと適用関数を一度に指定。

```haskell
maybe "n/a" show (Just 42) == "42"
maybe "n/a" show Nothing  == "n/a"
```

### 7. fromMaybe

- 狙い: デフォルト値でアンラップ。

```haskell
fromMaybe 0 (Just 5) == 5
fromMaybe 0 Nothing  == 0
```

### 8. isJust / isNothing

- 狙い: 判定。副作用のないログや分岐に。

### 9. maybeToList / 10. listToMaybe

- 狙い: リストとの相互運用。安全な head/単一化。

### 11. catMaybes / 12. mapMaybe

- 狙い: 欠損を落として抽出、同時に変換。

```haskell
mapMaybe readMaybe ["10","x","20"] == [10,20]
```

### 13. guard-even（guard）

- 狙い: 条件不一致なら `Nothing` に落とすフィルタ。

```haskell
guardEven (Just 4) == Just 4
guardEven (Just 3) == Nothing
```

### 14. alternative-fallback（<|>）

- 狙い: フォールバック合成。左が失敗なら右。

```haskell
Nothing <|> Just 2 == Just 2
Just 1  <|> Just 2 == Just 1
```

### 15. mfilter

- 狙い: 中身の述語フィルタ。

### 16. safeHead

- 狙い: 部分関数 `head` を安全化。

### 17. nested-lookup（段階的ルックアップ）

- 狙い: `lookup` の合成で参照チェインを安全化。

```haskell
lookupEmailByName "bob" == Just "bob@example.com"
lookupEmailByName "carol" == Nothing
```

### 18. sequence-list / 19. traverse-parseInts

- 狙い: 全要素成功時だけ全体成功。

```haskell
sequence [Just 1, Just 2] == Just [1,2]
traverse readMaybe ["1","x","3"] == Nothing
```

### 20. kleisli-compose（>=>）

- 狙い: 失敗する関数の合成を演算子で表現。

### 21. first-last-monoid（First/Last）

- 狙い: 候補列から「最初/最後の Just」を抽出するモノイド。

```haskell
getFirst (mconcat [First Nothing, First (Just 2), First (Just 3)]) == Just 2
```

---

## Maybe を使う指針

- 値が存在しないことが通常系として想定される時は `Maybe`
- 失敗理由を伝える必要がある時は `Either e a`
- 多段の IO/非同期と混ざる場合は `MaybeT m a` を検討

## よくある落とし穴と対策

- head/!! など部分関数の使用 → 安全版（`listToMaybe`, `atMay`相当）へ
- 深いネスト → `do` 記法、`(>=>)`, `Applicative` 合成で平坦化
- 曖昧な `Nothing` → ドメインに応じて `Either` で理由を保持

## 参考

- `base` の `Data.Maybe`, `Control.Applicative`, `Control.Monad`, `Data.Monoid`
