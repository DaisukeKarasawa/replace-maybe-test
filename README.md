# 超簡単 CLI（内部で Maybe を活用）

実用的なサブコマンドを備えつつ、実装内部で `Maybe` を複数箇所に活用する最小構成の CLI です。依存は `base` のみ。

## インストール/実行

```bash
# ビルド
stack build

# ヘルプ
stack run --
```

## コマンド一覧

### greet

```
stack run -- greet [--yell] [<name>]
```

- 名前が省略された場合は `$USER`、それも無ければ `stranger` を使用
- `--yell` で大文字化
- 内部での Maybe 活用: `lookupEnv`, `listToMaybe`, `mfilter`, `fromMaybe`

例:

```bash
stack run -- greet            #=> Hello, <USER or stranger>!
stack run -- greet alice      #=> Hello, alice!
stack run -- greet --yell bob #=> HELLO, BOB!
```

### sum

```
stack run -- sum [--strict|--lenient] <nums...>
```

- `--strict`: 1つでも不正なトークンがあるとエラー
- `--lenient`(デフォルト): 不正トークンはスキップして合計
- 内部での Maybe 活用: `traverse readMaybe`(strict), `mapMaybe readMaybe`(lenient), `maybe`

例:

```bash
stack run -- sum 1 2 3            #=> 6
stack run -- sum 1 2 x 3          #=> 6 (xをスキップ)
stack run -- sum --strict 1 2 x 3 #=> Error: invalid number: x
```

### head

```
stack run -- head <file>
```

- ファイルの先頭行を出力。空ファイルは `Empty file` を表示
- 内部での Maybe 活用: `listToMaybe`, `maybe`

例:

```bash
stack run -- head README.md  #=> READMEの最初の1行
```

### email

```
stack run -- email <name>
```

- 名前からメールを2段階で検索（name→id→email）
- 内部での Maybe 活用: `lookup` 合成、`(>>=)`, `<|>` によるフォールバック

例:

```bash
stack run -- email alice  #=> alice@example.com
stack run -- email carol  #=> not found
```

## 備考

- 旧来の「Maybeパターンの一覧/デモ」機能は削除しました（Breaking change）
- `docs/MaybePatternsDeepDive.md` は歴史的資料として残しています
