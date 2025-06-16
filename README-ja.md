# OCaml 論理式計算機

*OC*aml *L*ogic Expression Calculator (OCL) は、論理式の解析、評価、分析を行うコマンドラインツールです。様々な論理演算子をサポートし、真理値表の生成や論理的性質の検出などの機能を提供します。

## 機能

- 複数の記法による式解析:
  - リテラル: `true`, `false`
  - 否定: `~`, `not`, `!`
  - 論理積: `&`, `and`, `&&`
  - 論理和: `|`, `or`, `||`
  - 含意: `->`, `=>`
  - 双条件: `<->`, `<=>`

- 変数割り当てによる式評価
- 真理値表生成 
- 論理的性質の検出:
  - トートロジー（恒真式）の検査
  - 矛盾の検査

## ビルド & 実行

### 依存関係
- OCaml (>= 5.2.1)
- Dune (>= 3.19)
- Menhir
- Alcotest 

### ビルド
```bash
dune build
```

### 実行
```bash
dune install ; ocl
```

### テスト
```bash
dune test
```

## 使用方法

```
> p & q
Expression: (p & q)
Satisfiable

Truth table:
p    q    | (p & q)
---  ---  | ---
F    F    | F
F    T    | F
T    F    | F
T    T    | T

> p | ~p
Expression: (p | ~p)
Tautology

Truth table:
p   | (p | ~p)
--- | ---
F   | T
T   | T
```

## プロジェクト構造

```
├── bin/              # 実行ファイル
│   └── main.ml       # REPL インターフェース
├── lib/              # ライブラリ
│   ├── syntax.ml     # AST 定義
│   ├── eval.ml       # 評価器
│   ├── parser.mly    # Menhir パーサー
│   └── lexer.mll     # OCamllex レクサー
└── test/ 
    └── test.ml       # テスト
```

## テスト済み論理法則

- 排中律: `p | ~p`
- 無矛盾律: `~(p & ~p)`
- ド・モルガンの法則: `~(p & q) <-> (~p | ~q)`
- 分配法則: `p & (q | r) <-> (p & q) | (p & r)`
- 結合法則: `(p & q) & r <-> p & (q & r)`
- 対偶: `p -> q <-> ~q -> ~p`
- 実質含意: `p -> q <-> ~p | q`
- 仮言的三段論法: `(p -> q) & (q -> r) -> (p -> r)`
