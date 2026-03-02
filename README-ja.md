# sproof

**やさしい定理証明系 — プログラマーのための Proof Assistant**

sproof は Scala 3 で書かれた依存型定理証明系です。Scala・Java・Rust・C++ を知っているプログラマーが、形式的検証を自然に書けることを目指しています。

[![CI](https://github.com/kmizu/sproof/actions/workflows/ci.yml/badge.svg)](https://github.com/kmizu/sproof/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## なぜ sproof？

従来の証明支援系（Coq・Lean・Agda）が一般プログラマーに広まらなかった原因は、依存型の難しさだけではありません。**構文という UI が「プログラマーに使ってもらう気がない」設計**だったことも大きな原因です。

```coq
(* Coq — 初見で読める？ *)
Fixpoint plus (n m : nat) : nat :=
  match n with
  | O => m
  | S n' => S (plus n' m)
  end.

Theorem plus_O_n : forall n : nat, 0 + n = n.
Proof.
  intros n. simpl. reflexivity.
Qed.
```

```scala
// sproof — Scala/Java/Rust を知っていれば初見で読める
def plus(n: Nat, m: Nat): Nat {
  match n {
    case Nat.zero    => m
    case Nat.succ(k) => Nat.succ(plus(k, m))
  }
}

// defspec: 仕様（命題）に対して証明プログラムを与える
defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n {
  by induction n {
    case zero      => trivial
    case succ k ih => simplify [ih]
  }
}
```

sproof が目指すのは「**本質的な難しさだけを残す**」こと。

- **学習コスト = 型理論の概念のみ** — 構文は追加コストにしない
- **ブレース `{ }` で統一** — Java/Rust/Scala を知る人が初見で読める
- **フルスペルの英単語タクティク** — `trivial`, `induction`, `simplify`（暗号略語なし）
- **省略形も提供** — `triv`, `induct`, `simp`（自明に略せるものだけ）
- **丁寧なエラーメッセージ** — 内部用語ではなく、次のステップを示す

---

## 比較表

|                | Coq             | Lean 4         | sproof                        |
|----------------|-----------------|----------------|-------------------------------|
| 実装言語        | OCaml           | C++            | **Scala 3**                   |
| 型理論          | CIC             | CIC            | **Predicative CIC**           |
| 構文            | 数学者向け       | 改善されたが独自 | **Scala ライク、ブレース統一** |
| Extraction 先   | OCaml / Haskell | Lean 自身       | **Scala 3（デフォルト）**     |
| Native バイナリ | —               | —              | **Scala Native 対応**         |
| 反射律タクティク | `rfl`           | `rfl`          | **`trivial`**                 |
| 前提導入        | `intros`        | `intro`        | **`assume`**                  |

---

## クイックスタート

```bash
# クローン & ビルド
git clone https://github.com/kmizu/sproof
cd sproof
sbt cli/run

# 証明ファイルを検査
sbt "cli/run check examples/nat.sproof"
```

### 出力例

```
OK: examples/nat.sproof — 1 inductive(s), 1 definition(s), 4 defspec(s)
```

---

## 構文ガイド

### 帰納型

```scala
inductive Nat {
  case zero: Nat
  case succ(n: Nat): Nat
}

inductive List(A: Type) {
  case nil: List(A)
  case cons(head: A, tail: List(A)): List(A)
}

inductive Bool {
  case true:  Bool
  case false: Bool
}
```

### 関数定義

```scala
// ブロック形式（再帰関数）
def plus(n: Nat, m: Nat): Nat {
  match n {
    case Nat.zero    => m
    case Nat.succ(k) => Nat.succ(plus(k, m))
  }
}

// 式形式（短い場合）
def id(x: Nat): Nat = x
```

### 仕様定義（defspec）

`defspec` は Curry-Howard 同型対応を直接表現するキーワードです。
**命題 = 型**、**証明 = プログラム**。

```
defspec 名前(引数): 命題 { 証明プログラム }
```

`def` との対称性：

```scala
def     foo(n: Nat): Nat  =         { n }         // 関数: 型に対してプログラム
defspec bar(n: Nat): P(n) { ... }       // 仕様: 命題に対して証明プログラム
```

証明プログラムが間違った型を持つ場合は型エラーとして弾かれます。普通のコードの型エラーと同じ扱いです。

### タクティク証明

```scala
// trivial: 両辺が定義的に等しい場合
defspec plus_zero_left(m: Nat): plus(Nat.zero, m) = m {
  by trivial
}

// 帰納法と帰納仮説（IH）
defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n {
  by induction n {
    case zero      => trivial
    case succ k ih => simplify [ih]
  }
}
```

### 項証明（Curry-Howard 項を直接書く）

```scala
defspec refl_intro(n: Nat): n = n {
  by induction n {
    case zero   => trivial
    case succ k => trivial
  }
}
```

---

## タクティク一覧

| タクティク              | 省略形       | 意味                                              |
|------------------------|-------------|---------------------------------------------------|
| `trivial`              | `triv`      | 両辺が定義的に等しければゴールを閉じる             |
| `simplify [f, g, ...]` | `simp`      | 指定した補題で簡約し、trivial で閉じる            |
| `induction x { ... }` | `induct x`  | `x` の型でコンストラクタ分割。再帰ケースに IH 付き |
| `assumption x`         | `assume x`  | `x : A` をコンテキストに導入し、ゴールを `B` に   |
| `apply f`              | —           | ゴールを `f` の戻り型と単一化、引数をサブゴールに  |
| `cases x { ... }`      | —           | 帰納仮説なしでコンストラクタ分割                  |
| `have h : T = proof`   | —           | ローカル補題を定義                                |
| `calc { ... }`         | —           | 等式の連鎖推論                                    |
| `ring`                 | —           | 環の等式を自動証明                                |
| `sorry`                | —           | 未完プレースホルダー（警告付き）                  |

**初心者へ**: まずフルスペル（`trivial`, `induction`, `simplify`）で書いてください。意味が直感的にわかります。省略形は慣れてから使えば十分です。

---

## Coq との構文対比

| 概念         | Coq                        | sproof                              |
|-------------|----------------------------|-------------------------------------|
| 帰納型定義   | `Inductive Nat : Set :=`   | `inductive Nat {`                   |
| 関数定義     | `Fixpoint plus ...`        | `def plus ...`                      |
| 定理         | `Theorem plus_zero ...`    | `defspec plus_zero ... {` |
| 証明開始     | `Proof.`                   | `{`                                 |
| 証明終了     | `Qed.`                     | `}`                                 |
| 反射律       | `reflexivity` / `rfl`      | `trivial`                           |
| 簡約         | `simpl` / `simp`           | `simplify` / `simp`                 |
| 前提導入     | `intros`                   | `assume`                            |
| 帰納法       | `induction n`              | `induction n {`                     |

---

## Scala 3 への Extraction

```bash
sbt "cli/run extract examples/nat.sproof --output Nat.scala"
```

命題（証明）は実行時に消去され、計算部分だけが Scala 3 コードとして出力されます。

```scala
// sproof
def plus(n: Nat, m: Nat): Nat { ... }
defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n { ... }

// 生成された Scala 3
def plus(n: Nat, m: Nat): Nat = ...
def plus_zero_right(n: Nat): Unit = ()   // 証明は消去
```

---

## アーキテクチャ

```
sproof/
├── core/       # Term ADT、De Bruijn 置換、型付けコンテキスト
├── eval/       # NbE（Normalization by Evaluation）
├── checker/    # 双方向型検査（bidirectional type checking）
├── tactic/     # TacticM モナド、組み込みタクティク
├── syntax/     # Parsley ベースパーサー、表面 AST、pretty-print
├── extract/    # Scala 3 コード生成
├── kernel/     # 信頼済みカーネル（< 500 行、監査可能）
└── cli/        # REPL、ファイルローダー
```

**型理論**: Predicative CIC（Calculus of Inductive Constructions）
- 宇宙階層: `Type`, `Type1`, `Type2`, ...
- 帰納型 + 不動点（再帰関数）
- Curry-Howard 同型対応（証明 = プログラム）

---

## Scala Native（ネイティブバイナリ）

sproof は [Scala Native](https://scala-native.org/) 経由で自己完結型のネイティブバイナリにコンパイルできます。実行時に JVM が不要になります。

### 前提条件

```bash
# Ubuntu / WSL2
sudo apt-get install clang lld libunwind-dev
```

### ビルド

```bash
# 全モジュールを native コンパイルしてリンク
sbt cliNative/nativeLink

# 生成されたネイティブバイナリを実行
./cli-native/target/scala-3.3.6/sproof-cli-native-out check examples/nat.sproof
```

### 設定

```
nativeLink     : releaseFast + LTO.thin + immix GC（デフォルト）
                 リンクが速く、日常的な開発に向く
releaseFull    : より積極的な最適化（リンクが遅い）→ リリース時に切り替える
```

### LLVM なしでコンパイルだけ確認

```bash
# clang がなくてもコンパイルは通せる
sbt cliNative/compile
```

---

## sbt プラグイン

sbt ビルドへの組み込みは [sbt-sproof](sbt-sproof/README.md) を参照してください。

```sbt
// project/plugins.sbt
addSbtPlugin("io.sproof" % "sbt-sproof" % "0.1.0")

// build.sbt
enablePlugins(SproofPlugin)
```

```bash
sbt sproofCheck    # すべての .sproof ファイルを型検査
sbt sproofExtract  # Scala 3 ソースへ抽出（コンパイル前に自動実行）
sbt sproofRepl     # 対話的 REPL を起動
```

---

## stdlib v1

`Nat`, `List`, `Vec`, `Bool`, `Relation`, `Dictionary`, `Effect` の標準ライブラリは
[`stdlib/`](stdlib) 以下にあります。

- レイアウトと命名規約: [docs/stdlib.md](docs/stdlib.md)
- 利用例: [`examples/stdlib/`](examples/stdlib)

---

## stdlib Bundles

再利用可能な補題 bundle マニフェストは `stdlib/bundles/` にあります。

- ドキュメントと互換性ポリシー: [docs/lemma-bundles.md](docs/lemma-bundles.md)
- 利用可能な bundle: `nat-simplify`, `list-core`, `bool-normalize`, `dictionary-core`, `relation-core`
- bundle 利用例: [examples/bundles/nat_bundle_usage.sproof](examples/bundles/nat_bundle_usage.sproof)

---

## ライセンス

MIT
