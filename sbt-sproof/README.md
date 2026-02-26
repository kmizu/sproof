# sbt-sproof

An sbt plugin for the [sproof](https://github.com/kmizu/sproof) theorem prover.

Automatically type-checks `.sproof` files and extracts verified Scala 3 source code as part of your sbt build.

---

## Setup

### 1. Add the plugin

In `project/plugins.sbt`:

```sbt
addSbtPlugin("io.sproof" % "sbt-sproof" % "0.1.0")
```

### 2. Enable in your build

In `build.sbt`:

```sbt
enablePlugins(SproofPlugin)
```

### 3. Install the sproof CLI

Either put `sproof` on your `PATH`:

```bash
# Download the sproof CLI uber-JAR and create a wrapper script, or
# build from source:
git clone https://github.com/kmizu/sproof && cd sproof
sbt cli/assembly   # produces cli/target/scala-3.x.x/sproof-cli-assembly-*.jar
```

Or configure the plugin to use a specific JAR:

```sbt
// build.sbt
enablePlugins(SproofPlugin)
sproofJar := Some(file("/path/to/sproof-cli-assembly.jar"))
```

---

## Tasks

| Task           | Description                                                        |
|----------------|--------------------------------------------------------------------|
| `sproofCheck`  | Type-check all `.sproof` files in `sproofSources`                  |
| `sproofExtract`| Extract proofs to Scala 3 (runs automatically before `compile`)    |
| `sproofRepl`   | Start the interactive sproof REPL                                  |

```bash
sbt sproofCheck    # check all .sproof files
sbt sproofExtract  # extract to target/src_managed/main/sproof/
sbt sproofRepl     # interactive REPL
sbt compile        # sproofExtract runs automatically before Scala compilation
```

---

## Settings

| Setting         | Type              | Default                            | Description                                         |
|-----------------|-------------------|------------------------------------|-----------------------------------------------------|
| `sproofVersion` | `String`          | `"0.1.0"`                          | Version of sproof to use                            |
| `sproofSources` | `Seq[File]`       | `Seq(src/main/sproof)`             | Directories containing `.sproof` files              |
| `sproofOutput`  | `File`            | `target/src_managed/main/sproof`   | Output directory for generated Scala files          |
| `sproofJar`     | `Option[File]`    | `None` (use `sproof` on PATH)      | Path to the sproof CLI uber-JAR                     |
| `sproofJvmOpts` | `Seq[String]`     | `Seq("-Xss8m")`                    | JVM options when forking the sproof process         |

---

## Example

### Project layout

```
my-project/
├── project/
│   └── plugins.sbt            # addSbtPlugin("io.sproof" % "sbt-sproof" % "0.1.0")
├── src/
│   └── main/
│       ├── scala/
│       │   └── Main.scala
│       └── sproof/
│           └── nat.sproof     # your proof file
└── build.sbt
```

### `src/main/sproof/nat.sproof`

```scala
inductive Nat {
  case zero: Nat
  case succ(n: Nat): Nat
}

def plus(n: Nat, m: Nat): Nat {
  match n {
    case Nat.zero    => m
    case Nat.succ(k) => Nat.succ(plus(k, m))
  }
}

defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n program = {
  by induction n {
    case zero      => trivial
    case succ k ih => simplify [ih]
  }
}
```

### `build.sbt`

```sbt
scalaVersion := "3.3.6"
enablePlugins(SproofPlugin)

// Optional: custom source directory
// sproofSources := Seq(baseDirectory.value / "proofs")

// Optional: use a specific JAR instead of PATH
// sproofJar := Some(file("/path/to/sproof-cli-assembly.jar"))
```

### Running

```bash
$ sbt sproofCheck
[info] sproof: Checking 1 file(s)...
[info]   OK: src/main/sproof/nat.sproof — 1 inductive(s), 1 definition(s), 1 defspec(s)
[success] sproof: All 1 file(s) passed.

$ sbt compile        # also runs sproofExtract automatically
[info] sproof: Extracting nat.sproof → nat.scala
[info] compiling 2 Scala sources ...
```

---

## Custom source directory

```sbt
enablePlugins(SproofPlugin)
sproofSources := Seq(baseDirectory.value / "proofs")
```

---

## Incremental extraction

`sproofExtract` skips files that haven't changed since the last extraction (source `.lastModified` vs generated file). The generated files are placed in `target/src_managed/main/sproof/` and automatically added to the Scala compilation classpath.

---

## Development

To test the plugin locally:

```bash
cd sbt-sproof
sbt publishLocal
```

Then in your test project's `project/plugins.sbt`:

```sbt
addSbtPlugin("io.sproof" % "sbt-sproof" % "0.1.0")
```
