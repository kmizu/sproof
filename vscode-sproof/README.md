# sproof — VS Code Extension

Language support for the [sproof](https://github.com/kmizu/sproof) theorem prover.

## Features

### Syntax Highlighting

Full TextMate grammar for `.sproof` files, covering:

- **Keywords**: `inductive`, `def`, `defspec`, `case`, `match`, `by`, `program`, `fun`
- **Tactic keywords**: `trivial`, `triv`, `assume`, `apply`, `simplify`, `simp`, `induction`, `sorry`, `have`, `calc`, `ring`
- **Type keywords**: `Type`, `Type0`, `Type1`, `Type2`, `Pi`
- **Operators**: `->`, `=>`, `=`, `:`
- **Comments**: `//` line comments and `/* */` block comments
- **Constructor references**: `Nat.zero` style qualified names
- **Numbers**

### Language Configuration

- Bracket matching and auto-closing for `{}`, `()`, `[]`, `""`
- Comment toggling with `//` (line) and `/* */` (block)
- Auto-indent on `{`

### Snippets

| Prefix | Description |
|--------|-------------|
| `inductive` | Inductive type definition |
| `inductivep` | Parameterized inductive type |
| `def` | Function definition |
| `defspec` | Theorem with proof |
| `match` | Pattern match expression |
| `induction` | Induction tactic (natural numbers) |
| `assume` | Assume tactic |
| `apply` | Apply tactic |
| `have` | Intermediate result |
| `Pi` | Pi (dependent function) type |
| `fun` | Lambda abstraction |

### Hover Documentation

Hovering over any sproof keyword shows a brief description of its meaning and usage.

### Document Outline

The VS Code outline panel (and breadcrumbs) show all top-level `def`, `defspec`, and `inductive` definitions in the current file.

### Goal/Subgoal Visualization

- Run `sproof: Show Goals` from the command palette to inspect the current proof state.
- The extension writes goal output to the `sproof goals` output panel.
- Saving a `.sproof` file automatically refreshes the goal panel.
- Solved/empty states are shown as `No open goals`.

### Diagnostics in Problems Panel

On save, the extension runs `sproof check --json` and translates structured diagnostics into VS Code Problems:

- Source range highlighting
- Expected vs actual type details (for type mismatch errors)
- Repair hints when available

## Installation

### From source

```bash
cd vscode-sproof
npm install
npm run compile
# Install vsce if needed: npm install -g @vscode/vsce
vsce package
code --install-extension vscode-sproof-0.2.0.vsix
```

### Development (F5)

Open this folder in VS Code and press `F5` to launch an Extension Development Host with the extension loaded.

## Example sproof file

```sproof
// Natural numbers
inductive Nat {
  case zero: Nat
  case succ(n: Nat): Nat
}

// Addition
def add(n: Nat, m: Nat): Nat = match n {
  case Nat.zero => m
  case Nat.succ k => Nat.succ(add(k, m))
}

// add(n, 0) = n
defspec add_zero(n: Nat): add(n, Nat.zero) = n program = {
  by induction n {
    case zero => trivial
    case succ k => simplify
  }
}
```

## License

MIT
