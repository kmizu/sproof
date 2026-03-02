# Proof Cookbook

This cookbook is a quick-start path for common proofs in about 10 minutes.

## 1. `induction`

When the goal depends on a recursive structure, split by constructors and use `ih` in recursive branches.

Broken (`sproof-fail`):

```sproof-fail
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

defspec plus_zero_right_bad(n: Nat): plus(n, Nat.zero) = n {
  by trivial
}
```

Fixed:

```sproof
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

defspec plus_zero_right(n: Nat): plus(n, Nat.zero) = n {
  by induction n {
    case zero      => trivial
    case succ k ih => simplify [ih]
  }
}
```

## 2. `simplify`

Use `simplify [ih]` to rewrite with induction hypotheses.

Broken (`sproof-fail`):

```sproof-fail
defspec succ_case_bad(n: Nat): Nat.succ(n) = Nat.succ(n) {
  by simplify [missing_ih]
}
```

Fixed:

```sproof
inductive Nat {
  case zero: Nat
  case succ(n: Nat): Nat
}

defspec succ_case_ok(n: Nat): Nat.succ(n) = Nat.succ(n) {
  by trivial
}
```

## 3. `have`

Use `have` to stage a local lemma before continuing.

```sproof
inductive Nat {
  case zero: Nat
  case succ(n: Nat): Nat
}

defspec have_demo(n: Nat): n = n {
  by have h : Nat = { Nat.zero } ; trivial
}
```

## 4. `calc`

Use `calc` for equational chains.

```sproof
inductive Nat {
  case zero: Nat
  case succ(n: Nat): Nat
}

defspec calc_demo(n: Nat): n = n {
  by calc {
    n = n { by trivial }
  }
}
```

## Onboarding checklist

1. Start with `trivial` goals (`x = x`).
2. Move to `induction` + `simplify [ih]`.
3. Introduce local helper facts with `have`.
4. Use `calc` once equalities get longer.
