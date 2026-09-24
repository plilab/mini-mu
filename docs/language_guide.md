# MiniMu Language Guide

MiniMu is a language based on the duality between **terms** (producers) and **continuations** (consumers). 
Computation is driven by the interaction ("cut") between a producer and a consumer.

## 1. Program Structure
A program consists of imports, declarations, and optional exports.
```haskell
import "std_list" (map, filter)

fn add x y k :=
  match x with
    Z -> y . k
  | S x' -> add @ x' (S y) k;

run main;
```

## 2. Declarations
| Syntax | Description | Desugars to |
| :--- | :--- | :--- |
| `fn f x k := cmd` | Function definition | `f := { (x, k) -> cmd }` |
| `run cmd` | Entry point | `main := { halt -> cmd }` |
| `x := e` | Global binding | N/A |
| `module M := ... end` | Object/Module definition | See *Objects* section |

## 3. Commands (Control Flow)
Commands represent the active computation step (a "cut").

| Syntax | Description | Core Semantics |
| :--- | :--- | :--- |
| `e . k` | **Cut** | `e . k` |
| `let x = e in cmd` | Bind value `e` to `x` | `e . { x -> cmd }` |
| `letcc k = e in cmd` | Bind consumer `e` to `k` | `{ k -> cmd } . e` |
| `match e with p -> cmd` | Pattern match on value | `e . { p -> cmd }` |
| `patch k with p -> cmd` | Pattern match on continuation | `{ p -> cmd } . k` |
| `do x <- e1, y <- e2 then cmd` | CPS sequencing | `e1 . { x -> e2 . { y -> cmd } }` |
| `f @ a b` | Push args to function | `f . (a, b)` |
| `a b @ k` | Push args to continuation | `(a, b) . k` |
| `yield e` | Yield value to current continuation (currently used in OOP only but might be extended to general case) | `e . _k` (implicit `_k`) |

## 4. Expressions (Values & Producers)
Expressions produce values.

| Syntax | Description |
| :--- | :--- |
| `x`, `42`, `(a, b)`, `[1, 2]` | Variables, Literals, Tuples, Lists |
| `Foo e1 e2` | Constructor application |
| `{ p1 -> cmd1 ... }` | Mu-abstraction |
| `f(x, y)` | Function application (sugar for `{ k -> f @ x y k }`) |
| `f{k1}(x, y, k1)` | Application with explicit continuations |
| `< cmd >` | **Delimited Control**: Capture continuation up to `< >` |
| `have x = e in body` | Substitution (macro-like replacement) |

## 5. Core Concepts

### Functions
Functions are simply consumers that expect a tuple of arguments and return sites.
*   Definition: `fn f x k := f = { (x, k) -> x . k }`
*   Call: `f(42, halt)`, halt with result 42

### Objects & Modules
Modules are objects defined by fields and methods (currently stateless).
```haskell
module Counter :=
  field count = 0
  Inc() -> yield inc(count)
end
```
*   **Desugaring**: An object is a consumer that matches on method constructors.
```haskell
Counter := {
  Inc _k ->
    (0) . { (count) -> inc(count) . _k }
}
```
*   **Method Call**: `obj::Inc(return_k)` sends the message `Inc(return_k)` to `obj`.

### Delimited Control
*   `< cmd >`: Evaluates `cmd` in a delimited context.
*   If `cmd` performs a `Return e`, the value `e` is returned as the result of the `< ... >` expression.
*   This allows implementing advanced control flow like generators or coroutines.

## 6. Pattern Matching
Patterns can be nested and include:
*   **Wildcard**: `_`
*   **Variables**: `x`, `y`
*   **Constructors**: `Cons p1 p2`, `(p1, p2)`, `42`

Example:
```haskell
match tup with
| (0, _) -> 0 . k
| _ -> 1 . k
```
## 7. Example
```haskell
import "std_list" (list_length, list_contains)
import "std_nat" (eq)

fn format_input_aux n acc k :=
  match n with
    Z -> acc . k
  | S n' -> format_input_aux @ n' (List:: (S n') acc) k;

fn format_input n k :=
  format_input_aux @ n Nil k;

fn perm_aux xs acc res n k :=
  do 
    len <- list_length(acc),
    is_full <- eq(len, n)
  then 
  match (is_full, xs) with 
    (True, _) -> List:: acc res . k
  | (_, Nil) -> res . k
  | (_, List:: x xs') ->
    let contain = list_contains(acc, x) in
    match contain with
      True -> perm_aux(xs', acc, res, n) . k
    | _ ->
      do
        full_list <- format_input(n),
        res' <- perm_aux(full_list, (List:: x acc), res, n)
      then perm_aux(xs', acc, res', n) . k;

fn permutation n k :=
  let xs = format_input(n)
  in perm_aux(xs, Nil, Nil, n) . k;

run permutation(4) . halt;

```

## 8. Internals: Delimited Control Mechanism

The delimited control operator `< cmd >` is implemented via a **Shift/Reset**-like mechanism in the evaluator (`Eval.hs`).

1.  **Delimiter Creation (`reset`)**:
    When the evaluator encounters `< cmd >` (a `DelimExpr`), it captures the current continuation (the "rest of the computation") and stores it in a `OneHoleContext`. This effectively sets a boundary (a "reset" point).

2.  **Isolation**:
    The inner command `cmd` is executed within a `DelimConfig` wrapper. This isolates the inner computation from the outer context.

3.  **Completion (`return`)**:
    The inner computation runs until it produces a value to a special continuation named `Return` (represented as `ConsValue "Return" []`).
    *   This corresponds to the `return e` command (or `yield e` if `_k` is bound to `Return`).
    *   When `e . Return` occurs inside the delimiter, the evaluator intercepts it.

4.  **Restoration**:
    The value `e` produced by the inner command is then "plugged" into the hole of the captured `OneHoleContext`. The outer computation resumes with this value, effectively treating `< cmd >` as an expression that evaluated to `e`.

This mechanism allows the program to manipulate control flow by capturing and restoring contexts, enabling features like coroutines, generators, or effect handlers.
