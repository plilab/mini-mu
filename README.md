<div align="center">
<img src="./docs/icon.png" width=50%/>

# Mini-Mu (MuLe)
### An Experimental Programming Language Based on the Dual Calculus
</div>

Mini-Mu is an experimental programming language that explores the dual $\mu\tilde{\mu}$-calculus. It demonstrates the duality between terms and continuations, making continuations first-class CITIZEN in the language.

## Overview

Mini-Mu implements a variant of the $\mu\tilde{\mu}$-calculus (dual calculus), inspired by classical sequent calculus:
- **$\tilde{\mu}$ (mu-tilde)** - binds terms to variables, representing values and data flow
- **$\mu$ (mu)** - binds continuations to co-variables, representing control flow and contexts

This duality provides an elegant and powerful foundation for expressing complex control flow patterns like early returns, and imperative-style programming within a functional framework.

### Key Concepts

The language is based on the **dual calculus**, which extends the lambda calculus with first-class continuations:
- **Commands** (written `e . k`) represent computation steps pairing an expression `e` with a continuation `k`
- **Pattern matching** on algebraic data types (naturals, lists, booleans, pairs)
- **Explicit continuation passing** enables sophisticated control flow patterns
- **Dual syntax** that treats terms and continuations symmetrically

## Features

### Language Features
- **First-class continuations** via $\mu$ and $\tilde{\mu}$ abstractions
- **Pattern matching** on algebraic data types (Nat, List, Bool, Pair)
- **Nested and wildcard patterns** for flexible data deconstruction
- **Module system** with imports and exports for code organization
- **Standard library** with functional primitives (map, filter, fold, etc.)

### Development Tools
- **Interpreter** with step-by-step evaluation mode
- **AST visualization** to understand program structure
- **Evaluation tree generation** (text and SVG) for debugging
- **VS Code extension** with syntax highlighting
- **Comprehensive test suite** with 20+ example programs

### Example Programs
The `tests/` directory includes implementations of:
- Factorial, insertion sort, quicksort
- List operations (map, filter, reverse, append)
- Control flow patterns (early return, loops with break, coroutines)
- Imperative-style programming using continuations

## Installation & Setup

### Prerequisites
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- GHC (Glasgow Haskell Compiler) - should be installed automatically by Stack

### Building
```bash
git clone https://github.com/plilab/mini-mu.git
cd mini-mu
stack build
```

This will build the project and install all necessary dependencies.

## Usage

### Use Syntax Highlighting

```bash
cd vscode-extension

npm install

vsce build

code --install-extension mini-mu-language-support-0.0.1.vsix

```

### Running Programs

```bash
# Run a Mini-Mu program
./src/Main run ./tests/add.mmu

# Run with step-by-step evaluation
./src/Main run --step-by-step ./tests/quicksort.mmu

# View full environment during evaluation
./src/Main run --full-env ./tests/imperative_loop.mmu

# Record run as standard (saves AST and output)
./src/Main run --standard ./tests/add.mmu
```

### Visualization

```bash
# Visualize Abstract Syntax Tree
./src/Main viz ./tests/quicksort.mmu

# Generate evaluation tree as text
./src/Main tree ./tests/add.mmu

# Generate evaluation tree as SVG
./src/Main tree --output svg ./tests/quicksort.mmu

# Control tree depth
./src/Main tree --depth 5 --output svg ./tests/add.mmu
```

### Testing

```bash
# Run all test cases
./src/Main test-all

# Run tests and record as standard
./src/Main test-all --standard
```

## Language Syntax

Mini-Mu uses a unique syntax reflecting its dual calculus foundation:

TODO

## Standard Library

Located in `lib/`, the standard library provides essential functions:

- **std_nat.mmu**: Natural number operations (add, sub, mul, lt, eq, etc.)
- **std_list.mmu**: List operations (append, map, filter, length, reverse, etc.)  
- **std_bool.mmu**: Boolean operations and conditional utilities

Import them in your programs:
```haskell
import "std_nat"
import "std_list"

run add @ 2 1 halt
```

## Command Reference

### Main Commands
- `run [OPTIONS] PROGRAM_FILE` - Execute a Mini-Mu program
- `viz [OPTIONS] PROGRAM_FILE` - Visualize program AST
- `tree [OPTIONS] PROGRAM_FILE` - Generate evaluation tree
- `test-all [OPTIONS]` - Run all test cases

### Run Options
- `-e, --entry-point NAME` - Specify entry point (default: "main")
- `-s, --step-by-step` - Show step-by-step evaluation
- `-f, --full-env` - Display full evaluation environment
- `--standard` - Record run as standard (saves AST and output)

### Tree Options  
- `-d, --depth N` - Maximum tree depth (default: 10)
- `-o, --output FORMAT` - Output format: text or svg (default: "text")
- `-f, --full-env` - Show full evaluation environment

## Development

### Architecture
- **Parser.hs**: Parses Mini-Mu source code into AST
- **Syntax.hs**: Defines the core language AST
- **Eval.hs**: Implements the evaluation semantics
- **EvalTree.hs**: Constructs evaluation trees for analysis
- **Graph.hs**: Generates GraphViz visualizations
- **Pretty.hs**: Pretty-printing for ASTs and configurations
- **Module.hs**: Module system and import resolution

## Example: Factorial

Here's a simple factorial implementation demonstrating continuation patterns:

```haskell
import "std_nat" (mul, add)

fn factorial n k :=
    let rec = do f <- factorial n' then mul @ (S n') f k in
    n . { 0 -> 1 . k 
        | S n' -> rec };

run factorial @ 5 halt;
```

## Research Context

Mini-Mu serves as a research platform for exploring:
- **Duality between terms and continuations** based on classical sequent calculus
- **Applications of the dual calculus** to practical programming patterns

This work demonstrates how theoretical concepts from logic and type theory can inform practical programming language design.

## Contributing

Contributions are welcome! Areas of interest include:
- Language features and syntactic improvements
- Additional standard library functions
- Better error messages and debugging tools
- Performance optimizations, including compiler
- Documentation and examples

## License

This project is licensed under the BSD-2-Clause License - see the [LICENSE](LICENSE) file for details.

## Authors

- Ding Feng, National University of Singapore
- Kyriel Abad, National University of Singapore  
- Michael D. Adams, National University of Singapore

## References

For more information on the dual calculus and $\mu\tilde{\mu}$-calculus:
- Curien, P.-L., & Herbelin, H. (2000). *The duality of computation*. ICFP.
- Wadler, P. (2003). *Call-by-value is dual to call-by-name*. ICFP.

---

**Note**: This is an experimental research language. It is not intended for production use but as a platform for exploring programming language theory and continuation-based control flow.
