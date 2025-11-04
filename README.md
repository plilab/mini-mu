<div align="center">
<img src="./docs/icon.png" width=50%/>

# Mini-Mu (MuLe) 
</div>

Mini-Mu is an experimental programming language designed to explore the dual $\mu\tilde{\mu}$-calculus.
It features continuation-based evaluation, pattern matching, and a unique approach to control flow that makes explicit the duality between terms and continuations.

## Overview

Mini-Mu implements a variant of the μμ̃-calculus, where:
- $\tilde{\mu}$ binds terms to variables
- $\mu$ binds continuations to co-variables

This duality provides a powerful foundation for extracting and expressing control-flow in a elegant and useful way.

## Features

- **Continuation-based evaluation** with explicit control flow
- **Pattern matching** on algebraic data types (Nat, List, Bool, etc.)
- **Module system** with imports and exports
- **AST visualization** and evaluation tree generation
- **Control-flow graph output** for program analysis
- **Comprehensive test suite** with standard library

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

## Standard Library

The standard library provides essential functions:

- **std_nat.mmu**: Natural number operations (add, lt, eq, inc, etc.)
- **std_list.mmu**: List operations (append, map, filter, length, etc.)  
- **std_bool.mmu**: Boolean operations and utilities

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

## Research Context

Mini-Mu serves as a testbed for exploring:
- Duality between terms and continuations
- Explict control-flow representations in functional languages
- Applications of sequent calculus in practical programming

## License

This project is licensed under the BSD-2-Clause License - see the [LICENSE](LICENSE) file for details.

## Authors

- Ding Feng
- Kyriel Abad  
- Michael D. Adams
