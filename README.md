# OCaml Logic Expression Calculator

*OC*aml *L*ogic Expression Calculator (OCL) is a command-line tool for parsing, evaluating, and analyzing logical expressions. It supports a variety of logical operators and provides features such as truth table generation and logic property detection.

## Features

- Expression parsing with multiple notations:
  - Literals: `true`, `false`
  - Negation: `~`, `not`, `!`
  - Conjunction: `&`, `and`, `&&`
  - Disjunction: `|`, `or`, `||`
  - Implication: `->`, `=>`
  - Biconditional: `<->`, `<=>`

- Expression evaluation with variable assignments
- Truth table generation
- Logic property detection:
  - Tautology checking
  - Contradiction checking

## Build & Run

### Dependencies
- OCaml (>= 5.2.1)
- Dune (>= 3.19)
- Menhir
- Alcotest 

### Build
```bash
dune build
```

### Run
```bash
dune install ; ocl
```

### Test
```bash
dune test
```

## Usage

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

## Project Structure

```
├── bin/              # Executable
│   └── main.ml       # REPL interface
├── lib/              # Library
│   ├── syntax.ml     # AST definitions
│   ├── eval.ml       # Evaluator
│   ├── parser.mly    # Menhir parser
│   └── lexer.mll     # OCamllex lexer
└── test/
    └── testl.ml      # Tests
```

## Logic Laws Tested

- Law of excluded middle: `p | ~p`
- Law of non-contradiction: `~(p & ~p)`
- De Morgan's laws: `~(p & q) <-> (~p | ~q)`
- Distributive law: `p & (q | r) <-> (p & q) | (p & r)`
- Associative law: `(p & q) & r <-> p & (q & r)`
- Contrapositive: `p -> q <-> ~q -> ~p`
- Material implication: `p -> q <-> ~p | q`
- Hypothetical syllogism: `(p -> q) & (q -> r) -> (p -> r)`
