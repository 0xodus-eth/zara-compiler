# Zara Compiler

A compiler implementation for the Zara programming language, written in Rust.

## Features

We wrote it in phases as follows:

1. **Week 1: Symbol Table** - Manages variable declarations with types (integer, float, string, array, stack)
2. **Week 2: Lexical Analysis** - Tokenizes source code using pattern matching
3. **Week 3: Top-Down Parsing** - Recursive descent parser for syntax analysis
4. **Week 4: Bottom-Up Parsing** - Shift-reduce parser for expression evaluation
5. **Week 5: Semantic Analysis** - Type checking, scope rules, and error detection
6. **Week 6: Syntax-Directed Translation** - Translates constructs to intermediate form
7. **Week 7: Intermediate Representation** - Three-address code (TAC) generation
8. **Week 8: Code Optimization** - Constant folding, dead code elimination, etc.
9. **Week 9: Object-Oriented Code** - Classes, inheritance, and methods
10. **Week 10: Machine Code** - Assembly code generation (x86_64, ARM64, RISC-V)
11. **Week 11: Error Handling** - Robust error detection and recovery

## Building

```bash
cd compiler
cargo build --release
```

## Usage

```bash
# Basic compilation
cargo run -- examples/demo.zara

# With all phases shown
cargo run -- examples/demo.zara --tokens --ast --symbols --ir --optimize --asm

# Options:
#   --tokens     Print tokens from lexical analysis
#   --ast        Print abstract syntax tree
#   --symbols    Print symbol table
#   --ir         Print intermediate representation (TAC)
#   --optimize   Apply optimizations
#   --asm        Generate assembly code
#   --arch=X     Target architecture (x86_64, arm64, riscv)
```

## Zara Language Syntax

### Data Types

```zara
let x: integer = 42;
let y: float = 3.14;
let name: string = "Hello";
let flag: boolean = true;
let arr: array[10] of integer = new array[10] of integer;
let stk: stack of integer = new stack of integer;
```

### Control Structures

```zara
// If-else
if (condition) {
    // code
} else {
    // code
}

// While loop
while (condition) {
    // code
}

// Do-while loop
do {
    // code
} while (condition);

// For loop
for (let i: integer = 0; i < 10; i = i + 1) {
    // code
}
```

### Functions

```zara
func add(a: integer, b: integer) -> integer {
    return a + b;
}

func greet(name: string) -> void {
    print(name);
}
```

### Classes

```zara
class Point {
    x: integer;
    y: integer;

    constructor(initX: integer, initY: integer) {
        this.x = initX;
        this.y = initY;
    }

    func move(dx: integer, dy: integer) -> void {
        this.x = this.x + dx;
        this.y = this.y + dy;
    }
}

class ColoredPoint extends Point {
    color: string;
}
```

### I/O

```zara
print("Hello, World!");
input(variable);
```

## Running Tests

```bash
cargo test
```

## Project Structure

```
compiler/
├── Cargo.toml
├── src/
│   ├── main.rs              # Entry point
│   ├── lib.rs               # Library exports
│   ├── lexer/               # Lexical analysis (Week 2)
│   │   ├── mod.rs
│   │   └── token.rs
│   ├── parser/              # Syntax analysis (Week 3-4)
│   │   ├── mod.rs
│   │   ├── ast.rs
│   │   ├── recursive_descent.rs
│   │   └── shift_reduce.rs
│   ├── symbol_table/        # Symbol table (Week 1)
│   │   └── mod.rs
│   ├── semantic/            # Semantic analysis (Week 5)
│   │   ├── mod.rs
│   │   └── type_checker.rs
│   ├── ir/                  # Intermediate representation (Week 6-7)
│   │   ├── mod.rs
│   │   └── tac.rs
│   ├── optimizer/           # Code optimization (Week 8)
│   │   └── mod.rs
│   ├── codegen/             # Machine code generation (Week 10)
│   │   └── mod.rs
│   └── error/               # Error handling (Week 11)
│       └── mod.rs
└── examples/
    └── demo.zara            # Example program
```

## License

This project is part of my compiler construction unit course work and shouldn't be used in production as it may contain bugs
