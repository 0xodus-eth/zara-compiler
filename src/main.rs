// Zara Compiler - Main entry point
// A complete compiler for the Zara programming language

use std::env;
use std::fs;
use std::process;

mod codegen;
mod error;
mod ir;
mod lexer;
mod optimizer;
mod parser;
mod semantic;
mod symbol_table;

use codegen::{Architecture, CodeGenerator};
use ir::IRGenerator;
use lexer::Lexer;
use optimizer::Optimizer;
use parser::RecursiveDescentParser;
use semantic::SemanticAnalyzer;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Zara Compiler v0.1.0");
        println!("Usage: zara <source.zara> [options]");
        println!();
        println!("Options:");
        println!("  --tokens     Print tokens from lexical analysis");
        println!("  --ast        Print abstract syntax tree");
        println!("  --symbols    Print symbol table");
        println!("  --ir         Print intermediate representation (TAC)");
        println!("  --optimize   Apply optimizations");
        println!("  --asm        Generate assembly code");
        println!("  --arch=X     Target architecture (x86_64, arm64, riscv)");
        println!("  --help       Show this help message");
        println!();
        println!("Example: zara program.zara --ir --optimize --asm");
        process::exit(0);
    }

    let filename = &args[1];

    // Check for help
    if filename == "--help" || filename == "-h" {
        println!("Zara Compiler - A compiler for the Zara programming language");
        println!();
        println!("The Zara language supports:");
        println!("  - Data types: integer, float, string, boolean, array, stack");
        println!("  - Control flow: if-else, while, do-while, for");
        println!("  - Functions with parameters and return types");
        println!("  - Classes with fields, methods, and inheritance");
        println!("  - Built-in print and input statements");
        process::exit(0);
    }

    // Parse options
    let print_tokens = args.contains(&"--tokens".to_string());
    let print_ast = args.contains(&"--ast".to_string());
    let print_symbols = args.contains(&"--symbols".to_string());
    let print_ir = args.contains(&"--ir".to_string());
    let optimize = args.contains(&"--optimize".to_string());
    let gen_asm = args.contains(&"--asm".to_string());

    let arch = if args.iter().any(|a| a.contains("--arch=arm64")) {
        Architecture::ARM64
    } else if args.iter().any(|a| a.contains("--arch=riscv")) {
        Architecture::RISCV
    } else {
        Architecture::X86_64
    };

    // Read source file
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filename, e);
            process::exit(1);
        }
    };

    println!("=== Compiling: {} ===\n", filename);

    // Phase 1: Lexical Analysis
    println!("[Phase 1] Lexical Analysis...");
    let mut lexer = Lexer::new(&source);
    match lexer.tokenize() {
        Ok(tokens) => {
            println!("  ✓ Tokenization successful ({} tokens)", tokens.len());
            if print_tokens {
                lexer.print_tokens();
            }
        }
        Err(errors) => {
            println!("  ✗ Lexical errors found:");
            for error in errors {
                eprintln!("    {}", error);
            }
            // Continue with what we have for error recovery demonstration
        }
    }

    // Phase 2: Syntax Analysis (Top-Down)
    println!("[Phase 2] Syntax Analysis (Recursive Descent)...");
    let mut parser = RecursiveDescentParser::new(lexer.tokens().to_vec());
    let program = match parser.parse() {
        Ok(prog) => {
            println!(
                "  ✓ Parsing successful ({} statements)",
                prog.statements.len()
            );
            if print_ast {
                prog.print_ast();
            }
            prog
        }
        Err(errors) => {
            println!("  ✗ Syntax errors found:");
            for error in errors {
                eprintln!("    {}", error);
            }
            process::exit(1);
        }
    };

    // Phase 3: Semantic Analysis
    println!("[Phase 3] Semantic Analysis...");
    let mut analyzer = SemanticAnalyzer::new();
    match analyzer.analyze(&program) {
        Ok(()) => {
            println!("  ✓ Semantic analysis successful");
            if print_symbols {
                analyzer.dump_symbols();
            }
        }
        Err(errors) => {
            println!("  ✗ Semantic errors found:");
            for error in errors.errors() {
                eprintln!("    {}", error);
            }
            // Continue for demonstration
        }
    }

    // Phase 4: Intermediate Code Generation
    println!("[Phase 4] Intermediate Code Generation (TAC)...");
    let mut ir_gen = IRGenerator::new();
    let tac = ir_gen.generate(&program).to_vec();
    println!("  ✓ Generated {} TAC instructions", tac.len());

    if print_ir && !optimize {
        ir_gen.print_tac();
    }

    // Phase 5: Optimization
    let optimized_tac = if optimize {
        println!("[Phase 5] Code Optimization...");
        let mut optimizer = Optimizer::new();
        let optimized = optimizer.optimize(tac.clone());
        println!(
            "  ✓ Optimization complete ({} -> {} instructions)",
            tac.len(),
            optimized.len()
        );

        if print_ir {
            println!("\n=== Optimized TAC ===");
            for (i, instr) in optimized.iter().enumerate() {
                println!("{:4}: {}", i, instr);
            }
            println!("=====================\n");
        }

        optimizer.print_report();
        optimized
    } else {
        tac
    };

    // Phase 6: Code Generation
    if gen_asm {
        println!("[Phase 6] Code Generation ({:?})...", arch);
        let mut codegen = CodeGenerator::new(arch);
        let assembly = codegen.generate(&optimized_tac);
        println!("  ✓ Assembly code generated");

        // Write assembly to file
        let asm_filename = filename.replace(".zara", ".asm");
        match fs::write(&asm_filename, &assembly) {
            Ok(()) => println!("  ✓ Assembly written to {}", asm_filename),
            Err(e) => eprintln!("  ✗ Failed to write assembly: {}", e),
        }

        codegen.print_assembly();
    }

    println!("\n=== Compilation Complete ===");
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    fn compile(source: &str) -> bool {
        let mut lexer = Lexer::new(source);
        if lexer.tokenize().is_err() {
            return false;
        }

        let mut parser = RecursiveDescentParser::new(lexer.tokens().to_vec());
        let program = match parser.parse() {
            Ok(p) => p,
            Err(_) => return false,
        };

        let mut analyzer = SemanticAnalyzer::new();
        if analyzer.analyze(&program).is_err() {
            return false;
        }

        let mut ir_gen = IRGenerator::new();
        ir_gen.generate(&program);

        true
    }

    #[test]
    fn test_full_compilation() {
        let source = r#"
            // Variable declarations
            let x: integer = 10;
            let y: float = 3.14;
            let name: string = "Zara";
            
            // Function definition
            func add(a: integer, b: integer) -> integer {
                return a + b;
            }
            
            // Control structures
            let sum: integer = 0;
            for (let i: integer = 0; i < 10; i = i + 1) {
                sum = sum + i;
            }
            
            // Conditionals
            if (sum > 20) {
                print("Sum is large");
            } else {
                print("Sum is small");
            }
            
            // Function call
            let result: integer = add(5, 3);
            print(result);
        "#;

        assert!(compile(source));
    }

    #[test]
    fn test_class_compilation() {
        let source = r#"
            class Point {
                x: integer;
                y: integer;
                
                func move(dx: integer, dy: integer) {
                    this.x = this.x + dx;
                    this.y = this.y + dy;
                }
            }
            
            let p: Point = new Point();
        "#;

        assert!(compile(source));
    }

    #[test]
    fn test_array_and_stack() {
        let source = r#"
            let numbers: array[10] of integer = new array[10] of integer;
            let mystack: stack of integer = new stack of integer;
        "#;

        assert!(compile(source));
    }
}
