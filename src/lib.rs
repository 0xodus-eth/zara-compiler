// Zara Compiler - A complete compiler implementation
// Covers all phases: Lexing, Parsing, Semantic Analysis, IR, Optimization, Code Generation

pub mod codegen;
pub mod error;
pub mod ir;
pub mod lexer;
pub mod optimizer;
pub mod parser;
pub mod semantic;
pub mod symbol_table;

pub use codegen::CodeGenerator;
pub use error::CompilerError;
pub use ir::IRGenerator;
pub use lexer::Lexer;
pub use optimizer::Optimizer;
pub use parser::{RecursiveDescentParser, ShiftReduceParser};
pub use semantic::SemanticAnalyzer;
pub use symbol_table::SymbolTable;
