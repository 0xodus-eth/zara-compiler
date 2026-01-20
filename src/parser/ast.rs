// Abstract Syntax Tree definitions for Zara
// Supports expressions, statements, control structures, functions, and classes

use crate::symbol_table::DataType;

/// Source location for AST nodes
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl Span {
    pub fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }

    pub fn from_line(line: usize, col: usize) -> Self {
        Self::new(line, col, line, col)
    }
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,          // +
    Subtract,     // -
    Multiply,     // *
    Divide,       // /
    Modulo,       // %
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=
    And,          // &&
    Or,           // ||
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::Modulo => write!(f, "%"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::LessEqual => write!(f, "<="),
            BinaryOp::GreaterEqual => write!(f, ">="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,    // -
    Not,       // !
    Increment, // ++
    Decrement, // --
}

/// Assignment operators
#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Assign,    // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
}

/// Expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Null,

    // Variables and access
    Identifier(String),
    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },

    // Operations
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    // Function call
    Call {
        callee: String,
        args: Vec<Expr>,
    },

    // Object creation
    NewObject {
        class_name: String,
        args: Vec<Expr>,
    },

    // Array/Stack creation
    NewArray {
        element_type: DataType,
        size: Box<Expr>,
    },
    NewStack {
        element_type: DataType,
    },

    // Stack operations
    StackPush {
        stack: Box<Expr>,
        value: Box<Expr>,
    },
    StackPop {
        stack: Box<Expr>,
    },
    StackPeek {
        stack: Box<Expr>,
    },

    // This reference
    This,

    // Grouped expression
    Grouped(Box<Expr>),
}

/// Type annotation in source code
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation {
    pub base_type: String,
    pub is_array: bool,
    pub array_size: Option<usize>,
    pub is_stack: bool,
}

impl TypeAnnotation {
    pub fn simple(name: impl Into<String>) -> Self {
        Self {
            base_type: name.into(),
            is_array: false,
            array_size: None,
            is_stack: false,
        }
    }

    pub fn array(element_type: impl Into<String>, size: Option<usize>) -> Self {
        Self {
            base_type: element_type.into(),
            is_array: true,
            array_size: size,
            is_stack: false,
        }
    }

    pub fn stack(element_type: impl Into<String>) -> Self {
        Self {
            base_type: element_type.into(),
            is_array: false,
            array_size: None,
            is_stack: true,
        }
    }

    pub fn to_data_type(&self) -> DataType {
        let base = match self.base_type.as_str() {
            "integer" => DataType::Integer,
            "float" => DataType::Float,
            "string" => DataType::String,
            "boolean" => DataType::Boolean,
            "void" => DataType::Void,
            name => DataType::Class(name.to_string()),
        };

        if self.is_array {
            DataType::Array(Box::new(base), self.array_size)
        } else if self.is_stack {
            DataType::Stack(Box::new(base))
        } else {
            base
        }
    }
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: TypeAnnotation,
}

/// Statements
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    // Variable declaration
    VarDecl {
        name: String,
        type_annotation: Option<TypeAnnotation>,
        initializer: Option<Expr>,
        is_const: bool,
    },

    // Assignment
    Assignment {
        target: Expr,
        op: AssignOp,
        value: Expr,
    },

    // Expression statement
    Expression(Expr),

    // Control flow
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
    },
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        update: Option<Box<Stmt>>,
        body: Box<Stmt>,
    },

    // Jump statements
    Return(Option<Expr>),
    Break,
    Continue,

    // Block
    Block(Vec<Stmt>),

    // Print and Input
    Print(Vec<Expr>),
    Input(String), // variable name to store input

    // Function declaration
    FuncDecl {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        body: Box<Stmt>,
    },

    // Class declaration (Week 9: OOP)
    ClassDecl {
        name: String,
        parent: Option<String>,
        members: Vec<ClassMember>,
    },

    // Empty statement
    Empty,
}

/// Class member (field or method)
#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Field {
        name: String,
        type_annotation: TypeAnnotation,
        is_static: bool,
        is_public: bool,
        initializer: Option<Expr>,
    },
    Method {
        name: String,
        params: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        body: Stmt,
        is_static: bool,
        is_public: bool,
    },
    Constructor {
        params: Vec<Parameter>,
        body: Stmt,
    },
}

/// A complete Zara program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }

    /// Pretty print the AST
    pub fn print_ast(&self) {
        println!("\n=== Abstract Syntax Tree ===");
        for (i, stmt) in self.statements.iter().enumerate() {
            println!("Statement {}:", i + 1);
            print_stmt(stmt, 1);
        }
        println!("============================\n");
    }
}

fn print_stmt(stmt: &Stmt, indent: usize) {
    let prefix = "  ".repeat(indent);
    match stmt {
        Stmt::VarDecl {
            name,
            type_annotation,
            initializer,
            is_const,
        } => {
            let kw = if *is_const { "const" } else { "let" };
            let type_str = type_annotation
                .as_ref()
                .map(|t| format!(": {}", t.base_type))
                .unwrap_or_default();
            println!("{}{} {}{}", prefix, kw, name, type_str);
            if let Some(init) = initializer {
                print_expr(init, indent + 1);
            }
        }
        Stmt::Assignment { target, op, value } => {
            println!("{}Assignment {:?}", prefix, op);
            print_expr(target, indent + 1);
            print_expr(value, indent + 1);
        }
        Stmt::Expression(expr) => {
            println!("{}Expression:", prefix);
            print_expr(expr, indent + 1);
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            println!("{}If:", prefix);
            println!("{}  Condition:", prefix);
            print_expr(condition, indent + 2);
            println!("{}  Then:", prefix);
            print_stmt(then_branch, indent + 2);
            if let Some(else_stmt) = else_branch {
                println!("{}  Else:", prefix);
                print_stmt(else_stmt, indent + 2);
            }
        }
        Stmt::While { condition, body } => {
            println!("{}While:", prefix);
            print_expr(condition, indent + 1);
            print_stmt(body, indent + 1);
        }
        Stmt::DoWhile { body, condition } => {
            println!("{}Do-While:", prefix);
            print_stmt(body, indent + 1);
            print_expr(condition, indent + 1);
        }
        Stmt::For {
            init,
            condition,
            update,
            body,
        } => {
            println!("{}For:", prefix);
            if let Some(i) = init {
                println!("{}  Init:", prefix);
                print_stmt(i, indent + 2);
            }
            if let Some(c) = condition {
                println!("{}  Condition:", prefix);
                print_expr(c, indent + 2);
            }
            if let Some(u) = update {
                println!("{}  Update:", prefix);
                print_stmt(u, indent + 2);
            }
            println!("{}  Body:", prefix);
            print_stmt(body, indent + 2);
        }
        Stmt::Return(expr) => {
            println!("{}Return:", prefix);
            if let Some(e) = expr {
                print_expr(e, indent + 1);
            }
        }
        Stmt::Break => println!("{}Break", prefix),
        Stmt::Continue => println!("{}Continue", prefix),
        Stmt::Block(stmts) => {
            println!("{}Block:", prefix);
            for s in stmts {
                print_stmt(s, indent + 1);
            }
        }
        Stmt::Print(exprs) => {
            println!("{}Print:", prefix);
            for e in exprs {
                print_expr(e, indent + 1);
            }
        }
        Stmt::Input(var) => {
            println!("{}Input -> {}", prefix, var);
        }
        Stmt::FuncDecl {
            name,
            params,
            return_type,
            body,
        } => {
            let ret = return_type
                .as_ref()
                .map(|t| format!(" -> {}", t.base_type))
                .unwrap_or_default();
            let params_str: Vec<String> = params
                .iter()
                .map(|p| format!("{}: {}", p.name, p.type_annotation.base_type))
                .collect();
            println!(
                "{}Function {}({}){}",
                prefix,
                name,
                params_str.join(", "),
                ret
            );
            print_stmt(body, indent + 1);
        }
        Stmt::ClassDecl {
            name,
            parent,
            members,
        } => {
            let extends = parent
                .as_ref()
                .map(|p| format!(" extends {}", p))
                .unwrap_or_default();
            println!("{}Class {}{}", prefix, name, extends);
            for member in members {
                match member {
                    ClassMember::Field {
                        name,
                        type_annotation,
                        ..
                    } => {
                        println!("{}  field {}: {}", prefix, name, type_annotation.base_type);
                    }
                    ClassMember::Method { name, params, .. } => {
                        println!("{}  method {}(...)", prefix, name);
                        let _ = params; // suppress warning
                    }
                    ClassMember::Constructor { .. } => {
                        println!("{}  constructor(...)", prefix);
                    }
                }
            }
        }
        Stmt::Empty => println!("{}(empty)", prefix),
    }
}

fn print_expr(expr: &Expr, indent: usize) {
    let prefix = "  ".repeat(indent);
    match expr {
        Expr::IntegerLiteral(n) => println!("{}Int({})", prefix, n),
        Expr::FloatLiteral(n) => println!("{}Float({})", prefix, n),
        Expr::StringLiteral(s) => println!("{}String(\"{}\")", prefix, s),
        Expr::BoolLiteral(b) => println!("{}Bool({})", prefix, b),
        Expr::Null => println!("{}Null", prefix),
        Expr::Identifier(name) => println!("{}Ident({})", prefix, name),
        Expr::ArrayAccess { array, index } => {
            println!("{}ArrayAccess:", prefix);
            print_expr(array, indent + 1);
            print_expr(index, indent + 1);
        }
        Expr::FieldAccess { object, field } => {
            println!("{}FieldAccess .{}:", prefix, field);
            print_expr(object, indent + 1);
        }
        Expr::MethodCall {
            object,
            method,
            args,
        } => {
            println!("{}MethodCall .{}():", prefix, method);
            print_expr(object, indent + 1);
            for arg in args {
                print_expr(arg, indent + 1);
            }
        }
        Expr::Binary { left, op, right } => {
            println!("{}Binary({}):", prefix, op);
            print_expr(left, indent + 1);
            print_expr(right, indent + 1);
        }
        Expr::Unary { op, operand } => {
            println!("{}Unary({:?}):", prefix, op);
            print_expr(operand, indent + 1);
        }
        Expr::Call { callee, args } => {
            println!("{}Call {}():", prefix, callee);
            for arg in args {
                print_expr(arg, indent + 1);
            }
        }
        Expr::NewObject { class_name, args } => {
            println!("{}New {}():", prefix, class_name);
            for arg in args {
                print_expr(arg, indent + 1);
            }
        }
        Expr::NewArray { element_type, size } => {
            println!("{}NewArray[{}]:", prefix, element_type);
            print_expr(size, indent + 1);
        }
        Expr::NewStack { element_type } => {
            println!("{}NewStack<{}>", prefix, element_type);
        }
        Expr::StackPush { stack, value } => {
            println!("{}StackPush:", prefix);
            print_expr(stack, indent + 1);
            print_expr(value, indent + 1);
        }
        Expr::StackPop { stack } => {
            println!("{}StackPop:", prefix);
            print_expr(stack, indent + 1);
        }
        Expr::StackPeek { stack } => {
            println!("{}StackPeek:", prefix);
            print_expr(stack, indent + 1);
        }
        Expr::This => println!("{}This", prefix),
        Expr::Grouped(inner) => {
            println!("{}Grouped:", prefix);
            print_expr(inner, indent + 1);
        }
    }
}
