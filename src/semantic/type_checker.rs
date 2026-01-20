// Week 5: Semantic Analyzer - Type Checker
// Checks type consistency, scope rules, and function/method usage

use crate::error::{CompilerError, ErrorCollector, SourceLocation};
use crate::parser::ast::*;
use crate::symbol_table::{DataType, Symbol, SymbolKind, SymbolTable};

/// Semantic analyzer for Zara
pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: ErrorCollector,
    current_function: Option<String>,
    current_class: Option<String>,
    in_loop: bool,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: ErrorCollector::new(),
            current_function: None,
            current_class: None,
            in_loop: false,
        }
    }

    /// Analyze a complete program
    pub fn analyze(&mut self, program: &Program) -> Result<(), &ErrorCollector> {
        // First pass: collect all class and function declarations
        for stmt in &program.statements {
            self.collect_declarations(stmt);
        }

        // Second pass: analyze all statements
        for stmt in &program.statements {
            self.analyze_stmt(stmt);
        }

        if self.errors.has_errors() {
            Err(&self.errors)
        } else {
            Ok(())
        }
    }

    /// Collect top-level declarations
    fn collect_declarations(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FuncDecl {
                name,
                params,
                return_type,
                ..
            } => {
                let param_types: Vec<DataType> = params
                    .iter()
                    .map(|p| p.type_annotation.to_data_type())
                    .collect();
                let ret_type = return_type
                    .as_ref()
                    .map(|t| t.to_data_type())
                    .unwrap_or(DataType::Void);

                if let Err(e) =
                    self.symbol_table
                        .add_function(name.clone(), param_types, ret_type, 0)
                {
                    self.errors.add_error(CompilerError::semantic(e, None));
                }
            }
            Stmt::ClassDecl { name, .. } => {
                if let Err(e) = self.symbol_table.add_class(name.clone(), 0) {
                    self.errors.add_error(CompilerError::semantic(e, None));
                }
            }
            _ => {}
        }
    }

    /// Analyze a statement
    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl {
                name,
                type_annotation,
                initializer,
                is_const,
            } => {
                // Determine the type
                let declared_type = type_annotation.as_ref().map(|t| t.to_data_type());
                let init_type = initializer.as_ref().map(|e| self.analyze_expr(e));

                let final_type = match (&declared_type, &init_type) {
                    (Some(dt), Some(it)) => {
                        if !self.types_compatible(dt, it) {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Type mismatch: cannot assign {} to {}", it, dt),
                                None,
                            ));
                        }
                        dt.clone()
                    }
                    (Some(dt), None) => {
                        if *is_const {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Constant '{}' must be initialized", name),
                                None,
                            ));
                        }
                        dt.clone()
                    }
                    (None, Some(it)) => it.clone(),
                    (None, None) => {
                        self.errors.add_error(CompilerError::semantic(
                            format!("Cannot determine type for '{}'", name),
                            None,
                        ));
                        DataType::Unknown
                    }
                };

                if let Err(e) = self.symbol_table.add_variable(name.clone(), final_type, 0) {
                    self.errors.add_error(CompilerError::semantic(e, None));
                } else if initializer.is_some() {
                    let _ = self.symbol_table.update(name, None, Some(true));
                }
            }

            Stmt::Assignment {
                target,
                op: _,
                value,
            } => {
                let target_type = self.analyze_expr(target);
                let value_type = self.analyze_expr(value);

                // Check if target is assignable
                if let Expr::Identifier(name) = target {
                    if let Some(symbol) = self.symbol_table.lookup(name) {
                        if !symbol.is_mutable {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Cannot assign to constant '{}'", name),
                                None,
                            ));
                        }
                    }
                }

                if !self.types_compatible(&target_type, &value_type) {
                    self.errors.add_error(CompilerError::semantic(
                        format!(
                            "Type mismatch in assignment: cannot assign {} to {}",
                            value_type, target_type
                        ),
                        None,
                    ));
                }
            }

            Stmt::Expression(expr) => {
                self.analyze_expr(expr);
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.analyze_expr(condition);
                if cond_type != DataType::Boolean && cond_type != DataType::Unknown {
                    self.errors.add_error(CompilerError::semantic(
                        format!("Condition must be boolean, found {}", cond_type),
                        None,
                    ));
                }

                self.symbol_table.enter_scope("if_then");
                self.analyze_stmt(then_branch);
                self.symbol_table.exit_scope();

                if let Some(else_stmt) = else_branch {
                    self.symbol_table.enter_scope("if_else");
                    self.analyze_stmt(else_stmt);
                    self.symbol_table.exit_scope();
                }
            }

            Stmt::While { condition, body } => {
                let cond_type = self.analyze_expr(condition);
                if cond_type != DataType::Boolean && cond_type != DataType::Unknown {
                    self.errors.add_error(CompilerError::semantic(
                        format!("While condition must be boolean, found {}", cond_type),
                        None,
                    ));
                }

                let was_in_loop = self.in_loop;
                self.in_loop = true;
                self.symbol_table.enter_scope("while");
                self.analyze_stmt(body);
                self.symbol_table.exit_scope();
                self.in_loop = was_in_loop;
            }

            Stmt::DoWhile { body, condition } => {
                let was_in_loop = self.in_loop;
                self.in_loop = true;
                self.symbol_table.enter_scope("do_while");
                self.analyze_stmt(body);
                self.symbol_table.exit_scope();
                self.in_loop = was_in_loop;

                let cond_type = self.analyze_expr(condition);
                if cond_type != DataType::Boolean && cond_type != DataType::Unknown {
                    self.errors.add_error(CompilerError::semantic(
                        format!("Do-while condition must be boolean, found {}", cond_type),
                        None,
                    ));
                }
            }

            Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                self.symbol_table.enter_scope("for");

                if let Some(init_stmt) = init {
                    self.analyze_stmt(init_stmt);
                }

                if let Some(cond) = condition {
                    let cond_type = self.analyze_expr(cond);
                    if cond_type != DataType::Boolean && cond_type != DataType::Unknown {
                        self.errors.add_error(CompilerError::semantic(
                            format!("For condition must be boolean, found {}", cond_type),
                            None,
                        ));
                    }
                }

                if let Some(update_stmt) = update {
                    self.analyze_stmt(update_stmt);
                }

                let was_in_loop = self.in_loop;
                self.in_loop = true;
                self.analyze_stmt(body);
                self.in_loop = was_in_loop;

                self.symbol_table.exit_scope();
            }

            Stmt::Return(value) => {
                if self.current_function.is_none() {
                    self.errors.add_error(CompilerError::semantic(
                        "Return statement outside of function",
                        None,
                    ));
                }

                if let Some(expr) = value {
                    let _ = self.analyze_expr(expr);
                    // TODO: Check return type matches function signature
                }
            }

            Stmt::Break => {
                if !self.in_loop {
                    self.errors.add_error(CompilerError::semantic(
                        "Break statement outside of loop",
                        None,
                    ));
                }
            }

            Stmt::Continue => {
                if !self.in_loop {
                    self.errors.add_error(CompilerError::semantic(
                        "Continue statement outside of loop",
                        None,
                    ));
                }
            }

            Stmt::Block(statements) => {
                self.symbol_table.enter_scope("block");
                for s in statements {
                    self.analyze_stmt(s);
                }
                self.symbol_table.exit_scope();
            }

            Stmt::Print(exprs) => {
                for expr in exprs {
                    self.analyze_expr(expr);
                }
            }

            Stmt::Input(var_name) => {
                if self.symbol_table.lookup(var_name).is_none() {
                    self.errors.add_error(CompilerError::semantic(
                        format!("Undefined variable '{}' in input", var_name),
                        None,
                    ));
                }
            }

            Stmt::FuncDecl {
                name,
                params,
                return_type,
                body,
            } => {
                let prev_function = self.current_function.clone();
                self.current_function = Some(name.clone());

                self.symbol_table.enter_scope(format!("func_{}", name));

                // Add parameters to scope
                for param in params {
                    let param_type = param.type_annotation.to_data_type();
                    let symbol = Symbol::new(
                        param.name.clone(),
                        param_type,
                        SymbolKind::Parameter,
                        self.symbol_table.current_scope_level(),
                        0,
                    )
                    .with_initialized(true);

                    if let Err(e) = self.symbol_table.add(symbol) {
                        self.errors.add_error(CompilerError::semantic(e, None));
                    }
                }

                self.analyze_stmt(body);

                // TODO: Check all code paths return a value if return type is not void
                let _ = return_type;

                self.symbol_table.exit_scope();
                self.current_function = prev_function;
            }

            Stmt::ClassDecl {
                name,
                parent,
                members,
            } => {
                let prev_class = self.current_class.clone();
                self.current_class = Some(name.clone());

                // Check parent class exists
                if let Some(parent_name) = parent {
                    if self.symbol_table.lookup(parent_name).is_none() {
                        self.errors.add_error(CompilerError::semantic(
                            format!("Parent class '{}' not found", parent_name),
                            None,
                        ));
                    }
                }

                self.symbol_table.enter_scope(format!("class_{}", name));

                for member in members {
                    match member {
                        ClassMember::Field {
                            name: field_name,
                            type_annotation,
                            initializer,
                            ..
                        } => {
                            let field_type = type_annotation.to_data_type();
                            let symbol = Symbol::new(
                                field_name.clone(),
                                field_type.clone(),
                                SymbolKind::Field,
                                self.symbol_table.current_scope_level(),
                                0,
                            );

                            if let Err(e) = self.symbol_table.add(symbol) {
                                self.errors.add_error(CompilerError::semantic(e, None));
                            }

                            if let Some(init) = initializer {
                                let init_type = self.analyze_expr(init);
                                if !self.types_compatible(&field_type, &init_type) {
                                    self.errors.add_error(CompilerError::semantic(
                                        format!(
                                            "Field '{}' type mismatch: expected {}, found {}",
                                            field_name, field_type, init_type
                                        ),
                                        None,
                                    ));
                                }
                            }
                        }
                        ClassMember::Method {
                            name: method_name,
                            params,
                            return_type,
                            body,
                            ..
                        } => {
                            let prev_function = self.current_function.clone();
                            self.current_function = Some(method_name.clone());

                            self.symbol_table
                                .enter_scope(format!("method_{}", method_name));

                            // Add 'this' to scope
                            let this_type = DataType::Class(name.clone());
                            let this_symbol = Symbol::new(
                                "this",
                                this_type,
                                SymbolKind::Variable,
                                self.symbol_table.current_scope_level(),
                                0,
                            )
                            .with_initialized(true);
                            let _ = self.symbol_table.add(this_symbol);

                            // Add parameters
                            for param in params {
                                let param_type = param.type_annotation.to_data_type();
                                let symbol = Symbol::new(
                                    param.name.clone(),
                                    param_type,
                                    SymbolKind::Parameter,
                                    self.symbol_table.current_scope_level(),
                                    0,
                                )
                                .with_initialized(true);

                                if let Err(e) = self.symbol_table.add(symbol) {
                                    self.errors.add_error(CompilerError::semantic(e, None));
                                }
                            }

                            self.analyze_stmt(body);
                            let _ = return_type;

                            self.symbol_table.exit_scope();
                            self.current_function = prev_function;
                        }
                        ClassMember::Constructor { params, body } => {
                            self.symbol_table.enter_scope("constructor");

                            for param in params {
                                let param_type = param.type_annotation.to_data_type();
                                let symbol = Symbol::new(
                                    param.name.clone(),
                                    param_type,
                                    SymbolKind::Parameter,
                                    self.symbol_table.current_scope_level(),
                                    0,
                                )
                                .with_initialized(true);

                                if let Err(e) = self.symbol_table.add(symbol) {
                                    self.errors.add_error(CompilerError::semantic(e, None));
                                }
                            }

                            self.analyze_stmt(body);

                            self.symbol_table.exit_scope();
                        }
                    }
                }

                self.symbol_table.exit_scope();
                self.current_class = prev_class;
            }

            Stmt::Empty => {}
        }
    }

    /// Analyze an expression and return its type
    fn analyze_expr(&mut self, expr: &Expr) -> DataType {
        match expr {
            Expr::IntegerLiteral(_) => DataType::Integer,
            Expr::FloatLiteral(_) => DataType::Float,
            Expr::StringLiteral(_) => DataType::String,
            Expr::BoolLiteral(_) => DataType::Boolean,
            Expr::Null => DataType::Unknown,

            Expr::Identifier(name) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    if !symbol.is_initialized {
                        self.errors.add_error(
                            CompilerError::semantic(
                                format!("Variable '{}' used before initialization", name),
                                None,
                            )
                            .with_hint("Initialize the variable before use"),
                        );
                    }
                    symbol.data_type.clone()
                } else {
                    self.errors.add_error(CompilerError::semantic(
                        format!("Undefined variable '{}'", name),
                        None,
                    ));
                    DataType::Unknown
                }
            }

            Expr::ArrayAccess { array, index } => {
                let array_type = self.analyze_expr(array);
                let index_type = self.analyze_expr(index);

                if index_type != DataType::Integer && index_type != DataType::Unknown {
                    self.errors.add_error(CompilerError::semantic(
                        format!("Array index must be integer, found {}", index_type),
                        None,
                    ));
                }

                match array_type {
                    DataType::Array(element_type, _) => *element_type,
                    DataType::Unknown => DataType::Unknown,
                    _ => {
                        self.errors.add_error(CompilerError::semantic(
                            format!("Cannot index non-array type {}", array_type),
                            None,
                        ));
                        DataType::Unknown
                    }
                }
            }

            Expr::FieldAccess { object, field } => {
                let obj_type = self.analyze_expr(object);

                match obj_type {
                    DataType::Class(class_name) => {
                        // TODO: Look up field in class definition
                        let _ = (class_name, field);
                        DataType::Unknown
                    }
                    DataType::Unknown => DataType::Unknown,
                    _ => {
                        self.errors.add_error(CompilerError::semantic(
                            format!(
                                "Cannot access field '{}' on non-object type {}",
                                field, obj_type
                            ),
                            None,
                        ));
                        DataType::Unknown
                    }
                }
            }

            Expr::MethodCall {
                object,
                method,
                args,
            } => {
                let obj_type = self.analyze_expr(object);

                // Analyze arguments
                for arg in args {
                    self.analyze_expr(arg);
                }

                match obj_type {
                    DataType::Class(_) => {
                        // TODO: Look up method and check argument types
                        let _ = method;
                        DataType::Unknown
                    }
                    DataType::Stack(element_type) => {
                        // Built-in stack methods
                        match method.as_str() {
                            "push" => {
                                if args.len() != 1 {
                                    self.errors.add_error(CompilerError::semantic(
                                        "Stack push requires exactly one argument",
                                        None,
                                    ));
                                } else {
                                    let arg_type = self.analyze_expr(&args[0]);
                                    if !self.types_compatible(&element_type, &arg_type) {
                                        self.errors.add_error(CompilerError::semantic(
                                            format!(
                                                "Stack push type mismatch: expected {}, found {}",
                                                element_type, arg_type
                                            ),
                                            None,
                                        ));
                                    }
                                }
                                DataType::Void
                            }
                            "pop" | "peek" => *element_type,
                            "isEmpty" => DataType::Boolean,
                            "size" => DataType::Integer,
                            _ => {
                                self.errors.add_error(CompilerError::semantic(
                                    format!("Unknown stack method '{}'", method),
                                    None,
                                ));
                                DataType::Unknown
                            }
                        }
                    }
                    DataType::Array(element_type, _) => match method.as_str() {
                        "length" => DataType::Integer,
                        "get" => *element_type,
                        _ => {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Unknown array method '{}'", method),
                                None,
                            ));
                            DataType::Unknown
                        }
                    },
                    DataType::Unknown => DataType::Unknown,
                    _ => {
                        self.errors.add_error(CompilerError::semantic(
                            format!("Cannot call method '{}' on type {}", method, obj_type),
                            None,
                        ));
                        DataType::Unknown
                    }
                }
            }

            Expr::Binary { left, op, right } => {
                let left_type = self.analyze_expr(left);
                let right_type = self.analyze_expr(right);

                match op {
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Modulo => {
                        // String concatenation
                        if *op == BinaryOp::Add
                            && (left_type == DataType::String || right_type == DataType::String)
                        {
                            return DataType::String;
                        }

                        // Numeric operations
                        if !self.is_numeric(&left_type) && left_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!(
                                    "Operator {} requires numeric type, found {}",
                                    op, left_type
                                ),
                                None,
                            ));
                        }
                        if !self.is_numeric(&right_type) && right_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!(
                                    "Operator {} requires numeric type, found {}",
                                    op, right_type
                                ),
                                None,
                            ));
                        }

                        // Result type
                        if left_type == DataType::Float || right_type == DataType::Float {
                            DataType::Float
                        } else {
                            DataType::Integer
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        if !self.types_compatible(&left_type, &right_type) {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Cannot compare {} and {}", left_type, right_type),
                                None,
                            ));
                        }
                        DataType::Boolean
                    }
                    BinaryOp::Less
                    | BinaryOp::Greater
                    | BinaryOp::LessEqual
                    | BinaryOp::GreaterEqual => {
                        if !self.is_numeric(&left_type) && left_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Comparison requires numeric type, found {}", left_type),
                                None,
                            ));
                        }
                        if !self.is_numeric(&right_type) && right_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Comparison requires numeric type, found {}", right_type),
                                None,
                            ));
                        }
                        DataType::Boolean
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        if left_type != DataType::Boolean && left_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Logical operator requires boolean, found {}", left_type),
                                None,
                            ));
                        }
                        if right_type != DataType::Boolean && right_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Logical operator requires boolean, found {}", right_type),
                                None,
                            ));
                        }
                        DataType::Boolean
                    }
                }
            }

            Expr::Unary { op, operand } => {
                let operand_type = self.analyze_expr(operand);

                match op {
                    UnaryOp::Negate => {
                        if !self.is_numeric(&operand_type) && operand_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Cannot negate non-numeric type {}", operand_type),
                                None,
                            ));
                        }
                        operand_type
                    }
                    UnaryOp::Not => {
                        if operand_type != DataType::Boolean && operand_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Cannot apply ! to non-boolean type {}", operand_type),
                                None,
                            ));
                        }
                        DataType::Boolean
                    }
                    UnaryOp::Increment | UnaryOp::Decrement => {
                        if !self.is_numeric(&operand_type) && operand_type != DataType::Unknown {
                            self.errors.add_error(CompilerError::semantic(
                                format!(
                                    "Increment/decrement requires numeric type, found {}",
                                    operand_type
                                ),
                                None,
                            ));
                        }
                        operand_type
                    }
                }
            }

            Expr::Call { callee, args } => {
                // First, get the function info we need (clone to avoid borrow issues)
                let func_info: Option<(Vec<DataType>, DataType)> =
                    self.symbol_table.lookup(callee).and_then(|sym| {
                        if let DataType::Function {
                            params,
                            return_type,
                        } = &sym.data_type
                        {
                            Some((params.clone(), (**return_type).clone()))
                        } else {
                            None
                        }
                    });

                match func_info {
                    Some((params, return_type)) => {
                        // Check argument count
                        if args.len() != params.len() {
                            self.errors.add_error(CompilerError::semantic(
                                format!(
                                    "Function '{}' expects {} arguments, found {}",
                                    callee,
                                    params.len(),
                                    args.len()
                                ),
                                None,
                            ));
                        }

                        // Check argument types
                        for (i, (arg, param_type)) in args.iter().zip(params.iter()).enumerate() {
                            let arg_type = self.analyze_expr(arg);
                            if !self.types_compatible(param_type, &arg_type) {
                                self.errors.add_error(CompilerError::semantic(
                                    format!(
                                        "Argument {} type mismatch: expected {}, found {}",
                                        i + 1,
                                        param_type,
                                        arg_type
                                    ),
                                    None,
                                ));
                            }
                        }

                        return_type
                    }
                    None => {
                        // Check if symbol exists but is not a function
                        #[allow(clippy::redundant_pattern_matching)]
                        if let Some(_) = self.symbol_table.lookup(callee) {
                            self.errors.add_error(CompilerError::semantic(
                                format!("'{}' is not a function", callee),
                                None,
                            ));
                        } else {
                            self.errors.add_error(CompilerError::semantic(
                                format!("Undefined function '{}'", callee),
                                None,
                            ));
                        }
                        DataType::Unknown
                    }
                }
            }

            Expr::NewObject { class_name, args } => {
                if self.symbol_table.lookup(class_name).is_none() {
                    self.errors.add_error(CompilerError::semantic(
                        format!("Undefined class '{}'", class_name),
                        None,
                    ));
                }

                // Analyze constructor arguments
                for arg in args {
                    self.analyze_expr(arg);
                }

                DataType::Class(class_name.clone())
            }

            Expr::NewArray { element_type, size } => {
                let size_type = self.analyze_expr(size);
                if size_type != DataType::Integer && size_type != DataType::Unknown {
                    self.errors.add_error(CompilerError::semantic(
                        format!("Array size must be integer, found {}", size_type),
                        None,
                    ));
                }

                DataType::Array(Box::new(element_type.clone()), None)
            }

            Expr::NewStack { element_type } => DataType::Stack(Box::new(element_type.clone())),

            Expr::StackPush { stack, value } => {
                let stack_type = self.analyze_expr(stack);
                let value_type = self.analyze_expr(value);

                if let DataType::Stack(element_type) = stack_type {
                    if !self.types_compatible(&element_type, &value_type) {
                        self.errors.add_error(CompilerError::semantic(
                            format!(
                                "Stack push type mismatch: expected {}, found {}",
                                element_type, value_type
                            ),
                            None,
                        ));
                    }
                }

                DataType::Void
            }

            Expr::StackPop { stack } | Expr::StackPeek { stack } => {
                let stack_type = self.analyze_expr(stack);

                match stack_type {
                    DataType::Stack(element_type) => *element_type,
                    DataType::Unknown => DataType::Unknown,
                    _ => {
                        self.errors.add_error(CompilerError::semantic(
                            format!("Expected stack type, found {}", stack_type),
                            None,
                        ));
                        DataType::Unknown
                    }
                }
            }

            Expr::This => {
                if let Some(class_name) = &self.current_class {
                    DataType::Class(class_name.clone())
                } else {
                    self.errors.add_error(CompilerError::semantic(
                        "'this' used outside of class context",
                        None,
                    ));
                    DataType::Unknown
                }
            }

            Expr::Grouped(inner) => self.analyze_expr(inner),
        }
    }

    /// Check if two types are compatible (no implicit conversions)
    fn types_compatible(&self, expected: &DataType, actual: &DataType) -> bool {
        if *expected == DataType::Unknown || *actual == DataType::Unknown {
            return true; // Unknown types are compatible with anything (error already reported)
        }

        match (expected, actual) {
            (DataType::Integer, DataType::Integer) => true,
            (DataType::Float, DataType::Float) => true,
            (DataType::String, DataType::String) => true,
            (DataType::Boolean, DataType::Boolean) => true,
            (DataType::Void, DataType::Void) => true,
            (DataType::Array(e1, _), DataType::Array(e2, _)) => self.types_compatible(e1, e2),
            (DataType::Stack(e1), DataType::Stack(e2)) => self.types_compatible(e1, e2),
            (DataType::Class(n1), DataType::Class(n2)) => n1 == n2,
            (
                DataType::Function {
                    params: p1,
                    return_type: r1,
                },
                DataType::Function {
                    params: p2,
                    return_type: r2,
                },
            ) => {
                p1.len() == p2.len()
                    && p1
                        .iter()
                        .zip(p2.iter())
                        .all(|(a, b)| self.types_compatible(a, b))
                    && self.types_compatible(r1, r2)
            }
            // No implicit conversions between integer and float
            _ => false,
        }
    }

    /// Check if a type is numeric
    fn is_numeric(&self, data_type: &DataType) -> bool {
        matches!(data_type, DataType::Integer | DataType::Float)
    }

    /// Get the symbol table
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    /// Get the errors
    pub fn errors(&self) -> &ErrorCollector {
        &self.errors
    }

    /// Dump the symbol table for debugging
    pub fn dump_symbols(&self) {
        self.symbol_table.dump();
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::RecursiveDescentParser;

    fn analyze(source: &str) -> Result<(), Vec<CompilerError>> {
        let mut lexer = Lexer::new(source);
        lexer.tokenize().ok();
        let mut parser = RecursiveDescentParser::new(lexer.tokens().to_vec());
        let program = parser.parse().map_err(|e| e.to_vec())?;

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program).map_err(|e| e.errors().to_vec())
    }

    #[test]
    fn test_type_mismatch() {
        let result = analyze("let x: integer = \"hello\";");
        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_variable() {
        let result = analyze("let x: integer = y;");
        assert!(result.is_err());
    }

    #[test]
    fn test_valid_program() {
        let result = analyze("let x: integer = 42; let y: integer = x + 1;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_boolean_condition() {
        let result = analyze("let x: integer = 5; if (x) { print(x); }");
        assert!(result.is_err()); // x is not boolean
    }

    #[test]
    fn test_break_outside_loop() {
        let result = analyze("break;");
        assert!(result.is_err());
    }
}
