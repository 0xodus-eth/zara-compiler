// Week 6 & 7: Three-Address Code (TAC) Generation
// Generates intermediate code for expressions, control structures, and method calls

use crate::parser::ast::*;
use std::fmt;

/// Three-address code instructions
#[derive(Debug, Clone)]
pub enum TACInstruction {
    // Assignment: x = y
    Copy {
        dest: String,
        src: String,
    },

    // Binary operation: x = y op z
    BinaryOp {
        dest: String,
        left: String,
        op: String,
        right: String,
    },

    // Unary operation: x = op y
    UnaryOp {
        dest: String,
        op: String,
        operand: String,
    },

    // Indexed assignment: x[i] = y
    IndexedStore {
        array: String,
        index: String,
        value: String,
    },

    // Indexed access: x = y[i]
    IndexedLoad {
        dest: String,
        array: String,
        index: String,
    },

    // Field access: x = y.field
    FieldLoad {
        dest: String,
        object: String,
        field: String,
    },

    // Field assignment: x.field = y
    FieldStore {
        object: String,
        field: String,
        value: String,
    },

    // Unconditional jump: goto L
    Goto {
        label: String,
    },

    // Conditional jump: if x goto L
    IfGoto {
        condition: String,
        label: String,
    },

    // Conditional jump: if x relop y goto L
    IfRelGoto {
        left: String,
        op: String,
        right: String,
        label: String,
    },

    // Conditional jump: ifFalse x goto L
    IfFalseGoto {
        condition: String,
        label: String,
    },

    // Label definition
    Label {
        name: String,
    },

    // Function call: x = call f, n (n arguments)
    Call {
        dest: Option<String>,
        func: String,
        args: Vec<String>,
    },

    // Parameter passing: param x
    Param {
        value: String,
    },

    // Return: return x
    Return {
        value: Option<String>,
    },

    // Function entry
    FuncBegin {
        name: String,
    },

    // Function exit
    FuncEnd {
        name: String,
    },

    // Object creation: x = new Class
    NewObject {
        dest: String,
        class: String,
    },

    // Array creation: x = new array[n]
    NewArray {
        dest: String,
        size: String,
    },

    // Print statement
    Print {
        values: Vec<String>,
    },

    // Input statement
    Input {
        dest: String,
    },

    // Comment (for debugging)
    Comment {
        text: String,
    },
}

impl fmt::Display for TACInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TACInstruction::Copy { dest, src } => write!(f, "{} = {}", dest, src),
            TACInstruction::BinaryOp {
                dest,
                left,
                op,
                right,
            } => {
                write!(f, "{} = {} {} {}", dest, left, op, right)
            }
            TACInstruction::UnaryOp { dest, op, operand } => {
                write!(f, "{} = {} {}", dest, op, operand)
            }
            TACInstruction::IndexedStore {
                array,
                index,
                value,
            } => {
                write!(f, "{}[{}] = {}", array, index, value)
            }
            TACInstruction::IndexedLoad { dest, array, index } => {
                write!(f, "{} = {}[{}]", dest, array, index)
            }
            TACInstruction::FieldLoad {
                dest,
                object,
                field,
            } => {
                write!(f, "{} = {}.{}", dest, object, field)
            }
            TACInstruction::FieldStore {
                object,
                field,
                value,
            } => {
                write!(f, "{}.{} = {}", object, field, value)
            }
            TACInstruction::Goto { label } => write!(f, "goto {}", label),
            TACInstruction::IfGoto { condition, label } => {
                write!(f, "if {} goto {}", condition, label)
            }
            TACInstruction::IfRelGoto {
                left,
                op,
                right,
                label,
            } => {
                write!(f, "if {} {} {} goto {}", left, op, right, label)
            }
            TACInstruction::IfFalseGoto { condition, label } => {
                write!(f, "ifFalse {} goto {}", condition, label)
            }
            TACInstruction::Label { name } => write!(f, "{}:", name),
            TACInstruction::Call { dest, func, args } => {
                let args_str = args.join(", ");
                match dest {
                    Some(d) => write!(f, "{} = call {}({})", d, func, args_str),
                    None => write!(f, "call {}({})", func, args_str),
                }
            }
            TACInstruction::Param { value } => write!(f, "param {}", value),
            TACInstruction::Return { value } => match value {
                Some(v) => write!(f, "return {}", v),
                None => write!(f, "return"),
            },
            TACInstruction::FuncBegin { name } => write!(f, "func_begin {}", name),
            TACInstruction::FuncEnd { name } => write!(f, "func_end {}", name),
            TACInstruction::NewObject { dest, class } => {
                write!(f, "{} = new {}", dest, class)
            }
            TACInstruction::NewArray { dest, size } => {
                write!(f, "{} = new array[{}]", dest, size)
            }
            TACInstruction::Print { values } => {
                write!(f, "print {}", values.join(", "))
            }
            TACInstruction::Input { dest } => write!(f, "input {}", dest),
            TACInstruction::Comment { text } => write!(f, "// {}", text),
        }
    }
}

/// IR Generator using syntax-directed translation
pub struct IRGenerator {
    instructions: Vec<TACInstruction>,
    temp_counter: usize,
    label_counter: usize,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            temp_counter: 0,
            label_counter: 0,
        }
    }

    /// Generate a new temporary variable
    fn new_temp(&mut self) -> String {
        let temp = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        temp
    }

    /// Generate a new label
    fn new_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Emit an instruction
    fn emit(&mut self, instruction: TACInstruction) {
        self.instructions.push(instruction);
    }

    /// Generate IR for a complete program
    pub fn generate(&mut self, program: &Program) -> &[TACInstruction] {
        for stmt in &program.statements {
            self.gen_stmt(stmt);
        }
        &self.instructions
    }

    /// Generate IR for a statement (synthesized attributes)
    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl {
                name, initializer, ..
            } => {
                if let Some(init) = initializer {
                    let init_temp = self.gen_expr(init);
                    self.emit(TACInstruction::Copy {
                        dest: name.clone(),
                        src: init_temp,
                    });
                }
            }

            Stmt::Assignment { target, op, value } => {
                let value_temp = self.gen_expr(value);

                match target {
                    Expr::Identifier(name) => {
                        let final_value = match op {
                            AssignOp::Assign => value_temp,
                            AssignOp::AddAssign => {
                                let temp = self.new_temp();
                                self.emit(TACInstruction::BinaryOp {
                                    dest: temp.clone(),
                                    left: name.clone(),
                                    op: "+".to_string(),
                                    right: value_temp,
                                });
                                temp
                            }
                            AssignOp::SubAssign => {
                                let temp = self.new_temp();
                                self.emit(TACInstruction::BinaryOp {
                                    dest: temp.clone(),
                                    left: name.clone(),
                                    op: "-".to_string(),
                                    right: value_temp,
                                });
                                temp
                            }
                            AssignOp::MulAssign => {
                                let temp = self.new_temp();
                                self.emit(TACInstruction::BinaryOp {
                                    dest: temp.clone(),
                                    left: name.clone(),
                                    op: "*".to_string(),
                                    right: value_temp,
                                });
                                temp
                            }
                            AssignOp::DivAssign => {
                                let temp = self.new_temp();
                                self.emit(TACInstruction::BinaryOp {
                                    dest: temp.clone(),
                                    left: name.clone(),
                                    op: "/".to_string(),
                                    right: value_temp,
                                });
                                temp
                            }
                        };
                        self.emit(TACInstruction::Copy {
                            dest: name.clone(),
                            src: final_value,
                        });
                    }
                    Expr::ArrayAccess { array, index } => {
                        let array_temp = self.gen_expr(array);
                        let index_temp = self.gen_expr(index);
                        self.emit(TACInstruction::IndexedStore {
                            array: array_temp,
                            index: index_temp,
                            value: value_temp,
                        });
                    }
                    Expr::FieldAccess { object, field } => {
                        let obj_temp = self.gen_expr(object);
                        self.emit(TACInstruction::FieldStore {
                            object: obj_temp,
                            field: field.clone(),
                            value: value_temp,
                        });
                    }
                    _ => {}
                }
            }

            Stmt::Expression(expr) => {
                self.gen_expr(expr);
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_temp = self.gen_expr(condition);

                match else_branch {
                    Some(else_stmt) => {
                        // if condition goto L_then
                        // else_branch
                        // goto L_end
                        // L_then:
                        // then_branch
                        // L_end:
                        let label_else = self.new_label();
                        let label_end = self.new_label();

                        self.emit(TACInstruction::IfFalseGoto {
                            condition: cond_temp,
                            label: label_else.clone(),
                        });

                        self.gen_stmt(then_branch);

                        self.emit(TACInstruction::Goto {
                            label: label_end.clone(),
                        });

                        self.emit(TACInstruction::Label { name: label_else });
                        self.gen_stmt(else_stmt);

                        self.emit(TACInstruction::Label { name: label_end });
                    }
                    None => {
                        let label_end = self.new_label();

                        self.emit(TACInstruction::IfFalseGoto {
                            condition: cond_temp,
                            label: label_end.clone(),
                        });

                        self.gen_stmt(then_branch);

                        self.emit(TACInstruction::Label { name: label_end });
                    }
                }
            }

            Stmt::While { condition, body } => {
                // L_begin:
                // if !condition goto L_end
                // body
                // goto L_begin
                // L_end:
                let label_begin = self.new_label();
                let label_end = self.new_label();

                self.emit(TACInstruction::Label {
                    name: label_begin.clone(),
                });

                let cond_temp = self.gen_expr(condition);
                self.emit(TACInstruction::IfFalseGoto {
                    condition: cond_temp,
                    label: label_end.clone(),
                });

                self.gen_stmt(body);

                self.emit(TACInstruction::Goto { label: label_begin });
                self.emit(TACInstruction::Label { name: label_end });
            }

            Stmt::DoWhile { body, condition } => {
                // L_begin:
                // body
                // if condition goto L_begin
                let label_begin = self.new_label();

                self.emit(TACInstruction::Label {
                    name: label_begin.clone(),
                });

                self.gen_stmt(body);

                let cond_temp = self.gen_expr(condition);
                self.emit(TACInstruction::IfGoto {
                    condition: cond_temp,
                    label: label_begin,
                });
            }

            Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                // init
                // L_begin:
                // if !condition goto L_end
                // body
                // update
                // goto L_begin
                // L_end:

                if let Some(init_stmt) = init {
                    self.gen_stmt(init_stmt);
                }

                let label_begin = self.new_label();
                let label_end = self.new_label();

                self.emit(TACInstruction::Label {
                    name: label_begin.clone(),
                });

                if let Some(cond) = condition {
                    let cond_temp = self.gen_expr(cond);
                    self.emit(TACInstruction::IfFalseGoto {
                        condition: cond_temp,
                        label: label_end.clone(),
                    });
                }

                self.gen_stmt(body);

                if let Some(update_stmt) = update {
                    self.gen_stmt(update_stmt);
                }

                self.emit(TACInstruction::Goto { label: label_begin });
                self.emit(TACInstruction::Label { name: label_end });
            }

            Stmt::Return(value) => {
                let ret_val = value.as_ref().map(|v| self.gen_expr(v));
                self.emit(TACInstruction::Return { value: ret_val });
            }

            Stmt::Break => {
                // In a real implementation, we'd track the enclosing loop's end label
                self.emit(TACInstruction::Comment {
                    text: "break".to_string(),
                });
            }

            Stmt::Continue => {
                // In a real implementation, we'd track the enclosing loop's begin label
                self.emit(TACInstruction::Comment {
                    text: "continue".to_string(),
                });
            }

            Stmt::Block(statements) => {
                for s in statements {
                    self.gen_stmt(s);
                }
            }

            Stmt::Print(exprs) => {
                let values: Vec<String> = exprs.iter().map(|e| self.gen_expr(e)).collect();
                self.emit(TACInstruction::Print { values });
            }

            Stmt::Input(var_name) => {
                self.emit(TACInstruction::Input {
                    dest: var_name.clone(),
                });
            }

            Stmt::FuncDecl {
                name, params, body, ..
            } => {
                self.emit(TACInstruction::FuncBegin { name: name.clone() });

                // Parameters are available by their names
                for param in params {
                    self.emit(TACInstruction::Comment {
                        text: format!("param {}", param.name),
                    });
                }

                self.gen_stmt(body);

                self.emit(TACInstruction::FuncEnd { name: name.clone() });
            }

            Stmt::ClassDecl { name, members, .. } => {
                self.emit(TACInstruction::Comment {
                    text: format!("class {}", name),
                });

                for member in members {
                    match member {
                        ClassMember::Method {
                            name: method_name,
                            params,
                            body,
                            ..
                        } => {
                            let full_name = format!("{}_{}", name, method_name);
                            self.emit(TACInstruction::FuncBegin {
                                name: full_name.clone(),
                            });

                            for param in params {
                                self.emit(TACInstruction::Comment {
                                    text: format!("param {}", param.name),
                                });
                            }

                            self.gen_stmt(body);

                            self.emit(TACInstruction::FuncEnd { name: full_name });
                        }
                        ClassMember::Constructor { params, body } => {
                            let full_name = format!("{}_constructor", name);
                            self.emit(TACInstruction::FuncBegin {
                                name: full_name.clone(),
                            });

                            for param in params {
                                self.emit(TACInstruction::Comment {
                                    text: format!("param {}", param.name),
                                });
                            }

                            self.gen_stmt(body);

                            self.emit(TACInstruction::FuncEnd { name: full_name });
                        }
                        ClassMember::Field { .. } => {
                            // Fields are handled during object creation
                        }
                    }
                }
            }

            Stmt::Empty => {}
        }
    }

    /// Generate IR for an expression, return the temporary holding the result
    fn gen_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::IntegerLiteral(n) => n.to_string(),
            Expr::FloatLiteral(n) => n.to_string(),
            Expr::StringLiteral(s) => format!("\"{}\"", s),
            Expr::BoolLiteral(b) => if *b { "1" } else { "0" }.to_string(),
            Expr::Null => "null".to_string(),

            Expr::Identifier(name) => name.clone(),

            Expr::ArrayAccess { array, index } => {
                let array_temp = self.gen_expr(array);
                let index_temp = self.gen_expr(index);
                let result = self.new_temp();
                self.emit(TACInstruction::IndexedLoad {
                    dest: result.clone(),
                    array: array_temp,
                    index: index_temp,
                });
                result
            }

            Expr::FieldAccess { object, field } => {
                let obj_temp = self.gen_expr(object);
                let result = self.new_temp();
                self.emit(TACInstruction::FieldLoad {
                    dest: result.clone(),
                    object: obj_temp,
                    field: field.clone(),
                });
                result
            }

            Expr::MethodCall {
                object,
                method,
                args,
            } => {
                let obj_temp = self.gen_expr(object);
                let arg_temps: Vec<String> = args.iter().map(|a| self.gen_expr(a)).collect();

                // Push object as first argument (this)
                let mut all_args = vec![obj_temp.clone()];
                all_args.extend(arg_temps);

                let result = self.new_temp();
                self.emit(TACInstruction::Call {
                    dest: Some(result.clone()),
                    func: method.clone(),
                    args: all_args,
                });
                result
            }

            Expr::Binary { left, op, right } => {
                let left_temp = self.gen_expr(left);
                let right_temp = self.gen_expr(right);
                let result = self.new_temp();

                let op_str = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Subtract => "-",
                    BinaryOp::Multiply => "*",
                    BinaryOp::Divide => "/",
                    BinaryOp::Modulo => "%",
                    BinaryOp::Equal => "==",
                    BinaryOp::NotEqual => "!=",
                    BinaryOp::Less => "<",
                    BinaryOp::Greater => ">",
                    BinaryOp::LessEqual => "<=",
                    BinaryOp::GreaterEqual => ">=",
                    BinaryOp::And => "&&",
                    BinaryOp::Or => "||",
                };

                self.emit(TACInstruction::BinaryOp {
                    dest: result.clone(),
                    left: left_temp,
                    op: op_str.to_string(),
                    right: right_temp,
                });
                result
            }

            Expr::Unary { op, operand } => {
                let operand_temp = self.gen_expr(operand);
                let result = self.new_temp();

                let op_str = match op {
                    UnaryOp::Negate => "-",
                    UnaryOp::Not => "!",
                    UnaryOp::Increment => "++",
                    UnaryOp::Decrement => "--",
                };

                self.emit(TACInstruction::UnaryOp {
                    dest: result.clone(),
                    op: op_str.to_string(),
                    operand: operand_temp,
                });
                result
            }

            Expr::Call { callee, args } => {
                let arg_temps: Vec<String> = args.iter().map(|a| self.gen_expr(a)).collect();

                let result = self.new_temp();
                self.emit(TACInstruction::Call {
                    dest: Some(result.clone()),
                    func: callee.clone(),
                    args: arg_temps,
                });
                result
            }

            Expr::NewObject { class_name, args } => {
                let result = self.new_temp();
                self.emit(TACInstruction::NewObject {
                    dest: result.clone(),
                    class: class_name.clone(),
                });

                // Call constructor if there are arguments
                if !args.is_empty() {
                    let arg_temps: Vec<String> = args.iter().map(|a| self.gen_expr(a)).collect();
                    let mut all_args = vec![result.clone()];
                    all_args.extend(arg_temps);

                    self.emit(TACInstruction::Call {
                        dest: None,
                        func: format!("{}_constructor", class_name),
                        args: all_args,
                    });
                }

                result
            }

            Expr::NewArray { size, .. } => {
                let size_temp = self.gen_expr(size);
                let result = self.new_temp();
                self.emit(TACInstruction::NewArray {
                    dest: result.clone(),
                    size: size_temp,
                });
                result
            }

            Expr::NewStack { .. } => {
                let result = self.new_temp();
                self.emit(TACInstruction::NewArray {
                    dest: result.clone(),
                    size: "0".to_string(),
                });
                result
            }

            Expr::StackPush { stack, value } => {
                let stack_temp = self.gen_expr(stack);
                let value_temp = self.gen_expr(value);
                self.emit(TACInstruction::Call {
                    dest: None,
                    func: "stack_push".to_string(),
                    args: vec![stack_temp, value_temp],
                });
                "void".to_string()
            }

            Expr::StackPop { stack } => {
                let stack_temp = self.gen_expr(stack);
                let result = self.new_temp();
                self.emit(TACInstruction::Call {
                    dest: Some(result.clone()),
                    func: "stack_pop".to_string(),
                    args: vec![stack_temp],
                });
                result
            }

            Expr::StackPeek { stack } => {
                let stack_temp = self.gen_expr(stack);
                let result = self.new_temp();
                self.emit(TACInstruction::Call {
                    dest: Some(result.clone()),
                    func: "stack_peek".to_string(),
                    args: vec![stack_temp],
                });
                result
            }

            Expr::This => "this".to_string(),

            Expr::Grouped(inner) => self.gen_expr(inner),
        }
    }

    /// Get all generated instructions
    pub fn instructions(&self) -> &[TACInstruction] {
        &self.instructions
    }

    /// Print the generated TAC
    pub fn print_tac(&self) {
        println!("\n=== Three-Address Code ===");
        for (i, instr) in self.instructions.iter().enumerate() {
            println!("{:4}: {}", i, instr);
        }
        println!("==========================\n");
    }
}

impl Default for IRGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::RecursiveDescentParser;

    fn generate_ir(source: &str) -> Vec<TACInstruction> {
        let mut lexer = Lexer::new(source);
        lexer.tokenize().ok();
        let mut parser = RecursiveDescentParser::new(lexer.tokens().to_vec());
        let program = parser.parse().unwrap();

        let mut generator = IRGenerator::new();
        generator.generate(&program);
        generator.instructions().to_vec()
    }

    #[test]
    fn test_simple_assignment() {
        let ir = generate_ir("let x: integer = 42;");
        assert!(!ir.is_empty());
    }

    #[test]
    fn test_binary_expression() {
        let ir = generate_ir("let x: integer = 1 + 2 * 3;");
        assert!(ir.len() >= 2); // At least multiply and add
    }

    #[test]
    fn test_if_statement() {
        let ir = generate_ir("let x: integer = 1; if (x == 1) { x = 2; }");
        let has_label = ir.iter().any(|i| matches!(i, TACInstruction::Label { .. }));
        assert!(has_label);
    }

    #[test]
    fn test_while_loop() {
        let ir = generate_ir("let i: integer = 0; while (i < 10) { i = i + 1; }");
        let goto_count = ir
            .iter()
            .filter(|i| matches!(i, TACInstruction::Goto { .. }))
            .count();
        assert!(goto_count >= 1);
    }

    #[test]
    fn test_function() {
        let ir = generate_ir("func add(a: integer, b: integer) -> integer { return a + b; }");
        let has_func_begin = ir
            .iter()
            .any(|i| matches!(i, TACInstruction::FuncBegin { .. }));
        assert!(has_func_begin);
    }
}
