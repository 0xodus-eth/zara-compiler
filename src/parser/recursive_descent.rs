// Week 3: Top-Down Parser (Recursive Descent)
// Parses expressions, control structures (if-else, for, do-while), and sub-programs

use crate::error::{CompilerError, SourceLocation};
use crate::lexer::{Token, TokenKind};
use crate::parser::ast::*;

/// Recursive Descent Parser for Zara
pub struct RecursiveDescentParser {
    tokens: Vec<Token>,
    position: usize,
    errors: Vec<CompilerError>,
}

impl RecursiveDescentParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
            errors: Vec::new(),
        }
    }

    /// Parse the entire program
    pub fn parse(&mut self) -> Result<Program, Vec<CompilerError>> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }

        if self.errors.is_empty() {
            Ok(Program::new(statements))
        } else {
            Err(self.errors.clone())
        }
    }

    // ===================== Helper Methods =====================

    fn current(&self) -> &Token {
        self.tokens
            .get(self.position)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.position.saturating_sub(1)]
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::EOF
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.position += 1;
        }
        self.previous()
    }

    fn check(&self, kind: &TokenKind) -> bool {
        !self.is_at_end() && &self.current().kind == kind
    }

    fn match_token(&mut self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.check(kind) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, kind: TokenKind, message: &str) -> Result<Token, CompilerError> {
        if self.check(&kind) {
            Ok(self.advance().clone())
        } else {
            Err(self.error(message))
        }
    }

    fn error(&self, message: &str) -> CompilerError {
        let token = self.current();
        CompilerError::syntactic(
            format!("{} (found '{}')", message, token.value),
            Some(SourceLocation::new(token.line, token.column)),
        )
    }

    /// Panic mode error recovery - skip until synchronizing token
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }

            match self.current().kind {
                TokenKind::Class
                | TokenKind::Func
                | TokenKind::Let
                | TokenKind::Const
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Return => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    // ===================== Parsing Methods =====================

    fn parse_declaration(&mut self) -> Result<Stmt, CompilerError> {
        if self.match_token(&[TokenKind::Class]) {
            self.parse_class_declaration()
        } else if self.match_token(&[TokenKind::Func]) {
            self.parse_function_declaration()
        } else if self.match_token(&[TokenKind::Let, TokenKind::Const]) {
            self.parse_var_declaration()
        } else {
            self.parse_statement()
        }
    }

    fn parse_class_declaration(&mut self) -> Result<Stmt, CompilerError> {
        let name = self.consume(TokenKind::Identifier, "Expected class name")?;

        let parent = if self.match_token(&[TokenKind::Extends]) {
            Some(
                self.consume(TokenKind::Identifier, "Expected parent class name")?
                    .value,
            )
        } else {
            None
        };

        self.consume(TokenKind::LeftBrace, "Expected '{' after class name")?;

        let mut members = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            members.push(self.parse_class_member()?);
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after class body")?;

        Ok(Stmt::ClassDecl {
            name: name.value,
            parent,
            members,
        })
    }

    fn parse_class_member(&mut self) -> Result<ClassMember, CompilerError> {
        let is_public = if self.match_token(&[TokenKind::Public]) {
            true
        } else if self.match_token(&[TokenKind::Private]) {
            false
        } else {
            true // default public
        };

        let is_static = self.match_token(&[TokenKind::Static]);

        if self.match_token(&[TokenKind::Func]) {
            // Method
            let name = self.consume(TokenKind::Identifier, "Expected method name")?;
            let params = self.parse_parameters()?;

            let return_type = if self.match_token(&[TokenKind::Arrow]) {
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            let body = self.parse_block()?;

            Ok(ClassMember::Method {
                name: name.value,
                params,
                return_type,
                body,
                is_static,
                is_public,
            })
        } else if self.check(&TokenKind::Identifier) && self.current().value == "constructor" {
            // Constructor
            self.advance();
            let params = self.parse_parameters()?;
            let body = self.parse_block()?;

            Ok(ClassMember::Constructor { params, body })
        } else {
            // Field
            let name = self.consume(TokenKind::Identifier, "Expected field name")?;
            self.consume(TokenKind::Colon, "Expected ':' after field name")?;
            let type_annotation = self.parse_type_annotation()?;

            let initializer = if self.match_token(&[TokenKind::Equal]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            self.consume(TokenKind::Semicolon, "Expected ';' after field")?;

            Ok(ClassMember::Field {
                name: name.value,
                type_annotation,
                is_static,
                is_public,
                initializer,
            })
        }
    }

    fn parse_function_declaration(&mut self) -> Result<Stmt, CompilerError> {
        let name = self.consume(TokenKind::Identifier, "Expected function name")?;
        let params = self.parse_parameters()?;

        let return_type = if self.match_token(&[TokenKind::Arrow]) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Stmt::FuncDecl {
            name: name.value,
            params,
            return_type,
            body: Box::new(body),
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, CompilerError> {
        self.consume(TokenKind::LeftParen, "Expected '(' after function name")?;

        let mut params = Vec::new();

        if !self.check(&TokenKind::RightParen) {
            loop {
                let name = self.consume(TokenKind::Identifier, "Expected parameter name")?;
                self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;
                let type_annotation = self.parse_type_annotation()?;

                params.push(Parameter {
                    name: name.value,
                    type_annotation,
                });

                if !self.match_token(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;
        Ok(params)
    }

    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, CompilerError> {
        if self.match_token(&[TokenKind::TypeArray]) {
            // array[size] of type OR array of type
            let size = if self.match_token(&[TokenKind::LeftBracket]) {
                let size_token = self.consume(TokenKind::IntegerLiteral, "Expected array size")?;
                self.consume(TokenKind::RightBracket, "Expected ']'")?;
                Some(size_token.value.parse().unwrap_or(0))
            } else {
                None
            };

            // 'of' keyword (optional, we'll just check for a type)
            if self.current().value == "of" {
                self.advance();
            }

            let element_type = self.parse_base_type()?;
            Ok(TypeAnnotation::array(element_type, size))
        } else if self.match_token(&[TokenKind::TypeStack]) {
            // stack of type
            if self.current().value == "of" {
                self.advance();
            }
            let element_type = self.parse_base_type()?;
            Ok(TypeAnnotation::stack(element_type))
        } else {
            let base = self.parse_base_type()?;
            Ok(TypeAnnotation::simple(base))
        }
    }

    fn parse_base_type(&mut self) -> Result<String, CompilerError> {
        if self.match_token(&[
            TokenKind::TypeInteger,
            TokenKind::TypeFloat,
            TokenKind::TypeString,
            TokenKind::TypeBoolean,
            TokenKind::Void,
        ]) {
            Ok(self.previous().value.clone())
        } else if self.match_token(&[TokenKind::Identifier]) {
            Ok(self.previous().value.clone())
        } else {
            Err(self.error("Expected type"))
        }
    }

    fn parse_var_declaration(&mut self) -> Result<Stmt, CompilerError> {
        let is_const = self.previous().kind == TokenKind::Const;
        let name = self.consume(TokenKind::Identifier, "Expected variable name")?;

        let type_annotation = if self.match_token(&[TokenKind::Colon]) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        let initializer = if self.match_token(&[TokenKind::Equal]) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(
            TokenKind::Semicolon,
            "Expected ';' after variable declaration",
        )?;

        Ok(Stmt::VarDecl {
            name: name.value,
            type_annotation,
            initializer,
            is_const,
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, CompilerError> {
        if self.match_token(&[TokenKind::If]) {
            self.parse_if_statement()
        } else if self.match_token(&[TokenKind::While]) {
            self.parse_while_statement()
        } else if self.match_token(&[TokenKind::Do]) {
            self.parse_do_while_statement()
        } else if self.match_token(&[TokenKind::For]) {
            self.parse_for_statement()
        } else if self.match_token(&[TokenKind::Return]) {
            self.parse_return_statement()
        } else if self.match_token(&[TokenKind::Break]) {
            self.consume(TokenKind::Semicolon, "Expected ';' after 'break'")?;
            Ok(Stmt::Break)
        } else if self.match_token(&[TokenKind::Continue]) {
            self.consume(TokenKind::Semicolon, "Expected ';' after 'continue'")?;
            Ok(Stmt::Continue)
        } else if self.match_token(&[TokenKind::Print]) {
            self.parse_print_statement()
        } else if self.match_token(&[TokenKind::Input]) {
            self.parse_input_statement()
        } else if self.match_token(&[TokenKind::LeftBrace]) {
            self.parse_block_inner()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_block(&mut self) -> Result<Stmt, CompilerError> {
        self.consume(TokenKind::LeftBrace, "Expected '{'")?;
        self.parse_block_inner()
    }

    fn parse_block_inner(&mut self) -> Result<Stmt, CompilerError> {
        let mut statements = Vec::new();

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            match self.parse_declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after block")?;
        Ok(Stmt::Block(statements))
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, CompilerError> {
        self.consume(TokenKind::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RightParen, "Expected ')' after condition")?;

        let then_branch = Box::new(self.parse_statement()?);

        let else_branch = if self.match_token(&[TokenKind::Else]) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, CompilerError> {
        self.consume(TokenKind::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RightParen, "Expected ')' after condition")?;

        let body = Box::new(self.parse_statement()?);

        Ok(Stmt::While { condition, body })
    }

    fn parse_do_while_statement(&mut self) -> Result<Stmt, CompilerError> {
        let body = Box::new(self.parse_statement()?);

        self.consume(TokenKind::While, "Expected 'while' after do block")?;
        self.consume(TokenKind::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::RightParen, "Expected ')' after condition")?;
        self.consume(TokenKind::Semicolon, "Expected ';' after do-while")?;

        Ok(Stmt::DoWhile { body, condition })
    }

    fn parse_for_statement(&mut self) -> Result<Stmt, CompilerError> {
        self.consume(TokenKind::LeftParen, "Expected '(' after 'for'")?;

        // Initializer
        let init = if self.match_token(&[TokenKind::Semicolon]) {
            None
        } else if self.match_token(&[TokenKind::Let, TokenKind::Const]) {
            Some(Box::new(self.parse_var_declaration()?))
        } else {
            let expr = self.parse_expression()?;
            self.consume(TokenKind::Semicolon, "Expected ';' after for initializer")?;
            Some(Box::new(Stmt::Expression(expr)))
        };

        // Condition
        let condition = if self.check(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume(TokenKind::Semicolon, "Expected ';' after for condition")?;

        // Update - handle assignment as well
        let update = if self.check(&TokenKind::RightParen) {
            None
        } else {
            let expr = self.parse_expression()?;
            // Check if this is an assignment
            if self.match_token(&[
                TokenKind::Equal,
                TokenKind::PlusAssign,
                TokenKind::MinusAssign,
                TokenKind::StarAssign,
                TokenKind::SlashAssign,
            ]) {
                let op = match self.previous().kind {
                    TokenKind::Equal => AssignOp::Assign,
                    TokenKind::PlusAssign => AssignOp::AddAssign,
                    TokenKind::MinusAssign => AssignOp::SubAssign,
                    TokenKind::StarAssign => AssignOp::MulAssign,
                    TokenKind::SlashAssign => AssignOp::DivAssign,
                    _ => unreachable!(),
                };
                let value = self.parse_expression()?;
                Some(Box::new(Stmt::Assignment {
                    target: expr,
                    op,
                    value,
                }))
            } else {
                Some(Box::new(Stmt::Expression(expr)))
            }
        };

        self.consume(TokenKind::RightParen, "Expected ')' after for clauses")?;

        let body = Box::new(self.parse_statement()?);

        Ok(Stmt::For {
            init,
            condition,
            update,
            body,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Stmt, CompilerError> {
        let value = if !self.check(&TokenKind::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume(TokenKind::Semicolon, "Expected ';' after return value")?;
        Ok(Stmt::Return(value))
    }

    fn parse_print_statement(&mut self) -> Result<Stmt, CompilerError> {
        self.consume(TokenKind::LeftParen, "Expected '(' after 'print'")?;

        let mut exprs = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                exprs.push(self.parse_expression()?);
                if !self.match_token(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')' after print arguments")?;
        self.consume(TokenKind::Semicolon, "Expected ';' after print statement")?;

        Ok(Stmt::Print(exprs))
    }

    fn parse_input_statement(&mut self) -> Result<Stmt, CompilerError> {
        self.consume(TokenKind::LeftParen, "Expected '(' after 'input'")?;
        let name = self.consume(TokenKind::Identifier, "Expected variable name")?;
        self.consume(TokenKind::RightParen, "Expected ')' after input variable")?;
        self.consume(TokenKind::Semicolon, "Expected ';' after input statement")?;

        Ok(Stmt::Input(name.value))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, CompilerError> {
        let expr = self.parse_expression()?;

        // Check for assignment
        if self.match_token(&[
            TokenKind::Equal,
            TokenKind::PlusAssign,
            TokenKind::MinusAssign,
            TokenKind::StarAssign,
            TokenKind::SlashAssign,
        ]) {
            let op = match self.previous().kind {
                TokenKind::Equal => AssignOp::Assign,
                TokenKind::PlusAssign => AssignOp::AddAssign,
                TokenKind::MinusAssign => AssignOp::SubAssign,
                TokenKind::StarAssign => AssignOp::MulAssign,
                TokenKind::SlashAssign => AssignOp::DivAssign,
                _ => unreachable!(),
            };
            let value = self.parse_expression()?;
            self.consume(TokenKind::Semicolon, "Expected ';' after assignment")?;

            return Ok(Stmt::Assignment {
                target: expr,
                op,
                value,
            });
        }

        self.consume(TokenKind::Semicolon, "Expected ';' after expression")?;
        Ok(Stmt::Expression(expr))
    }

    // ===================== Expression Parsing =====================

    fn parse_expression(&mut self) -> Result<Expr, CompilerError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, CompilerError> {
        let mut expr = self.parse_and()?;

        while self.match_token(&[TokenKind::Or]) {
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, CompilerError> {
        let mut expr = self.parse_equality()?;

        while self.match_token(&[TokenKind::And]) {
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, CompilerError> {
        let mut expr = self.parse_comparison()?;

        while self.match_token(&[TokenKind::EqualEqual, TokenKind::NotEqual]) {
            let op = match self.previous().kind {
                TokenKind::EqualEqual => BinaryOp::Equal,
                TokenKind::NotEqual => BinaryOp::NotEqual,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, CompilerError> {
        let mut expr = self.parse_term()?;

        while self.match_token(&[
            TokenKind::Less,
            TokenKind::Greater,
            TokenKind::LessEqual,
            TokenKind::GreaterEqual,
        ]) {
            let op = match self.previous().kind {
                TokenKind::Less => BinaryOp::Less,
                TokenKind::Greater => BinaryOp::Greater,
                TokenKind::LessEqual => BinaryOp::LessEqual,
                TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, CompilerError> {
        let mut expr = self.parse_factor()?;

        while self.match_token(&[TokenKind::Plus, TokenKind::Minus]) {
            let op = match self.previous().kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Subtract,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, CompilerError> {
        let mut expr = self.parse_unary()?;

        while self.match_token(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
            let op = match self.previous().kind {
                TokenKind::Star => BinaryOp::Multiply,
                TokenKind::Slash => BinaryOp::Divide,
                TokenKind::Percent => BinaryOp::Modulo,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, CompilerError> {
        if self.match_token(&[TokenKind::Not, TokenKind::Minus]) {
            let op = match self.previous().kind {
                TokenKind::Not => UnaryOp::Not,
                TokenKind::Minus => UnaryOp::Negate,
                _ => unreachable!(),
            };
            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                op,
                operand: Box::new(operand),
            });
        }

        if self.match_token(&[TokenKind::Increment, TokenKind::Decrement]) {
            let op = match self.previous().kind {
                TokenKind::Increment => UnaryOp::Increment,
                TokenKind::Decrement => UnaryOp::Decrement,
                _ => unreachable!(),
            };
            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                op,
                operand: Box::new(operand),
            });
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, CompilerError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(&[TokenKind::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_token(&[TokenKind::Dot]) {
                let name = self.consume(TokenKind::Identifier, "Expected property name")?;
                if self.match_token(&[TokenKind::LeftParen]) {
                    // Method call
                    let args = self.parse_arguments()?;
                    expr = Expr::MethodCall {
                        object: Box::new(expr),
                        method: name.value,
                        args,
                    };
                } else {
                    // Field access
                    expr = Expr::FieldAccess {
                        object: Box::new(expr),
                        field: name.value,
                    };
                }
            } else if self.match_token(&[TokenKind::LeftBracket]) {
                let index = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "Expected ']' after index")?;
                expr = Expr::ArrayAccess {
                    array: Box::new(expr),
                    index: Box::new(index),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, CompilerError> {
        let args = self.parse_arguments()?;

        if let Expr::Identifier(name) = callee {
            Ok(Expr::Call { callee: name, args })
        } else {
            Err(self.error("Invalid function call"))
        }
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, CompilerError> {
        let mut args = Vec::new();

        if !self.check(&TokenKind::RightParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;
        Ok(args)
    }

    fn parse_primary(&mut self) -> Result<Expr, CompilerError> {
        // Literals
        if self.match_token(&[TokenKind::IntegerLiteral]) {
            let value: i64 = self.previous().value.parse().unwrap_or(0);
            return Ok(Expr::IntegerLiteral(value));
        }

        if self.match_token(&[TokenKind::FloatLiteral]) {
            let value: f64 = self.previous().value.parse().unwrap_or(0.0);
            return Ok(Expr::FloatLiteral(value));
        }

        if self.match_token(&[TokenKind::StringLiteral]) {
            return Ok(Expr::StringLiteral(self.previous().value.clone()));
        }

        if self.match_token(&[TokenKind::True]) {
            return Ok(Expr::BoolLiteral(true));
        }

        if self.match_token(&[TokenKind::False]) {
            return Ok(Expr::BoolLiteral(false));
        }

        if self.match_token(&[TokenKind::Null]) {
            return Ok(Expr::Null);
        }

        if self.match_token(&[TokenKind::This]) {
            return Ok(Expr::This);
        }

        // new keyword for object/array creation
        if self.match_token(&[TokenKind::New]) {
            return self.parse_new_expression();
        }

        // Identifier
        if self.match_token(&[TokenKind::Identifier]) {
            return Ok(Expr::Identifier(self.previous().value.clone()));
        }

        // Grouped expression
        if self.match_token(&[TokenKind::LeftParen]) {
            let expr = self.parse_expression()?;
            self.consume(TokenKind::RightParen, "Expected ')' after expression")?;
            return Ok(Expr::Grouped(Box::new(expr)));
        }

        Err(self.error("Expected expression"))
    }

    fn parse_new_expression(&mut self) -> Result<Expr, CompilerError> {
        if self.match_token(&[TokenKind::TypeArray]) {
            // new array[size] of type
            self.consume(TokenKind::LeftBracket, "Expected '[' after 'array'")?;
            let size = self.parse_expression()?;
            self.consume(TokenKind::RightBracket, "Expected ']'")?;

            if self.current().value == "of" {
                self.advance();
            }

            let element_type = self.parse_type_annotation()?.to_data_type();

            Ok(Expr::NewArray {
                element_type,
                size: Box::new(size),
            })
        } else if self.match_token(&[TokenKind::TypeStack]) {
            // new stack of type
            if self.current().value == "of" {
                self.advance();
            }

            let element_type = self.parse_type_annotation()?.to_data_type();

            Ok(Expr::NewStack { element_type })
        } else {
            // new ClassName(args)
            let class_name = self.consume(TokenKind::Identifier, "Expected class name")?;
            self.consume(TokenKind::LeftParen, "Expected '(' after class name")?;
            let args = self.parse_arguments()?;

            Ok(Expr::NewObject {
                class_name: class_name.value,
                args,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(source: &str) -> Result<Program, Vec<CompilerError>> {
        let mut lexer = Lexer::new(source);
        lexer.tokenize().ok();
        let mut parser = RecursiveDescentParser::new(lexer.tokens().to_vec());
        parser.parse()
    }

    #[test]
    fn test_variable_declaration() {
        let program = parse("let x: integer = 42;").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_if_else() {
        let program = parse("if (x > 5) { print(x); } else { print(0); }").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_while_loop() {
        let program = parse("while (x < 10) { x = x + 1; }").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_do_while() {
        let program = parse("do { x = x + 1; } while (x < 10);").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_for_loop() {
        let program = parse("for (let i: integer = 0; i < 10; i = i + 1) { print(i); }").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_function() {
        let program =
            parse("func add(a: integer, b: integer) -> integer { return a + b; }").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_class() {
        let source = r#"
            class Point {
                x: integer;
                y: integer;
                
                func move(dx: integer, dy: integer) {
                    this.x = this.x + dx;
                    this.y = this.y + dy;
                }
            }
        "#;
        let program = parse(source).unwrap();
        assert_eq!(program.statements.len(), 1);
    }
}
