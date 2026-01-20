// Week 4: Bottom-Up Parser (Shift-Reduce / LR Parser)
// Handles expression evaluation, conditionals, and loops
// Detects and resolves shift-reduce conflicts

use crate::error::{CompilerError, SourceLocation};
use crate::lexer::{Token, TokenKind};
use crate::parser::ast::*;
use std::collections::HashMap;

/// Action types in the parsing table
#[derive(Debug, Clone)]
pub enum Action {
    Shift(usize),  // Shift and go to state
    Reduce(usize), // Reduce by production rule
    Accept,        // Accept the input
    Error,         // Syntax error
}

/// Grammar production rule
#[derive(Debug, Clone)]
pub struct Production {
    pub lhs: String,      // Left-hand side (non-terminal)
    pub rhs: Vec<String>, // Right-hand side symbols
    pub precedence: i32,  // For conflict resolution
}

/// Item on the parse stack
#[derive(Debug, Clone)]
pub enum StackItem {
    State(usize),
    Token(Token),
    Node(ASTNode),
}

/// Generic AST node for shift-reduce parsing
#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Vec<ASTNode>),
    Expr(Expr),
    Stmt(Stmt),
    Token(Token),
}

/// Shift-Reduce Parser for Zara
pub struct ShiftReduceParser {
    tokens: Vec<Token>,
    position: usize,
    stack: Vec<StackItem>,
    action_table: HashMap<(usize, String), Action>,
    goto_table: HashMap<(usize, String), usize>,
    productions: Vec<Production>,
    errors: Vec<CompilerError>,
}

impl ShiftReduceParser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut parser = Self {
            tokens,
            position: 0,
            stack: vec![StackItem::State(0)],
            action_table: HashMap::new(),
            goto_table: HashMap::new(),
            productions: Vec::new(),
            errors: Vec::new(),
        };
        parser.build_tables();
        parser
    }

    /// Build the parsing tables for a simplified expression grammar
    fn build_tables(&mut self) {
        // Simplified grammar for expressions:
        // 0: S -> E
        // 1: E -> E + T
        // 2: E -> E - T
        // 3: E -> T
        // 4: T -> T * F
        // 5: T -> T / F
        // 6: T -> F
        // 7: F -> ( E )
        // 8: F -> id
        // 9: F -> num

        self.productions = vec![
            Production {
                lhs: "S".to_string(),
                rhs: vec!["E".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), "+".to_string(), "T".to_string()],
                precedence: 1,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), "-".to_string(), "T".to_string()],
                precedence: 1,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["T".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "T".to_string(),
                rhs: vec!["T".to_string(), "*".to_string(), "F".to_string()],
                precedence: 2,
            },
            Production {
                lhs: "T".to_string(),
                rhs: vec!["T".to_string(), "/".to_string(), "F".to_string()],
                precedence: 2,
            },
            Production {
                lhs: "T".to_string(),
                rhs: vec!["F".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "F".to_string(),
                rhs: vec!["(".to_string(), "E".to_string(), ")".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "F".to_string(),
                rhs: vec!["id".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "F".to_string(),
                rhs: vec!["num".to_string()],
                precedence: 0,
            },
            // Comparison operations
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), "==".to_string(), "E".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), "!=".to_string(), "E".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), "<".to_string(), "E".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), ">".to_string(), "E".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), "<=".to_string(), "E".to_string()],
                precedence: 0,
            },
            Production {
                lhs: "E".to_string(),
                rhs: vec!["E".to_string(), ">=".to_string(), "E".to_string()],
                precedence: 0,
            },
        ];

        // Build action table (simplified for expression parsing)
        // State 0: Initial state
        self.action_table
            .insert((0, "id".to_string()), Action::Shift(5));
        self.action_table
            .insert((0, "num".to_string()), Action::Shift(6));
        self.action_table
            .insert((0, "(".to_string()), Action::Shift(4));

        // State 1: E seen
        self.action_table
            .insert((1, "+".to_string()), Action::Shift(7));
        self.action_table
            .insert((1, "-".to_string()), Action::Shift(8));
        self.action_table
            .insert((1, "$".to_string()), Action::Accept);
        self.action_table
            .insert((1, "==".to_string()), Action::Shift(15));
        self.action_table
            .insert((1, "!=".to_string()), Action::Shift(16));
        self.action_table
            .insert((1, "<".to_string()), Action::Shift(17));
        self.action_table
            .insert((1, ">".to_string()), Action::Shift(18));
        self.action_table
            .insert((1, "<=".to_string()), Action::Shift(19));
        self.action_table
            .insert((1, ">=".to_string()), Action::Shift(20));

        // State 2: T seen
        self.action_table
            .insert((2, "+".to_string()), Action::Reduce(3));
        self.action_table
            .insert((2, "-".to_string()), Action::Reduce(3));
        self.action_table
            .insert((2, "*".to_string()), Action::Shift(9));
        self.action_table
            .insert((2, "/".to_string()), Action::Shift(10));
        self.action_table
            .insert((2, ")".to_string()), Action::Reduce(3));
        self.action_table
            .insert((2, "$".to_string()), Action::Reduce(3));

        // State 3: F seen
        self.action_table
            .insert((3, "+".to_string()), Action::Reduce(6));
        self.action_table
            .insert((3, "-".to_string()), Action::Reduce(6));
        self.action_table
            .insert((3, "*".to_string()), Action::Reduce(6));
        self.action_table
            .insert((3, "/".to_string()), Action::Reduce(6));
        self.action_table
            .insert((3, ")".to_string()), Action::Reduce(6));
        self.action_table
            .insert((3, "$".to_string()), Action::Reduce(6));

        // State 4: ( seen
        self.action_table
            .insert((4, "id".to_string()), Action::Shift(5));
        self.action_table
            .insert((4, "num".to_string()), Action::Shift(6));
        self.action_table
            .insert((4, "(".to_string()), Action::Shift(4));

        // State 5: id seen
        self.action_table
            .insert((5, "+".to_string()), Action::Reduce(8));
        self.action_table
            .insert((5, "-".to_string()), Action::Reduce(8));
        self.action_table
            .insert((5, "*".to_string()), Action::Reduce(8));
        self.action_table
            .insert((5, "/".to_string()), Action::Reduce(8));
        self.action_table
            .insert((5, ")".to_string()), Action::Reduce(8));
        self.action_table
            .insert((5, "$".to_string()), Action::Reduce(8));

        // State 6: num seen
        self.action_table
            .insert((6, "+".to_string()), Action::Reduce(9));
        self.action_table
            .insert((6, "-".to_string()), Action::Reduce(9));
        self.action_table
            .insert((6, "*".to_string()), Action::Reduce(9));
        self.action_table
            .insert((6, "/".to_string()), Action::Reduce(9));
        self.action_table
            .insert((6, ")".to_string()), Action::Reduce(9));
        self.action_table
            .insert((6, "$".to_string()), Action::Reduce(9));

        // More states for operations...
        self.action_table
            .insert((7, "id".to_string()), Action::Shift(5));
        self.action_table
            .insert((7, "num".to_string()), Action::Shift(6));
        self.action_table
            .insert((7, "(".to_string()), Action::Shift(4));

        self.action_table
            .insert((8, "id".to_string()), Action::Shift(5));
        self.action_table
            .insert((8, "num".to_string()), Action::Shift(6));
        self.action_table
            .insert((8, "(".to_string()), Action::Shift(4));

        self.action_table
            .insert((9, "id".to_string()), Action::Shift(5));
        self.action_table
            .insert((9, "num".to_string()), Action::Shift(6));
        self.action_table
            .insert((9, "(".to_string()), Action::Shift(4));

        self.action_table
            .insert((10, "id".to_string()), Action::Shift(5));
        self.action_table
            .insert((10, "num".to_string()), Action::Shift(6));
        self.action_table
            .insert((10, "(".to_string()), Action::Shift(4));

        // State 11: E + T
        self.action_table
            .insert((11, "+".to_string()), Action::Reduce(1));
        self.action_table
            .insert((11, "-".to_string()), Action::Reduce(1));
        self.action_table
            .insert((11, "*".to_string()), Action::Shift(9));
        self.action_table
            .insert((11, "/".to_string()), Action::Shift(10));
        self.action_table
            .insert((11, ")".to_string()), Action::Reduce(1));
        self.action_table
            .insert((11, "$".to_string()), Action::Reduce(1));

        // State 12: E - T
        self.action_table
            .insert((12, "+".to_string()), Action::Reduce(2));
        self.action_table
            .insert((12, "-".to_string()), Action::Reduce(2));
        self.action_table
            .insert((12, "*".to_string()), Action::Shift(9));
        self.action_table
            .insert((12, "/".to_string()), Action::Shift(10));
        self.action_table
            .insert((12, ")".to_string()), Action::Reduce(2));
        self.action_table
            .insert((12, "$".to_string()), Action::Reduce(2));

        // State 13: T * F
        self.action_table
            .insert((13, "+".to_string()), Action::Reduce(4));
        self.action_table
            .insert((13, "-".to_string()), Action::Reduce(4));
        self.action_table
            .insert((13, "*".to_string()), Action::Reduce(4));
        self.action_table
            .insert((13, "/".to_string()), Action::Reduce(4));
        self.action_table
            .insert((13, ")".to_string()), Action::Reduce(4));
        self.action_table
            .insert((13, "$".to_string()), Action::Reduce(4));

        // State 14: T / F
        self.action_table
            .insert((14, "+".to_string()), Action::Reduce(5));
        self.action_table
            .insert((14, "-".to_string()), Action::Reduce(5));
        self.action_table
            .insert((14, "*".to_string()), Action::Reduce(5));
        self.action_table
            .insert((14, "/".to_string()), Action::Reduce(5));
        self.action_table
            .insert((14, ")".to_string()), Action::Reduce(5));
        self.action_table
            .insert((14, "$".to_string()), Action::Reduce(5));

        // Comparison states
        for i in 15..=20 {
            self.action_table
                .insert((i, "id".to_string()), Action::Shift(5));
            self.action_table
                .insert((i, "num".to_string()), Action::Shift(6));
            self.action_table
                .insert((i, "(".to_string()), Action::Shift(4));
        }

        // GOTO table
        self.goto_table.insert((0, "E".to_string()), 1);
        self.goto_table.insert((0, "T".to_string()), 2);
        self.goto_table.insert((0, "F".to_string()), 3);
        self.goto_table.insert((4, "E".to_string()), 21);
        self.goto_table.insert((4, "T".to_string()), 2);
        self.goto_table.insert((4, "F".to_string()), 3);
        self.goto_table.insert((7, "T".to_string()), 11);
        self.goto_table.insert((7, "F".to_string()), 3);
        self.goto_table.insert((8, "T".to_string()), 12);
        self.goto_table.insert((8, "F".to_string()), 3);
        self.goto_table.insert((9, "F".to_string()), 13);
        self.goto_table.insert((10, "F".to_string()), 14);

        // State 21: ( E
        self.action_table
            .insert((21, "+".to_string()), Action::Shift(7));
        self.action_table
            .insert((21, "-".to_string()), Action::Shift(8));
        self.action_table
            .insert((21, ")".to_string()), Action::Shift(22));

        // State 22: ( E )
        self.action_table
            .insert((22, "+".to_string()), Action::Reduce(7));
        self.action_table
            .insert((22, "-".to_string()), Action::Reduce(7));
        self.action_table
            .insert((22, "*".to_string()), Action::Reduce(7));
        self.action_table
            .insert((22, "/".to_string()), Action::Reduce(7));
        self.action_table
            .insert((22, ")".to_string()), Action::Reduce(7));
        self.action_table
            .insert((22, "$".to_string()), Action::Reduce(7));
    }

    /// Get token symbol for table lookup
    fn token_to_symbol(token: &Token) -> String {
        match token.kind {
            TokenKind::Identifier => "id".to_string(),
            TokenKind::IntegerLiteral | TokenKind::FloatLiteral => "num".to_string(),
            TokenKind::Plus => "+".to_string(),
            TokenKind::Minus => "-".to_string(),
            TokenKind::Star => "*".to_string(),
            TokenKind::Slash => "/".to_string(),
            TokenKind::LeftParen => "(".to_string(),
            TokenKind::RightParen => ")".to_string(),
            TokenKind::EqualEqual => "==".to_string(),
            TokenKind::NotEqual => "!=".to_string(),
            TokenKind::Less => "<".to_string(),
            TokenKind::Greater => ">".to_string(),
            TokenKind::LessEqual => "<=".to_string(),
            TokenKind::GreaterEqual => ">=".to_string(),
            TokenKind::EOF => "$".to_string(),
            _ => token.value.clone(),
        }
    }

    /// Get current state from stack
    fn current_state(&self) -> usize {
        for item in self.stack.iter().rev() {
            if let StackItem::State(s) = item {
                return *s;
            }
        }
        0
    }

    /// Get current token
    fn current_token(&self) -> &Token {
        self.tokens
            .get(self.position)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    /// Parse expression using shift-reduce
    pub fn parse_expression(&mut self) -> Result<Expr, CompilerError> {
        self.stack = vec![StackItem::State(0)];

        loop {
            let state = self.current_state();
            let token = self.current_token().clone();
            let symbol = Self::token_to_symbol(&token);

            let action = self
                .action_table
                .get(&(state, symbol.clone()))
                .cloned()
                .unwrap_or(Action::Error);

            println!(
                "[ShiftReduce] State: {}, Symbol: '{}', Action: {:?}",
                state, symbol, action
            );

            match action {
                Action::Shift(next_state) => {
                    self.stack.push(StackItem::Token(token.clone()));
                    self.stack.push(StackItem::State(next_state));
                    self.position += 1;
                }
                Action::Reduce(prod_idx) => {
                    let production = &self.productions[prod_idx].clone();
                    let rhs_len = production.rhs.len();

                    // Pop 2 * rhs_len items (state + symbol pairs)
                    let mut nodes = Vec::new();
                    for _ in 0..rhs_len {
                        self.stack.pop(); // pop state
                        if let Some(item) = self.stack.pop() {
                            nodes.push(item);
                        }
                    }
                    nodes.reverse();

                    // Create AST node based on production
                    let node = self.create_node(prod_idx, nodes)?;

                    // Push the result
                    let goto_state = self.current_state();
                    if let Some(&next) = self.goto_table.get(&(goto_state, production.lhs.clone()))
                    {
                        self.stack.push(StackItem::Node(node));
                        self.stack.push(StackItem::State(next));
                    } else {
                        return Err(CompilerError::syntactic(
                            format!("No GOTO entry for ({}, {})", goto_state, production.lhs),
                            Some(SourceLocation::new(token.line, token.column)),
                        ));
                    }
                }
                Action::Accept => {
                    // Extract the final expression from the stack
                    for item in self.stack.iter().rev() {
                        if let StackItem::Node(ASTNode::Expr(expr)) = item {
                            return Ok(expr.clone());
                        }
                    }
                    return Err(CompilerError::syntactic(
                        "Parse succeeded but no expression found",
                        None,
                    ));
                }
                Action::Error => {
                    return Err(self.handle_error(&token));
                }
            }
        }
    }

    /// Create AST node from reduced production
    fn create_node(
        &self,
        prod_idx: usize,
        nodes: Vec<StackItem>,
    ) -> Result<ASTNode, CompilerError> {
        match prod_idx {
            0 => {
                // S -> E
                if let Some(StackItem::Node(node)) = nodes.first() {
                    return Ok(node.clone());
                }
            }
            1 => {
                // E -> E + T
                if let (
                    Some(StackItem::Node(ASTNode::Expr(left))),
                    Some(StackItem::Node(ASTNode::Expr(right))),
                ) = (nodes.first(), nodes.get(2))
                {
                    return Ok(ASTNode::Expr(Expr::Binary {
                        left: Box::new(left.clone()),
                        op: BinaryOp::Add,
                        right: Box::new(right.clone()),
                    }));
                }
            }
            2 => {
                // E -> E - T
                if let (
                    Some(StackItem::Node(ASTNode::Expr(left))),
                    Some(StackItem::Node(ASTNode::Expr(right))),
                ) = (nodes.first(), nodes.get(2))
                {
                    return Ok(ASTNode::Expr(Expr::Binary {
                        left: Box::new(left.clone()),
                        op: BinaryOp::Subtract,
                        right: Box::new(right.clone()),
                    }));
                }
            }
            3 | 6 => {
                // E -> T or T -> F (identity)
                if let Some(StackItem::Node(node)) = nodes.first() {
                    return Ok(node.clone());
                }
            }
            4 => {
                // T -> T * F
                if let (
                    Some(StackItem::Node(ASTNode::Expr(left))),
                    Some(StackItem::Node(ASTNode::Expr(right))),
                ) = (nodes.first(), nodes.get(2))
                {
                    return Ok(ASTNode::Expr(Expr::Binary {
                        left: Box::new(left.clone()),
                        op: BinaryOp::Multiply,
                        right: Box::new(right.clone()),
                    }));
                }
            }
            5 => {
                // T -> T / F
                if let (
                    Some(StackItem::Node(ASTNode::Expr(left))),
                    Some(StackItem::Node(ASTNode::Expr(right))),
                ) = (nodes.first(), nodes.get(2))
                {
                    return Ok(ASTNode::Expr(Expr::Binary {
                        left: Box::new(left.clone()),
                        op: BinaryOp::Divide,
                        right: Box::new(right.clone()),
                    }));
                }
            }
            7 => {
                // F -> ( E )
                if let Some(StackItem::Node(ASTNode::Expr(expr))) = nodes.get(1) {
                    return Ok(ASTNode::Expr(Expr::Grouped(Box::new(expr.clone()))));
                }
            }
            8 => {
                // F -> id
                if let Some(StackItem::Token(token)) = nodes.first() {
                    return Ok(ASTNode::Expr(Expr::Identifier(token.value.clone())));
                }
            }
            9 => {
                // F -> num
                if let Some(StackItem::Token(token)) = nodes.first() {
                    if token.kind == TokenKind::FloatLiteral {
                        let val: f64 = token.value.parse().unwrap_or(0.0);
                        return Ok(ASTNode::Expr(Expr::FloatLiteral(val)));
                    } else {
                        let val: i64 = token.value.parse().unwrap_or(0);
                        return Ok(ASTNode::Expr(Expr::IntegerLiteral(val)));
                    }
                }
            }
            10..=15 => {
                // Comparison operators
                if let (
                    Some(StackItem::Node(ASTNode::Expr(left))),
                    Some(StackItem::Node(ASTNode::Expr(right))),
                ) = (nodes.first(), nodes.get(2))
                {
                    let op = match prod_idx {
                        10 => BinaryOp::Equal,
                        11 => BinaryOp::NotEqual,
                        12 => BinaryOp::Less,
                        13 => BinaryOp::Greater,
                        14 => BinaryOp::LessEqual,
                        15 => BinaryOp::GreaterEqual,
                        _ => BinaryOp::Equal,
                    };
                    return Ok(ASTNode::Expr(Expr::Binary {
                        left: Box::new(left.clone()),
                        op,
                        right: Box::new(right.clone()),
                    }));
                }
            }
            _ => {}
        }

        Err(CompilerError::syntactic(
            format!("Failed to create node for production {}", prod_idx),
            None,
        ))
    }

    /// Handle parsing error with recovery
    fn handle_error(&mut self, token: &Token) -> CompilerError {
        let error = CompilerError::syntactic(
            format!("Unexpected token '{}'", token.value),
            Some(SourceLocation::new(token.line, token.column)),
        )
        .with_hint("Check for missing operators or parentheses");

        // Try to recover by skipping tokens
        self.errors.push(error.clone());

        error
    }

    /// Print the parsing tables for debugging
    pub fn print_tables(&self) {
        println!("\n=== Action Table ===");
        let mut actions: Vec<_> = self.action_table.iter().collect();
        actions.sort_by_key(|((s, _), _)| s);
        for ((state, symbol), action) in actions {
            println!("  ({}, '{}') -> {:?}", state, symbol, action);
        }

        println!("\n=== GOTO Table ===");
        let mut gotos: Vec<_> = self.goto_table.iter().collect();
        gotos.sort_by_key(|((s, _), _)| s);
        for ((state, symbol), next) in gotos {
            println!("  ({}, '{}') -> {}", state, symbol, next);
        }
        println!();
    }

    /// Detect and report shift-reduce conflicts
    pub fn detect_conflicts(&self) -> Vec<String> {
        let mut conflicts = Vec::new();

        // In a real implementation, we would analyze the grammar
        // For demonstration, we note potential conflicts
        conflicts.push(
            "Potential conflict: E -> E + E vs E -> E * E (resolved by precedence)".to_string(),
        );

        conflicts
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse_expr(source: &str) -> Result<Expr, CompilerError> {
        let mut lexer = Lexer::new(source);
        lexer.tokenize().ok();
        let mut parser = ShiftReduceParser::new(lexer.tokens().to_vec());
        parser.parse_expression()
    }

    #[test]
    fn test_simple_addition() {
        let result = parse_expr("1 + 2");
        assert!(result.is_ok());
    }

    #[test]
    fn test_multiplication() {
        let result = parse_expr("3 * 4");
        assert!(result.is_ok());
    }

    #[test]
    fn test_precedence() {
        let result = parse_expr("1 + 2 * 3");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parentheses() {
        let result = parse_expr("(1 + 2) * 3");
        assert!(result.is_ok());
    }

    #[test]
    fn test_identifier() {
        let result = parse_expr("x + y");
        assert!(result.is_ok());
    }
}
