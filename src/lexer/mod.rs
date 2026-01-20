// Week 2: Lexical Analysis
// Creates a lexical analyzer for Zara that identifies tokens
// Uses regular expressions for token recognition

pub mod token;

pub use token::{Token, TokenKind};

use crate::error::{CompilerError, SourceLocation};

/// The lexical analyzer for Zara
pub struct Lexer {
    source: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
    tokens: Vec<Token>,
    errors: Vec<CompilerError>,
}

impl Lexer {
    /// Create a new lexer with source code
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Get current character
    fn current(&self) -> Option<char> {
        self.source.get(self.position).copied()
    }

    /// Peek at next character without consuming
    fn peek(&self) -> Option<char> {
        self.source.get(self.position + 1).copied()
    }

    /// Advance to next character
    fn advance(&mut self) -> Option<char> {
        let ch = self.current();
        if ch.is_some() {
            self.position += 1;
            self.column += 1;
        }
        ch
    }

    /// Skip whitespace and track newlines
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            match ch {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    self.advance();
                    self.line += 1;
                    self.column = 1;
                }
                _ => break,
            }
        }
    }

    /// Skip single-line comments
    fn skip_line_comment(&mut self) {
        while let Some(ch) = self.current() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    /// Skip multi-line comments
    fn skip_block_comment(&mut self) {
        self.advance(); // skip '/'
        self.advance(); // skip '*'

        while let Some(ch) = self.current() {
            if ch == '*' && self.peek() == Some('/') {
                self.advance(); // skip '*'
                self.advance(); // skip '/'
                return;
            }
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            }
            self.advance();
        }

        self.errors.push(CompilerError::lexical(
            "Unterminated block comment",
            SourceLocation::new(self.line, self.column),
        ));
    }

    /// Read an identifier or keyword
    fn read_identifier(&mut self) -> Token {
        let start_line = self.line;
        let start_col = self.column;
        let mut value = String::new();

        while let Some(ch) = self.current() {
            if ch.is_alphanumeric() || ch == '_' {
                value.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let kind = match value.as_str() {
            // Keywords
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "do" => TokenKind::Do,
            "for" => TokenKind::For,
            "func" => TokenKind::Func,
            "return" => TokenKind::Return,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "class" => TokenKind::Class,
            "new" => TokenKind::New,
            "this" => TokenKind::This,
            "extends" => TokenKind::Extends,
            "public" => TokenKind::Public,
            "private" => TokenKind::Private,
            "static" => TokenKind::Static,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            "print" => TokenKind::Print,
            "input" => TokenKind::Input,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            // Data types
            "integer" => TokenKind::TypeInteger,
            "float" => TokenKind::TypeFloat,
            "string" => TokenKind::TypeString,
            "boolean" => TokenKind::TypeBoolean,
            "array" => TokenKind::TypeArray,
            "stack" => TokenKind::TypeStack,
            "void" => TokenKind::Void,
            // Identifier
            _ => TokenKind::Identifier,
        };

        Token::new(kind, value, start_line, start_col)
    }

    /// Read a number (integer or float)
    fn read_number(&mut self) -> Token {
        let start_line = self.line;
        let start_col = self.column;
        let mut value = String::new();
        let mut is_float = false;

        while let Some(ch) = self.current() {
            if ch.is_ascii_digit() {
                value.push(ch);
                self.advance();
            } else if ch == '.' && !is_float {
                if let Some(next) = self.peek() {
                    if next.is_ascii_digit() {
                        is_float = true;
                        value.push(ch);
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let kind = if is_float {
            TokenKind::FloatLiteral
        } else {
            TokenKind::IntegerLiteral
        };

        Token::new(kind, value, start_line, start_col)
    }

    /// Read a string literal
    fn read_string(&mut self) -> Token {
        let start_line = self.line;
        let start_col = self.column;
        let quote = self.advance().unwrap(); // consume opening quote
        let mut value = String::new();

        while let Some(ch) = self.current() {
            if ch == quote {
                self.advance(); // consume closing quote
                return Token::new(TokenKind::StringLiteral, value, start_line, start_col);
            } else if ch == '\\' {
                self.advance();
                if let Some(escaped) = self.current() {
                    let escaped_char = match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        '\'' => '\'',
                        _ => escaped,
                    };
                    value.push(escaped_char);
                    self.advance();
                }
            } else if ch == '\n' {
                self.errors.push(CompilerError::lexical(
                    "Unterminated string literal",
                    SourceLocation::new(start_line, start_col),
                ));
                break;
            } else {
                value.push(ch);
                self.advance();
            }
        }

        Token::new(TokenKind::StringLiteral, value, start_line, start_col)
    }

    /// Tokenize the entire source code
    pub fn tokenize(&mut self) -> Result<&[Token], &[CompilerError]> {
        while self.position < self.source.len() {
            self.skip_whitespace();

            if self.position >= self.source.len() {
                break;
            }

            let ch = self.current().unwrap();
            let start_line = self.line;
            let start_col = self.column;

            // Comments
            if ch == '/' {
                if self.peek() == Some('/') {
                    self.skip_line_comment();
                    continue;
                } else if self.peek() == Some('*') {
                    self.skip_block_comment();
                    continue;
                }
            }

            let token = match ch {
                // Identifiers and keywords
                'a'..='z' | 'A'..='Z' | '_' => self.read_identifier(),

                // Numbers
                '0'..='9' => self.read_number(),

                // Strings
                '"' | '\'' => self.read_string(),

                // Operators and punctuation
                '+' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::PlusAssign, "+=", start_line, start_col)
                    } else if self.current() == Some('+') {
                        self.advance();
                        Token::new(TokenKind::Increment, "++", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Plus, "+", start_line, start_col)
                    }
                }
                '-' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::MinusAssign, "-=", start_line, start_col)
                    } else if self.current() == Some('-') {
                        self.advance();
                        Token::new(TokenKind::Decrement, "--", start_line, start_col)
                    } else if self.current() == Some('>') {
                        self.advance();
                        Token::new(TokenKind::Arrow, "->", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Minus, "-", start_line, start_col)
                    }
                }
                '*' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::StarAssign, "*=", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Star, "*", start_line, start_col)
                    }
                }
                '/' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::SlashAssign, "/=", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Slash, "/", start_line, start_col)
                    }
                }
                '%' => {
                    self.advance();
                    Token::new(TokenKind::Percent, "%", start_line, start_col)
                }
                '=' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::EqualEqual, "==", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Equal, "=", start_line, start_col)
                    }
                }
                '!' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::NotEqual, "!=", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Not, "!", start_line, start_col)
                    }
                }
                '<' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::LessEqual, "<=", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Less, "<", start_line, start_col)
                    }
                }
                '>' => {
                    self.advance();
                    if self.current() == Some('=') {
                        self.advance();
                        Token::new(TokenKind::GreaterEqual, ">=", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Greater, ">", start_line, start_col)
                    }
                }
                '&' => {
                    self.advance();
                    if self.current() == Some('&') {
                        self.advance();
                        Token::new(TokenKind::And, "&&", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Ampersand, "&", start_line, start_col)
                    }
                }
                '|' => {
                    self.advance();
                    if self.current() == Some('|') {
                        self.advance();
                        Token::new(TokenKind::Or, "||", start_line, start_col)
                    } else {
                        Token::new(TokenKind::Pipe, "|", start_line, start_col)
                    }
                }
                '(' => {
                    self.advance();
                    Token::new(TokenKind::LeftParen, "(", start_line, start_col)
                }
                ')' => {
                    self.advance();
                    Token::new(TokenKind::RightParen, ")", start_line, start_col)
                }
                '{' => {
                    self.advance();
                    Token::new(TokenKind::LeftBrace, "{", start_line, start_col)
                }
                '}' => {
                    self.advance();
                    Token::new(TokenKind::RightBrace, "}", start_line, start_col)
                }
                '[' => {
                    self.advance();
                    Token::new(TokenKind::LeftBracket, "[", start_line, start_col)
                }
                ']' => {
                    self.advance();
                    Token::new(TokenKind::RightBracket, "]", start_line, start_col)
                }
                ';' => {
                    self.advance();
                    Token::new(TokenKind::Semicolon, ";", start_line, start_col)
                }
                ':' => {
                    self.advance();
                    Token::new(TokenKind::Colon, ":", start_line, start_col)
                }
                ',' => {
                    self.advance();
                    Token::new(TokenKind::Comma, ",", start_line, start_col)
                }
                '.' => {
                    self.advance();
                    Token::new(TokenKind::Dot, ".", start_line, start_col)
                }
                _ => {
                    // Error recovery: skip unknown character
                    self.errors.push(
                        CompilerError::lexical(
                            format!("Unexpected character '{}'", ch),
                            SourceLocation::new(start_line, start_col),
                        )
                        .with_hint("Remove or replace this character"),
                    );
                    self.advance();
                    continue;
                }
            };

            self.tokens.push(token);
        }

        // Add EOF token
        self.tokens
            .push(Token::new(TokenKind::EOF, "", self.line, self.column));

        if self.errors.is_empty() {
            Ok(&self.tokens)
        } else {
            Err(&self.errors)
        }
    }

    /// Get all tokens (even if there were errors)
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// Get all lexical errors
    pub fn errors(&self) -> &[CompilerError] {
        &self.errors
    }

    /// Print tokens for debugging
    pub fn print_tokens(&self) {
        println!("\n=== Tokens ===");
        for token in &self.tokens {
            println!("{}", token);
        }
        println!("==============\n");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("if else while do for func return");
        lexer.tokenize().unwrap();

        let kinds: Vec<TokenKind> = lexer.tokens().iter().map(|t| t.kind.clone()).collect();
        assert_eq!(kinds[0], TokenKind::If);
        assert_eq!(kinds[1], TokenKind::Else);
        assert_eq!(kinds[2], TokenKind::While);
        assert_eq!(kinds[3], TokenKind::Do);
        assert_eq!(kinds[4], TokenKind::For);
        assert_eq!(kinds[5], TokenKind::Func);
        assert_eq!(kinds[6], TokenKind::Return);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * / = == != < > <= >= && ||");
        lexer.tokenize().unwrap();

        let kinds: Vec<TokenKind> = lexer.tokens().iter().map(|t| t.kind.clone()).collect();
        assert_eq!(kinds[0], TokenKind::Plus);
        assert_eq!(kinds[1], TokenKind::Minus);
        assert_eq!(kinds[4], TokenKind::Equal);
        assert_eq!(kinds[5], TokenKind::EqualEqual);
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new("42 3.14 100");
        lexer.tokenize().unwrap();

        assert_eq!(lexer.tokens()[0].kind, TokenKind::IntegerLiteral);
        assert_eq!(lexer.tokens()[0].value, "42");
        assert_eq!(lexer.tokens()[1].kind, TokenKind::FloatLiteral);
        assert_eq!(lexer.tokens()[1].value, "3.14");
    }

    #[test]
    fn test_strings() {
        let mut lexer = Lexer::new(r#""hello" "world""#);
        lexer.tokenize().unwrap();

        assert_eq!(lexer.tokens()[0].kind, TokenKind::StringLiteral);
        assert_eq!(lexer.tokens()[0].value, "hello");
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("myVar _private count123");
        lexer.tokenize().unwrap();

        assert_eq!(lexer.tokens()[0].kind, TokenKind::Identifier);
        assert_eq!(lexer.tokens()[0].value, "myVar");
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("x // this is a comment\ny /* block */ z");
        lexer.tokenize().unwrap();

        let values: Vec<&str> = lexer
            .tokens()
            .iter()
            .filter(|t| t.kind == TokenKind::Identifier)
            .map(|t| t.value.as_str())
            .collect();
        assert_eq!(values, vec!["x", "y", "z"]);
    }

    #[test]
    fn test_data_types() {
        let mut lexer = Lexer::new("integer float string boolean array stack");
        lexer.tokenize().unwrap();

        assert_eq!(lexer.tokens()[0].kind, TokenKind::TypeInteger);
        assert_eq!(lexer.tokens()[1].kind, TokenKind::TypeFloat);
        assert_eq!(lexer.tokens()[4].kind, TokenKind::TypeArray);
        assert_eq!(lexer.tokens()[5].kind, TokenKind::TypeStack);
    }
}
