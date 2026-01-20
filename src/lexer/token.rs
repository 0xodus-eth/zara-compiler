// Token definitions for the Zara language

use std::fmt;

/// Token types in Zara
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Literals
    IntegerLiteral,
    FloatLiteral,
    StringLiteral,
    Identifier,

    // Keywords
    If,
    Else,
    While,
    Do,
    For,
    Func,
    Return,
    Let,
    Const,
    Class,
    New,
    This,
    Extends,
    Public,
    Private,
    Static,
    True,
    False,
    Null,
    Print,
    Input,
    Break,
    Continue,

    // Data Types
    TypeInteger,
    TypeFloat,
    TypeString,
    TypeBoolean,
    TypeArray,
    TypeStack,
    Void,

    // Operators
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Equal,        // =
    EqualEqual,   // ==
    NotEqual,     // !=
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=
    And,          // &&
    Or,           // ||
    Not,          // !
    Increment,    // ++
    Decrement,    // --
    PlusAssign,   // +=
    MinusAssign,  // -=
    StarAssign,   // *=
    SlashAssign,  // /=
    Arrow,        // ->
    Ampersand,    // &
    Pipe,         // |

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Semicolon,    // ;
    Colon,        // :
    Comma,        // ,
    Dot,          // .

    // Special
    EOF,
    Error,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::IntegerLiteral => write!(f, "INTEGER"),
            TokenKind::FloatLiteral => write!(f, "FLOAT"),
            TokenKind::StringLiteral => write!(f, "STRING"),
            TokenKind::Identifier => write!(f, "IDENTIFIER"),
            TokenKind::If => write!(f, "IF"),
            TokenKind::Else => write!(f, "ELSE"),
            TokenKind::While => write!(f, "WHILE"),
            TokenKind::Do => write!(f, "DO"),
            TokenKind::For => write!(f, "FOR"),
            TokenKind::Func => write!(f, "FUNC"),
            TokenKind::Return => write!(f, "RETURN"),
            TokenKind::Let => write!(f, "LET"),
            TokenKind::Const => write!(f, "CONST"),
            TokenKind::Class => write!(f, "CLASS"),
            TokenKind::New => write!(f, "NEW"),
            TokenKind::This => write!(f, "THIS"),
            TokenKind::Extends => write!(f, "EXTENDS"),
            TokenKind::Public => write!(f, "PUBLIC"),
            TokenKind::Private => write!(f, "PRIVATE"),
            TokenKind::Static => write!(f, "STATIC"),
            TokenKind::True => write!(f, "TRUE"),
            TokenKind::False => write!(f, "FALSE"),
            TokenKind::Null => write!(f, "NULL"),
            TokenKind::Print => write!(f, "PRINT"),
            TokenKind::Input => write!(f, "INPUT"),
            TokenKind::Break => write!(f, "BREAK"),
            TokenKind::Continue => write!(f, "CONTINUE"),
            TokenKind::TypeInteger => write!(f, "TYPE_INTEGER"),
            TokenKind::TypeFloat => write!(f, "TYPE_FLOAT"),
            TokenKind::TypeString => write!(f, "TYPE_STRING"),
            TokenKind::TypeBoolean => write!(f, "TYPE_BOOLEAN"),
            TokenKind::TypeArray => write!(f, "TYPE_ARRAY"),
            TokenKind::TypeStack => write!(f, "TYPE_STACK"),
            TokenKind::Void => write!(f, "VOID"),
            TokenKind::Plus => write!(f, "PLUS"),
            TokenKind::Minus => write!(f, "MINUS"),
            TokenKind::Star => write!(f, "STAR"),
            TokenKind::Slash => write!(f, "SLASH"),
            TokenKind::Percent => write!(f, "PERCENT"),
            TokenKind::Equal => write!(f, "EQUAL"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenKind::NotEqual => write!(f, "NOT_EQUAL"),
            TokenKind::Less => write!(f, "LESS"),
            TokenKind::Greater => write!(f, "GREATER"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL"),
            TokenKind::And => write!(f, "AND"),
            TokenKind::Or => write!(f, "OR"),
            TokenKind::Not => write!(f, "NOT"),
            TokenKind::Increment => write!(f, "INCREMENT"),
            TokenKind::Decrement => write!(f, "DECREMENT"),
            TokenKind::PlusAssign => write!(f, "PLUS_ASSIGN"),
            TokenKind::MinusAssign => write!(f, "MINUS_ASSIGN"),
            TokenKind::StarAssign => write!(f, "STAR_ASSIGN"),
            TokenKind::SlashAssign => write!(f, "SLASH_ASSIGN"),
            TokenKind::Arrow => write!(f, "ARROW"),
            TokenKind::Ampersand => write!(f, "AMPERSAND"),
            TokenKind::Pipe => write!(f, "PIPE"),
            TokenKind::LeftParen => write!(f, "LEFT_PAREN"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE"),
            TokenKind::LeftBracket => write!(f, "LEFT_BRACKET"),
            TokenKind::RightBracket => write!(f, "RIGHT_BRACKET"),
            TokenKind::Semicolon => write!(f, "SEMICOLON"),
            TokenKind::Colon => write!(f, "COLON"),
            TokenKind::Comma => write!(f, "COMMA"),
            TokenKind::Dot => write!(f, "DOT"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Error => write!(f, "ERROR"),
        }
    }
}

/// A token with its value and location
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, value: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            kind,
            value: value.into(),
            line,
            column,
        }
    }

    /// Check if this token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::If
                | TokenKind::Else
                | TokenKind::While
                | TokenKind::Do
                | TokenKind::For
                | TokenKind::Func
                | TokenKind::Return
                | TokenKind::Let
                | TokenKind::Const
                | TokenKind::Class
                | TokenKind::New
                | TokenKind::This
                | TokenKind::Extends
                | TokenKind::Public
                | TokenKind::Private
                | TokenKind::Static
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Null
                | TokenKind::Print
                | TokenKind::Input
                | TokenKind::Break
                | TokenKind::Continue
        )
    }

    /// Check if this token is a data type
    pub fn is_type(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::TypeInteger
                | TokenKind::TypeFloat
                | TokenKind::TypeString
                | TokenKind::TypeBoolean
                | TokenKind::TypeArray
                | TokenKind::TypeStack
                | TokenKind::Void
        )
    }

    /// Check if this token is a literal
    pub fn is_literal(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::IntegerLiteral
                | TokenKind::FloatLiteral
                | TokenKind::StringLiteral
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Null
        )
    }

    /// Check if this token is an operator
    pub fn is_operator(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Equal
                | TokenKind::EqualEqual
                | TokenKind::NotEqual
                | TokenKind::Less
                | TokenKind::Greater
                | TokenKind::LessEqual
                | TokenKind::GreaterEqual
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Not
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{:3}:{:3}] {:15} '{}'",
            self.line, self.column, self.kind, self.value
        )
    }
}
