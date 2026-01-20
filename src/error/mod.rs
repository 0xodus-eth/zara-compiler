// Week 11: Error Handling and Recovery
// Provides robust error detection and recovery mechanisms

use std::fmt;

/// Represents different types of compiler errors
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    Lexical,
    Syntactic,
    Semantic,
    Internal,
}

/// Represents a location in source code
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub file: Option<String>,
}

impl SourceLocation {
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            line,
            column,
            file: None,
        }
    }

    pub fn with_file(line: usize, column: usize, file: String) -> Self {
        Self {
            line,
            column,
            file: Some(file),
        }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.file {
            Some(file) => write!(f, "{}:{}:{}", file, self.line, self.column),
            None => write!(f, "line {}, column {}", self.line, self.column),
        }
    }
}

/// Main compiler error structure
#[derive(Debug, Clone)]
pub struct CompilerError {
    pub kind: ErrorKind,
    pub message: String,
    pub location: Option<SourceLocation>,
    pub hint: Option<String>,
    pub recovered: bool,
}

impl CompilerError {
    pub fn lexical(message: impl Into<String>, location: SourceLocation) -> Self {
        Self {
            kind: ErrorKind::Lexical,
            message: message.into(),
            location: Some(location),
            hint: None,
            recovered: false,
        }
    }

    pub fn syntactic(message: impl Into<String>, location: Option<SourceLocation>) -> Self {
        Self {
            kind: ErrorKind::Syntactic,
            message: message.into(),
            location,
            hint: None,
            recovered: false,
        }
    }

    pub fn semantic(message: impl Into<String>, location: Option<SourceLocation>) -> Self {
        Self {
            kind: ErrorKind::Semantic,
            message: message.into(),
            location,
            hint: None,
            recovered: false,
        }
    }

    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = Some(hint.into());
        self
    }

    pub fn mark_recovered(mut self) -> Self {
        self.recovered = true;
        self
    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind_str = match self.kind {
            ErrorKind::Lexical => "Lexical Error",
            ErrorKind::Syntactic => "Syntax Error",
            ErrorKind::Semantic => "Semantic Error",
            ErrorKind::Internal => "Internal Error",
        };

        write!(f, "[{}]", kind_str)?;

        if let Some(loc) = &self.location {
            write!(f, " at {}", loc)?;
        }

        write!(f, ": {}", self.message)?;

        if let Some(hint) = &self.hint {
            write!(f, "\n  Hint: {}", hint)?;
        }

        if self.recovered {
            write!(f, " (recovered)")?;
        }

        Ok(())
    }
}

impl std::error::Error for CompilerError {}

/// Error recovery strategies
#[derive(Debug, Clone, Copy)]
pub enum RecoveryStrategy {
    /// Skip tokens until a synchronizing token is found
    PanicMode,
    /// Insert a missing token
    Insertion,
    /// Delete the current token
    Deletion,
    /// Replace the current token
    Replacement,
}

/// Error collector for accumulating multiple errors
#[derive(Debug, Default)]
pub struct ErrorCollector {
    errors: Vec<CompilerError>,
    warnings: Vec<CompilerError>,
    max_errors: usize,
}

impl ErrorCollector {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
            max_errors: 100,
        }
    }

    pub fn with_max_errors(mut self, max: usize) -> Self {
        self.max_errors = max;
        self
    }

    pub fn add_error(&mut self, error: CompilerError) {
        if self.errors.len() < self.max_errors {
            self.errors.push(error);
        }
    }

    pub fn add_warning(&mut self, warning: CompilerError) {
        self.warnings.push(warning);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    pub fn errors(&self) -> &[CompilerError] {
        &self.errors
    }

    pub fn warnings(&self) -> &[CompilerError] {
        &self.warnings
    }

    pub fn report(&self) {
        for error in &self.errors {
            eprintln!("{}", error);
        }
        for warning in &self.warnings {
            eprintln!("Warning: {}", warning);
        }

        if !self.errors.is_empty() || !self.warnings.is_empty() {
            eprintln!(
                "\n{} error(s), {} warning(s)",
                self.errors.len(),
                self.warnings.len()
            );
        }
    }

    pub fn clear(&mut self) {
        self.errors.clear();
        self.warnings.clear();
    }
}

/// Synchronizing tokens for panic mode recovery
pub const SYNC_TOKENS: &[&str] = &[";", "}", "func", "class", "if", "while", "for", "return"];

/// Check if a token is a synchronizing token
pub fn is_sync_token(token: &str) -> bool {
    SYNC_TOKENS.contains(&token)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let error = CompilerError::lexical("Unexpected character '@'", SourceLocation::new(1, 5))
            .with_hint("Remove the invalid character");

        let display = format!("{}", error);
        assert!(display.contains("Lexical Error"));
        assert!(display.contains("line 1"));
        assert!(display.contains("Unexpected character"));
    }

    #[test]
    fn test_error_collector() {
        let mut collector = ErrorCollector::new();

        collector.add_error(CompilerError::syntactic(
            "Missing semicolon",
            Some(SourceLocation::new(10, 1)),
        ));

        assert!(collector.has_errors());
        assert_eq!(collector.error_count(), 1);
    }
}
