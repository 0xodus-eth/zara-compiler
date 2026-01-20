// Week 1: Symbol Table Implementation
// Supports variable declarations with types: integer, float, string, array, stack
// Allows adding, updating, and retrieving symbols

use std::collections::HashMap;
use std::fmt;

/// Data types supported by Zara
#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Integer,
    Float,
    String,
    Boolean,
    Array(Box<DataType>, Option<usize>), // element type, optional size
    Stack(Box<DataType>),                // element type
    Class(String),                       // class name
    Function {
        params: Vec<DataType>,
        return_type: Box<DataType>,
    },
    Void,
    Unknown,
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::Integer => write!(f, "integer"),
            DataType::Float => write!(f, "float"),
            DataType::String => write!(f, "string"),
            DataType::Boolean => write!(f, "boolean"),
            DataType::Array(inner, size) => match size {
                Some(s) => write!(f, "array[{}] of {}", s, inner),
                None => write!(f, "array of {}", inner),
            },
            DataType::Stack(inner) => write!(f, "stack of {}", inner),
            DataType::Class(name) => write!(f, "class {}", name),
            DataType::Function {
                params,
                return_type,
            } => {
                let params_str: Vec<String> = params.iter().map(|p| p.to_string()).collect();
                write!(f, "func({}) -> {}", params_str.join(", "), return_type)
            }
            DataType::Void => write!(f, "void"),
            DataType::Unknown => write!(f, "unknown"),
        }
    }
}

/// Symbol kinds
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Variable,
    Constant,
    Function,
    Parameter,
    Class,
    Method,
    Field,
}

/// A symbol entry in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub data_type: DataType,
    pub kind: SymbolKind,
    pub scope_level: usize,
    pub line_declared: usize,
    pub is_initialized: bool,
    pub is_mutable: bool,
    pub attributes: HashMap<String, String>, // Additional attributes
}

impl Symbol {
    pub fn new(
        name: impl Into<String>,
        data_type: DataType,
        kind: SymbolKind,
        scope_level: usize,
        line_declared: usize,
    ) -> Self {
        Self {
            name: name.into(),
            data_type,
            kind,
            scope_level,
            line_declared,
            is_initialized: false,
            is_mutable: true,
            attributes: HashMap::new(),
        }
    }

    pub fn with_initialized(mut self, initialized: bool) -> Self {
        self.is_initialized = initialized;
        self
    }

    pub fn with_mutable(mut self, mutable: bool) -> Self {
        self.is_mutable = mutable;
        self
    }

    pub fn with_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.insert(key.into(), value.into());
        self
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} '{}' : {} (scope: {}, line: {})",
            match self.kind {
                SymbolKind::Variable => "var",
                SymbolKind::Constant => "const",
                SymbolKind::Function => "func",
                SymbolKind::Parameter => "param",
                SymbolKind::Class => "class",
                SymbolKind::Method => "method",
                SymbolKind::Field => "field",
            },
            self.name,
            self.data_type,
            self.scope_level,
            self.line_declared
        )
    }
}

/// A scope containing symbols
#[derive(Debug)]
struct Scope {
    symbols: HashMap<String, Symbol>,
    parent: Option<usize>, // Index of parent scope
    name: String,
}

impl Scope {
    fn new(name: impl Into<String>, parent: Option<usize>) -> Self {
        Self {
            symbols: HashMap::new(),
            parent,
            name: name.into(),
        }
    }
}

/// The symbol table with scope management
#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    current_scope: usize,
    scope_counter: usize,
}

impl SymbolTable {
    /// Create a new symbol table with a global scope
    pub fn new() -> Self {
        let global_scope = Scope::new("global", None);
        Self {
            scopes: vec![global_scope],
            current_scope: 0,
            scope_counter: 1,
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self, name: impl Into<String>) {
        let new_scope = Scope::new(name, Some(self.current_scope));
        self.scopes.push(new_scope);
        self.current_scope = self.scopes.len() - 1;
        self.scope_counter += 1;
    }

    /// Exit the current scope and return to parent
    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current_scope].parent {
            self.current_scope = parent;
        }
    }

    /// Get the current scope level
    pub fn current_scope_level(&self) -> usize {
        self.current_scope
    }

    /// Add a symbol to the current scope
    pub fn add(&mut self, symbol: Symbol) -> Result<(), String> {
        let scope = &mut self.scopes[self.current_scope];

        if scope.symbols.contains_key(&symbol.name) {
            return Err(format!(
                "Symbol '{}' already declared in scope '{}'",
                symbol.name, scope.name
            ));
        }

        println!("[SymbolTable] Adding: {}", symbol);
        scope.symbols.insert(symbol.name.clone(), symbol);
        Ok(())
    }

    /// Add a variable to the symbol table
    pub fn add_variable(
        &mut self,
        name: impl Into<String>,
        data_type: DataType,
        line: usize,
    ) -> Result<(), String> {
        let symbol = Symbol::new(
            name,
            data_type,
            SymbolKind::Variable,
            self.current_scope,
            line,
        );
        self.add(symbol)
    }

    /// Add a function to the symbol table
    pub fn add_function(
        &mut self,
        name: impl Into<String>,
        params: Vec<DataType>,
        return_type: DataType,
        line: usize,
    ) -> Result<(), String> {
        let func_type = DataType::Function {
            params,
            return_type: Box::new(return_type),
        };
        let symbol = Symbol::new(
            name,
            func_type,
            SymbolKind::Function,
            self.current_scope,
            line,
        );
        self.add(symbol)
    }

    /// Add a class to the symbol table
    pub fn add_class(&mut self, name: impl Into<String>, line: usize) -> Result<(), String> {
        let class_name: String = name.into();
        let symbol = Symbol::new(
            class_name.clone(),
            DataType::Class(class_name),
            SymbolKind::Class,
            self.current_scope,
            line,
        );
        self.add(symbol)
    }

    /// Look up a symbol in the current scope and all parent scopes
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let mut scope_idx = self.current_scope;

        loop {
            let scope = &self.scopes[scope_idx];
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }

            match scope.parent {
                Some(parent_idx) => scope_idx = parent_idx,
                None => return None,
            }
        }
    }

    /// Look up a symbol only in the current scope
    pub fn lookup_current_scope(&self, name: &str) -> Option<&Symbol> {
        self.scopes[self.current_scope].symbols.get(name)
    }

    /// Update a symbol's properties
    pub fn update(
        &mut self,
        name: &str,
        data_type: Option<DataType>,
        initialized: Option<bool>,
    ) -> Result<(), String> {
        let mut scope_idx = self.current_scope;

        loop {
            let scope = &mut self.scopes[scope_idx];
            if let Some(symbol) = scope.symbols.get_mut(name) {
                if let Some(dt) = data_type {
                    symbol.data_type = dt;
                }
                if let Some(init) = initialized {
                    symbol.is_initialized = init;
                }
                println!("[SymbolTable] Updated: {}", symbol);
                return Ok(());
            }

            match scope.parent {
                Some(parent_idx) => scope_idx = parent_idx,
                None => return Err(format!("Symbol '{}' not found", name)),
            }
        }
    }

    /// Get all symbols in the current scope
    pub fn get_current_scope_symbols(&self) -> Vec<&Symbol> {
        self.scopes[self.current_scope].symbols.values().collect()
    }

    /// Get all symbols across all scopes
    pub fn get_all_symbols(&self) -> Vec<&Symbol> {
        self.scopes
            .iter()
            .flat_map(|s| s.symbols.values())
            .collect()
    }

    /// Print all symbols for debugging
    pub fn dump(&self) {
        println!("\n=== Symbol Table Dump ===");
        for (idx, scope) in self.scopes.iter().enumerate() {
            println!("\nScope {} ('{}'):", idx, scope.name);
            if scope.symbols.is_empty() {
                println!("  (empty)");
            } else {
                for symbol in scope.symbols.values() {
                    println!("  {}", symbol);
                }
            }
        }
        println!("=========================\n");
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_and_lookup() {
        let mut table = SymbolTable::new();

        // Add variables
        table.add_variable("x", DataType::Integer, 1).unwrap();
        table.add_variable("name", DataType::String, 2).unwrap();

        // Lookup
        let x = table.lookup("x").unwrap();
        assert_eq!(x.name, "x");
        assert_eq!(x.data_type, DataType::Integer);

        let name = table.lookup("name").unwrap();
        assert_eq!(name.data_type, DataType::String);
    }

    #[test]
    fn test_scopes() {
        let mut table = SymbolTable::new();

        // Global scope
        table
            .add_variable("global_var", DataType::Integer, 1)
            .unwrap();

        // Enter function scope
        table.enter_scope("main");
        table.add_variable("local_var", DataType::Float, 5).unwrap();

        // Can see both
        assert!(table.lookup("global_var").is_some());
        assert!(table.lookup("local_var").is_some());

        // Exit scope
        table.exit_scope();

        // Can only see global
        assert!(table.lookup("global_var").is_some());
        assert!(table.lookup("local_var").is_none());
    }

    #[test]
    fn test_duplicate_declaration() {
        let mut table = SymbolTable::new();

        table.add_variable("x", DataType::Integer, 1).unwrap();
        let result = table.add_variable("x", DataType::Float, 2);

        assert!(result.is_err());
    }

    #[test]
    fn test_array_and_stack_types() {
        let mut table = SymbolTable::new();

        // Array of integers with size 10
        let array_type = DataType::Array(Box::new(DataType::Integer), Some(10));
        table
            .add_variable("numbers", array_type.clone(), 1)
            .unwrap();

        // Stack of strings
        let stack_type = DataType::Stack(Box::new(DataType::String));
        table
            .add_variable("string_stack", stack_type.clone(), 2)
            .unwrap();

        let numbers = table.lookup("numbers").unwrap();
        assert_eq!(numbers.data_type, array_type);

        let stack = table.lookup("string_stack").unwrap();
        assert_eq!(stack.data_type, stack_type);
    }

    #[test]
    fn test_function_symbol() {
        let mut table = SymbolTable::new();

        table
            .add_function(
                "add",
                vec![DataType::Integer, DataType::Integer],
                DataType::Integer,
                1,
            )
            .unwrap();

        let func = table.lookup("add").unwrap();
        assert_eq!(func.kind, SymbolKind::Function);

        if let DataType::Function {
            params,
            return_type,
        } = &func.data_type
        {
            assert_eq!(params.len(), 2);
            assert_eq!(**return_type, DataType::Integer);
        } else {
            panic!("Expected function type");
        }
    }
}
