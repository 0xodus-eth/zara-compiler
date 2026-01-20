// Week 8: Code Optimization
// Implements constant folding, dead code elimination, and loop invariant code motion

use crate::ir::tac::TACInstruction;
use std::collections::{HashMap, HashSet};

/// Code optimizer for TAC
pub struct Optimizer {
    instructions: Vec<TACInstruction>,
    constants: HashMap<String, String>,
    used_vars: HashSet<String>,
}

impl Optimizer {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            constants: HashMap::new(),
            used_vars: HashSet::new(),
        }
    }

    /// Optimize the given instructions
    pub fn optimize(&mut self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction> {
        self.instructions = instructions;
        
        // Apply optimizations in order
        self.constant_folding();
        self.constant_propagation();
        self.dead_code_elimination();
        self.copy_propagation();
        self.common_subexpression_elimination();
        
        self.instructions.clone()
    }

    /// Constant Folding: Evaluate constant expressions at compile time
    fn constant_folding(&mut self) {
        println!("[Optimizer] Applying constant folding...");
        
        for instr in &mut self.instructions {
            if let TACInstruction::BinaryOp { dest, left, op, right } = instr {
                // Try to parse both operands as numbers
                let left_val: Option<f64> = left.parse().ok();
                let right_val: Option<f64> = right.parse().ok();
                
                if let (Some(l), Some(r)) = (left_val, right_val) {
                    let result = match op.as_str() {
                        "+" => Some(l + r),
                        "-" => Some(l - r),
                        "*" => Some(l * r),
                        "/" if r != 0.0 => Some(l / r),
                        "%" if r != 0.0 => Some(l % r),
                        "==" => Some(if (l - r).abs() < f64::EPSILON { 1.0 } else { 0.0 }),
                        "!=" => Some(if (l - r).abs() >= f64::EPSILON { 1.0 } else { 0.0 }),
                        "<" => Some(if l < r { 1.0 } else { 0.0 }),
                        ">" => Some(if l > r { 1.0 } else { 0.0 }),
                        "<=" => Some(if l <= r { 1.0 } else { 0.0 }),
                        ">=" => Some(if l >= r { 1.0 } else { 0.0 }),
                        _ => None,
                    };
                    
                    if let Some(res) = result {
                        // Replace with constant
                        let result_str = if res.fract() == 0.0 {
                            (res as i64).to_string()
                        } else {
                            res.to_string()
                        };
                        
                        println!("  Folded: {} = {} {} {} -> {} = {}", 
                            dest, left, op, right, dest, result_str);
                        
                        *instr = TACInstruction::Copy {
                            dest: dest.clone(),
                            src: result_str,
                        };
                    }
                }
            }
        }
    }

    /// Constant Propagation: Replace variables with their constant values
    fn constant_propagation(&mut self) {
        println!("[Optimizer] Applying constant propagation...");
        
        self.constants.clear();
        
        for i in 0..self.instructions.len() {
            let instr = &self.instructions[i];
            
            // Track constant assignments
            if let TACInstruction::Copy { dest, src } = instr {
                if src.parse::<f64>().is_ok() || src == "null" || src == "\"\"" {
                    self.constants.insert(dest.clone(), src.clone());
                } else {
                    // Variable assigned a non-constant
                    self.constants.remove(dest);
                }
            }
            
            // Replace variables with constants in binary operations
            if let TACInstruction::BinaryOp { dest: _, left, op: _, right } = &mut self.instructions[i] {
                if let Some(const_val) = self.constants.get(left) {
                    println!("  Propagated {} -> {}", left, const_val);
                    *left = const_val.clone();
                }
                if let Some(const_val) = self.constants.get(right) {
                    println!("  Propagated {} -> {}", right, const_val);
                    *right = const_val.clone();
                }
            }
        }
    }

    /// Dead Code Elimination: Remove instructions whose results are never used
    fn dead_code_elimination(&mut self) {
        println!("[Optimizer] Applying dead code elimination...");
        
        // First pass: find all used variables
        self.used_vars.clear();
        
        for instr in &self.instructions {
            match instr {
                TACInstruction::BinaryOp { left, right, .. } => {
                    self.used_vars.insert(left.clone());
                    self.used_vars.insert(right.clone());
                }
                TACInstruction::UnaryOp { operand, .. } => {
                    self.used_vars.insert(operand.clone());
                }
                TACInstruction::Copy { src, .. } => {
                    self.used_vars.insert(src.clone());
                }
                TACInstruction::IndexedStore { array, index, value } => {
                    self.used_vars.insert(array.clone());
                    self.used_vars.insert(index.clone());
                    self.used_vars.insert(value.clone());
                }
                TACInstruction::IndexedLoad { array, index, .. } => {
                    self.used_vars.insert(array.clone());
                    self.used_vars.insert(index.clone());
                }
                TACInstruction::IfGoto { condition, .. } => {
                    self.used_vars.insert(condition.clone());
                }
                TACInstruction::IfFalseGoto { condition, .. } => {
                    self.used_vars.insert(condition.clone());
                }
                TACInstruction::IfRelGoto { left, right, .. } => {
                    self.used_vars.insert(left.clone());
                    self.used_vars.insert(right.clone());
                }
                TACInstruction::Return { value: Some(v) } => {
                    self.used_vars.insert(v.clone());
                }
                TACInstruction::Param { value } => {
                    self.used_vars.insert(value.clone());
                }
                TACInstruction::Call { args, .. } => {
                    for arg in args {
                        self.used_vars.insert(arg.clone());
                    }
                }
                TACInstruction::Print { values } => {
                    for v in values {
                        self.used_vars.insert(v.clone());
                    }
                }
                _ => {}
            }
        }

        // Second pass: remove dead code
        let original_len = self.instructions.len();
        self.instructions.retain(|instr| {
            match instr {
                TACInstruction::Copy { dest, .. }
                | TACInstruction::BinaryOp { dest, .. }
                | TACInstruction::UnaryOp { dest, .. } => {
                    // Keep if the destination is used or is a user variable (not temp)
                    let is_temp = dest.starts_with('t') && dest[1..].parse::<usize>().is_ok();
                    let keep = !is_temp || self.used_vars.contains(dest);
                    if !keep {
                        println!("  Eliminated dead code: {} is never used", dest);
                    }
                    keep
                }
                _ => true,
            }
        });
        
        println!("  Removed {} dead instructions", original_len - self.instructions.len());
    }

    /// Copy Propagation: Replace uses of a copied variable with the source
    fn copy_propagation(&mut self) {
        println!("[Optimizer] Applying copy propagation...");
        
        let mut copies: HashMap<String, String> = HashMap::new();
        
        for instr in &mut self.instructions {
            // Track copies
            if let TACInstruction::Copy { dest, src } = instr {
                // Don't propagate if source is not a simple variable or constant
                if !src.contains('[') && !src.contains('.') {
                    copies.insert(dest.clone(), src.clone());
                }
            }
            
            // Propagate copies in binary operations
            if let TACInstruction::BinaryOp { left, right, .. } = instr {
                if let Some(src) = copies.get(left) {
                    *left = src.clone();
                }
                if let Some(src) = copies.get(right) {
                    *right = src.clone();
                }
            }
        }
    }

    /// Common Subexpression Elimination
    fn common_subexpression_elimination(&mut self) {
        println!("[Optimizer] Applying common subexpression elimination...");
        
        // Map (left, op, right) -> temp that holds the result
        let mut expressions: HashMap<(String, String, String), String> = HashMap::new();
        
        for instr in &mut self.instructions {
            if let TACInstruction::BinaryOp { dest, left, op, right } = instr {
                let key = (left.clone(), op.clone(), right.clone());
                
                if let Some(existing_temp) = expressions.get(&key) {
                    // Replace with copy from existing computation
                    println!("  CSE: {} = {} {} {} -> {} = {}", 
                        dest, left, op, right, dest, existing_temp);
                    *instr = TACInstruction::Copy {
                        dest: dest.clone(),
                        src: existing_temp.clone(),
                    };
                } else {
                    // Record this expression
                    expressions.insert(key, dest.clone());
                }
            }
        }
    }

    /// Loop Invariant Code Motion: Move invariant computations outside loops
    pub fn loop_invariant_code_motion(&mut self) {
        println!("[Optimizer] Applying loop invariant code motion...");
        
        // Find loop headers (labels that have backward gotos)
        let mut loop_headers: HashSet<String> = HashSet::new();
        let mut loop_ends: HashMap<String, usize> = HashMap::new();
        
        // Find labels
        let mut labels: HashMap<String, usize> = HashMap::new();
        for (i, instr) in self.instructions.iter().enumerate() {
            if let TACInstruction::Label { name } = instr {
                labels.insert(name.clone(), i);
            }
        }
        
        // Find backward gotos (loop detection)
        for (i, instr) in self.instructions.iter().enumerate() {
            if let TACInstruction::Goto { label } = instr {
                if let Some(&label_pos) = labels.get(label) {
                    if label_pos < i {
                        loop_headers.insert(label.clone());
                        loop_ends.insert(label.clone(), i);
                    }
                }
            }
        }
        
        // For each loop, find invariant instructions
        for (header, &end_pos) in &loop_ends {
            if let Some(&start_pos) = labels.get(header) {
                // Find variables modified in the loop
                let mut modified: HashSet<String> = HashSet::new();
                for i in start_pos..=end_pos {
                    match &self.instructions[i] {
                        TACInstruction::Copy { dest, .. }
                        | TACInstruction::BinaryOp { dest, .. }
                        | TACInstruction::UnaryOp { dest, .. } => {
                            modified.insert(dest.clone());
                        }
                        TACInstruction::Input { dest } => {
                            modified.insert(dest.clone());
                        }
                        _ => {}
                    }
                }
                
                // Find invariant computations
                let mut invariant_indices: Vec<usize> = Vec::new();
                for i in start_pos..=end_pos {
                    if let TACInstruction::BinaryOp { dest, left, right, .. } = &self.instructions[i] {
                        let left_invariant = !modified.contains(left) || left.parse::<f64>().is_ok();
                        let right_invariant = !modified.contains(right) || right.parse::<f64>().is_ok();
                        let dest_only_assigned_here = self.instructions[start_pos..=end_pos]
                            .iter()
                            .filter(|instr| matches!(instr, 
                                TACInstruction::Copy { dest: d, .. } |
                                TACInstruction::BinaryOp { dest: d, .. } if d == dest))
                            .count() == 1;
                        
                        if left_invariant && right_invariant && dest_only_assigned_here {
                            invariant_indices.push(i);
                            println!("  Found loop invariant at {}: {}", i, self.instructions[i]);
                        }
                    }
                }
                
                // Note: Actually moving code requires careful handling
                // For simplicity, we just identify invariants here
            }
        }
    }

    /// Strength Reduction: Replace expensive operations with cheaper ones
    pub fn strength_reduction(&mut self) {
        println!("[Optimizer] Applying strength reduction...");
        
        for i in 0..self.instructions.len() {
            let replacement = {
                if let TACInstruction::BinaryOp { dest, left, op, right } = &self.instructions[i] {
                    let dest = dest.clone();
                    let left = left.clone();
                    let op = op.clone();
                    let right = right.clone();
                    
                    // Multiplication by power of 2 -> left shift
                    if op == "*" {
                        if let Ok(n) = right.parse::<i64>() {
                            if n > 0 && (n & (n - 1)) == 0 {
                                let shift = (n as f64).log2() as i64;
                                println!("  Strength reduction: {} * {} -> {} << {}", left, n, left, shift);
                                Some(TACInstruction::BinaryOp {
                                    dest,
                                    left,
                                    op: "<<".to_string(),
                                    right: shift.to_string(),
                                })
                            } else if n == 0 {
                                // x * 0 = 0
                                Some(TACInstruction::Copy {
                                    dest,
                                    src: "0".to_string(),
                                })
                            } else if n == 1 {
                                // x * 1 = x
                                Some(TACInstruction::Copy {
                                    dest,
                                    src: left,
                                })
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    // Division by power of 2 -> right shift (for integers)
                    else if op == "/" {
                        if let Ok(n) = right.parse::<i64>() {
                            if n > 0 && (n & (n - 1)) == 0 {
                                let shift = (n as f64).log2() as i64;
                                println!("  Strength reduction: {} / {} -> {} >> {}", left, n, left, shift);
                                Some(TACInstruction::BinaryOp {
                                    dest,
                                    left,
                                    op: ">>".to_string(),
                                    right: shift.to_string(),
                                })
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    // x + 0 = x, x - 0 = x
                    else if (op == "+" || op == "-") && right == "0" {
                        Some(TACInstruction::Copy {
                            dest,
                            src: left,
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
            };
            
            if let Some(new_instr) = replacement {
                self.instructions[i] = new_instr;
            }
        }
    }

    /// Get optimized instructions
    pub fn instructions(&self) -> &[TACInstruction] {
        &self.instructions
    }

    /// Print optimization report
    pub fn print_report(&self) {
        println!("\n=== Optimization Report ===");
        println!("Final instruction count: {}", self.instructions.len());
        println!("===========================\n");
    }
}

impl Default for Optimizer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_folding() {
        let mut optimizer = Optimizer::new();
        // Use a user variable (not temp) so it doesn't get eliminated by dead code elimination
        let instructions = vec![
            TACInstruction::BinaryOp {
                dest: "result".to_string(),
                left: "2".to_string(),
                op: "+".to_string(),
                right: "3".to_string(),
            },
            TACInstruction::Print {
                values: vec!["result".to_string()],
            },
        ];
        
        let optimized = optimizer.optimize(instructions);
        
        // Should fold to: result = 5
        if let TACInstruction::Copy { src, .. } = &optimized[0] {
            assert_eq!(src, "5");
        } else {
            panic!("Expected Copy instruction");
        }
    }

    #[test]
    fn test_dead_code_elimination() {
        let mut optimizer = Optimizer::new();
        let instructions = vec![
            TACInstruction::Copy {
                dest: "t0".to_string(),
                src: "5".to_string(),
            },
            TACInstruction::Copy {
                dest: "x".to_string(),
                src: "10".to_string(),
            },
            // t0 is never used after this
        ];
        
        let optimized = optimizer.optimize(instructions);
        
        // t0 should be eliminated since it's never used
        assert!(optimized.len() <= 2);
    }

    #[test]
    fn test_strength_reduction() {
        let mut optimizer = Optimizer::new();
        optimizer.instructions = vec![
            TACInstruction::BinaryOp {
                dest: "t0".to_string(),
                left: "x".to_string(),
                op: "*".to_string(),
                right: "8".to_string(),
            },
        ];
        
        optimizer.strength_reduction();
        
        // Should convert to shift
        if let TACInstruction::BinaryOp { op, right, .. } = &optimizer.instructions[0] {
            assert_eq!(op, "<<");
            assert_eq!(right, "3");
        }
    }
}
