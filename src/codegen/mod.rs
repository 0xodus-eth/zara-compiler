// Week 10: Machine Code Generation
// Generates low-level assembly code from TAC

use crate::ir::tac::TACInstruction;
use std::collections::HashMap;

/// Target architecture
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Architecture {
    X86_64,
    ARM64,
    RISCV,
}

/// Assembly instruction
#[derive(Debug, Clone)]
pub struct AsmInstruction {
    pub opcode: String,
    pub operands: Vec<String>,
    pub comment: Option<String>,
}

impl AsmInstruction {
    pub fn new(opcode: impl Into<String>, operands: Vec<String>) -> Self {
        Self {
            opcode: opcode.into(),
            operands,
            comment: None,
        }
    }

    pub fn with_comment(mut self, comment: impl Into<String>) -> Self {
        self.comment = Some(comment.into());
        self
    }
}

impl std::fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let operands_str = self.operands.join(", ");
        match &self.comment {
            Some(c) => write!(f, "    {:8} {:30} ; {}", self.opcode, operands_str, c),
            None => write!(f, "    {:8} {}", self.opcode, operands_str),
        }
    }
}

/// Code generator for machine code
pub struct CodeGenerator {
    arch: Architecture,
    instructions: Vec<String>,
    data_section: Vec<String>,
    stack_offset: i32,
    var_locations: HashMap<String, String>,
    label_counter: usize,
    string_counter: usize,
}

impl CodeGenerator {
    pub fn new(arch: Architecture) -> Self {
        Self {
            arch,
            instructions: Vec::new(),
            data_section: Vec::new(),
            stack_offset: 0,
            var_locations: HashMap::new(),
            label_counter: 0,
            string_counter: 0,
        }
    }

    /// Generate assembly code from TAC
    pub fn generate(&mut self, tac: &[TACInstruction]) -> String {
        self.emit_prologue();

        for instr in tac {
            self.emit_comment(&format!("{}", instr));
            self.gen_instruction(instr);
        }

        self.emit_epilogue();
        self.build_output()
    }

    /// Emit file prologue
    fn emit_prologue(&mut self) {
        match self.arch {
            Architecture::X86_64 => {
                self.instructions.push("section .data".to_string());
                self.instructions
                    .push("    fmt_int:    db \"%d\", 10, 0".to_string());
                self.instructions
                    .push("    fmt_float:  db \"%f\", 10, 0".to_string());
                self.instructions
                    .push("    fmt_string: db \"%s\", 10, 0".to_string());
                self.instructions.push("".to_string());
                self.instructions.push("section .bss".to_string());
                self.instructions
                    .push("    input_buf:  resb 256".to_string());
                self.instructions.push("".to_string());
                self.instructions.push("section .text".to_string());
                self.instructions.push("    global _start".to_string());
                self.instructions
                    .push("    extern printf, scanf, malloc, free".to_string());
                self.instructions.push("".to_string());
                self.instructions.push("_start:".to_string());
                self.instructions.push("    push rbp".to_string());
                self.instructions.push("    mov rbp, rsp".to_string());
            }
            Architecture::ARM64 => {
                self.instructions.push(".data".to_string());
                self.instructions
                    .push("fmt_int:    .asciz \"%d\\n\"".to_string());
                self.instructions.push("".to_string());
                self.instructions.push(".text".to_string());
                self.instructions.push(".global _start".to_string());
                self.instructions.push("".to_string());
                self.instructions.push("_start:".to_string());
                self.instructions
                    .push("    stp x29, x30, [sp, #-16]!".to_string());
                self.instructions.push("    mov x29, sp".to_string());
            }
            Architecture::RISCV => {
                self.instructions.push(".data".to_string());
                self.instructions
                    .push("fmt_int:    .asciz \"%d\\n\"".to_string());
                self.instructions.push("".to_string());
                self.instructions.push(".text".to_string());
                self.instructions.push(".global _start".to_string());
                self.instructions.push("".to_string());
                self.instructions.push("_start:".to_string());
                self.instructions.push("    addi sp, sp, -16".to_string());
                self.instructions.push("    sd ra, 8(sp)".to_string());
                self.instructions.push("    sd s0, 0(sp)".to_string());
                self.instructions.push("    addi s0, sp, 16".to_string());
            }
        }
    }

    /// Emit file epilogue
    fn emit_epilogue(&mut self) {
        match self.arch {
            Architecture::X86_64 => {
                self.instructions.push("".to_string());
                self.instructions.push("    ; Exit program".to_string());
                self.instructions.push("    mov rsp, rbp".to_string());
                self.instructions.push("    pop rbp".to_string());
                self.instructions.push("    mov rax, 60".to_string());
                self.instructions.push("    xor rdi, rdi".to_string());
                self.instructions.push("    syscall".to_string());
            }
            Architecture::ARM64 => {
                self.instructions.push("".to_string());
                self.instructions.push("    // Exit program".to_string());
                self.instructions
                    .push("    ldp x29, x30, [sp], #16".to_string());
                self.instructions.push("    mov x0, #0".to_string());
                self.instructions.push("    mov x8, #93".to_string());
                self.instructions.push("    svc #0".to_string());
            }
            Architecture::RISCV => {
                self.instructions.push("".to_string());
                self.instructions.push("    # Exit program".to_string());
                self.instructions.push("    ld ra, 8(sp)".to_string());
                self.instructions.push("    ld s0, 0(sp)".to_string());
                self.instructions.push("    addi sp, sp, 16".to_string());
                self.instructions.push("    li a7, 93".to_string());
                self.instructions.push("    li a0, 0".to_string());
                self.instructions.push("    ecall".to_string());
            }
        }
    }

    /// Generate code for a single TAC instruction
    fn gen_instruction(&mut self, instr: &TACInstruction) {
        match instr {
            TACInstruction::Copy { dest, src } => {
                self.gen_copy(dest, src);
            }
            TACInstruction::BinaryOp {
                dest,
                left,
                op,
                right,
            } => {
                self.gen_binary_op(dest, left, op, right);
            }
            TACInstruction::UnaryOp { dest, op, operand } => {
                self.gen_unary_op(dest, op, operand);
            }
            TACInstruction::Label { name } => {
                self.instructions.push(format!("{}:", name));
            }
            TACInstruction::Goto { label } => match self.arch {
                Architecture::X86_64 => {
                    self.instructions.push(format!("    jmp {}", label));
                }
                Architecture::ARM64 => {
                    self.instructions.push(format!("    b {}", label));
                }
                Architecture::RISCV => {
                    self.instructions.push(format!("    j {}", label));
                }
            },
            TACInstruction::IfGoto { condition, label } => {
                self.gen_conditional_jump(condition, label, true);
            }
            TACInstruction::IfFalseGoto { condition, label } => {
                self.gen_conditional_jump(condition, label, false);
            }
            TACInstruction::IfRelGoto {
                left,
                op,
                right,
                label,
            } => {
                self.gen_relational_jump(left, op, right, label);
            }
            TACInstruction::Call { dest, func, args } => {
                self.gen_call(dest.as_deref(), func, args);
            }
            TACInstruction::Param { value } => {
                self.gen_param(value);
            }
            TACInstruction::Return { value } => {
                self.gen_return(value.as_deref());
            }
            TACInstruction::FuncBegin { name } => {
                self.gen_func_begin(name);
            }
            TACInstruction::FuncEnd { name } => {
                self.gen_func_end(name);
            }
            TACInstruction::Print { values } => {
                self.gen_print(values);
            }
            TACInstruction::Input { dest } => {
                self.gen_input(dest);
            }
            TACInstruction::NewObject { dest, class } => {
                self.gen_new_object(dest, class);
            }
            TACInstruction::NewArray { dest, size } => {
                self.gen_new_array(dest, size);
            }
            TACInstruction::IndexedStore {
                array,
                index,
                value,
            } => {
                self.gen_indexed_store(array, index, value);
            }
            TACInstruction::IndexedLoad { dest, array, index } => {
                self.gen_indexed_load(dest, array, index);
            }
            TACInstruction::FieldLoad {
                dest,
                object,
                field,
            } => {
                self.gen_field_load(dest, object, field);
            }
            TACInstruction::FieldStore {
                object,
                field,
                value,
            } => {
                self.gen_field_store(object, field, value);
            }
            TACInstruction::Comment { text } => {
                self.emit_comment(text);
            }
        }
    }

    /// Emit a comment
    fn emit_comment(&mut self, text: &str) {
        let comment = match self.arch {
            Architecture::X86_64 => format!("    ; {}", text),
            Architecture::ARM64 => format!("    // {}", text),
            Architecture::RISCV => format!("    # {}", text),
        };
        self.instructions.push(comment);
    }

    /// Get or allocate storage for a variable
    fn get_var_location(&mut self, var: &str) -> String {
        if let Some(loc) = self.var_locations.get(var) {
            return loc.clone();
        }

        // Check if it's a constant
        if var.parse::<i64>().is_ok() || var.parse::<f64>().is_ok() {
            return var.to_string();
        }

        // Allocate stack space
        self.stack_offset += 8;
        let location = match self.arch {
            Architecture::X86_64 => format!("[rbp-{}]", self.stack_offset),
            Architecture::ARM64 => format!("[x29, #-{}]", self.stack_offset),
            Architecture::RISCV => format!("-{}(s0)", self.stack_offset),
        };
        self.var_locations.insert(var.to_string(), location.clone());
        location
    }

    /// Generate copy instruction
    fn gen_copy(&mut self, dest: &str, src: &str) {
        let dest_loc = self.get_var_location(dest);

        match self.arch {
            Architecture::X86_64 => {
                if let Ok(n) = src.parse::<i64>() {
                    self.instructions
                        .push(format!("    mov qword {}, {}", dest_loc, n));
                } else {
                    let src_loc = self.get_var_location(src);
                    self.instructions.push(format!("    mov rax, {}", src_loc));
                    self.instructions.push(format!("    mov {}, rax", dest_loc));
                }
            }
            Architecture::ARM64 => {
                if let Ok(n) = src.parse::<i64>() {
                    self.instructions.push(format!("    mov x0, #{}", n));
                    self.instructions.push(format!("    str x0, {}", dest_loc));
                } else {
                    let src_loc = self.get_var_location(src);
                    self.instructions.push(format!("    ldr x0, {}", src_loc));
                    self.instructions.push(format!("    str x0, {}", dest_loc));
                }
            }
            Architecture::RISCV => {
                if let Ok(n) = src.parse::<i64>() {
                    self.instructions.push(format!("    li t0, {}", n));
                    self.instructions.push(format!("    sd t0, {}", dest_loc));
                } else {
                    let src_loc = self.get_var_location(src);
                    self.instructions.push(format!("    ld t0, {}", src_loc));
                    self.instructions.push(format!("    sd t0, {}", dest_loc));
                }
            }
        }
    }

    /// Generate binary operation
    fn gen_binary_op(&mut self, dest: &str, left: &str, op: &str, right: &str) {
        let dest_loc = self.get_var_location(dest);

        match self.arch {
            Architecture::X86_64 => {
                // Load left operand
                if let Ok(n) = left.parse::<i64>() {
                    self.instructions.push(format!("    mov rax, {}", n));
                } else {
                    let left_loc = self.get_var_location(left);
                    self.instructions.push(format!("    mov rax, {}", left_loc));
                }

                // Load right operand
                if let Ok(n) = right.parse::<i64>() {
                    self.instructions.push(format!("    mov rbx, {}", n));
                } else {
                    let right_loc = self.get_var_location(right);
                    self.instructions
                        .push(format!("    mov rbx, {}", right_loc));
                }

                // Perform operation
                match op {
                    "+" => self.instructions.push("    add rax, rbx".to_string()),
                    "-" => self.instructions.push("    sub rax, rbx".to_string()),
                    "*" => self.instructions.push("    imul rax, rbx".to_string()),
                    "/" => {
                        self.instructions.push("    cqo".to_string());
                        self.instructions.push("    idiv rbx".to_string());
                    }
                    "%" => {
                        self.instructions.push("    cqo".to_string());
                        self.instructions.push("    idiv rbx".to_string());
                        self.instructions.push("    mov rax, rdx".to_string());
                    }
                    "==" => {
                        self.instructions.push("    cmp rax, rbx".to_string());
                        self.instructions.push("    sete al".to_string());
                        self.instructions.push("    movzx rax, al".to_string());
                    }
                    "!=" => {
                        self.instructions.push("    cmp rax, rbx".to_string());
                        self.instructions.push("    setne al".to_string());
                        self.instructions.push("    movzx rax, al".to_string());
                    }
                    "<" => {
                        self.instructions.push("    cmp rax, rbx".to_string());
                        self.instructions.push("    setl al".to_string());
                        self.instructions.push("    movzx rax, al".to_string());
                    }
                    ">" => {
                        self.instructions.push("    cmp rax, rbx".to_string());
                        self.instructions.push("    setg al".to_string());
                        self.instructions.push("    movzx rax, al".to_string());
                    }
                    "<=" => {
                        self.instructions.push("    cmp rax, rbx".to_string());
                        self.instructions.push("    setle al".to_string());
                        self.instructions.push("    movzx rax, al".to_string());
                    }
                    ">=" => {
                        self.instructions.push("    cmp rax, rbx".to_string());
                        self.instructions.push("    setge al".to_string());
                        self.instructions.push("    movzx rax, al".to_string());
                    }
                    "&&" => {
                        self.instructions.push("    and rax, rbx".to_string());
                    }
                    "||" => {
                        self.instructions.push("    or rax, rbx".to_string());
                    }
                    "<<" => {
                        self.instructions.push("    mov rcx, rbx".to_string());
                        self.instructions.push("    shl rax, cl".to_string());
                    }
                    ">>" => {
                        self.instructions.push("    mov rcx, rbx".to_string());
                        self.instructions.push("    sar rax, cl".to_string());
                    }
                    _ => {}
                }

                self.instructions.push(format!("    mov {}, rax", dest_loc));
            }
            Architecture::ARM64 => {
                self.load_arm64_operand("x0", left);
                self.load_arm64_operand("x1", right);

                match op {
                    "+" => self.instructions.push("    add x0, x0, x1".to_string()),
                    "-" => self.instructions.push("    sub x0, x0, x1".to_string()),
                    "*" => self.instructions.push("    mul x0, x0, x1".to_string()),
                    "/" => self.instructions.push("    sdiv x0, x0, x1".to_string()),
                    _ => {}
                }

                self.instructions.push(format!("    str x0, {}", dest_loc));
            }
            Architecture::RISCV => {
                self.load_riscv_operand("t0", left);
                self.load_riscv_operand("t1", right);

                match op {
                    "+" => self.instructions.push("    add t0, t0, t1".to_string()),
                    "-" => self.instructions.push("    sub t0, t0, t1".to_string()),
                    "*" => self.instructions.push("    mul t0, t0, t1".to_string()),
                    "/" => self.instructions.push("    div t0, t0, t1".to_string()),
                    _ => {}
                }

                self.instructions.push(format!("    sd t0, {}", dest_loc));
            }
        }
    }

    fn load_arm64_operand(&mut self, reg: &str, operand: &str) {
        if let Ok(n) = operand.parse::<i64>() {
            self.instructions.push(format!("    mov {}, #{}", reg, n));
        } else {
            let loc = self.get_var_location(operand);
            self.instructions.push(format!("    ldr {}, {}", reg, loc));
        }
    }

    fn load_riscv_operand(&mut self, reg: &str, operand: &str) {
        if let Ok(n) = operand.parse::<i64>() {
            self.instructions.push(format!("    li {}, {}", reg, n));
        } else {
            let loc = self.get_var_location(operand);
            self.instructions.push(format!("    ld {}, {}", reg, loc));
        }
    }

    /// Generate unary operation
    fn gen_unary_op(&mut self, dest: &str, op: &str, operand: &str) {
        let dest_loc = self.get_var_location(dest);
        let op_loc = self.get_var_location(operand);

        match self.arch {
            Architecture::X86_64 => {
                self.instructions.push(format!("    mov rax, {}", op_loc));
                match op {
                    "-" => self.instructions.push("    neg rax".to_string()),
                    "!" => {
                        self.instructions.push("    test rax, rax".to_string());
                        self.instructions.push("    sete al".to_string());
                        self.instructions.push("    movzx rax, al".to_string());
                    }
                    _ => {}
                }
                self.instructions.push(format!("    mov {}, rax", dest_loc));
            }
            Architecture::ARM64 => {
                self.instructions.push(format!("    ldr x0, {}", op_loc));
                match op {
                    "-" => self.instructions.push("    neg x0, x0".to_string()),
                    "!" => {
                        self.instructions.push("    cmp x0, #0".to_string());
                        self.instructions.push("    cset x0, eq".to_string());
                    }
                    _ => {}
                }
                self.instructions.push(format!("    str x0, {}", dest_loc));
            }
            Architecture::RISCV => {
                self.instructions.push(format!("    ld t0, {}", op_loc));
                match op {
                    "-" => self.instructions.push("    neg t0, t0".to_string()),
                    "!" => self.instructions.push("    seqz t0, t0".to_string()),
                    _ => {}
                }
                self.instructions.push(format!("    sd t0, {}", dest_loc));
            }
        }
    }

    /// Generate conditional jump
    fn gen_conditional_jump(&mut self, condition: &str, label: &str, jump_if_true: bool) {
        let cond_loc = self.get_var_location(condition);

        match self.arch {
            Architecture::X86_64 => {
                self.instructions.push(format!("    mov rax, {}", cond_loc));
                self.instructions.push("    test rax, rax".to_string());
                if jump_if_true {
                    self.instructions.push(format!("    jnz {}", label));
                } else {
                    self.instructions.push(format!("    jz {}", label));
                }
            }
            Architecture::ARM64 => {
                self.instructions.push(format!("    ldr x0, {}", cond_loc));
                self.instructions.push("    cmp x0, #0".to_string());
                if jump_if_true {
                    self.instructions.push(format!("    bne {}", label));
                } else {
                    self.instructions.push(format!("    beq {}", label));
                }
            }
            Architecture::RISCV => {
                self.instructions.push(format!("    ld t0, {}", cond_loc));
                if jump_if_true {
                    self.instructions.push(format!("    bnez t0, {}", label));
                } else {
                    self.instructions.push(format!("    beqz t0, {}", label));
                }
            }
        }
    }

    /// Generate relational jump
    fn gen_relational_jump(&mut self, left: &str, op: &str, right: &str, label: &str) {
        match self.arch {
            Architecture::X86_64 => {
                if let Ok(n) = left.parse::<i64>() {
                    self.instructions.push(format!("    mov rax, {}", n));
                } else {
                    let left_loc = self.get_var_location(left);
                    self.instructions.push(format!("    mov rax, {}", left_loc));
                }

                if let Ok(n) = right.parse::<i64>() {
                    self.instructions.push(format!("    cmp rax, {}", n));
                } else {
                    let right_loc = self.get_var_location(right);
                    self.instructions
                        .push(format!("    cmp rax, {}", right_loc));
                }

                let jmp = match op {
                    "==" => "je",
                    "!=" => "jne",
                    "<" => "jl",
                    ">" => "jg",
                    "<=" => "jle",
                    ">=" => "jge",
                    _ => "jmp",
                };
                self.instructions.push(format!("    {} {}", jmp, label));
            }
            _ => {
                // Simplified for other architectures
                self.emit_comment(&format!("if {} {} {} goto {}", left, op, right, label));
            }
        }
    }

    /// Generate function call
    fn gen_call(&mut self, dest: Option<&str>, func: &str, args: &[String]) {
        match self.arch {
            Architecture::X86_64 => {
                // Push arguments in reverse order (cdecl convention)
                let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                for (i, arg) in args.iter().enumerate() {
                    if i < arg_regs.len() {
                        if let Ok(n) = arg.parse::<i64>() {
                            self.instructions
                                .push(format!("    mov {}, {}", arg_regs[i], n));
                        } else {
                            let arg_loc = self.get_var_location(arg);
                            self.instructions
                                .push(format!("    mov {}, {}", arg_regs[i], arg_loc));
                        }
                    } else {
                        // Push to stack
                        if let Ok(n) = arg.parse::<i64>() {
                            self.instructions.push(format!("    push {}", n));
                        } else {
                            let arg_loc = self.get_var_location(arg);
                            self.instructions
                                .push(format!("    push qword {}", arg_loc));
                        }
                    }
                }

                self.instructions.push(format!("    call {}", func));

                // Clean up stack if needed
                if args.len() > 6 {
                    let cleanup = (args.len() - 6) * 8;
                    self.instructions.push(format!("    add rsp, {}", cleanup));
                }

                // Store result
                if let Some(d) = dest {
                    let dest_loc = self.get_var_location(d);
                    self.instructions.push(format!("    mov {}, rax", dest_loc));
                }
            }
            _ => {
                self.emit_comment(&format!("call {}({})", func, args.join(", ")));
                if let Some(d) = dest {
                    self.emit_comment(&format!("{} = result", d));
                }
            }
        }
    }

    fn gen_param(&mut self, value: &str) {
        self.emit_comment(&format!("param {}", value));
    }

    fn gen_return(&mut self, value: Option<&str>) {
        match self.arch {
            Architecture::X86_64 => {
                if let Some(v) = value {
                    if let Ok(n) = v.parse::<i64>() {
                        self.instructions.push(format!("    mov rax, {}", n));
                    } else {
                        let v_loc = self.get_var_location(v);
                        self.instructions.push(format!("    mov rax, {}", v_loc));
                    }
                }
                self.instructions.push("    mov rsp, rbp".to_string());
                self.instructions.push("    pop rbp".to_string());
                self.instructions.push("    ret".to_string());
            }
            _ => {
                if let Some(v) = value {
                    self.emit_comment(&format!("return {}", v));
                } else {
                    self.emit_comment("return");
                }
            }
        }
    }

    fn gen_func_begin(&mut self, name: &str) {
        self.instructions.push("".to_string());
        self.instructions.push(format!("{}:", name));

        match self.arch {
            Architecture::X86_64 => {
                self.instructions.push("    push rbp".to_string());
                self.instructions.push("    mov rbp, rsp".to_string());
            }
            Architecture::ARM64 => {
                self.instructions
                    .push("    stp x29, x30, [sp, #-16]!".to_string());
                self.instructions.push("    mov x29, sp".to_string());
            }
            Architecture::RISCV => {
                self.instructions.push("    addi sp, sp, -16".to_string());
                self.instructions.push("    sd ra, 8(sp)".to_string());
                self.instructions.push("    sd s0, 0(sp)".to_string());
                self.instructions.push("    addi s0, sp, 16".to_string());
            }
        }

        // Reset stack offset for new function
        self.stack_offset = 0;
        self.var_locations.clear();
    }

    fn gen_func_end(&mut self, _name: &str) {
        // Return is handled separately
    }

    fn gen_print(&mut self, values: &[String]) {
        for value in values {
            match self.arch {
                Architecture::X86_64 => {
                    if let Ok(n) = value.parse::<i64>() {
                        self.instructions.push(format!("    mov rsi, {}", n));
                    } else if value.starts_with('"') {
                        // String literal
                        let str_label = format!("str_{}", self.string_counter);
                        self.string_counter += 1;
                        self.data_section
                            .push(format!("    {}: db {}, 0", str_label, value));
                        self.instructions
                            .push(format!("    lea rsi, [{}]", str_label));
                        self.instructions
                            .push("    lea rdi, [fmt_string]".to_string());
                        self.instructions.push("    xor rax, rax".to_string());
                        self.instructions.push("    call printf".to_string());
                        continue;
                    } else {
                        let v_loc = self.get_var_location(value);
                        self.instructions.push(format!("    mov rsi, {}", v_loc));
                    }
                    self.instructions.push("    lea rdi, [fmt_int]".to_string());
                    self.instructions.push("    xor rax, rax".to_string());
                    self.instructions.push("    call printf".to_string());
                }
                _ => {
                    self.emit_comment(&format!("print {}", value));
                }
            }
        }
    }

    fn gen_input(&mut self, dest: &str) {
        match self.arch {
            Architecture::X86_64 => {
                self.instructions.push("    lea rdi, [fmt_int]".to_string());
                let dest_loc = self.get_var_location(dest);
                self.instructions.push(format!("    lea rsi, {}", dest_loc));
                self.instructions.push("    xor rax, rax".to_string());
                self.instructions.push("    call scanf".to_string());
            }
            _ => {
                self.emit_comment(&format!("input {}", dest));
            }
        }
    }

    fn gen_new_object(&mut self, dest: &str, class: &str) {
        // Allocate memory for object (simplified: 64 bytes per object)
        match self.arch {
            Architecture::X86_64 => {
                self.instructions.push("    mov rdi, 64".to_string());
                self.instructions.push("    call malloc".to_string());
                let dest_loc = self.get_var_location(dest);
                self.instructions.push(format!("    mov {}, rax", dest_loc));
            }
            _ => {
                self.emit_comment(&format!("{} = new {}", dest, class));
            }
        }
    }

    fn gen_new_array(&mut self, dest: &str, size: &str) {
        match self.arch {
            Architecture::X86_64 => {
                if let Ok(n) = size.parse::<i64>() {
                    self.instructions.push(format!("    mov rdi, {}", n * 8));
                } else {
                    let size_loc = self.get_var_location(size);
                    self.instructions.push(format!("    mov rdi, {}", size_loc));
                    self.instructions.push("    shl rdi, 3".to_string()); // multiply by 8
                }
                self.instructions.push("    call malloc".to_string());
                let dest_loc = self.get_var_location(dest);
                self.instructions.push(format!("    mov {}, rax", dest_loc));
            }
            _ => {
                self.emit_comment(&format!("{} = new array[{}]", dest, size));
            }
        }
    }

    fn gen_indexed_store(&mut self, array: &str, index: &str, value: &str) {
        match self.arch {
            Architecture::X86_64 => {
                let array_loc = self.get_var_location(array);
                self.instructions
                    .push(format!("    mov rax, {}", array_loc));

                if let Ok(n) = index.parse::<i64>() {
                    self.instructions.push(format!("    mov rbx, {}", n));
                } else {
                    let index_loc = self.get_var_location(index);
                    self.instructions
                        .push(format!("    mov rbx, {}", index_loc));
                }

                if let Ok(n) = value.parse::<i64>() {
                    self.instructions
                        .push(format!("    mov qword [rax + rbx*8], {}", n));
                } else {
                    let value_loc = self.get_var_location(value);
                    self.instructions
                        .push(format!("    mov rcx, {}", value_loc));
                    self.instructions
                        .push("    mov [rax + rbx*8], rcx".to_string());
                }
            }
            _ => {
                self.emit_comment(&format!("{}[{}] = {}", array, index, value));
            }
        }
    }

    fn gen_indexed_load(&mut self, dest: &str, array: &str, index: &str) {
        match self.arch {
            Architecture::X86_64 => {
                let array_loc = self.get_var_location(array);
                self.instructions
                    .push(format!("    mov rax, {}", array_loc));

                if let Ok(n) = index.parse::<i64>() {
                    self.instructions.push(format!("    mov rbx, {}", n));
                } else {
                    let index_loc = self.get_var_location(index);
                    self.instructions
                        .push(format!("    mov rbx, {}", index_loc));
                }

                self.instructions
                    .push("    mov rax, [rax + rbx*8]".to_string());
                let dest_loc = self.get_var_location(dest);
                self.instructions.push(format!("    mov {}, rax", dest_loc));
            }
            _ => {
                self.emit_comment(&format!("{} = {}[{}]", dest, array, index));
            }
        }
    }

    fn gen_field_load(&mut self, dest: &str, object: &str, field: &str) {
        self.emit_comment(&format!("{} = {}.{}", dest, object, field));
        // Simplified: would need class layout information
    }

    fn gen_field_store(&mut self, object: &str, field: &str, value: &str) {
        self.emit_comment(&format!("{}.{} = {}", object, field, value));
        // Simplified: would need class layout information
    }

    /// Build the final assembly output
    fn build_output(&self) -> String {
        let mut output = String::new();

        // Insert data section entries at the right place
        if !self.data_section.is_empty() {
            let mut lines = self.instructions.clone();
            let data_idx = lines
                .iter()
                .position(|l| l.starts_with("section .data"))
                .unwrap_or(0);
            for (i, data) in self.data_section.iter().enumerate() {
                lines.insert(data_idx + 1 + i, data.clone());
            }
            output = lines.join("\n");
        } else {
            output = self.instructions.join("\n");
        }

        output
    }

    /// Print the generated assembly
    pub fn print_assembly(&self) {
        println!("\n=== Generated Assembly ({:?}) ===", self.arch);
        println!("{}", self.build_output());
        println!("=====================================\n");
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new(Architecture::X86_64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_code_gen() {
        let mut codegen = CodeGenerator::new(Architecture::X86_64);
        let tac = vec![TACInstruction::Copy {
            dest: "x".to_string(),
            src: "42".to_string(),
        }];

        let asm = codegen.generate(&tac);
        assert!(asm.contains("42"));
    }

    #[test]
    fn test_binary_op_code_gen() {
        let mut codegen = CodeGenerator::new(Architecture::X86_64);
        let tac = vec![TACInstruction::BinaryOp {
            dest: "t0".to_string(),
            left: "10".to_string(),
            op: "+".to_string(),
            right: "20".to_string(),
        }];

        let asm = codegen.generate(&tac);
        assert!(asm.contains("add"));
    }
}
