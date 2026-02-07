mod expressions;
mod statements;

use rawjs_ast::*;
use rawjs_common::{RawJsError, Result};

use crate::chunk::{Chunk, Constant};
use crate::opcode::Instruction;

// ---------------------------------------------------------------------------
// Internal bookkeeping types
// ---------------------------------------------------------------------------

/// A local variable tracked by the compiler.
#[derive(Debug, Clone)]
pub(crate) struct Local {
    pub(crate) name: String,
    pub(crate) depth: u32,
    /// Set to `true` when a nested function captures this local as an upvalue.
    /// Used by the VM to keep the value alive after the scope exits.
    #[allow(dead_code)]
    pub(crate) is_captured: bool,
}

/// Describes how an upvalue is captured.
#[derive(Debug, Clone, Copy)]
pub struct UpvalueDesc {
    /// Index in the *enclosing* scope's locals (if `is_local` is true)
    /// or in the enclosing scope's upvalue list (if `is_local` is false).
    pub index: u16,
    /// `true` if the upvalue captures a local in the immediately enclosing scope.
    pub is_local: bool,
}

/// Context saved for break/continue inside loops.
#[derive(Debug, Clone)]
pub(crate) struct LoopContext {
    /// Instruction indices of `Jump(0)` placeholders that need patching to the
    /// first instruction *after* the loop (for `break`).
    pub(crate) break_jumps: Vec<usize>,
    /// Instruction index of the loop header (target for `continue`).
    pub(crate) continue_target: usize,
    /// Optional label for labeled loops.
    pub(crate) label: Option<String>,
    /// Scope depth at the start of the loop body (so we can pop locals on break/continue).
    pub(crate) scope_depth: u32,
}

// ---------------------------------------------------------------------------
// Compiler
// ---------------------------------------------------------------------------

/// AST-to-bytecode compiler.
///
/// Each `Compiler` instance compiles exactly one lexical scope (top-level
/// script or function body).  Nested functions are compiled recursively by
/// creating a child `Compiler`.
/// Snapshot of a parent compiler's locals for upvalue resolution.
#[derive(Debug, Clone)]
pub(crate) struct ParentLocal {
    pub(crate) name: String,
    pub(crate) index: u16,
}

/// Snapshot of a parent compiler's upvalues for upvalue resolution.
#[derive(Debug, Clone)]
pub(crate) struct ParentUpvalue {
    pub(crate) name: String,
    pub(crate) index: u16,
}

pub struct Compiler {
    pub(crate) chunk: Chunk,
    pub(crate) locals: Vec<Local>,
    pub(crate) upvalues: Vec<UpvalueDesc>,
    /// Parallel to `upvalues` -- the variable name each upvalue captures.
    pub(crate) upvalue_names: Vec<String>,
    pub(crate) scope_depth: u32,
    pub(crate) loop_stack: Vec<LoopContext>,
    /// Track the maximum number of locals ever live at once.
    pub(crate) max_locals: usize,
    /// Parent compiler's locals snapshot (for upvalue resolution in nested functions).
    pub(crate) parent_locals: Vec<ParentLocal>,
    /// Parent compiler's upvalues snapshot (for chained upvalue resolution).
    pub(crate) parent_upvalues: Vec<ParentUpvalue>,
    /// Whether the current function is a generator.
    pub(crate) is_generator: bool,
    /// Whether the current function is async.
    pub(crate) is_async: bool,
    /// Stack of dispose-tracking scopes. Each entry is a list of local slots
    /// that hold `using` resources and need to be disposed when the scope exits.
    /// Outer Vec = scope stack, inner Vec = (slot, is_await) pairs.
    pub(crate) dispose_scopes: Vec<Vec<(u16, bool)>>,
}

impl Compiler {
    // ------------------------------------------------------------------
    //  Public entry point
    // ------------------------------------------------------------------

    /// Compile a parsed program into a top-level bytecode chunk.
    pub fn compile_program(program: &Program) -> Result<Chunk> {
        let mut compiler = Compiler::new("<script>");

        // Hoist import declarations to the top (ESM semantics).
        for stmt in &program.body {
            if let Statement::ImportDeclaration(import_decl) = stmt {
                compiler.compile_statement(&Statement::ImportDeclaration(import_decl.clone()))?;
            }
        }

        // Hoist function declarations to the top of the scope.
        // Also hoist exported function declarations.
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDeclaration(func) => {
                    compiler.hoist_function_declaration(func)?;
                }
                Statement::ExportDeclaration(export_decl) => {
                    if let ExportKind::Declaration(inner) = &export_decl.kind {
                        if let Statement::FunctionDeclaration(func) = inner.as_ref() {
                            compiler.hoist_function_declaration(func)?;
                        }
                    }
                }
                _ => {}
            }
        }

        for stmt in &program.body {
            // Skip already-hoisted declarations.
            if matches!(
                stmt,
                Statement::FunctionDeclaration(_) | Statement::ImportDeclaration(_)
            ) {
                continue;
            }
            compiler.compile_statement(stmt)?;
        }

        // Implicit `undefined` return for scripts.
        compiler.chunk.emit(Instruction::Undefined);
        compiler.chunk.emit(Instruction::Return);

        compiler.chunk.local_count = compiler.max_locals as u16;
        compiler.chunk.upvalue_count = compiler.upvalues.len() as u16;
        Ok(compiler.chunk)
    }

    // ------------------------------------------------------------------
    //  Constructor
    // ------------------------------------------------------------------

    pub(crate) fn new(name: &str) -> Self {
        Compiler {
            chunk: Chunk::new(name),
            locals: Vec::new(),
            upvalues: Vec::new(),
            upvalue_names: Vec::new(),
            scope_depth: 0,
            loop_stack: Vec::new(),
            max_locals: 0,
            parent_locals: Vec::new(),
            parent_upvalues: Vec::new(),
            is_generator: false,
            is_async: false,
            dispose_scopes: Vec::new(),
        }
    }

    // ------------------------------------------------------------------
    //  Scope helpers
    // ------------------------------------------------------------------

    pub(crate) fn begin_scope(&mut self) {
        self.scope_depth += 1;
        self.dispose_scopes.push(Vec::new());
    }

    pub(crate) fn end_scope(&mut self) {
        // Emit DisposeResource instructions for any `using` bindings in this scope (LIFO order).
        if let Some(dispose_slots) = self.dispose_scopes.pop() {
            for &(slot, _is_await) in dispose_slots.iter().rev() {
                self.chunk.emit(Instruction::DisposeResource(slot));
            }
        }
        self.pop_locals_to_depth(self.scope_depth);
        self.scope_depth -= 1;
    }

    /// Remove locals whose depth >= `min_depth` from the compiler's tracking.
    /// Since the VM uses a separate locals vector (not the operand stack),
    /// we do NOT emit Pop instructions here.
    pub(crate) fn pop_locals_to_depth(&mut self, min_depth: u32) {
        while let Some(local) = self.locals.last() {
            if local.depth < min_depth {
                break;
            }
            self.locals.pop();
        }
    }

    /// Declare a local in the current scope, push `Undefined` as placeholder,
    /// and return its slot index.
    pub(crate) fn declare_local(&mut self, name: &str) -> Result<u16> {
        let index = self.locals.len();
        if index > u16::MAX as usize {
            return Err(RawJsError::internal_error("Too many local variables"));
        }
        self.locals.push(Local {
            name: name.to_string(),
            depth: self.scope_depth,
            is_captured: false,
        });
        if self.locals.len() > self.max_locals {
            self.max_locals = self.locals.len();
        }
        Ok(index as u16)
    }

    /// Resolve a name to a local slot index.
    pub(crate) fn resolve_local(&self, name: &str) -> Option<u16> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some(i as u16);
            }
        }
        None
    }

    /// Resolve a name to an upvalue index by searching the parallel
    /// `upvalue_names` vector.  This only finds upvalues that were
    /// previously registered via `add_upvalue`.
    pub(crate) fn resolve_upvalue(&mut self, name: &str) -> Option<u16> {
        // First check if already captured.
        for (i, uv_name) in self.upvalue_names.iter().enumerate() {
            if uv_name == name {
                return Some(i as u16);
            }
        }

        // Check parent locals -- capture as upvalue with is_local=true.
        for pl in &self.parent_locals {
            if pl.name == name {
                let desc = UpvalueDesc {
                    index: pl.index,
                    is_local: true,
                };
                return Some(self.add_upvalue(name, desc));
            }
        }

        // Check parent upvalues -- capture as upvalue with is_local=false.
        for pu in &self.parent_upvalues {
            if pu.name == name {
                let desc = UpvalueDesc {
                    index: pu.index,
                    is_local: false,
                };
                return Some(self.add_upvalue(name, desc));
            }
        }

        None
    }

    /// Register a new upvalue (or return the index of an existing identical one).
    /// Used when compiling closures that capture variables from enclosing scopes.
    pub(crate) fn add_upvalue(&mut self, name: &str, desc: UpvalueDesc) -> u16 {
        // Check if we already captured the same source.
        for (i, existing) in self.upvalues.iter().enumerate() {
            if existing.index == desc.index && existing.is_local == desc.is_local {
                return i as u16;
            }
        }
        let idx = self.upvalues.len() as u16;
        self.upvalues.push(desc);
        self.upvalue_names.push(name.to_string());
        idx
    }

    // ------------------------------------------------------------------
    //  Constant helpers
    // ------------------------------------------------------------------

    pub(crate) fn add_number_constant(&mut self, value: f64) -> Result<u16> {
        self.chunk
            .add_constant(Constant::Number(value))
            .ok_or_else(|| RawJsError::internal_error("Constant pool overflow"))
    }

    pub(crate) fn add_string_constant(&mut self, value: &str) -> Result<u16> {
        self.chunk
            .add_constant(Constant::String(value.to_string()))
            .ok_or_else(|| RawJsError::internal_error("Constant pool overflow"))
    }

    // ------------------------------------------------------------------
    //  Emit helpers
    // ------------------------------------------------------------------

    pub(crate) fn emit(&mut self, instr: Instruction) -> usize {
        self.chunk.emit(instr)
    }

    pub(crate) fn emit_jump(&mut self, instr_fn: fn(i32) -> Instruction) -> usize {
        self.chunk.emit(instr_fn(0)) // placeholder offset
    }

    pub(crate) fn patch_jump_to_here(&mut self, index: usize) {
        let here = self.chunk.current_offset() as i32;
        let target = here - index as i32 - 1;
        self.chunk.patch_jump(index, target);
    }

    // ------------------------------------------------------------------
    //  Hoisting
    // ------------------------------------------------------------------

    pub(crate) fn hoist_function_declaration(&mut self, func: &FunctionDeclaration) -> Result<()> {
        if let Some(ref name) = func.id {
            let slot = self.declare_local(name)?;
            // Emit Undefined placeholder -- will be overwritten immediately.
            self.emit(Instruction::Undefined);
            self.compile_function_body(func)?;
            self.emit(Instruction::StoreLocal(slot));
            self.emit(Instruction::Pop);
        }
        Ok(())
    }

    // ------------------------------------------------------------------
    //  Loop context helpers
    // ------------------------------------------------------------------

    pub(crate) fn push_loop_context(&mut self, continue_target: usize, label: Option<String>) {
        self.loop_stack.push(LoopContext {
            break_jumps: Vec::new(),
            continue_target,
            label,
            scope_depth: self.scope_depth,
        });
    }

    pub(crate) fn pop_loop_context_and_patch_breaks(&mut self) {
        if let Some(ctx) = self.loop_stack.pop() {
            for jump_idx in ctx.break_jumps {
                self.patch_jump_to_here(jump_idx);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Utility functions
// ---------------------------------------------------------------------------

/// Extract a property name from an expression that is used as an object key.
pub(super) fn expression_to_property_name(expr: &Expression) -> Result<String> {
    match expr {
        Expression::Identifier(id) => Ok(id.name.clone()),
        Expression::StringLiteral(s) => Ok(s.value.clone()),
        Expression::NumberLiteral(n) => Ok(format!("{}", n.value)),
        _ => Err(RawJsError::internal_error(
            "Cannot convert expression to property name",
        )),
    }
}

/// Extract a simple name from a pattern (for parameter declaration).
pub(super) fn pattern_to_name(pat: &Pattern) -> String {
    match pat {
        Pattern::Identifier(id) => id.name.clone(),
        Pattern::Assignment(asn) => pattern_to_name(&asn.left),
        Pattern::Rest(rest) => pattern_to_name(&rest.argument),
        _ => "<destructured>".to_string(),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests;
