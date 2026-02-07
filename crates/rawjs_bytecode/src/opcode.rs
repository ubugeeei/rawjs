/// Bytecode instructions for the rawjs virtual machine.
///
/// Each instruction is a flat enum variant with embedded operands.
/// Operand meanings:
///   - u16 index values refer into local/constant/global/upvalue tables
///   - i32 offsets are signed jump offsets relative to the *next* instruction

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Instruction {
    /// Push a constant from the constant pool onto the stack.
    LoadConst(u16),

    /// Push the value of a local variable onto the stack.
    LoadLocal(u16),
    /// Pop the top of the stack and store it in a local variable.
    StoreLocal(u16),

    /// Push the value of a global variable (by name-constant index) onto the stack.
    LoadGlobal(u16),
    /// Pop the top of the stack and store it in a global variable (by name-constant index).
    StoreGlobal(u16),

    /// Push the value of an upvalue onto the stack.
    LoadUpvalue(u16),
    /// Pop the top of the stack and store it in an upvalue.
    StoreUpvalue(u16),

    /// Discard the top value on the stack.
    Pop,
    /// Duplicate the top value on the stack.
    Dup,

    // ---- arithmetic ----
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,

    // ---- bitwise ----
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    UShr,

    // ---- comparison ----
    Eq,
    StrictEq,
    Ne,
    StrictNe,
    Lt,
    Le,
    Gt,
    Ge,

    // ---- unary ----
    Not,
    BitNot,
    Neg,
    Pos,
    TypeOf,
    Void,
    Delete,

    // ---- control flow ----
    /// Unconditional jump by a signed offset (relative to *next* instruction).
    Jump(i32),
    /// Pop the top; if falsy, jump by offset.
    JumpIfFalse(i32),
    /// Pop the top; if truthy, jump by offset.
    JumpIfTrue(i32),

    // ---- functions ----
    /// Call a function with `arg_count` arguments. The function and arguments must
    /// already be on the stack (function pushed first, then arguments left-to-right).
    Call(u16),
    /// Method call with `arg_count` arguments.
    /// Stack: `[..., receiver, method, arg0, arg1, ...]` -> `[..., result]`
    /// The receiver becomes `this` inside the method.
    CallMethod(u16),
    /// Return from the current function. The return value must be on the stack top.
    Return,
    /// Create a closure from the function chunk at the given constant index.
    CreateClosure(u16),

    // ---- objects / arrays ----
    /// Push a new empty object onto the stack.
    CreateObject,
    /// Create an array from the top `element_count` stack values.
    CreateArray(u16),

    /// Get a named property (name is a constant string at index).
    /// Stack: `[..., object]` -> `[..., value]`
    GetProperty(u16),
    /// Set a named property (name is a constant string at index).
    /// Stack: `[..., object, value]` -> `[..., value]`
    SetProperty(u16),

    /// Get a property by index value already on the stack.
    /// Stack: `[..., object, index]` -> `[..., value]`
    GetIndex,
    /// Set a property by index value already on the stack.
    /// Stack: `[..., object, index, value]` -> `[..., value]`
    SetIndex,

    /// Get a computed property (key on the stack).
    /// Stack: `[..., object, key]` -> `[..., value]`
    GetComputed,
    /// Set a computed property (key and value on the stack).
    /// Stack: `[..., object, key, value]` -> `[..., value]`
    SetComputed,

    // ---- exceptions ----
    /// Throw the top-of-stack value as an exception.
    Throw,
    /// Enter a try block.
    /// First i32: offset to catch handler (0 if no catch).
    /// Second i32: offset to finally handler (0 if no finally).
    EnterTry(i32, i32),
    /// Leave a try/catch/finally scope.
    LeaveTry,

    // ---- literals ----
    This,
    Undefined,
    Null,
    True,
    False,

    // ---- relational keywords ----
    In,
    Instanceof,

    // ---- iterator (for-of) ----
    /// Get the iterator from TOS object by calling [Symbol.iterator]().
    /// Stack: `[..., object]` -> `[..., iterator]`
    GetIterator,
    /// Call iterator.next() and push the result object.
    /// Stack: `[..., iterator]` -> `[..., iterator, result]`
    IteratorNext,
    /// Check if iterator result is done. If done, jump by offset.
    /// Stack: `[..., iterator, result]` -> `[..., iterator]` (pops result)
    /// If not done, also pushes the value:
    /// Stack: `[..., iterator, result]` -> `[..., iterator, value]`
    IteratorDone(i32),

    // ---- for-in iteration ----
    /// Initialize a for-in loop.
    /// Stack: `[..., object]` -> `[..., keys_array, index(0)]`
    /// Collects the object's enumerable keys into an array and pushes
    /// the array and a starting index of 0.
    ForInInit,
    /// Advance a for-in iterator.
    /// Stack: `[..., keys_array, index]` -> `[..., keys_array, index+1, key]`
    /// If index >= keys.length, jumps by offset and leaves
    /// `[..., keys_array, index]` (for cleanup).
    ForInNext(i32),

    // ---- postfix ----
    /// Postfix `++`.  The *previous* value stays on the stack; the variable is updated.
    PostfixIncrement,
    /// Postfix `--`.  The *previous* value stays on the stack; the variable is updated.
    PostfixDecrement,

    // ---- ESM ----
    /// Load a module and push its namespace object onto the stack.
    /// Operand: constant index of the module source string.
    ImportModule(u16),
    /// Get a binding from TOS namespace object and push it.
    /// Operand: constant index of the binding name string.
    ImportBinding(u16),
    /// Register a named export binding.
    /// Operand: constant index of the export name string.
    /// Stack: `[..., value]` -> `[...]`
    ExportBinding(u16),
    /// Register the default export.
    /// Stack: `[..., value]` -> `[...]`
    ExportDefault,

    // ---- generators / async ----
    /// Create a Generator object from the current closure on TOS.
    /// Stack: `[..., closure]` -> `[..., generator]`
    CreateGenerator,
    /// Yield the TOS value and suspend the generator.
    /// Stack: `[..., value]` -> `[..., received_value]` (after resume)
    Yield,
    /// Await the TOS promise and suspend the async function.
    /// Stack: `[..., value]` -> `[..., resolved_value]` (after resume)
    Await,

    // ---- resource management (using) ----
    /// Dispose a resource stored in a local slot by calling its [Symbol.dispose]() method.
    /// If the local is null/undefined, this is a no-op.
    DisposeResource(u16),
}

impl Instruction {
    /// Return a human-readable mnemonic for debugging / disassembly.
    pub fn name(&self) -> &'static str {
        match self {
            Instruction::LoadConst(_) => "LOAD_CONST",
            Instruction::LoadLocal(_) => "LOAD_LOCAL",
            Instruction::StoreLocal(_) => "STORE_LOCAL",
            Instruction::LoadGlobal(_) => "LOAD_GLOBAL",
            Instruction::StoreGlobal(_) => "STORE_GLOBAL",
            Instruction::LoadUpvalue(_) => "LOAD_UPVALUE",
            Instruction::StoreUpvalue(_) => "STORE_UPVALUE",
            Instruction::Pop => "POP",
            Instruction::Dup => "DUP",
            Instruction::Add => "ADD",
            Instruction::Sub => "SUB",
            Instruction::Mul => "MUL",
            Instruction::Div => "DIV",
            Instruction::Mod => "MOD",
            Instruction::Exp => "EXP",
            Instruction::BitAnd => "BIT_AND",
            Instruction::BitOr => "BIT_OR",
            Instruction::BitXor => "BIT_XOR",
            Instruction::Shl => "SHL",
            Instruction::Shr => "SHR",
            Instruction::UShr => "USHR",
            Instruction::Eq => "EQ",
            Instruction::StrictEq => "STRICT_EQ",
            Instruction::Ne => "NE",
            Instruction::StrictNe => "STRICT_NE",
            Instruction::Lt => "LT",
            Instruction::Le => "LE",
            Instruction::Gt => "GT",
            Instruction::Ge => "GE",
            Instruction::Not => "NOT",
            Instruction::BitNot => "BIT_NOT",
            Instruction::Neg => "NEG",
            Instruction::Pos => "POS",
            Instruction::TypeOf => "TYPEOF",
            Instruction::Void => "VOID",
            Instruction::Delete => "DELETE",
            Instruction::Jump(_) => "JUMP",
            Instruction::JumpIfFalse(_) => "JUMP_IF_FALSE",
            Instruction::JumpIfTrue(_) => "JUMP_IF_TRUE",
            Instruction::Call(_) => "CALL",
            Instruction::CallMethod(_) => "CALL_METHOD",
            Instruction::Return => "RETURN",
            Instruction::CreateClosure(_) => "CREATE_CLOSURE",
            Instruction::CreateObject => "CREATE_OBJECT",
            Instruction::CreateArray(_) => "CREATE_ARRAY",
            Instruction::GetProperty(_) => "GET_PROPERTY",
            Instruction::SetProperty(_) => "SET_PROPERTY",
            Instruction::GetIndex => "GET_INDEX",
            Instruction::SetIndex => "SET_INDEX",
            Instruction::GetComputed => "GET_COMPUTED",
            Instruction::SetComputed => "SET_COMPUTED",
            Instruction::Throw => "THROW",
            Instruction::EnterTry(_, _) => "ENTER_TRY",
            Instruction::LeaveTry => "LEAVE_TRY",
            Instruction::This => "THIS",
            Instruction::Undefined => "UNDEFINED",
            Instruction::Null => "NULL",
            Instruction::True => "TRUE",
            Instruction::False => "FALSE",
            Instruction::In => "IN",
            Instruction::Instanceof => "INSTANCEOF",
            Instruction::GetIterator => "GET_ITERATOR",
            Instruction::IteratorNext => "ITERATOR_NEXT",
            Instruction::IteratorDone(_) => "ITERATOR_DONE",
            Instruction::ForInInit => "FOR_IN_INIT",
            Instruction::ForInNext(_) => "FOR_IN_NEXT",
            Instruction::PostfixIncrement => "POSTFIX_INC",
            Instruction::PostfixDecrement => "POSTFIX_DEC",
            Instruction::ImportModule(_) => "IMPORT_MODULE",
            Instruction::ImportBinding(_) => "IMPORT_BINDING",
            Instruction::ExportBinding(_) => "EXPORT_BINDING",
            Instruction::ExportDefault => "EXPORT_DEFAULT",
            Instruction::CreateGenerator => "CREATE_GENERATOR",
            Instruction::Yield => "YIELD",
            Instruction::Await => "AWAIT",
            Instruction::DisposeResource(_) => "DISPOSE_RESOURCE",
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadConst(idx) => write!(f, "LOAD_CONST {}", idx),
            Instruction::LoadLocal(idx) => write!(f, "LOAD_LOCAL {}", idx),
            Instruction::StoreLocal(idx) => write!(f, "STORE_LOCAL {}", idx),
            Instruction::LoadGlobal(idx) => write!(f, "LOAD_GLOBAL {}", idx),
            Instruction::StoreGlobal(idx) => write!(f, "STORE_GLOBAL {}", idx),
            Instruction::LoadUpvalue(idx) => write!(f, "LOAD_UPVALUE {}", idx),
            Instruction::StoreUpvalue(idx) => write!(f, "STORE_UPVALUE {}", idx),
            Instruction::Call(argc) => write!(f, "CALL {}", argc),
            Instruction::CallMethod(argc) => write!(f, "CALL_METHOD {}", argc),
            Instruction::CreateClosure(idx) => write!(f, "CREATE_CLOSURE {}", idx),
            Instruction::CreateArray(cnt) => write!(f, "CREATE_ARRAY {}", cnt),
            Instruction::GetProperty(idx) => write!(f, "GET_PROPERTY {}", idx),
            Instruction::SetProperty(idx) => write!(f, "SET_PROPERTY {}", idx),
            Instruction::Jump(off) => write!(f, "JUMP {}", off),
            Instruction::JumpIfFalse(off) => write!(f, "JUMP_IF_FALSE {}", off),
            Instruction::JumpIfTrue(off) => write!(f, "JUMP_IF_TRUE {}", off),
            Instruction::EnterTry(catch_off, finally_off) => {
                write!(f, "ENTER_TRY catch={} finally={}", catch_off, finally_off)
            }
            Instruction::IteratorDone(off) => write!(f, "ITERATOR_DONE {}", off),
            Instruction::ForInNext(off) => write!(f, "FOR_IN_NEXT {}", off),
            Instruction::ImportModule(idx) => write!(f, "IMPORT_MODULE {}", idx),
            Instruction::ImportBinding(idx) => write!(f, "IMPORT_BINDING {}", idx),
            Instruction::ExportBinding(idx) => write!(f, "EXPORT_BINDING {}", idx),
            Instruction::DisposeResource(slot) => write!(f, "DISPOSE_RESOURCE {}", slot),
            other => write!(f, "{}", other.name()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instruction_display() {
        assert_eq!(format!("{}", Instruction::LoadConst(42)), "LOAD_CONST 42");
        assert_eq!(format!("{}", Instruction::Add), "ADD");
        assert_eq!(format!("{}", Instruction::Jump(-5)), "JUMP -5");
        assert_eq!(
            format!("{}", Instruction::EnterTry(3, 7)),
            "ENTER_TRY catch=3 finally=7"
        );
    }

    #[test]
    fn instruction_name() {
        assert_eq!(Instruction::Pop.name(), "POP");
        assert_eq!(Instruction::StoreLocal(0).name(), "STORE_LOCAL");
        assert_eq!(Instruction::CreateClosure(1).name(), "CREATE_CLOSURE");
    }

    #[test]
    fn instruction_clone_and_eq() {
        let a = Instruction::LoadConst(10);
        let b = a;
        assert_eq!(a, b);
    }
}
