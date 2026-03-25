/// Bytecode instructions for the rawjs virtual machine.
///
/// Each instruction is a flat enum variant with embedded operands.
/// Operand meanings:
///  - u16 index values refer into local/constant/global/upvalue tables
///  - i32 offsets are signed jump offsets relative to the *next* instruction
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
    /// Push the value of a global variable, or `undefined` if it is unresolvable.
    LoadGlobalOrUndefined(u16),
    /// Pop the top of the stack and store it in a global variable (by name-constant index).
    StoreGlobal(u16),
    /// Initialize or overwrite a global binding without strict-mode checks.
    InitGlobal(u16),
    /// Push the value of an upvalue onto the stack.
    LoadUpvalue(u16),
    /// Pop the top of the stack and store it in an upvalue.
    StoreUpvalue(u16),
    /// Discard the top value on the stack.
    Pop,
    /// Duplicate the top value on the stack.
    Dup,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    UShr,
    Eq,
    StrictEq,
    Ne,
    StrictNe,
    Lt,
    Le,
    Gt,
    Ge,
    Not,
    BitNot,
    Neg,
    Pos,
    TypeOf,
    Void,
    Delete,
    /// Delete a named global binding.
    DeleteName(u16),
    /// Delete a property from an object.
    DeleteProperty,
    /// Unconditional jump by a signed offset (relative to *next* instruction).
    Jump(i32),
    /// Pop the top; if falsy, jump by offset.
    JumpIfFalse(i32),
    /// Pop the top; if truthy, jump by offset.
    JumpIfTrue(i32),
    /// Pop the top; if null or undefined, jump by offset.
    JumpIfNullish(i32),
    /// Call a function with `arg_count` arguments. The function and arguments must
    /// already be on the stack (function pushed first, then arguments left-to-right).
    Call(u16),
    /// Construct a new object with `arg_count` arguments.
    /// The constructor and arguments must already be on the stack.
    New(u16),
    /// Method call with `arg_count` arguments.
    /// Stack: `[..., receiver, method, arg0, arg1, ...]` -> `[..., result]`
    /// The receiver becomes `this` inside the method.
    CallMethod(u16),
    /// Return from the current function. The return value must be on the stack top.
    Return,
    /// Create a closure from the function chunk at the given constant index.
    CreateClosure(u16),
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
    /// Throw the top-of-stack value as an exception.
    Throw,
    /// Enter a try block.
    /// First i32: offset to catch handler (0 if no catch).
    /// Second i32: offset to finally handler (0 if no finally).
    EnterTry(i32, i32),
    /// Leave a try/catch/finally scope.
    LeaveTry,
    This,
    Undefined,
    Null,
    True,
    False,
    In,
    Instanceof,
    /// Create and push the current frame's `arguments` object.
    LoadArguments,
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
    /// Postfix `++`.  The *previous* value stays on the stack; the variable is updated.
    PostfixIncrement,
    /// Postfix `--`.  The *previous* value stays on the stack; the variable is updated.
    PostfixDecrement,
    /// Load a module and push its namespace object onto the stack.
    /// Operand: constant index of the module source string.
    ImportModule(u16),
    /// Pop a module specifier, load the module asynchronously, and push a Promise.
    ImportModuleDynamic,
    /// Push the current module's import.meta object.
    ImportMeta,
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
    /// Create a Generator object from the current closure on TOS.
    /// Stack: `[..., closure]` -> `[..., generator]`
    CreateGenerator,
    /// Yield the TOS value and suspend the generator.
    /// Stack: `[..., value]` -> `[..., received_value]` (after resume)
    Yield,
    /// Await the TOS promise and suspend the async function.
    /// Stack: `[..., value]` -> `[..., resolved_value]` (after resume)
    Await,
    /// Dispose a resource stored in a local slot by calling its [Symbol.dispose]() method.
    /// If the local is null/undefined, this is a no-op.
    DisposeResource(u16),
    /// Dispose a resource stored in a local slot for `await using`.
    /// Uses [Symbol.asyncDispose]() when available, otherwise falls back to
    /// [Symbol.dispose]() without awaiting its return value.
    AsyncDisposeResource(u16),
}

use super::*;
