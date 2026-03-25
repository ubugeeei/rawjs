#[path = "opcode/variant.rs"]
mod variant;
#[allow(unused_imports)]
pub use self::variant::Instruction;
#[allow(unused_imports)]
use self::variant::*;
#[path = "opcode/name.rs"]
mod name;
#[allow(unused_imports)]
use self::name::*;
#[path = "opcode/instruction_fmt.rs"]
mod instruction_fmt;
#[allow(unused_imports)]
use self::instruction_fmt::*;
#[cfg(test)]
#[path = "opcode/tests.rs"]
mod tests;
