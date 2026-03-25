mod variant;
pub use self::variant::Instruction;
pub(crate) use self::variant::*;
mod name;
pub(crate) use self::name::*;
mod instruction_fmt;
pub(crate) use self::instruction_fmt::*;
#[cfg(test)]
mod tests;
