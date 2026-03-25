mod expressions;
mod statements;
include!("mod_parts/local_to_compiler.rs");
include!("mod_parts/compile_program_to_resolve_local.rs");
include!("mod_parts/resolve_local_storage_to_hoist_var_declarations.rs");
include!("mod_parts/hoist_var_declarations_in_statement_to_pop_loop_context_and_patch_breaks.rs");
include!("mod_parts/has_use_strict_directive_to_pattern_to_name.rs");
#[cfg(test)]
mod tests;
