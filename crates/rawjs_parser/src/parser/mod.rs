mod expressions;
mod statements;
include!("mod_parts/parser_to_expect.rs");
include!("mod_parts/expect_semicolon_to_parse_program.rs");
include!("mod_parts/parse_binding_pattern_to_object_pattern.rs");
include!("mod_parts/parse_formal_parameters_to_computed_property_end.rs");
include!("mod_parts/is_async_method_start_to_tests.rs");
