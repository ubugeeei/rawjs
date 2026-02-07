mod parser;

pub use parser::Parser;

pub fn parse(source: &str) -> rawjs_common::Result<rawjs_ast::Program> {
    let mut parser = Parser::new(source)?;
    parser.parse_program()
}
