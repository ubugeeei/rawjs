#[allow(unused_imports)]
use crate::token::{Token, TokenKind};
#[allow(unused_imports)]
use rawjs_common::{RawJsError, Result, SourceLocation};

#[path = "lexer/lexer_to_read_token_kind.rs"]
mod lexer_to_read_token_kind;
#[allow(unused_imports)]
pub use self::lexer_to_read_token_kind::Lexer;
#[allow(unused_imports)]
use self::lexer_to_read_token_kind::*;
#[path = "lexer/read_simple_punctuation_to_ampersand_token.rs"]
mod read_simple_punctuation_to_ampersand_token;
#[allow(unused_imports)]
use self::read_simple_punctuation_to_ampersand_token::*;
#[path = "lexer/read_pipe_token_to_number.rs"]
mod read_pipe_token_to_number;
#[allow(unused_imports)]
use self::read_pipe_token_to_number::*;
#[path = "lexer/read_string_to_template_middle_or_tail.rs"]
mod read_string_to_template_middle_or_tail;
#[allow(unused_imports)]
use self::read_string_to_template_middle_or_tail::*;
#[path = "lexer/read_identifier_or_keyword_to_tests.rs"]
mod read_identifier_or_keyword_to_tests;
#[allow(unused_imports)]
use self::read_identifier_or_keyword_to_tests::*;
