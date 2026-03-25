pub(super) fn has_use_strict_directive(statements: &[Statement]) -> bool {
    for stmt in statements {
        match stmt {
            Statement::Expression(expr_stmt) => match &expr_stmt.expression {
                Expression::StringLiteral(lit) if lit.value == "use strict" => {
                    return true;
                }
                Expression::StringLiteral(_) => continue,
                _ => return false,
            },
            _ => return false,
        }
    }
    false
}

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

#[allow(unused_imports)]
use super::*;

#[allow(unused_imports)]
use super::*;
