// Import necessary modules
use crate::{
    frontend::{
        ast::{ASTStatement, ComparisonExpression, VariableAssignmentExpression},
        lexer::{Token, TokenType},
    },
    runtime::{environment::Environment, interpreter::evaluate, values},
};

// Function that evaluates a variable assignment expression
pub fn evaluate_assignment_expression(
    assignment: VariableAssignmentExpression,
    environment: &mut Environment,
) -> Result<values::RuntimeValue, String> {
    // Check if the variable exists,
    // if not AND the command is not Clear, return an error,
    // otherwise store the value as an unsigned 64 bit integer
    let value = match environment.lookup_variable(assignment.variable.lexeme.to_owned()) {
        Some(value) => match value {
            values::RuntimeValue::Number(num) => num,
            _ => {
                return Err(format!(
                    "Variable {} is not a number",
                    assignment.variable.lexeme
                ));
            }
        },
        None => {
            if assignment.command.token_type != TokenType::Clear {
                return Err(format!(
                    "Variable {} is not defined",
                    assignment.variable.lexeme
                ));
            }
            0
        }
    };

    // Perform the assignment operation
    match assignment.command.token_type {
        TokenType::Clear => {
            environment
                .assign_variable(assignment.variable.lexeme, values::RuntimeValue::Number(0));
        }
        TokenType::Increment => {
            environment.assign_variable(
                assignment.variable.lexeme,
                values::RuntimeValue::Number(value + 1),
            );
        }
        TokenType::Decrement => {
            environment.assign_variable(
                assignment.variable.lexeme,
                values::RuntimeValue::Number(value - 1),
            );
        }
        _ => {
            return Err(format!(
                "Invalid assignment command {}",
                assignment.command.lexeme
            ));
        }
    };

    Ok(values::RuntimeValue::Null)
}

// Function that evaluates a variable expression
pub fn evaluate_variable_expression(
    variable: Token,
    environment: &mut Environment,
) -> Result<values::RuntimeValue, String> {
    // Just returns the value of the variable (or 0 if not found)
    match environment.lookup_variable(variable.lexeme) {
        Some(value) => Ok(value),
        None => Ok(values::RuntimeValue::Number(0)),
    }
}

// Function that evaluates a comparison expression
pub fn evaluate_comparison_expression(
    comparison: ComparisonExpression,
    environment: &mut Environment,
) -> Result<values::RuntimeValue, String> {
    // First evaluate the left hand side of the operator
    let left = match evaluate(ASTStatement::Expression(*comparison.left), environment) {
        Ok(value) => match value {
            values::RuntimeValue::Number(num) => num,
            _ => return Err("Left hand operator did not evaluate to a number".to_string()),
        },
        Err(err) => return Err(err),
    };

    // Then evaluate the right hand side of the operator
    let right = match evaluate(ASTStatement::Expression(*comparison.right), environment) {
        Ok(value) => match value {
            values::RuntimeValue::Number(num) => num,
            _ => return Err("Right hand operator did not evaluate to a number".to_string()),
        },
        Err(err) => return Err(err),
    };

    // Perform the comparison operation
    match comparison.operator.token_type {
        TokenType::Is => Ok(values::RuntimeValue::Boolean(left == right)),
        TokenType::Not => Ok(values::RuntimeValue::Boolean(left != right)),
        _ => Err(format!(
            "Invalid operator, only the 'is' and 'not is' operators are supported, got: {}",
            comparison.operator.token_type
        )),
    }
}
