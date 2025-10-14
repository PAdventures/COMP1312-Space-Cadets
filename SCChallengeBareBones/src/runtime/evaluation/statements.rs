// Import necessary modules
use crate::{
    frontend::ast::{ASTStatement, WhileLoopStatement},
    runtime::{environment::Environment, interpreter::evaluate, values},
};

// Function that evaluates a program or the root node of an AST
pub fn evaluate_program(
    program: Vec<ASTStatement>,
    environment: &mut Environment,
) -> Result<values::RuntimeValue, String> {
    // Initially the final result is null
    let mut result = values::RuntimeValue::Null;

    for statement in program {
        match evaluate(statement, environment) {
            // Continously update the result with the latest value
            Ok(value) => result = value,
            Err(err) => return Err(err),
        }
    }

    Ok(result)
}

// Function that evaluates a while loop statement
pub fn evaluate_while_statement(
    while_loop: WhileLoopStatement,
    environment: &mut Environment,
) -> Result<values::RuntimeValue, String> {
    let mut condition_resolved = true;

    // Disable an annoying Rust warning
    #[allow(unused_assignments)]
    let mut condition = values::RuntimeValue::Null;

    // Continuously evaluate the condition until it becomes false
    // and evaluate the body of the loop if the condition is true
    while condition_resolved {
        condition = match evaluate(
            ASTStatement::Expression(while_loop.condition.to_owned()),
            environment,
        ) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };

        match condition {
            values::RuntimeValue::Boolean(bool) => {
                if bool {
                    match evaluate(
                        ASTStatement::Program(while_loop.body.to_owned()),
                        environment,
                    ) {
                        Ok(_) => (),
                        Err(err) => return Err(err),
                    }
                } else {
                    condition_resolved = false;
                }
            }
            _ => return Err("Expected boolean value for while loop condition".to_string()),
        }
    }

    Ok(values::RuntimeValue::Null)
}
