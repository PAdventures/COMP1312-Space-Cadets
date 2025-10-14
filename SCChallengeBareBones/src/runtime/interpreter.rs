// Import necessary modules
use crate::frontend::ast;
use crate::runtime::environment::Environment;
use crate::runtime::evaluation::{expressions, statements};
use crate::runtime::values;

// Main function that evaluates an AST (Abstract Syntax Tree)
//
// This function can accept any AST Statement and evaluate it into a RuntimeValue.
// This function must be given a Program node first to evaluate a given AST
pub fn evaluate(
    program: ast::ASTStatement,
    environment: &mut Environment,
) -> Result<values::RuntimeValue, String> {
    match program {
        ast::ASTStatement::Program(program) => statements::evaluate_program(program, environment),
        ast::ASTStatement::While(while_loop) => {
            statements::evaluate_while_statement(while_loop, environment)
        }
        ast::ASTStatement::Expression(expression) => match expression {
            ast::ASTExpression::Comparison(comparison) => {
                expressions::evaluate_comparison_expression(comparison, environment)
            }
            ast::ASTExpression::Literal(literal) => match literal {
                ast::ASTLiteral::Number(number) => Ok(values::RuntimeValue::Number(number)),
            },
            ast::ASTExpression::Variable(identifier) => {
                expressions::evaluate_variable_expression(identifier, environment)
            }
            ast::ASTExpression::VariableAssignment(variable_assignment_expression) => {
                expressions::evaluate_assignment_expression(
                    variable_assignment_expression,
                    environment,
                )
            } // _ => {
              //     return Err(format!(
              //         "This AST Node has not yet been implemented for interpretation: {:#?}",
              //         expression
              //     ));
              // }
        },
    }
}
