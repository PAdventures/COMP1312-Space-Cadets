// Import necessary modules
use crate::frontend::{
    ast::definitions::{
        ASTExpression, ASTLiteral, ASTStatement, BinaryExpression, ComparisonExpression,
        ElseStatement, ForLoopStatement, FunctionCallExpression, FunctionStatement, IfStatement,
        LogicalExpression, UnaryExpression, VariableAssignmentExpression, WhileLoopStatement,
    },
    lexer::TokenType,
};

// The structure that can traverse and print an AST in a human-readable format.
pub struct ASTPrinter;

impl ASTPrinter {
    pub fn new() -> Self {
        ASTPrinter {}
    }

    // The main entry point for AST printing.
    pub fn output(&self, program: &ASTStatement) -> String {
        let mut output = String::new();

        // Only allows an ASTStatement::Program as the entry point
        match program {
            ASTStatement::Program(program) => {
                for statement in program {
                    output.push_str(&self.output_statement(statement, 0));
                }
            }
            _ => {
                return String::from(
                    "ASTPrinter Error: Only Program statements are supported for output",
                );
            }
        }

        output
    }

    // Helper function to indent the output based on the current level.
    fn indent(&self, level: usize) -> String {
        "\t".repeat(level)
    }

    // Helper function to output statements (excluding the ASTStatement::Program) with proper indentation.
    fn output_statement(&self, statement: &ASTStatement, indent: usize) -> String {
        let mut output = String::new();

        match statement {
            ASTStatement::Expression(expression) => {
                output.push_str(&format!(
                    "{}{}\n",
                    self.indent(indent),
                    self.output_expression(expression)
                ));
            }
            ASTStatement::While(while_loop) => {
                output.push_str(&format!(
                    "{}{}\n",
                    self.indent(indent),
                    self.output_while_loop(while_loop, indent)
                ));
            }
            ASTStatement::If(if_statement) => {
                output.push_str(&format!(
                    "{}{}\n",
                    self.indent(indent),
                    self.output_if_statement(if_statement, indent)
                ));
            }
            ASTStatement::For(for_loop) => output.push_str(&format!(
                "{}{}\n",
                self.indent(indent),
                self.output_for_statement(for_loop, indent)
            )),
            ASTStatement::Function(function_statement) => output.push_str(&format!(
                "{}{}\n",
                self.indent(indent),
                self.output_function_statement(function_statement, indent)
            )),
            _ => {
                return String::from(
                    "ASTPrinter Error: Program statements are supported for statement output",
                );
            }
        }

        output
    }

    fn output_function_statement(
        &self,
        function_statement: &FunctionStatement,
        indent: usize,
    ) -> String {
        let mut output = String::new();

        output.push_str(&format!("(function {}", function_statement.name.lexeme));

        if function_statement.parameters.len() > 0 {
            output.push_str(" params");
            for param in &function_statement.parameters {
                output.push_str(&format!(" {}", param.lexeme));
            }
        }

        output.push_str("\n");

        for statement in &function_statement.body {
            output.push_str(&self.output_statement(statement, indent + 1));
        }

        output.push_str(&format!("{})\n", self.indent(indent)));

        output
    }

    fn output_for_statement(&self, for_loop: &ForLoopStatement, indent: usize) -> String {
        let mut output = String::new();

        output.push_str(&format!(
            "(for {} {}\n",
            self.output_expression(&for_loop.condition),
            self.output_variable_assignment(&for_loop.step)
        ));

        for statement in &for_loop.body {
            output.push_str(&self.output_statement(statement, indent + 1));
        }

        output.push_str(&format!("{})\n", self.indent(indent)));

        output
    }

    fn output_if_statement(&self, if_statement: &IfStatement, indent: usize) -> String {
        let mut output = String::new();

        output.push_str(&format!(
            "(if {}\n",
            self.output_expression(&if_statement.condition)
        ));

        for statement in &if_statement.body {
            output.push_str(&self.output_statement(statement, indent + 1));
        }

        if if_statement.branches.len() == 0 {
            output.push_str(&format!("{})\n", self.indent(indent)));
            return output;
        }

        output.push_str(&format!("{}) ", self.indent(indent)));

        for branch in &if_statement.branches {
            output.push_str(&self.output_else_statement(branch, indent));
        }

        output
    }

    fn output_else_statement(&self, else_statement: &ElseStatement, indent: usize) -> String {
        let mut output = String::new();

        let mut is_final = false;

        if let Some(condition) = &else_statement.condition {
            output.push_str(&format!("(elif {}\n", self.output_expression(condition)));
        } else {
            output.push_str("(else\n");
            is_final = true
        }

        for statement in &else_statement.body {
            output.push_str(&self.output_statement(statement, indent + 1));
        }

        if is_final {
            output.push_str(&format!("{})\n", self.indent(indent)));
        } else {
            output.push_str(&format!("{}) ", self.indent(indent)));
        }

        output
    }

    // Helper function to output a while loop statement
    fn output_while_loop(&self, while_loop: &WhileLoopStatement, indent: usize) -> String {
        let mut output = String::new();

        // Call the comparison expression helper function to output the condition
        output.push_str(&format!(
            "(while {}\n",
            self.output_expression(&while_loop.condition)
        ));

        for statement in &while_loop.body {
            output.push_str(&self.output_statement(statement, indent + 1));
        }

        output.push_str(&format!("{})\n", self.indent(indent)));
        output
    }

    // Helper function to output expressions
    fn output_expression(&self, expression: &ASTExpression) -> String {
        match expression {
            ASTExpression::Literal(literal) => self.output_literal(literal),
            ASTExpression::VariableAssignment(variable_assignment) => {
                self.output_variable_assignment(variable_assignment)
            }
            ASTExpression::Comparison(comparison) => self.output_comparison(comparison),
            ASTExpression::Variable(variable) => variable.lexeme.clone(),
            ASTExpression::Call(call) => self.output_function_call(call),
            ASTExpression::Return(ret) => self.output_function_return(ret),
            ASTExpression::Binary(bin) => self.output_binary(bin),
            ASTExpression::Logical(logic) => self.output_logical(logic),
            ASTExpression::Unary(unary) => self.output_unary(unary),
            ASTExpression::Grouping(group) => format!("(group {})", self.output_expression(group)),
        }
    }

    fn output_function_return(&self, ret: &Option<Box<ASTExpression>>) -> String {
        let mut output = String::new();

        output.push_str("(return");

        if let Some(expression) = ret {
            output.push_str(&format!(" {}", self.output_expression(expression)));
        }

        output.push_str(")");

        output
    }

    fn output_function_call(&self, call: &FunctionCallExpression) -> String {
        let mut output = String::new();

        output.push_str(&format!("(call {}", call.function.lexeme));

        if call.arguments.len() > 0 {
            output.push_str(" args");
            for arg in &call.arguments {
                output.push_str(&format!(" {}", self.output_expression(arg)));
            }
        }

        output.push_str(")");

        output
    }

    fn output_unary(&self, unary: &UnaryExpression) -> String {
        format!(
            "(unary {} {})",
            unary.operator.lexeme,
            self.output_expression(&*unary.right)
        )
    }

    fn output_binary(&self, binary: &BinaryExpression) -> String {
        format!(
            "(binary {} {} {} into {})",
            self.output_expression(&*binary.left),
            binary.operator.lexeme,
            self.output_expression(&*binary.right),
            binary.into.lexeme
        )
    }

    fn output_logical(&self, logical: &LogicalExpression) -> String {
        format!(
            "(logical {} {} {})",
            self.output_expression(&*logical.left),
            logical.operator.lexeme,
            self.output_expression(&*logical.right)
        )
    }

    // Helper function to output comparison expressions
    fn output_comparison(&self, comparison: &ComparisonExpression) -> String {
        format!(
            "(comparison {} {} {})",
            self.output_expression(&*comparison.left),
            comparison.operator.lexeme,
            self.output_expression(&*comparison.right)
        )
    }

    // Helper function to output variable assignment expressions
    fn output_variable_assignment(&self, assignment: &VariableAssignmentExpression) -> String {
        match assignment.command.token_type {
            TokenType::Increment | TokenType::Decrement => {
                if let Some(expr) = &assignment.value {
                    match &**expr {
                        ASTExpression::Literal(literal) => match literal {
                            ASTLiteral::Integer(int) => {
                                return format!(
                                    "(assignment {} {} by {})",
                                    assignment.command.lexeme, assignment.variable.lexeme, int
                                );
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }
                }
                format!(
                    "(assignment {} {})",
                    assignment.command.lexeme, assignment.variable.lexeme
                )
            }
            TokenType::Clear => format!(
                "(assignment {} {})",
                assignment.command.lexeme, assignment.variable.lexeme
            ),
            TokenType::Set => format!(
                "(assignment set {} to {})",
                assignment.variable.lexeme,
                self.output_expression(assignment.value.as_ref().unwrap())
            ),
            TokenType::Copy => format!(
                "(assignment copy {} into {})",
                self.output_expression(assignment.value.as_ref().unwrap()),
                assignment.variable.lexeme
            ),
            _ => unreachable!(),
        }
    }

    // Helper function to output literal expressions
    fn output_literal(&self, literal: &ASTLiteral) -> String {
        match literal {
            ASTLiteral::Boolean(bool) => bool.to_string(),
            ASTLiteral::Character(char) => format!("'{}'", char),
            ASTLiteral::Float(fl) => fl.to_string(),
            ASTLiteral::Integer(int) => int.to_string(),
            ASTLiteral::Null => "null".to_string(),
            ASTLiteral::String(str) => format!("\"{}\"", str),
        }
    }
}
