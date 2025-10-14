// Import necessary modules
use crate::frontend::lexer::Token;

// Define the main definition of the AST
#[derive(Debug, Clone)]
pub enum ASTStatement {
    // The root of the AST, representing the entire program.
    Program(Vec<ASTStatement>),
    While(WhileLoopStatement),
    Expression(ASTExpression),
}

// A statement representing a while loop.
#[derive(Debug, Clone)]
pub struct WhileLoopStatement {
    pub condition: ASTExpression,
    pub body: Vec<ASTStatement>,
}

impl WhileLoopStatement {
    pub fn new(condition: ASTExpression, body: Vec<ASTStatement>) -> Self {
        WhileLoopStatement { condition, body }
    }
}

// A statement representing an expression.
#[derive(Debug, Clone)]
pub enum ASTExpression {
    Literal(ASTLiteral),
    // An expression representing a variable / identifier
    Variable(Token),
    VariableAssignment(VariableAssignmentExpression),
    Comparison(ComparisonExpression),
}

// An expression representing a variable assignment.
#[derive(Debug, Clone)]
pub struct VariableAssignmentExpression {
    pub variable: Token,
    pub command: Token,
}

impl VariableAssignmentExpression {
    pub fn new(variable: Token, command: Token) -> Self {
        VariableAssignmentExpression { variable, command }
    }
}

// An expression representing a comparison.
#[derive(Debug, Clone)]
pub struct ComparisonExpression {
    pub left: Box<ASTExpression>,
    pub operator: Token,
    pub right: Box<ASTExpression>,
}

impl ComparisonExpression {
    pub fn new(left: ASTExpression, operator: Token, right: ASTExpression) -> Self {
        ComparisonExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

// An expression representing a literal value.
#[derive(Debug, Clone)]
pub enum ASTLiteral {
    // A literal integer value (idk why I named it "Number")
    Number(u64),
}

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
            _ => {
                return String::from(
                    "ASTPrinter Error: Program statements are supported for statement output",
                );
            }
        }

        output
    }

    // Helper function to output a while loop statement
    fn output_while_loop(&self, while_loop: &WhileLoopStatement, indent: usize) -> String {
        let mut output = String::new();

        let condition = match &while_loop.condition {
            ASTExpression::Comparison(c) => c,
            _ => unreachable!(),
        };

        // Call the comparison expression helper function to output the condition
        output.push_str(&format!("(while {}\n", self.output_comparison(condition)));

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
        }
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
        format!(
            "(assignment {} {})",
            assignment.command.lexeme, assignment.variable.lexeme
        )
    }

    // Helper function to output literal expressions
    fn output_literal(&self, literal: &ASTLiteral) -> String {
        match literal {
            ASTLiteral::Number(number) => number.to_string(),
        }
    }
}
