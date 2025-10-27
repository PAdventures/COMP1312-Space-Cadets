// Import necessary modules
use crate::{
    frontend::{
        ast::definitions::{
            ASTExpression, ASTLiteral, ASTStatement, BinaryExpression, ComparisonExpression,
            ElseStatement, ForLoopStatement, FunctionCallExpression, FunctionStatement,
            IfStatement, LogicalExpression, UnaryExpression, VariableAssignmentExpression,
            WhileLoopStatement,
        },
        lexer::{Token, TokenLiteral, TokenType},
    },
    utils::token_pos,
};

// Define a custom error type for parsing errors
pub struct ParserError {
    pub token: Token,
    pub message: String,
}

impl ParserError {
    pub fn new(token: Token, message: String) -> Self {
        Self { token, message }
    }

    // Helper function for pretty printing
    pub fn to_string(&self) -> String {
        format!(
            "{} Parse error at '{}': {}",
            token_pos(&self.token),
            self.token.lexeme,
            self.message
        )
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Precedence {
    Lowest = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
}

impl Precedence {
    pub fn next(self) -> Precedence {
        use Precedence::*;
        match self {
            Lowest => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Unary, // highest, stays
        }
    }
}

// Define the Parser struct using a lifetime covariant specifier
pub struct Parser<'a> {
    _tokens: &'a [Token], // Use an array instead of a vector
    _current: usize,
}

// The Parser struct parses the tokens into an abstract syntax tree (AST) using a recursive descent parser.
//
// Below is the precedence of each AST node type
//
// Highest precedence
//
// - Primary (Literals and identifiers)
// - Comparison (X not Y, X is Y)
// - Variable assignment (Clear, Incr, Decr)
// - While loop
//
// Lowest precedence
//
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            _tokens: tokens,
            _current: 0,
        }
    }

    // Main entry point for parsing a program
    pub fn parse_program(&mut self) -> (ASTStatement, Vec<ParserError>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while !self.is_at_end() {
            // Begin the recursive descent parsing process
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    errors.push(err);
                    // Once an error is encountered, synchronize the parser to recover from the error
                    // by skipping tokens until we reach a statement boundary
                    self.synchronize();
                }
            }
        }

        (ASTStatement::Program(statements), errors)
    }

    // Helper function to parse a statement
    fn parse_statement(&mut self) -> Result<ASTStatement, ParserError> {
        // If the current token is a while keyword, parse a while statement
        if self.check(&TokenType::While) {
            return self.parse_while();
        }

        if self.check(&TokenType::If) {
            return self.parse_if();
        }

        if self.check(&TokenType::For) {
            return self.parse_for();
        }

        if self.check(&TokenType::Function) {
            return self.parse_function();
        }

        // otherwise, parse an expression statement
        let expression = self.parse_expression()?;

        // We always expect a semicolon after an expression statement
        match self.consume(TokenType::SemiColon, "Expected ';' after expression.") {
            Ok(_) => (),
            Err(err) => return Err(err),
        }
        Ok(ASTStatement::Expression(expression))
    }

    fn parse_function(&mut self) -> Result<ASTStatement, ParserError> {
        self.advance();

        let name = self.advance().clone();

        let mut parameters: Vec<Token> = Vec::new();

        while !self.is_at_end() && self.check(&TokenType::Parameter) {
            self.advance();
            if !self.check(&TokenType::Identifier) {
                return Err(self.error(&format!(
                    "Expected an identifier token after \"param\" keyword, got: {}",
                    self.peek().token_type
                )));
            };
            parameters.push(self.advance().clone());
        }

        if !self.check(&TokenType::Then) {
            return Err(self.error(&format!(
                "Expected a \"then\" token after identifier token, got: {}",
                self.peek().token_type
            )));
        };

        self.advance();

        // We always expect a semi-colon after a then keyword token
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"then\" token"));
        };

        // Parse the body of the function
        let mut statements: Vec<ASTStatement> = Vec::new();

        while self.peek().token_type != TokenType::End && !self.is_at_end() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        // We always expect an "end" token after the body of the function
        if !self.match_token(&[TokenType::End]) {
            return Err(self.error("Expected and \"end\" token"));
        };

        // We always expect a semi-colon after a statement
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"end\" token"));
        };

        Ok(ASTStatement::Function(FunctionStatement::new(
            name, parameters, statements,
        )))
    }

    fn parse_for(&mut self) -> Result<ASTStatement, ParserError> {
        self.advance();

        let condition = self.parse_expression()?;

        // We always expect a semi-colon after a condition
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after condition"));
        };

        let step = match self.parse_expression()? {
            ASTExpression::VariableAssignment(assignment) => match assignment.command.token_type {
                TokenType::Increment | TokenType::Decrement => assignment,
                _ => {
                    return Err(self.error(&format!(
                        "Expected an increment or decrement variable assignment, got: {}",
                        assignment.command.token_type
                    )));
                }
            },
            _ => return Err(self.error("Expected a variable assignment after semi-colon token")),
        };

        if !self.check(&TokenType::Do) {
            return Err(self.error(&format!(
                "Expected a \"do\" token after condition, got: {}",
                self.peek().token_type
            )));
        };

        self.advance();

        // We always expect a semi-colon after a then keyword do
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"do\" token"));
        };

        // Parse the body of the for loop
        let mut statements: Vec<ASTStatement> = Vec::new();

        while self.peek().token_type != TokenType::End && !self.is_at_end() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        // We always expect an "end" token after the body of the for loop
        if !self.match_token(&[TokenType::End]) {
            return Err(self.error("Expected and \"end\" token"));
        };

        // We always expect a semi-colon after a statement
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"end\" token"));
        };

        Ok(ASTStatement::For(ForLoopStatement::new(
            condition, step, statements,
        )))
    }

    fn parse_if(&mut self) -> Result<ASTStatement, ParserError> {
        self.advance();

        let condition = self.parse_expression()?;

        if !self.check(&TokenType::Then) {
            return Err(self.error(&format!(
                "Expected a \"then\" token after condition, got: {}",
                self.peek().token_type
            )));
        };

        self.advance();

        // We always expect a semi-colon after a then keyword token
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"then\" token"));
        };

        // Parse the body of the if statement
        let mut statements: Vec<ASTStatement> = Vec::new();

        while !self.is_at_end()
            && !self.check(&TokenType::End)
            && !self.check(&TokenType::ElseIf)
            && !self.check(&TokenType::Else)
        {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        let mut branches: Vec<ElseStatement> = Vec::new();

        while !self.is_at_end() && (self.check(&TokenType::ElseIf) || self.check(&TokenType::Else))
        {
            let branch = self.parse_else()?;
            branches.push(branch);
        }

        // We always expect an "end" token after the body of the if statement
        if !self.match_token(&[TokenType::End]) {
            return Err(self.error("Expected an \"end\" token"));
        };

        // We always expect a semi-colon after a statement
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"end\" token"));
        };

        Ok(ASTStatement::If(IfStatement::new(
            condition, statements, branches,
        )))
    }

    fn parse_else(&mut self) -> Result<ElseStatement, ParserError> {
        let branch = self.advance();

        if branch.token_type == TokenType::Else {
            if !self.check(&TokenType::Then) {
                return Err(self.error(&format!(
                    "Expected a \"then\" token after \"else\" keyword token, got: {}",
                    self.peek().token_type
                )));
            };

            self.advance();

            // We always expect a semi-colon after a then keyword token
            if !self.match_token(&[TokenType::SemiColon]) {
                return Err(self.error("Expected a semi-colon after \"then\" token"));
            };

            let mut statements: Vec<ASTStatement> = Vec::new();

            while !self.is_at_end() && !self.check(&TokenType::End) {
                let statement = self.parse_statement()?;
                statements.push(statement);
            }

            return Ok(ElseStatement::new(None, statements));
        };

        let condition = self.parse_expression()?;

        if !self.check(&TokenType::Then) {
            return Err(self.error(&format!(
                "Expected a \"then\" token after condition, got: {}",
                self.peek().token_type
            )));
        };

        self.advance();

        // We always expect a semi-colon after a then keyword token
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"then\" token"));
        };

        // Parse the body of the if statement
        let mut statements: Vec<ASTStatement> = Vec::new();

        while !self.is_at_end()
            && !self.check(&TokenType::End)
            && !self.check(&TokenType::ElseIf)
            && !self.check(&TokenType::Else)
        {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(ElseStatement::new(Some(condition), statements))
    }

    // Helper function to parse a while statement
    fn parse_while(&mut self) -> Result<ASTStatement, ParserError> {
        self.advance(); // Advance past the while keyword

        // Parse the condition expression
        let condition = self.parse_expression()?;

        // After the condition expression, we expect a do keyword token
        if !self.check(&TokenType::Do) {
            return Err(self.error(&format!(
                "Expected a \"do\" token after condition, got: {}",
                self.peek().token_type
            )));
        };

        self.advance(); // Advance past the do keyword

        // We always expect a semi-colon after a do keyword token
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"do\" token"));
        };

        // Parse the body of the while loop
        let mut statements: Vec<ASTStatement> = Vec::new();

        while self.peek().token_type != TokenType::End && !self.is_at_end() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        // We always expect an "end" token after the body of the while loop
        if !self.match_token(&[TokenType::End]) {
            return Err(self.error("Expected and \"end\" token"));
        };

        // We always expect a semi-colon after a statement
        if !self.match_token(&[TokenType::SemiColon]) {
            return Err(self.error("Expected a semi-colon after \"end\" token"));
        };

        Ok(ASTStatement::While(WhileLoopStatement::new(
            condition, statements,
        )))
    }

    // Helper function to parse expressions.
    fn parse_expression(&mut self) -> Result<ASTExpression, ParserError> {
        return self.parse_precedence(Precedence::Assignment);
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<ASTExpression, ParserError> {
        let mut expression = match self.peek().token_type {
            TokenType::NullLiteral => {
                self.advance();
                ASTExpression::Literal(ASTLiteral::Null)
            }
            TokenType::IntegerLiteral => match self.parse_integer_literal() {
                Ok(expr) => expr,
                Err(e) => return Err(e),
            },
            TokenType::FloatLiteral => match self.parse_float_literal() {
                Ok(expr) => expr,
                Err(e) => return Err(e),
            },
            TokenType::StringLiteral => match self.parse_string_literal() {
                Ok(expr) => expr,
                Err(e) => return Err(e),
            },
            TokenType::CharacterLiteral => match self.parse_character_literal() {
                Ok(expr) => expr,
                Err(e) => return Err(e),
            },
            TokenType::BooleanLiteral => match self.parse_boolean_literal() {
                Ok(expr) => expr,
                Err(e) => return Err(e),
            },
            TokenType::Identifier => {
                let identifier = self.advance().clone();
                ASTExpression::Variable(identifier)
            }
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                self.consume(TokenType::RightParen, "Expected ')' after expression")?;
                ASTExpression::Grouping(Box::new(expr))
            }
            TokenType::Negate | TokenType::Not => {
                let operator_token = self.advance().clone();
                let right = self.parse_precedence(Precedence::Unary)?;
                ASTExpression::Unary(UnaryExpression::new(operator_token, Box::new(right)))
            }
            TokenType::Add => {
                let operator_token = self.advance().clone();
                let left = self.parse_precedence(Precedence::Term)?;
                self.consume(TokenType::To, "Expected a 'to' keyword token after lhs")?;
                let right = self.parse_precedence(Precedence::Term)?;
                self.consume(TokenType::Into, "Expected a 'into' keyword token after rhs")?;
                let into = self.advance().clone();
                ASTExpression::Binary(BinaryExpression::new(left, operator_token, right, into))
            }
            TokenType::Subtract => {
                let operator_token = self.advance().clone();
                let left = self.parse_precedence(Precedence::Term)?;
                self.consume(TokenType::From, "Expected a 'from' keyword token after lhs")?;
                let right = self.parse_precedence(Precedence::Term)?;
                self.consume(TokenType::Into, "Expected a 'into' keyword token after rhs")?;
                let into = self.advance().clone();
                ASTExpression::Binary(BinaryExpression::new(left, operator_token, right, into))
            }
            TokenType::Multiply => {
                let operator_token = self.advance().clone();
                let left = self.parse_precedence(Precedence::Factor)?;
                self.consume(TokenType::By, "Expected a 'by' keyword token after lhs")?;
                let right = self.parse_precedence(Precedence::Factor)?;
                self.consume(TokenType::Into, "Expected a 'into' keyword token after rhs")?;
                let into = self.advance().clone();
                ASTExpression::Binary(BinaryExpression::new(left, operator_token, right, into))
            }
            TokenType::Divide => {
                let operator_token = self.advance().clone();
                let left = self.parse_precedence(Precedence::Factor)?;
                self.consume(TokenType::By, "Expected a 'by' keyword token after lhs")?;
                let right = self.parse_precedence(Precedence::Factor)?;
                self.consume(TokenType::Into, "Expected a 'into' keyword token after rhs")?;
                let into = self.advance().clone();
                ASTExpression::Binary(BinaryExpression::new(left, operator_token, right, into))
            }
            TokenType::Clear => {
                let operator_token = self.advance().clone();
                let variable = self
                    .consume(
                        TokenType::Identifier,
                        "Expected an identifier after assignment",
                    )?
                    .clone();
                ASTExpression::VariableAssignment(VariableAssignmentExpression::new(
                    variable,
                    operator_token,
                    None,
                ))
            }
            TokenType::Increment | TokenType::Decrement => {
                match self.parse_increment_decrement_expression() {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                }
            }
            TokenType::Set => {
                let command = self.advance().clone();
                let variable = self
                    .consume(
                        TokenType::Identifier,
                        "Expected an identifier after assignment",
                    )?
                    .clone();
                self.consume(
                    TokenType::To,
                    "Expected a 'to' keyword token after variable",
                )?;
                let value = self.parse_expression()?;
                ASTExpression::VariableAssignment(VariableAssignmentExpression::new(
                    variable,
                    command,
                    Some(Box::new(value)),
                ))
            }
            TokenType::Copy => {
                let command = self.advance().clone();
                let source = self
                    .consume(
                        TokenType::Identifier,
                        "Expected an identifier after assignment",
                    )?
                    .clone();
                self.consume(
                    TokenType::Into,
                    "Expected a 'into' keyword token after variable",
                )?;
                let destination = self
                    .consume(
                        TokenType::Identifier,
                        "Expected an identifier after 'into' keyword token",
                    )?
                    .clone();
                ASTExpression::VariableAssignment(VariableAssignmentExpression::new(
                    destination,
                    command,
                    Some(Box::new(ASTExpression::Variable(source))),
                ))
            }
            TokenType::Call => match self.parse_call_expression() {
                Ok(expr) => expr,
                Err(e) => return Err(e),
            },
            TokenType::Return => {
                self.advance();

                let expression = if self.peek().token_type != TokenType::SemiColon {
                    Some(Box::new(self.parse_expression()?))
                } else {
                    None
                };

                ASTExpression::Return(expression)
            }
            _ => return Err(self.error("Unexpected expression")),
        };

        while !self.is_at_end() && precedence <= self.get_precedence() {
            // Logical and comparison operators
            let operator = self.advance().clone();
            let next_precedence = self.get_precedence().next(); // ensure left-associativity
            let right = self.parse_precedence(next_precedence)?;

            match operator.token_type {
                TokenType::And | TokenType::Or => {
                    // Do a quick check to see if the lhs may be an invalid node
                    match expression {
			           	ASTExpression::Grouping(_) | ASTExpression::Literal(_) | ASTExpression::Variable(_) | ASTExpression::Logical(_) | ASTExpression::Comparison(_) => (),
			            _ => return Err(self.error("Invalid lhs node, only groupings, literals, variables, logicals and comparisons can be used"))
		            }
                    // Do a quick check to see if the rhs may be an invalid node
                    match right {
			           	ASTExpression::Grouping(_) | ASTExpression::Literal(_) | ASTExpression::Variable(_) | ASTExpression::Logical(_) | ASTExpression::Comparison(_) => (),
			            _ => return Err(self.error("Invalid lhs node, only groupings, literals, variables, logicals and comparisons can be used"))
		            }
                    expression =
                        ASTExpression::Logical(LogicalExpression::new(expression, operator, right))
                }
                TokenType::Equal
                | TokenType::NotEqual
                | TokenType::Less
                | TokenType::Greater
                | TokenType::LessEqual
                | TokenType::GreaterEqual => {
                    // Do a quick check to see if the lhs may be an invalid node
                    match expression {
                        ASTExpression::Grouping(_)
                        | ASTExpression::Literal(_)
                        | ASTExpression::Variable(_) => (),
                        _ => {
                            return Err(self.error(
                                "Invalid lhs node, only groupings, literals and variables can be used",
                            ));
                        }
                    }
                    // Do a quick check to see if the rhs may be an invalid node
                    match right {
                        ASTExpression::Grouping(_)
                        | ASTExpression::Literal(_)
                        | ASTExpression::Variable(_) => (),
                        _ => {
                            return Err(self.error(
                                "Invalid lhs node, only groupings, literals and variables can be used",
                            ));
                        }
                    }
                    expression = ASTExpression::Comparison(ComparisonExpression::new(
                        expression, operator, right,
                    ))
                }
                _ => return Err(self.error("Unexpected expression")),
            }
        }

        Ok(expression)
    }

    fn get_precedence(&self) -> Precedence {
        match self.peek().token_type {
            TokenType::Or => Precedence::Or,
            TokenType::And => Precedence::And,
            TokenType::Equal | TokenType::NotEqual => Precedence::Equality,
            TokenType::Less
            | TokenType::Greater
            | TokenType::LessEqual
            | TokenType::GreaterEqual => Precedence::Comparison,
            _ => Precedence::Lowest,
        }
    }

    fn parse_call_expression(&mut self) -> Result<ASTExpression, ParserError> {
        self.advance();
        let function = self.advance().clone();
        let mut arguments: Vec<ASTExpression> = Vec::new();
        while !self.is_at_end() && self.peek().token_type == TokenType::Argument {
            self.advance();
            let arg = self.parse_expression()?;
            arguments.push(arg);
        }
        Ok(ASTExpression::Call(FunctionCallExpression::new(
            function, arguments,
        )))
    }

    fn parse_increment_decrement_expression(&mut self) -> Result<ASTExpression, ParserError> {
        let operator_token = self.advance().clone();
        let variable = self
            .consume(
                TokenType::Identifier,
                "Expected an identifier after assignment",
            )?
            .clone();

        if self.check(&TokenType::By) {
            self.advance();
            let step = match self.parse_precedence(Precedence::Assignment) {
                Ok(expr) => match expr {
                    ASTExpression::Literal(literal) => match literal {
                        ASTLiteral::Integer(int) => int,
                        _ => {
                            return Err(
                                self.error("Expected an integer literal after 'by' keyword token")
                            );
                        }
                    },
                    _ => {
                        return Err(self.error("Expected a literal after 'by' keyword token"));
                    }
                },
                Err(e) => return Err(e),
            };
            if step < 1 {
                return Err(self.error("The step after the 'by' keyword token cannot be below 1"));
            }
            Ok(ASTExpression::VariableAssignment(
                VariableAssignmentExpression::new(
                    variable,
                    operator_token,
                    Some(Box::new(ASTExpression::Literal(ASTLiteral::Integer(step)))),
                ),
            ))
        } else {
            Ok(ASTExpression::VariableAssignment(
                VariableAssignmentExpression::new(variable, operator_token, None),
            ))
        }
    }

    fn parse_integer_literal(&mut self) -> Result<ASTExpression, ParserError> {
        let literal = match self.advance().literal {
            TokenLiteral::Integer(int) => ASTLiteral::Integer(int),
            _ => return Err(self.error("Unexpected unit conversion error")),
        };
        Ok(ASTExpression::Literal(literal))
    }

    fn parse_float_literal(&mut self) -> Result<ASTExpression, ParserError> {
        let literal = match self.advance().literal {
            TokenLiteral::Float(fl) => ASTLiteral::Float(fl),
            _ => return Err(self.error("Unexpected unit conversion error")),
        };
        Ok(ASTExpression::Literal(literal))
    }

    fn parse_character_literal(&mut self) -> Result<ASTExpression, ParserError> {
        let literal = match self.advance().literal {
            TokenLiteral::Character(char) => ASTLiteral::Character(char),
            _ => return Err(self.error("Unexpected unit conversion error")),
        };
        Ok(ASTExpression::Literal(literal))
    }

    fn parse_string_literal(&mut self) -> Result<ASTExpression, ParserError> {
        let literal = match &self.advance().literal {
            TokenLiteral::String(str) => ASTLiteral::String(str.to_owned()),
            _ => return Err(self.error("Unexpected unit conversion error")),
        };
        Ok(ASTExpression::Literal(literal))
    }

    fn parse_boolean_literal(&mut self) -> Result<ASTExpression, ParserError> {
        let literal = match self.advance().literal {
            TokenLiteral::Boolean(bool) => ASTLiteral::Boolean(bool),
            _ => return Err(self.error("Unexpected unit conversion error")),
        };
        Ok(ASTExpression::Literal(literal))
    }

    // Helper function to check if the current token is of the expected type
    // if so, advance over it and return the token
    // otherwise, return an error
    fn consume(&mut self, expected: TokenType, message: &str) -> Result<&Token, ParserError> {
        if self.check(&expected) {
            Ok(self.advance())
        } else {
            Err(self.error(message))
        }
    }

    // Helper function that continuously advances over tokens until it finds a semicolon or a statement
    fn synchronize(&mut self) {
        if !self.is_at_end() {
            self.advance();
        }

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::SemiColon {
                return;
            }

            match self.peek().token_type {
                TokenType::While => return,
                _ => {}
            }

            self.advance();
        }
    }

    // Helper function that returns the current token and increments the current index
    fn advance(&mut self) -> &Token {
        let c = &self._tokens[self._current];
        self._current += 1;
        c
    }

    // Helper function that returns the current token
    fn peek(&self) -> &Token {
        &self._tokens[self._current]
    }

    // Helper function that returns the previous token
    fn previous(&self) -> &Token {
        &self._tokens[self._current - 1]
    }

    // Helper function that checks if the current token is the EOF (End Of File) token
    fn is_at_end(&self) -> bool {
        self._current == self._tokens.len() || (self.peek().token_type == TokenType::EOF)
    }

    // Helper function that checks if the current token matches any of the given token types
    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    // Helper function that checks if the current token matches the given token type
    fn check(&self, token_type: &TokenType) -> bool {
        !self.is_at_end() && &self.peek().token_type == token_type
    }

    // Helper function that creates a ParserError with the given message and the current token
    fn error(&self, message: &str) -> ParserError {
        ParserError::new(self.previous().clone(), message.to_string())
    }
}
