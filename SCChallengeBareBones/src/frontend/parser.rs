// Import necessary modules
use crate::{
    frontend::{
        ast::{
            ASTExpression, ASTLiteral, ASTStatement, ComparisonExpression,
            VariableAssignmentExpression, WhileLoopStatement,
        },
        lexer::{Token, TokenType},
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
        if self.peek().token_type == TokenType::While {
            return self.parse_while();
        }

        // otherwise, parse an expression statement
        let expression = match self.parse_expression() {
            Ok(expression) => expression,
            Err(err) => return Err(err),
        };

        // We always expect a semicolon after an expression statement
        match self.consume(TokenType::SemiColon, "Expected ';' after expression.") {
            Ok(_) => (),
            Err(err) => return Err(err),
        }
        Ok(ASTStatement::Expression(expression))
    }

    // Helper function to parse a while statement
    fn parse_while(&mut self) -> Result<ASTStatement, ParserError> {
        self.advance(); // Advance past the while keyword

        // Parse the condition expression
        let condition = match self.parse_comparison() {
            Ok(c) => c,
            Err(e) => return Err(e),
        };

        // After the condition expression, we expect a do keyword token
        if self.peek().token_type != TokenType::Do {
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
            let statement = match self.parse_statement() {
                Ok(s) => s,
                Err(e) => return Err(e),
            };
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
    // In the future, this function may be used to start a Pratt parser for complex expressions
    fn parse_expression(&mut self) -> Result<ASTExpression, ParserError> {
        return self.parse_variable_assignment();
    }

    // Helper function to parse variable assignments
    fn parse_variable_assignment(&mut self) -> Result<ASTExpression, ParserError> {
        // Check if the current token is a clear, incr or decr keyword, otherwise
        // continue the recursive descent parsing
        match self.peek().token_type {
            TokenType::Clear | TokenType::Decrement | TokenType::Increment => (),
            _ => return self.parse_comparison(),
        };

        let command = self.advance().to_owned();

        // Check if the next token is an identifier
        if !self.check(&TokenType::Identifier) {
            return Err(self.error(&format!(
                "Expected an identifer after \"{}\" token, got {}",
                command.lexeme,
                self.peek().token_type
            )));
        };

        let variable = self.advance().to_owned();

        Ok(ASTExpression::VariableAssignment(
            VariableAssignmentExpression::new(variable, command),
        ))
    }

    // Helper function to parse a comparison expression
    fn parse_comparison(&mut self) -> Result<ASTExpression, ParserError> {
        let left = match self.parse_primary() {
            Ok(expression) => expression,
            Err(e) => return Err(e),
        };

        // Check if the previous token is a literal or identifier
        match left {
            ASTExpression::Literal(_) | ASTExpression::Variable(_) => (),
            _ => return Err(self.error("Expected a literal value or variable")),
        }

        // If the current token is a not or is keyword
        // if so, continue parsing the comparison expression
        // otherwise, return the left expression which is just a literal or identifier
        match self.peek().token_type {
            TokenType::Not | TokenType::Is => {
                let token = self.advance().clone();
                let right = match self.parse_primary() {
                    Ok(expression) => expression,
                    Err(e) => return Err(e),
                };
                match right {
                    ASTExpression::Literal(_) | ASTExpression::Variable(_) => (),
                    _ => return Err(self.error("Expected a literal value or variable")),
                };
                Ok(ASTExpression::Comparison(ComparisonExpression::new(
                    left, token, right,
                )))
            }
            _ => Ok(left),
        }
    }

    // Helper function to parse primary expressions
    fn parse_primary(&mut self) -> Result<ASTExpression, ParserError> {
        match self.peek().token_type {
            TokenType::Identifier => Ok(ASTExpression::Variable(self.advance().to_owned())),
            TokenType::IntegerLiteral => Ok(ASTExpression::Literal(ASTLiteral::Number(
                self.advance().literal,
            ))),
            _ => Err(self.error(&format!("Unexpected token: {}", self.peek()))),
        }
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
