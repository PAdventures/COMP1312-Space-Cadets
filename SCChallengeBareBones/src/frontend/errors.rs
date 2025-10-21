// This file contains the definition of the LexicalErrorType enum and the LexicalError struct.
//
// This has been done in a way that is easy to add new error types.

#[derive(Clone, Debug)]
pub enum LexicalErrorType {
    UnexpectedCharacter(char),
    InvalidCharacterLiteral(String),
    InvalidEscapeSequence(String),
    EmptyCharacterLiteral,
    UnterminatedCharacterLiteral,
    UnterminatedStringLiteral,
    InvalidIntegerLiteral(String, String),
    InvalidFloatLiteral(String, String),
}

#[derive(Clone, Debug)]
pub struct LexicalError {
    line: u32,
    char: u32,
    error_type: LexicalErrorType,
}

impl LexicalError {
    pub fn new(line: u32, char: u32, error_type: LexicalErrorType) -> Self {
        Self {
            line,
            char,
            error_type,
        }
    }

    pub fn to_string(self) -> String {
        match self.error_type {
            LexicalErrorType::UnexpectedCharacter(c) => {
                format!(
                    "[line:char {}:{}] Unexpected character: {}",
                    self.line, self.char, c
                )
            }
            LexicalErrorType::InvalidCharacterLiteral(s) => {
                format!(
                    "[line:char {}:{}] Invalid character literal: {}",
                    self.line, self.char, s
                )
            }
            LexicalErrorType::EmptyCharacterLiteral => {
                format!(
                    "[line:char {}:{}] Error: Character literals cannot be empty",
                    self.line, self.char
                )
            }
            LexicalErrorType::UnterminatedCharacterLiteral => {
                format!(
                    "[line:char {}:{}] Error: Unterminated character literal",
                    self.line, self.char
                )
            }
            LexicalErrorType::InvalidEscapeSequence(s) => {
                format!(
                    "[line:char {}:{}] Invalid escape sequence: {}",
                    self.line, self.char, s
                )
            }
            LexicalErrorType::UnterminatedStringLiteral => {
                format!(
                    "[line:char {}:{}] Error: Unterminated string literal",
                    self.line, self.char
                )
            }
            LexicalErrorType::InvalidIntegerLiteral(s, m) => {
                format!(
                    "[line:char {}:{}] Invalid integer literal: {}, {}",
                    self.line, self.char, s, m
                )
            }
            LexicalErrorType::InvalidFloatLiteral(s, m) => {
                format!(
                    "[line:char {}:{}] Invalid float literal: {}, {}",
                    self.line, self.char, s, m
                )
            }
        }
    }
}
