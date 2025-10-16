// This file contains the definition of the LexicalErrorType enum and the LexicalError struct.
//
// This has been done in a way that is easy to add new error types.

#[derive(Clone, Debug)]
pub enum LexicalErrorType {
    UnexpectedCharacter(char),
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
                    "[line:char {}:{}] Error: Unexpected character: {}",
                    self.line, self.char, c
                )
            }
        }
    }
}
