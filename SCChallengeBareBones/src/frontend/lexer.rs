// Import necessary modules
use super::errors::{LexicalError, LexicalErrorType};
use std::{fmt, str::FromStr};

// Define the TokenType enum which contains all possible token types
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Clear,     // clear
    Increment, // incr
    Decrement, // decr

    While, // while
    Is,    // is
    Not,   // not
    Do,    // do
    End,   // end

    SemiColon, // ;

    Identifier,
    IntegerLiteral,

    EOF, // End Of File
}

// Implement the Display trait for TokenType
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// Define the Token struct which contains all necessary information about a token
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: u64,
    pub line: u32,
    _debug: bool,
}

// Implement the Display trait for Token
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self._debug {
            return write!(
                f,
                "[line {}] {} {} {}",
                self.line, self.token_type, self.lexeme, self.literal
            );
        }
        write!(f, "{} {} {}", self.token_type, self.lexeme, self.literal)
    }
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: u64,
        line: u32,
        debug: bool,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
            _debug: debug,
        }
    }

    // Helper function to create a new EOF token
    pub fn eof(line: u32, debug: bool) -> Self {
        Token::new(TokenType::EOF, String::new(), 0, line, debug)
    }
}

// Define the Scanner struct that reads source code character by character and tokenises them
#[derive(Debug)]
pub struct Scanner {
    pub stream: Vec<Token>,
    pub errors: Vec<LexicalError>,
    _source: Vec<char>,
    _start: usize,   // Index of the start of the current lexeme
    _current: usize, // Index of the current character being processed
    _line: u32,
    _debug: bool,
}

impl Scanner {
    pub fn new(source: &String, debug: bool) -> Self {
        Scanner {
            stream: Vec::new(),
            errors: Vec::new(),
            _source: source.chars().collect(),
            _start: 0,
            _current: 0,
            _line: 1,
            _debug: debug,
        }
    }

    // Main entry point for scanning tokens
    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self._start = self._current;
            self.scan_token();
        }

        self.stream.push(Token::eof(self._line, self._debug));
    }

    // Helper function that turns a word into a keyword token or identifier
    fn keyword_token(&self, ident: &str) -> TokenType {
        match ident {
            "clear" => TokenType::Clear,
            "incr" => TokenType::Increment,
            "decr" => TokenType::Decrement,
            "while" => TokenType::While,
            "do" => TokenType::Do,
            "it" => TokenType::Is,
            "not" => TokenType::Not,
            "end" => TokenType::End,
            _ => TokenType::Identifier,
        }
    }

    // Helper function that scans one or more characters and turns them into a single token
    fn scan_token(&mut self) {
        let char = self.advance();
        match char {
            ';' => self.add_token(TokenType::SemiColon, 0),

            // Ignore whitespace characters (spaces, carriage returns, tabs)
            ' ' | '\r' | '\t' => {}

            // Ignore newline characters, but increment the line counter
            '\n' => self._line += 1,
            _ => {
                if char.is_ascii_digit() {
                    // Create number literal tokens
                    self.number();
                } else if char.is_ascii_alphabetic() || char == '_' {
                    // Create identifier and keyword tokens
                    self.identifier();
                } else {
                    self.errors.push(LexicalError::new(
                        self._line,
                        LexicalErrorType::UnexpectedCharacter(char),
                    ));
                }
            }
        }
    }

    // Helper function that converts a word into an identifier or keyword token
    fn identifier(&mut self) {
        while self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self._source[self._start..self._current];
        let token_type = self.keyword_token(text.iter().collect::<String>().as_str());
        self.add_token(token_type.to_owned(), 0);
    }

    // Helper function that converts a base 10 encoded word into an integer literal token
    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Convert the base 10 encoded word into an unsigned 64 bit integer literal token
        let number = match u64::from_str(
            &self._source[self._start..self._current]
                .iter()
                .collect::<String>(),
        ) {
            Ok(num) => num,
            Err(e) => panic!("Invalid number: {}", e),
        };
        self.add_token(TokenType::IntegerLiteral, number)
    }

    // Helper function to check if a character is alphanumeric or an underscore
    fn is_alpha_numeric(&self, char: char) -> bool {
        char.is_ascii_alphabetic() || char == '_' || char.is_ascii_digit()
    }

    // Helper function to advance the cursor and return the character at the new position
    fn advance(&mut self) -> char {
        let c = self._source[self._current];
        self._current += 1;
        c
    }

    // Helper function to create a new token with the given type and push it to the token stream
    fn add_token(&mut self, token_type: TokenType, literal: u64) {
        let text = &self._source[self._start..self._current];
        let token = Token::new(
            token_type,
            text.iter().collect::<String>(),
            literal,
            self._line,
            self._debug,
        );
        self.stream.push(token);
    }

    // Helper function that has the same effect as the Scanner.advance() function, but does not increment the current index
    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        };
        self._source[self._current]
    }

    // fn peek_next(&self) -> char {
    //     if self._current + 1 >= self._source.len() {
    //         return '\0';
    //     }
    //     self._source[self._current + 1]
    // }

    // fn match_char(&mut self, expected: char) -> bool {
    //     if self.is_at_end() {
    //         return false;
    //     }
    //     if self._source[self._current] != expected {
    //         return false;
    //     }
    //     self._current += 1;
    //     true
    // }

    // Helper function to check if the current character is at the end of the source code
    fn is_at_end(&self) -> bool {
        self._current >= self._source.len()
    }
}
