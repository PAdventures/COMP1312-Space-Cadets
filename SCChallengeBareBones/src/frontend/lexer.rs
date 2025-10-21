use crate::utils::token_pos;

// Import necessary modules
use super::errors::{LexicalError, LexicalErrorType};
use std::{fmt, str::FromStr};

// Define the TokenType enum which contains all possible token types
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Comparison
    Equal,        // eq
    NotEqual,     // ne
    Greater,      // gt
    Less,         // lt
    GreaterEqual, // ge
    LessEqual,    // le

    // Logical
    And, // and
    Or,  // or

    // Binary
    Add,      // add
    Subtract, // sub
    Multiply, // mul
    Divide,   // div

    // Unary
    Negate, // neg
    Not,    // not

    // Variable assignment
    Clear,     // clear
    Increment, // incr
    Decrement, // decr
    Set,       // set
    Copy,      // copy

    // Iteration
    For,   // for
    While, // while
    Do,    // do

    // Control flow
    If,     // if
    Then,   // then
    ElseIf, // elif
    Else,   // else

    // Functions
    Function,  // fn
    Parameter, // param
    Return,    // ret
    Call,      // call
    Argument,  // arg

    // Characters
    LeftParen,  // (
    RightParen, // )
    // LeftBracket,  // [
    // RightBracket, // ]
    SemiColon, // ;

    // Literals
    IntegerLiteral,
    FloatLiteral,
    CharacterLiteral,
    StringLiteral,
    BooleanLiteral,

    // Others
    Identifier,
    To,   // to
    From, // from
    By,   // by
    Into, // into
    In,   // in
    End,  // end

    EOF, // End Of File
}

// Implement the Display trait for TokenType
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// Define the TokenLiteral enum
#[derive(Debug, Clone)]
pub enum TokenLiteral {
    Identifier(String),
    Integer(i64),
    Float(f64),
    Character(char),
    String(String),
    Boolean(bool),
    Null,
}

// Implement the Display trait for TokenLiteral
impl fmt::Display for TokenLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenLiteral::Identifier(ident) => write!(f, "{}", ident),
            TokenLiteral::Integer(i) => write!(f, "{}", i),
            TokenLiteral::Float(fl) => write!(f, "{}", fl),
            TokenLiteral::Character(c) => write!(f, "'{}'", c),
            TokenLiteral::String(s) => write!(f, "\"{}\"", s),
            TokenLiteral::Boolean(b) => write!(f, "{}", b),
            TokenLiteral::Null => write!(f, "null"),
        }
    }
}

// Define the Token struct which contains all necessary information about a token
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: TokenLiteral,
    pub line: u32,
    pub char: u32,
    _debug: bool,
}

// Implement the Display trait for Token
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self._debug {
            return write!(
                f,
                "{} {} {} {}",
                token_pos(&self),
                self.token_type,
                self.lexeme,
                self.literal
            );
        }
        write!(f, "{} {} {}", self.token_type, self.lexeme, self.literal)
    }
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: TokenLiteral,
        line: u32,
        char: u32,
        debug: bool,
    ) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
            char,
            _debug: debug,
        }
    }

    // Helper function to create a new EOF token
    pub fn eof(line: u32, debug: bool) -> Self {
        Token::new(
            TokenType::EOF,
            String::new(),
            TokenLiteral::Null,
            line,
            0,
            debug,
        )
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
    _char: u32,
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
            _char: 0,
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
            "eq" => TokenType::Equal,
            "ne" => TokenType::NotEqual,
            "gt" => TokenType::Greater,
            "lt" => TokenType::Less,
            "ge" => TokenType::GreaterEqual,
            "le" => TokenType::LessEqual,
            "and" => TokenType::And,
            "or" => TokenType::Or,
            "add" => TokenType::Add,
            "sub" => TokenType::Subtract,
            "mul" => TokenType::Multiply,
            "div" => TokenType::Divide,
            "neg" => TokenType::Negate,
            "not" => TokenType::Not,
            "clear" => TokenType::Clear,
            "incr" => TokenType::Increment,
            "decr" => TokenType::Decrement,
            "set" => TokenType::Set,
            "copy" => TokenType::Copy,
            "for" => TokenType::For,
            "while" => TokenType::While,
            "do" => TokenType::Do,
            "if" => TokenType::If,
            "then" => TokenType::Then,
            "elif" => TokenType::ElseIf,
            "else" => TokenType::Else,
            "fn" => TokenType::Function,
            "param" => TokenType::Parameter,
            "ret" => TokenType::Return,
            "arg" => TokenType::Argument,
            "call" => TokenType::Call,
            "to" => TokenType::To,
            "from" => TokenType::From,
            "by" => TokenType::By,
            "into" => TokenType::Into,
            "in" => TokenType::In,
            "end" => TokenType::End,
            _ => TokenType::Identifier,
        }
    }

    // Helper function that scans one or more characters and turns them into a single token
    fn scan_token(&mut self) {
        let char = self.advance();
        match char {
            '(' => self.add_token(TokenType::LeftParen, TokenLiteral::Null),
            ')' => self.add_token(TokenType::RightParen, TokenLiteral::Null),
            // '[' => self.add_token(TokenType::LeftBracket, TokenLiteral::Null),
            // ']' => self.add_token(TokenType::RightBracket, TokenLiteral::Null),
            ';' => self.add_token(TokenType::SemiColon, TokenLiteral::Null),
            '\'' => self.character(),
            '"' => self.string(),
            '/' => {
                if self.peek() == '/' {
                    self.comment();
                } else {
                    self.error(LexicalErrorType::UnexpectedCharacter(char));
                }
            }

            // Ignore whitespace characters (spaces, carriage returns, tabs)
            ' ' | '\r' | '\t' => self._char += 1,

            // Ignore newline characters, but increment the line counter
            '\n' => {
                self._line += 1;
                self._char = 0;
            }
            _ => {
                if char.is_ascii_digit() || char == '-' {
                    // Create number literal tokens
                    self.number();
                } else if char.is_ascii_alphabetic() || char == '_' {
                    // Create identifier and keyword tokens
                    self.identifier();
                } else {
                    self.error(LexicalErrorType::UnexpectedCharacter(char));
                }
            }
        }
    }

    // Helper function that advances past a comment
    fn comment(&mut self) {
        self.advance(); // Advance past the second slash character

        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }

        self.advance(); // Advance past the newline character

        self._line += 1;
        self._char = 0;
    }

    // Helper function that converts a sequence of characters into a string literal token
    fn string(&mut self) {
        let mut result = String::new();

        while !self.is_at_end() {
            let c = self.peek();

            // End of string (unescaped quote)
            if c == '"' {
                break;
            }

            // Handle escape sequences
            if c == '\\' {
                self.advance(); // consume '\'
                if self.is_at_end() {
                    self.error(LexicalErrorType::UnterminatedStringLiteral);
                    return;
                }

                let next = self.peek();
                let escaped_char = self.escaped(&['\\', next]);
                if escaped_char == '\0' {
                    self.error(LexicalErrorType::InvalidEscapeSequence(format!(
                        "\\{}",
                        next
                    )));
                    return;
                }

                result.push(escaped_char);
                self.advance(); // consume escaped character
                continue;
            }

            // Normal character
            result.push(c);
            self.advance();
        }

        // Expect closing quote
        if !self.match_char('"') {
            self.error(LexicalErrorType::UnterminatedStringLiteral);
            return;
        }

        self.add_token(TokenType::StringLiteral, TokenLiteral::String(result));
    }

    // Helper function that converts a sequence of characters into a character literal token
    fn character(&mut self) {
        if self.is_at_end() {
            self.error(LexicalErrorType::UnterminatedCharacterLiteral);
            return;
        }

        #[allow(unused_assignments)]
        let mut value: Option<char> = None;

        if self.peek() == '\\' {
            // Handle escaped character like '\n' or '\\'
            self.advance(); // consume '\'
            if self.is_at_end() {
                self.error(LexicalErrorType::UnterminatedCharacterLiteral);
                return;
            }
            let esc = self.peek();
            let escaped_char = self.escaped(&['\\', esc]);
            if escaped_char == '\0' {
                self.error(LexicalErrorType::InvalidEscapeSequence(format!(
                    "\\{}",
                    esc
                )));
                return;
            }
            value = Some(escaped_char);
            self.advance(); // consume the escaped character
        } else {
            // Normal single char
            value = Some(self.peek());
            self.advance();
        }

        // Expect closing quote
        if !self.match_char('\'') {
            self.error(LexicalErrorType::UnterminatedCharacterLiteral);
            return;
        }

        // Check for empty or too long char literals
        if let Some(ch) = value {
            self.add_token(TokenType::CharacterLiteral, TokenLiteral::Character(ch));
        } else {
            self.error(LexicalErrorType::EmptyCharacterLiteral);
        }
    }

    // Helper function that converts a word into an identifier, boolean literal, or keyword token
    fn identifier(&mut self) {
        while !self.is_at_end() && self.is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text = &self._source[self._start..self._current];
        let ident = text.iter().collect::<String>();

        if ident == "True" || ident == "False" {
            self.add_token(
                TokenType::BooleanLiteral,
                TokenLiteral::Boolean(ident == "True"),
            );
            return;
        }

        let token_type = self.keyword_token(&ident);

        if token_type == TokenType::Identifier {
            self.add_token(token_type.to_owned(), TokenLiteral::Identifier(ident));
            return;
        }

        // The token is a keyword, so we set the token literal as null
        self.add_token(token_type.to_owned(), TokenLiteral::Null);
    }

    // Helper function that converts a base 10 encoded word into an integer or float literal token
    fn number(&mut self) {
        if self.peek() == '-' {
            self.advance();
        }

        let mut seen_period = false;

        // Support for floating point numbers
        while !self.is_at_end()
            && (self.peek().is_ascii_digit() || (self.peek() == '.' && !seen_period))
        {
            if self.peek() == '.' {
                seen_period = true
            }
            self.advance();
        }

        let number_str = &self._source[self._start..self._current]
            .iter()
            .collect::<String>();

        // Check if this number is a float and convert it into a 64 bit float literal token
        if number_str.contains('.') {
            let float = match f64::from_str(number_str) {
                Ok(fl) => fl,
                Err(e) => {
                    self.error(LexicalErrorType::InvalidFloatLiteral(
                        number_str.to_owned(),
                        e.to_string(),
                    ));
                    return;
                }
            };
            self.add_token(TokenType::FloatLiteral, TokenLiteral::Float(float));
            return;
        }

        // Convert the base 10 encoded word into an unsigned 64 bit integer literal token
        let integer = match i64::from_str(number_str) {
            Ok(int) => int,
            Err(e) => {
                self.error(LexicalErrorType::InvalidIntegerLiteral(
                    number_str.to_owned(),
                    e.to_string(),
                ));
                return;
            }
        };
        self.add_token(TokenType::IntegerLiteral, TokenLiteral::Integer(integer))
    }

    // Helper function to check if a character is alphanumeric or an underscore
    fn is_alpha_numeric(&self, char: char) -> bool {
        char.is_ascii_alphabetic() || char == '_' || char.is_ascii_digit()
    }

    // Helper function to take an array of characters and return the escaped character.
    // If the sequence is not valid, the null character is returned
    fn escaped(&self, seq: &[char]) -> char {
        if seq.iter().nth(0) == Some(&'\\') {
            let escaped_char = match seq.iter().nth(1) {
                Some(c) => c,
                None => return '\0',
            };
            match escaped_char {
                'n' => return '\n',
                't' => return '\t',
                'r' => return '\r',
                '\'' => return '\'',
                '"' => return '"',
                '\\' => return '\\',
                _ => return '\0',
            }
        };
        return '\0';
    }

    // Helper function to advance the cursor and return the character at the new position
    fn advance(&mut self) -> char {
        let c = self._source[self._current];
        self._current += 1;
        c
    }

    // Helper function to create a new token with the given type and push it to the token stream
    fn add_token(&mut self, token_type: TokenType, literal: TokenLiteral) {
        let text = &self._source[self._start..self._current];
        self._char += text.len() as u32;
        let token = Token::new(
            token_type,
            text.iter().collect::<String>(),
            literal,
            self._line,
            self._char,
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

    // // Helper function that returns the previous character in the source code
    // fn peek_previous(&self) -> char {
    //     if self._current == 0 {
    //         return '\0';
    //     };
    //     self._source[self._current - 1]
    // }

    // // Helper function that has the same effect as the Scanner.peek() function, but returns the next character
    // fn peek_next(&self) -> char {
    //     if self.is_at_end() {
    //         return '\0';
    //     }
    //     self._source[self._current + 1]
    // }

    // Helper function that checks if the current character matches the expected character
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self._source[self._current] != expected {
            return false;
        }
        self._current += 1;
        true
    }

    // Helper function to create an error and push it to the error stream
    fn error(&mut self, error_type: LexicalErrorType) {
        let error = LexicalError::new(self._line, self._char, error_type);
        self.errors.push(error);
    }

    // Helper function to check if the current character is at the end of the source code
    fn is_at_end(&self) -> bool {
        self._current >= self._source.len()
    }
}
