use crate::frontend::lexer::Token;

pub fn token_pos(token: &Token) -> String {
    format!("[line:char {}:{}]", token.line, token.char)
}
