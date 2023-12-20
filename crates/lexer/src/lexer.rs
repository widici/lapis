use crate::token::{Token, TokenType};
use crate::token::Literal::{Float, Int, Bool, Str, Char};
use crate::token::Op::{Div, Sub, Mul, Add, Pow, Rem, Eq, EqEq, Ne, Gt, Lt, Ge, Le, And, Or, Not};
use span::Span;

pub struct Lexer {
    input: Vec<char>,
    current_pos: usize,
    current_char: char,
    current_line: usize,
    current_col: usize,
}

impl Lexer {
    #[must_use]
    pub fn new(input: Vec<char>) -> Lexer {
        let mut lexer: Lexer = Lexer { input, current_pos: 0, current_char: '\0', current_line: 1, current_col: 1 };
        lexer.current_char = lexer.get_current_char();
        lexer
    }

    fn get_current_char(&self) -> char {
        if self.current_pos == self.input.len() {
            return '\0';
        }
        self.input[self.current_pos]
    }

    fn peek_char(&self) -> char {
        if &self.current_pos + 1 == self.input.len() {
            return '\0'
        }
        self.input[self.current_pos + 1]
    }

    fn advance(&mut self) {
        self.current_pos += 1;
        self.current_char = self.get_current_char()
    }

    fn get_token(&mut self) -> Option<TokenType> {
        Some(match &self.get_current_char() {
            '\0' => return Some(TokenType::EOF),
            '=' => match self.peek_char() {
                '=' => {
                    self.advance();
                    TokenType::Op(EqEq)
                },
                _ => TokenType::Op(Eq)
            },
            '>' => match self.peek_char() {
                '=' => {
                    self.advance();
                    TokenType::Op(Ge)
                },
                _ => TokenType::Op(Gt)
            }
            '<' => match self.peek_char() {
                '=' => {
                    self.advance();
                    TokenType::Op(Le)
                },
                _ => TokenType::Op(Lt)
            },
            '!' => match self.peek_char() {
                '=' => {
                    self.advance();
                    TokenType::Op(Ne)
                },
                _ => TokenType::Op(Not)
            }
            '+' => TokenType::Op(Add),
            '-' => TokenType::Op(Sub),
            '*' => TokenType::Op(Mul),
            '/' => match self.peek_char() {
                '/' => return self.lex_comment(),
                '*' => return self.lex_block_comment(),
                _ => TokenType::Op(Div)
            },
            '^' => TokenType::Op(Pow),
            '%' => TokenType::Op(Rem),
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            ',' => TokenType::Comma,
            '"' => self.get_string(),
            '\'' => self.get_char(),
            ' ' => {
                self.current_col += 1;
                return None
            },
            '\t' => {
                self.current_col += 4;
                return None
            }
            '\n' => {
                self.current_line += 1;
                self.current_col = 1;
                return None
            }
            '{' => TokenType::LCurly,
            '}' => TokenType::RCurly,
            v => {
                if v.is_numeric() {
                    self.get_numeric()
                } else if v.is_ascii_alphabetic() {
                    self.get_identifier()
                } else {
                    TokenType::Illegal{pos: self.current_pos, char: self.current_char}
                }
            }
        })
    }

    fn get_identifier(&mut self) -> TokenType {
        let start = self.current_pos;
        while (self.peek_char().is_ascii_alphabetic() || self.peek_char() == '_') && self.current_char != '\0' {
            self.advance();
            self.current_col += 1;
        }

        let ident: String = self.input[start..=self.current_pos].iter().collect();
        return match ident.as_str() {
            "var" => TokenType::Var,
            "fn" => TokenType::Fn,
            "return" => TokenType::Return,
            "while" => TokenType::While,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "if" => TokenType::If,
            "elif" => TokenType::Elif,
            "else" => TokenType::Else,
            "true" => TokenType::Literal(Bool(true)),
            "false" => TokenType::Literal(Bool(false)),
            "and" => TokenType::Op(And),
            "or" => TokenType::Op(Or),
            name => TokenType::Ident(name.to_string()),
        }
    }

    fn get_numeric(&mut self) -> TokenType {
        let mut int_str = String::new();
        while self.current_char.is_numeric() || (self.current_char == '.' && !int_str.contains('.')) {
            int_str.push(self.current_char);

            if !self.peek_char().is_numeric() && (self.peek_char() != '.' || int_str.contains('.')) {
                break;
            }

            self.advance();
            self.current_col += 1;
        }

        match int_str.contains('.') {
            true => {
                if int_str.ends_with('.') {
                    int_str.push('0')
                }
                TokenType::Literal(Float(int_str.parse::<f64>().unwrap()))
            },
            false => TokenType::Literal(Int(int_str.parse::<i64>().unwrap())),
        }
    }

    fn get_string(&mut self) -> TokenType {
        self.advance();
        //self.current_col += 1;
        let mut str = String::new();
        while self.current_char != '"' {
            str.push(self.current_char);
            if self.peek_char() == '\0' {
                break;
            }
            self.advance();
            self.current_col += 1;
        }
        TokenType::Literal(Str(str))
    }

    fn get_char(&mut self) -> TokenType {
        self.advance();
        assert_eq!(self.peek_char(), '\'');
        TokenType::Literal(Char(self.current_char))
    }

    fn lex_comment(&mut self) -> Option<TokenType> {
        while !['\0', '\n'].contains(&self.get_current_char()) {
            self.advance();
        }
        self.get_token()
    }

    fn lex_block_comment(&mut self) -> Option<TokenType> {
        while !(self.get_current_char() == '*' && self.peek_char() == '/') {
            let result = match self.get_current_char() {
                '\n' | '\0' => self.get_token(),
                _ => None
            };

            if result == Some(TokenType::EOF) {
                return result
            }

            self.advance()
        }
        self.advance();

        None
    }
    
    pub fn get_tokens(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            let start = self.current_col;
            let token = self.get_token();

            match token {
                None => {},
                Some(tt) => {
                    //info!("{:?} {:?} {:?}", self.get_current_char(), self.current_pos, tt);
                    let span = Span::new(start, self.current_pos);
                    tokens.push(Token::new(tt, span));
                    self.current_col += 1;
                }
            }

            if self.get_current_char() == '\0' {
                break;
            }
            self.advance()
        }

        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Literal;
    use crate::token::Op;

    #[test]
    fn lexer_test() {
        let test_cases = [
            ("11 + 2 * 8", vec![TokenType::Literal(Int(11)), TokenType::Op(Add), TokenType::Literal(Int(2)), TokenType::Op(Mul), TokenType::Literal(Int(8)), TokenType::EOF]),
            ("if true {\n\tvar x = 10\n}", vec![TokenType::If, TokenType::Literal(Literal::Bool(true)), TokenType::LCurly, TokenType::Var, TokenType::Ident("x".to_string()), TokenType::Op(Op::Eq), TokenType::Literal(Literal::Int(10 as i64)), TokenType::RCurly, TokenType::EOF])
        ];
        
        for case in test_cases {
            let input: Vec<char> = case.0.chars().collect();
            let mut lexer = Lexer::new(input);
            let result = lexer.get_tokens();
            let result: Vec<TokenType> = result.into_iter().map(|token| token.tt).collect();
            assert_eq!(result, case.1)
        }
    }
}