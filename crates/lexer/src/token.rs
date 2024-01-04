use span::Span;
use std::cmp::Ordering;
use std::mem::discriminant;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub tt: TokenType,
    pub span: Span,
}

impl Token {
    #[must_use]
    pub const fn new(tt: TokenType, span: Span) -> Self {
        Token { tt, span }
    }

    #[must_use]
    pub fn get_str_ident(&self) -> &String {
        match &self.tt {
            TokenType::Ident(ident) => ident,
            _ => unreachable!("Expected tt ident"),
        }
    }
}

impl From<Token> for Op {
    fn from(value: Token) -> Self {
        match value.tt {
            TokenType::Op(op) => op,
            _ => unreachable!("Expected tt op"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Illegal { pos: usize, char: char },
    EOF,
    Ident(String),
    Op(Op),
    Literal(Literal),

    // Keywords
    Var,
    Fn,
    Return,
    While,
    Continue,
    Break,
    If,
    Elif,
    Else,

    Comma,

    // Parentheses
    LParen,
    RParen,

    // Curly braces
    LCurly,
    RCurly,
}

#[derive(Debug, Clone, PartialOrd)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
}

impl Literal {
    pub fn cmp_type(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Op {
    Not,
    //Neg,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Rem,

    // Assignment
    Eq,
    /*
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    PowEq,
    RemEq,
    */
    // Comparison
    EqEq,
    Ne,
    Gt, // Greater than
    Lt, // Lesser than
    Ge, // Greater than or equal to
    Le, // Lesser than or equal to
    And,
    Or,
}

impl Op {
    #[must_use]
    pub const fn get_precedence(&self) -> u8 {
        match &self {
            Op::Eq => 0,                      // Eq
            Op::Or => 1,                      // Or
            Op::And => 2,                     // And
            Op::Add | Op::Sub => 4,           // Term
            Op::Mul | Op::Div | Op::Rem => 5, // Factor
            Op::Pow => 6,                     // Caret
            Op::Not => 7,
            _ => 3,
        }
    }
}

impl PartialOrd for Op {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.get_precedence().cmp(&other.get_precedence()))
    }
}

#[cfg(test)]
mod tests {
    use super::Op;
    use std::cmp::Ordering::Less;

    #[test]
    fn op_cmp_test() {
        let (a, b) = (Op::Add, Op::Mul);
        assert_eq!(a.partial_cmp(&b), Some(Less));
        assert!(a <= b);
        assert!(a < b);
        assert!(a != b);
    }
}
