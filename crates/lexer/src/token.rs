use error::span::Span;
use std::cmp::Ordering;
use std::mem::discriminant;

#[derive(Debug, Clone)]
pub struct Token {
    pub tt: TokenType,
    pub span: Span
}

impl Token {
    #[must_use]
    pub const fn new(tt: TokenType, span: Span) -> Self {
        Token { tt, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Illegal {
        pos: usize,
        char: char
    },
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

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (&self.get_bool(), &other.get_bool()) {
            (Some(self_bool), Some(other_bool)) => self_bool == other_bool,
            _ => discriminant(self) == discriminant(other)
        }
    }
}

impl Literal {
    const fn get_bool(&self) -> Option<bool> {
        match &self {
            Literal::Bool(boolean) => Some(*boolean),
            _ => None,
        }
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
            Op::Eq => 0, // Eq
            Op::Or => 1, // Or
            Op::And => 2, // And
            Op::Add | Op::Sub => 4, // Term
            Op::Mul | Op::Div | Op::Rem => 5, // Factor
            Op::Pow => 6, // Caret
            Op::Not  => 7,
            _ => 3,
        }
    }
}

impl PartialOrd for Op {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.get_precedence().cmp(&other.get_precedence()))
    }
}

/*
impl Debug for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::ILLEGAL { pos, char } => {
                write!(f, "{} Illegal char '{}'", " ".repeat(*pos), char)?
            }
            _ => write!(f, "{:?}", self)?
        }

        Ok(())
    }
}
*/


#[test]
fn op_cmp_test() {
    use std::cmp::Ordering::Less;
    let (a, b) = (Op::Add, Op::Mul);
    assert_eq!(a.partial_cmp(&b), Some(Less));
    assert!(a <= b);
    assert!(a < b);
    assert!(a != b);
}