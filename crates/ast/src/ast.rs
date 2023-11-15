use lexer::token::{Literal, Op};
use error::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expr: Box<ExpressionEnum>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionEnum {
    Literal(Literal),
    BinOp {
        left: Expression,
        operator: Op,
        right: Expression,
    },
    Var {
        ident: String
    },
    Assignment {
        ident: String,
        expr: Expression
    },
    Call {
        ident: String,
        params: Vec<Expression>
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub stmt: Box<StatementEnum>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementEnum {
    Expression(Expression),
    VarDeclaration {
        ident: String,
        expr: Expression
    },
    FnDeclaration {
        ident: String,
        params: Vec<String>,
        block: Statement,
    },
    Block {
        stmts: Vec<Statement>
    },
    Return {
        expr: Expression
    },
    If {
        if_branch: (Expression, Statement),
        elif_branches: Vec<(Expression, Statement)>,
        else_branch: Option<Statement>,
    },
    While {
        condition: Expression,
        block: Statement,
    },
    Continue,
    Break,
}
