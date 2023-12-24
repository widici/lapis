use std::{hash::{Hash, Hasher}, fmt::Debug};

use lexer::token::{Literal, Op};
use span::Span;
use span_macros::GetSpan;

#[derive(Debug, Clone, PartialEq, GetSpan)]
pub struct Expression {
    pub expr_enum: Box<ExpressionEnum>,
    #[span]
    pub span: Span,
    /// This limits the amount of exprs in one program based on the target
    /// E.g. on 2 ^ 64 - 1 on 64-bit targets
    pub id: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionEnum {
    Literal(Literal),
    BinOp {
        left: Expression,
        operator: Op,
        right: Expression,
    },
    UnaryOp {
        operator: Op,
        expr: Expression,
    },
    Var {
        ident: String
    },
    Assignment {
        ident: String,
        right: Expression,
    },
    Call {
        ident: String,
        params: Vec<Expression>
    }
}

#[derive(Debug, Clone, PartialEq, GetSpan)]
pub struct Statement {
    pub stmt_enum: Box<StatementEnum>,
    #[span]
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
        stmts: Vec<Statement>,
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

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl Eq for Expression {}