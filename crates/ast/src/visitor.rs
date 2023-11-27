use crate::ast::{Expression, Statement};

pub trait Visitor {
    type E;
    type S;

    fn visit_expr(&mut self, expr: Expression) -> Self::E;
    fn visit_stmt(&mut self, stmt: Statement) -> Self::S;
}
