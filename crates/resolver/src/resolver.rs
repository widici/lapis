use std::collections::{HashMap, hash_map::Entry::{Vacant, Occupied}};
use log::info;
use ast::{Visitor, ExpressionEnum};
use ast::{Expression, Statement, StatementEnum};
use error::{ErrorLocation, Error, ErrorKind::{StmtUnexpectedContext, Redefenition, NotFound}};
use error::impl_error_handling;

pub struct Resolver {
    /// Scopes -> scope -> idents
    scopes: Vec<Vec<String>>,
    // Call/var/assign-expr -> distance
    pub side_table: HashMap<Expression, usize>,
    errors: Vec<Error>,
    fn_type: FnType,
    in_loop: bool,
}

impl_error_handling!(Resolver, ErrorLocation::Resolver);

impl Resolver {
    #[must_use]
    pub fn new() -> Self {
        let scopes: Vec<Vec<String>> = Vec::new();
        let side_table: HashMap<Expression, usize> = HashMap::new();
        Resolver { scopes, side_table, errors: Vec::new(), fn_type: FnType::None, in_loop: false }
    }

    pub fn resolve(mut self, stmts: Vec<Statement>) -> HashMap<Expression, usize> {
        self.new_scope();
        self.resolve_inner(stmts);
        self.end_scope();
        self.report_errors();
        self.side_table
    }

    fn resolve_inner(&mut self, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.visit_stmt(stmt)
        }
    }

    fn new_scope(&mut self) {
        self.scopes.push(Vec::new());
        info!("Added scope: {:?}", self.scopes);
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
        info!("Removed scope: {:?}", self.scopes);
    }

    fn declare(&mut self, ident: String) {
        if let Some(scope) = self.scopes.last_mut() {
            if !scope.contains(&ident) {
                scope.push(ident.clone())
            } else {
                self.add_error(Redefenition { ident })
            }
        } else {
            unreachable!("Failed to find scope in resolver")
        }
        info!("Declared in scopes: {:?}", self.scopes);
    }

    fn update_side_table(&mut self, expr: Expression, ident: &String) {
        for (distance, scope) in self.scopes.iter().rev().enumerate() {
            if !scope.contains(ident) {
                continue;
            }

            match self.side_table.entry(expr) {
                Occupied(..) => unreachable!("Expr-key already added in resolver side-table"),
                Vacant(entry ) => { entry.insert(distance); },
            }
            return;
        }
        self.add_error(NotFound { ident: ident.clone() })
    }

    fn resolve_cond_branch(&mut self, branch: (Expression, Statement)) {
        let _ = self.visit_expr(branch.0);
        let _ = self.visit_stmt(branch.1);
    }
}

impl Visitor for Resolver {
    type E = ();
    type S = ();

    fn visit_expr(&mut self, expr: Expression) -> Self::E {
        match *expr.expr_enum.clone() {
            ExpressionEnum::BinOp { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            },
            ExpressionEnum::UnaryOp { expr, .. } => self.visit_expr(expr),
            ExpressionEnum::Call { ident, params } => {
                for param in params {
                    self.visit_expr(param);
                }
                self.update_side_table(expr, &ident)
            },
            ExpressionEnum::Assignment { ident, right } => {
                self.visit_expr(right);
                self.update_side_table(expr, &ident)
            }
            ExpressionEnum::Var { ident } => self.update_side_table(expr, &ident),
            ExpressionEnum::Literal(..) => {},
        }
    }

    fn visit_stmt(&mut self, stmt: Statement) -> Self::S {
        match *stmt.stmt_enum.clone() {
            ref stmt_enum @ StatementEnum::Return { ref expr } => {
                match self.fn_type {
                    FnType::Fn => self.visit_expr(expr.clone()),
                    FnType::None => self.add_error(StmtUnexpectedContext { stmt: format!("{:?}", stmt_enum), span: stmt.get_span().clone() })
                }
            },
            StatementEnum::Expression(expr) => self.visit_expr(expr),
            StatementEnum::Block { stmts } => {
                self.new_scope();
                self.resolve_inner(stmts);
                self.end_scope();
            },
            StatementEnum::FnDeclaration { stmts, ident, params } => {
                self.declare(ident);
                self.fn_type = FnType::Fn;
                self.new_scope();
                for param in params {
                    self.declare(param);
                }
                for stmt in stmts {
                    self.visit_stmt(stmt)
                }
                self.end_scope();
                self.fn_type = FnType::None;
            },
            StatementEnum::VarDeclaration { ident, expr } => {
                self.visit_expr(expr);
                self.declare(ident);
            },
            StatementEnum::If { if_branch, elif_branches, else_branch } => {
                self.resolve_cond_branch(if_branch);
                for branch in elif_branches {
                    self.resolve_cond_branch(branch);
                }
                if let Some(block) = else_branch {
                    self.visit_stmt(block);
                }
            },
            StatementEnum::While { condition, block } => {
                self.visit_expr(condition);
                self.in_loop = true;
                self.visit_stmt(block);
                self.in_loop = false;
            },
            stmt_enum @ StatementEnum::Break | stmt_enum @ StatementEnum::Continue => {
                if !self.in_loop {
                    self.add_error(StmtUnexpectedContext { stmt: format!("{:?}", stmt_enum), span: stmt.get_span().clone() })
                }
            },
        }
    }
}

enum FnType {
    Fn,
    None,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use ast::{StatementEnum, Expression, ExpressionEnum, Statement};
    use span::Span;
    use lexer::token::Literal;
    use crate::Resolver;

    #[test]
    fn resolver_test() {
        let var_expr = Expression { id: 0, expr_enum: Box::new(ExpressionEnum::Var { ident: "x".to_string() }), span: Span::new(0, 0) };
        let test_cases: Vec<Statement> = [
            StatementEnum::VarDeclaration { ident: "x".to_string(), expr: Expression { id: 0, expr_enum: Box::new(ExpressionEnum::Literal(Literal::Int(0))), span: Span::new(0, 0) } },
            StatementEnum::Expression(var_expr.clone()),
        ].into_iter().map(|stmt_enum| Statement { stmt_enum: Box::new(stmt_enum), span: Span::new(0, 0) }).collect();

        let resolver = Resolver::new();
        let side_table = resolver.resolve(test_cases);
        assert_eq!(side_table, HashMap::from([(var_expr, 0 as usize)]))
    }
}