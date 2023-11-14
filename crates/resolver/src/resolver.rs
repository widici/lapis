use log::info;

use ast::Visitor;
use ast::{Expression, Statement, StatementEnum};

pub struct Resolver {
    /// Scopes -> scope -> idents
    scopes: Vec<Vec<String>>,
}

impl Resolver {
    pub fn new() -> Self {
        let scopes: Vec<Vec<String>> = Vec::new();
        Resolver { scopes }
    }

    pub fn new_scope(&mut self) {
        self.scopes.push(Vec::new());
        info!("Added scope: {:?}", self.scopes);
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
        info!("Removed scope: {:?}", self.scopes);
    }

    fn declare(&mut self, ident: String) -> Result<(), String> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains(&ident) {
                unimplemented!()
            }
            scope.push(ident)
        } else {
            panic!("{:?}", self.scopes)
        }
        info!("Scopes: {:?}", self.scopes);
        Ok(())
    }
}

impl Visitor for Resolver {
    type E = Result<(), String>;
    type S = Result<(), String>;

    fn visit_expr(&mut self, _expr: Expression) -> Self::E {
        unimplemented!()
    }

    fn visit_stmt(&mut self, stmt: Statement) -> Self::S {
        return match *stmt.stmt {
            StatementEnum::Expression(expr) => self.visit_expr(expr),
            StatementEnum::VarDeclaration { ident, .. } => self.declare(ident),
            _ => unimplemented!(),
        }
    }
}