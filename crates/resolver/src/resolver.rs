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

    fn resolve(&mut self, stmts: Vec<Statement>) -> Result<(), String> {
        for stmt in stmts {
            self.visit_stmt(stmt)?
        }

        Ok(())
    }

    fn new_scope(&mut self) {
        self.scopes.push(Vec::new());
        info!("Added scope: {:?}", self.scopes);
    }

    fn end_scope(&mut self) {
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

    fn resolve_cond_branch(&mut self, branch: (Expression, Statement)) -> Result<(), String>{
        self.visit_expr(branch.0)?;
        self.visit_stmt(branch.1)?;
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
        match *stmt.stmt {
            StatementEnum::Expression(expr) => self.visit_expr(expr)?,
            StatementEnum::VarDeclaration { ident, .. } => self.declare(ident)?,
            StatementEnum::Return { expr } => self.visit_expr(expr)?,
            StatementEnum::Return { expr } => self.visit_expr(expr)?,
            StatementEnum::Block { stmts } => {
                self.new_scope();
                self.resolve(stmts)?;
                self.end_scope();
            },
            StatementEnum::FnDeclaration { block, ident, params } => {
                self.declare(ident)?;
                self.new_scope();
                for param in params {
                    self.declare(param)?;
                }
                self.visit_stmt(block)?;
                self.end_scope();
            },
            StatementEnum::VarDeclaration { ident, expr } => {
                self.visit_expr(expr)?;
                self.declare(ident)?;
            },
            StatementEnum::If { if_branch, elif_branches, else_branch } => {
                self.resolve_cond_branch(if_branch)?;
                for branch in elif_branches {
                    self.resolve_cond_branch(branch)?;
                }
                if let Some(block) = else_branch {
                    self.visit_stmt(block)?;
                }
            },
            StatementEnum::While { condition, block } => {
                self.visit_expr(condition)?;
                self.visit_stmt(block)?;
            },
            StatementEnum::Break | StatementEnum::Continue => {},
        }

        Ok(())
    }
}