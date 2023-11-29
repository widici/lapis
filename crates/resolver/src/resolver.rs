use std::collections::{HashMap, hash_map::Entry::{Vacant, Occupied}};
use log::info;
use ast::{Visitor, ExpressionEnum};
use ast::{Expression, Statement, StatementEnum};

pub struct Resolver {
    /// Scopes -> scope -> idents
    scopes: Vec<Vec<String>>,
    // Call/var/assign-expr -> disctance
    pub side_table: HashMap<Expression, usize>,
}

impl Resolver {
    pub fn new() -> Self {
        let scopes: Vec<Vec<String>> = Vec::new();
        let side_table: HashMap<Expression, usize> = HashMap::new();
        Resolver { scopes, side_table }
    }

    pub fn resolve(&mut self, stmts: Vec<Statement>) -> Result<(), String> {
        self.new_scope();
        self.resolve_inner(stmts)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_inner(&mut self, stmts: Vec<Statement>) -> Result<(), String> {
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

    fn update_side_table(&mut self, expr: Expression, ident: String) {
        for (distance, scope) in self.scopes.iter().rev().enumerate() {
            if !scope.contains(&ident) {
                continue;
            }
            
            match self.side_table.entry(expr) {
                Occupied(..) => unimplemented!(),
                Vacant(entry ) => { entry.insert(distance); },
            }
            return;
        }
        unimplemented!()
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

    fn visit_expr(&mut self, expr: Expression) -> Self::E {
        match *expr.expr_enum.clone() {
            ExpressionEnum::BinOp { left, right, .. } => {
                self.visit_expr(left)?;
                self.visit_expr(right)?;
            },
            ExpressionEnum::UnaryOp { expr, .. } => self.visit_expr(expr)?,
            ExpressionEnum::Call { ident, params } => {
                for param in params {
                    self.visit_expr(param)?;
                }
                self.update_side_table(expr, ident)
            },
            ExpressionEnum::Assignment { ident, right } => {
                self.visit_expr(right)?;
                self.update_side_table(expr, ident)
            }
            ExpressionEnum::Var { ident } => self.update_side_table(expr, ident),
            ExpressionEnum::Literal(..) => {},
        }

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: Statement) -> Self::S {
        match *stmt.stmt_enum {
            StatementEnum::Return { expr } => self.visit_expr(expr)?,
            StatementEnum::Expression(expr) => self.visit_expr(expr)?,
            StatementEnum::Block { stmts } => {
                self.new_scope();
                self.resolve_inner(stmts)?;
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