use ast::{Expression, Statement, ExpressionEnum, StatementEnum};
use crate::callable::{Function, Callable};
use resolver::Resolver;
use lexer::token::{Op, Literal::{Float, Int, Bool}};
use ast::Visitor;
use crate::env::{StackType, Enviroment};
use std::ops::{Add, Sub, Mul, Div, Rem};

pub struct Evaluator {
    pub(crate) resolver: Resolver,
    pub(crate) env: Enviroment,
}

impl Evaluator {
    pub fn new(mut resolver: Resolver, mut env: Enviroment) -> Self {
        resolver.new_scope(); // Add global scope
        env.new_node(); // Add global node
        Evaluator { resolver, env }
    }

    pub fn evaluate(&mut self, stmts: Vec<Statement>) -> Result<(), StatementErr> {
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    pub(crate) fn execute_block(&mut self, stmts: Vec<Statement>) -> Result<(), StatementErr> {
        self.env.new_node();
        self.resolver.new_scope();
        let result = self.evaluate(stmts);
        self.resolver.end_scope();
        self.env.drop();
        return result
    }

    fn perform_arth_op<T>(left: T, operator: Op, right: T) -> Option<T> 
        where T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + Rem<Output = T> + Pow<Output = T>
    {
        return Some(match operator {
            Op::Add => left + right,
            Op::Sub => left - right,
            Op::Mul => left * right,
            Op::Div => left / right,
            Op::Pow => left.pow(right),
            Op::Rem => left % right,
            _ => return None,
        })
    }

    fn perform_comp_op<T>(left: T, operator: Op, right: T) -> Option<bool>
        where T: PartialOrd + PartialEq
    {
        return Some(match operator {
            Op::EqEq | Op::Ne => Evaluator::perform_eq_op(left, operator, right).unwrap(),
            Op::Gt => left > right,
            Op::Ge => left >= right,
            Op::Lt => left < right,
            Op::Le => left <= right,
            _ => return None,
        })
    }

    fn perform_eq_op<T>(left: T, operator: Op, right: T) -> Option<bool>
        where T: PartialEq
    {
        return Some(match operator {
            Op::EqEq => left == right,
            Op::Ne => left != right,
            _ => return None,
        })
    }

    fn perform_bool_op(left: bool, operator: Op, right: bool) -> Option<bool> {
        Some(match operator {
            Op::EqEq | Op::Ne => return Self::perform_eq_op(left, operator, right),
            Op::And => left && right,
            Op::Or => left || right,
            _ => return None,
        })
    }

    fn execute_cond_branch(&mut self, condition: Expression, block: Statement) -> Option<Result<(), StatementErr>>{
        let evaled = self.visit_expr(condition);
        match evaled {
            StackType::Literal(Bool(boolean)) => {
                if boolean {
                    return Some(self.visit_stmt(block))
                }
            },
            _ => unimplemented!()
        }
        None
    }
}

pub enum StatementErr {
    Return(StackType),
    Continue,
    Break,
}

impl Visitor for Evaluator {
    type E = StackType;
    type S = Result<(), StatementErr>;

    fn visit_expr(&mut self, expr: Expression) -> Self::E {
        //println!("{:?}, {:?}-{:?}", expr.expr, expr.start, expr.end);
        return match *expr.expr {
            ExpressionEnum::Literal(numeric) => StackType::Literal(numeric),
            ExpressionEnum::BinOp { left, operator, right } => {
                let (left_st, right_st) = (self.visit_expr(left), self.visit_expr(right));
                match (left_st, right_st) {
                    (StackType::Literal(Int(left)), StackType::Literal(Int(right))) => {
                        if let Some(int) = Evaluator::perform_arth_op(left, operator, right) {
                            StackType::Literal(Int(int))
                        } else if let Some(bool) = Evaluator::perform_comp_op(left, operator, right) {
                            StackType::Literal(Bool(bool))
                        } else { unimplemented!() }
                    },
                    (StackType::Literal(Float(left)), StackType::Literal(Float(right))) => {
                        if let Some(float) = Evaluator::perform_arth_op(left, operator, right) {
                            StackType::Literal(Float(float))
                        } else if let Some(bool) = Evaluator::perform_comp_op(left, operator, right) {
                            StackType::Literal(Bool(bool))
                        } else { unimplemented!() }
                    },
                    (StackType::Literal(Bool(left_bool)), StackType::Literal(Bool(right_bool))) => {
                        if let Some(bool) = Evaluator::perform_bool_op(left_bool, operator, right_bool) {
                            StackType::Literal(Bool(bool))
                        } else { unimplemented!() }
                    }
                    _ => unimplemented!()
                }
            },
            ExpressionEnum::Var { ident } => {
                self.env.get(ident).unwrap()
            },
            ExpressionEnum::Assignment { ident, expr } => {  
                let value = self.visit_expr(expr);
                self.env.assign(ident, value.clone());
                value
            },
            ExpressionEnum::Call { ident, params } => {
                let params: Vec<StackType> = params.into_iter()
                    .map(|param| self.visit_expr(param))
                    .collect();

                let fn_decl = match self.env.get(ident) {
                    Some(decl) => decl,
                    None => unimplemented!()
                };

                let mut function: Function = fn_decl.into();
                function.call(self, params)
            }
        }
    }

    fn visit_stmt(&mut self, stmt: Statement) -> Self::S {
        match *stmt.stmt.clone() {
            StatementEnum::Expression(expr) => {
                self.visit_expr(expr);
            },
            StatementEnum::VarDeclaration { ident, expr } => {
                let evaluated_expr = self.visit_expr(expr);
                match self.resolver.visit_stmt(stmt) {
                    Ok(()) => self.env.declare(ident, evaluated_expr),
                    Err(_) => unimplemented!(),
                };
                return Ok(())
            },
            StatementEnum::Block { stmts } => {
                self.resolver.new_scope();
                self.env.new_node();
                return self.execute_block(stmts)
            },
            StatementEnum::FnDeclaration { ident, params, block } => {
                let value = StackType::Function { params, block: block };
                self.env.declare(ident, value)
            },
            StatementEnum::Return { expr } => return Err(StatementErr::Return(self.visit_expr(expr))),
            StatementEnum::If { if_branch, elif_branches, else_branch } => {
                if let Some(result) = self.execute_cond_branch(if_branch.0, if_branch.1) {
                    return result
                }
                for branch in elif_branches {
                    if let Some(result) = self.execute_cond_branch(branch.0, branch.1) {
                        return result
                    }
                }
                if let Some(block) = else_branch {
                    match *block.stmt {
                        StatementEnum::Block { stmts } => return self.execute_block(stmts),
                        _ => panic!("Expected block")
                    }
                }
            },
            StatementEnum::While { condition, block } => {
                while self.visit_expr(condition.clone()) == StackType::Literal(Bool(true)) {
                    if let StatementEnum::Block { stmts } = *block.stmt.clone() {
                        if let Err(e) = self.execute_block(stmts) {
                            match e {
                                StatementErr::Break => break,
                                StatementErr::Continue => continue,
                                e => return Err(e)
                            }
                        }
                    }
                }
            },
            StatementEnum::Break => return Err(StatementErr::Break),
            StatementEnum::Continue => return Err(StatementErr::Continue),
        }
        Ok(())
    }
}

trait Pow<Rhs = Self> {
    type Output;
    fn pow(self, rhs: Rhs) -> Self::Output;
}

impl Pow<i64> for i64 {
    type Output = i64;
    fn pow(self, rhs: i64) -> Self::Output {
        self.pow(rhs as u32) as i64
    }
}

impl Pow<f64> for f64 {
    type Output = f64;
    fn pow(self, rhs: f64) -> Self::Output {
        self.powf(rhs)
    }
}