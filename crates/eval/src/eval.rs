use crate::callable::{Callable, Function};
use crate::env::{Enviroment, StackType};
use ast::Visitor;
use ast::{Expression, ExpressionEnum, Statement, StatementEnum};
use lexer::token::TokenType;
use lexer::token::{
    Literal::{self, Bool, Char, Float, Int, Str},
    Op,
};
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};

pub struct Evaluator {
    pub(crate) env: Enviroment,
}

impl Evaluator {
    #[must_use]
    pub fn new(mut env: Enviroment) -> Self {
        //resolver.new_scope(); // Add global scope
        env.new_node(); // Add global node
        Evaluator { env }
    }

    pub fn evaluate(&mut self, stmts: Vec<Statement>) -> Result<(), StatementErr> {
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    pub(crate) fn execute_block(&mut self, stmts: Vec<Statement>) -> Result<(), StatementErr> {
        self.env.new_node();
        let result = self.evaluate(stmts);
        self.env.drop();
        result
    }

    fn perform_arth_op<T>(left: T, operator: Op, right: T) -> Option<T>
    where
        T: Add<Output = T>
            + Sub<Output = T>
            + Mul<Output = T>
            + Div<Output = T>
            + Rem<Output = T>
            + Pow<Output = T>,
    {
        Some(match operator {
            Op::Add => left + right,
            Op::Sub => left - right,
            Op::Mul => left * right,
            Op::Div => left / right,
            Op::Pow => left.pow(right),
            Op::Rem => left % right,
            _ => return None,
        })
    }

    fn perform_comp_op<T>(left: &T, operator: Op, right: &T) -> Option<bool>
    where
        T: PartialOrd + PartialEq,
    {
        Some(match operator {
            Op::EqEq | Op::Ne => Evaluator::perform_eq_op(left, operator, right).unwrap(),
            Op::Gt => left > right,
            Op::Ge => left >= right,
            Op::Lt => left < right,
            Op::Le => left <= right,
            _ => return None,
        })
    }

    fn perform_eq_op<T>(left: &T, operator: Op, right: &T) -> Option<bool>
    where
        T: PartialEq,
    {
        Some(match operator {
            Op::EqEq => left == right,
            Op::Ne => left != right,
            _ => return None,
        })
    }

    fn perform_bool_op(left: bool, operator: Op, right: bool) -> Option<bool> {
        Some(match operator {
            Op::EqEq | Op::Ne => return Self::perform_eq_op(&left, operator, &right),
            Op::And => left && right,
            Op::Or => left || right,
            _ => return None,
        })
    }

    fn execute_cond_branch(
        &mut self,
        condition: Expression,
        block: Statement,
    ) -> Option<Result<(), StatementErr>> {
        let evaluated = self.visit_expr(condition);
        match evaluated {
            StackType::Literal(Bool(boolean)) => {
                if boolean {
                    return Some(self.visit_stmt(block));
                }
            }
            _ => unimplemented!(),
        }
        None
    }
}

#[derive(PartialEq, Debug)]
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
        match *expr.expr_enum.clone() {
            ExpressionEnum::Literal(token) => {
                if let TokenType::Literal(literal) = token.tt {
                    StackType::Literal(literal)
                } else {
                    unimplemented!()
                }
            }
            ExpressionEnum::BinOp {
                left,
                operator,
                right,
            } => {
                let operator: Op = operator.into();
                let (left_st, right_st) = (self.visit_expr(left), self.visit_expr(right));
                match (left_st, right_st) {
                    (StackType::Literal(Int(left)), StackType::Literal(Int(right))) => {
                        if let Some(int) = Evaluator::perform_arth_op(left, operator, right) {
                            StackType::Literal(Int(int))
                        } else if let Some(bool) =
                            Evaluator::perform_comp_op(&left, operator, &right)
                        {
                            StackType::Literal(Bool(bool))
                        } else {
                            unimplemented!()
                        }
                    }
                    (StackType::Literal(Float(left)), StackType::Literal(Float(right))) => {
                        if let Some(float) = Evaluator::perform_arth_op(left, operator, right) {
                            StackType::Literal(Float(float))
                        } else if let Some(bool) =
                            Evaluator::perform_comp_op(&left, operator, &right)
                        {
                            StackType::Literal(Bool(bool))
                        } else {
                            unimplemented!()
                        }
                    }
                    (StackType::Literal(Bool(left_bool)), StackType::Literal(Bool(right_bool))) => {
                        if let Some(bool) =
                            Evaluator::perform_bool_op(left_bool, operator, right_bool)
                        {
                            StackType::Literal(Bool(bool))
                        } else {
                            unimplemented!()
                        }
                    }
                    (StackType::Literal(Str(left_str)), StackType::Literal(Str(right_str))) => {
                        if let Some(bool) =
                            Evaluator::perform_eq_op(&left_str, operator, &right_str)
                        {
                            StackType::Literal(Bool(bool))
                        } else {
                            unimplemented!()
                        }
                    }
                    (StackType::Literal(Char(left_char)), StackType::Literal(Char(right_char))) => {
                        if let Some(bool) =
                            Evaluator::perform_eq_op(&left_char, operator, &right_char)
                        {
                            StackType::Literal(Bool(bool))
                        } else {
                            unimplemented!()
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ExpressionEnum::UnaryOp { operator, expr } => {
                let stack_type = self.visit_expr(expr);
                match operator.into() {
                    Op::Sub => -stack_type,
                    Op::Not => !stack_type,
                    _ => unreachable!(),
                }
            }
            ExpressionEnum::Var { ident } => self.env.get(&ident.get_str_ident(), &expr).unwrap(),
            ExpressionEnum::Assignment { ident, right } => {
                let value = self.visit_expr(right);
                self.env
                    .assign(ident.get_str_ident().to_owned(), value.clone(), &expr);
                value
            }
            ExpressionEnum::Call { ident, params } => {
                let params: Vec<StackType> = params
                    .into_iter()
                    .map(|param| self.visit_expr(param))
                    .collect();

                let fn_decl = match self.env.get(ident.get_str_ident(), &expr) {
                    Some(decl) => decl,
                    None => unimplemented!(),
                };

                let mut function: Function = fn_decl.into();
                function.call(self, params)
            }
        }
    }

    fn visit_stmt(&mut self, stmt: Statement) -> Self::S {
        match *stmt.stmt_enum {
            StatementEnum::Expression(expr) => {
                self.visit_expr(expr);
            }
            StatementEnum::VarDeclaration { ident, expr } => {
                let evaluated_expr = self.visit_expr(expr);
                self.env
                    .declare(ident.get_str_ident().to_owned(), evaluated_expr);
            }
            StatementEnum::Block { stmts } => return self.execute_block(stmts),
            StatementEnum::FnDeclaration {
                ident,
                params,
                stmts,
            } => {
                let env_id = self.env.nodes.len() - 1;
                let value = StackType::Function {
                    params: params
                        .iter()
                        .map(|param| param.get_str_ident().to_owned())
                        .collect(),
                    stmts,
                    env_id,
                };
                self.env.declare(ident.get_str_ident().to_owned(), value)
            }
            StatementEnum::Return { expr } => {
                return Err(StatementErr::Return(self.visit_expr(expr)))
            }
            StatementEnum::If {
                if_branch,
                elif_branches,
                else_branch,
            } => {
                if let Some(result) = self.execute_cond_branch(if_branch.0, if_branch.1) {
                    return result;
                }
                for branch in elif_branches {
                    if let Some(result) = self.execute_cond_branch(branch.0, branch.1) {
                        return result;
                    }
                }
                if let Some(block) = else_branch {
                    match *block.stmt_enum {
                        StatementEnum::Block { stmts } => return self.execute_block(stmts),
                        _ => unreachable!(),
                    }
                }
            }
            StatementEnum::While { condition, block } => {
                while self.visit_expr(condition.clone()) == StackType::Literal(Bool(true)) {
                    if let StatementEnum::Block { stmts } = *block.stmt_enum.clone() {
                        if let Err(e) = self.execute_block(stmts) {
                            match e {
                                StatementErr::Break => break,
                                StatementErr::Continue => continue,
                                e @ StatementErr::Return(_) => return Err(e),
                            }
                        }
                    }
                }
            }
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
        self.pow(u32::try_from(rhs).unwrap())
    }
}

impl Pow<f64> for f64 {
    type Output = f64;
    fn pow(self, rhs: f64) -> Self::Output {
        self.powf(rhs)
    }
}

impl Neg for StackType {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let result = match self {
            Self::Literal(literal) => match literal {
                Literal::Int(int) => Literal::Int(-int),
                Literal::Float(float) => Literal::Float(-float),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        StackType::Literal(result)
    }
}

impl Not for StackType {
    type Output = Self;

    fn not(self) -> Self::Output {
        let result = match self {
            Self::Literal(Literal::Bool(boolean)) => Literal::Bool(!boolean),
            _ => unreachable!(),
        };
        StackType::Literal(result)
    }
}

#[cfg(test)]
mod tests {
    use resolver::Resolver;
    use span::Span;

    use super::*;

    #[test]
    fn test_neg_op() {
        assert_eq!(
            -StackType::Literal(Literal::Int(100)),
            StackType::Literal(Literal::Int(-100))
        );
        assert_eq!(
            -StackType::Literal(Literal::Float(44.44)),
            StackType::Literal(Literal::Float(-44.44))
        );
    }

    #[test]
    fn test_not_op() {
        assert_eq!(
            !StackType::Literal(Literal::Bool(true)),
            StackType::Literal(Literal::Bool(false))
        );
        assert_eq!(
            !StackType::Literal(Literal::Bool(false)),
            StackType::Literal(Literal::Bool(true))
        );
    }

    #[test]
    fn test_pow_op() {
        assert_eq!(3.pow(3), 27);
        assert_eq!(2.5.pow(2.0), 6.25);
    }

    /*#[test]
    fn test_visit_stmt() {
        let resolver = Resolver::new();
        let side_table = resolver.resolve(vec![]);
        let env = Enviroment::new(side_table);
        let mut evaluator = Evaluator::new(env);

        let test_cases = [
            (StatementEnum::Break, Err(StatementErr::Break)),
            (
                StatementEnum::Return {
                    expr: Expression {
                        id: 0,
                        expr_enum: Box::new(ExpressionEnum::Literal(Literal::Int(1))),
                        span: Span::new(0, 0),
                    },
                },
                Err(StatementErr::Return(StackType::Literal(Literal::Int(1)))),
            ),
            (StatementEnum::Continue, Err(StatementErr::Continue)),
            (
                StatementEnum::Expression(Expression {
                    id: 0,
                    expr_enum: Box::new(ExpressionEnum::Literal(Literal::Int(1))),
                    span: Span::new(0, 0),
                }),
                Ok(()),
            ),
        ];

        for case in test_cases {
            assert_eq!(
                evaluator.visit_stmt(Statement {
                    span: Span::new(0, 0),
                    stmt_enum: Box::new(case.0)
                }),
                case.1
            )
        }
    }*/
}
