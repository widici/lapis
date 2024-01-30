use crate::callable::{CallError, Callable, Function, Puts, PutsLn};
use crate::env::{Enviroment, EnviromentNode, StackType};
use ast::Visitor;
use ast::{Expression, ExpressionEnum, Statement, StatementEnum};
use error::ErrorKind::{
    InvalidOperands, MismatchedArity, MismatchedTypes, Unexpected, UnexpectedExpected,
};
use error::{impl_error_handling, report_errors, Error, ErrorLocation};
use lexer::ops::Pow;
use lexer::token::{
    Literal::{self, Bool},
    Op, TokenType,
};

pub struct Evaluator {
    pub(crate) env: Enviroment,
    global: EnviromentNode,
}

impl_error_handling!(Evaluator, ErrorLocation::Evaluator);

impl Evaluator {
    #[must_use]
    pub fn new(mut env: Enviroment) -> Self {
        env.new_node(); // Add global node
        let mut global = EnviromentNode::new();
        global.declare("puts", StackType::Function(Box::new(Puts {})));
        global.declare("putsln", StackType::Function(Box::new(PutsLn {})));
        Evaluator { env, global }
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

    fn execute_cond_branch(
        &mut self,
        condition: Expression,
        block: Statement,
    ) -> Option<Result<(), StatementErr>> {
        let evaluated = self.visit_expr(condition.clone());
        match evaluated {
            StackType::Literal(Bool(boolean)) => {
                if boolean {
                    return Some(self.visit_stmt(block));
                }
            }
            _ => {
                self.add_error(UnexpectedExpected {
                    expected: "bool".to_string(),
                    found: Box::new(condition),
                });
            }
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

    #[allow(clippy::too_many_lines)]
    fn visit_expr(&mut self, expr: Expression) -> Self::E {
        match *expr.expr_enum.clone() {
            ExpressionEnum::Literal(token) => {
                if let TokenType::Literal(literal) = token.tt {
                    StackType::Literal(literal)
                } else {
                    unreachable!()
                }
            }
            ExpressionEnum::BinOp {
                left,
                operator,
                right,
            } => {
                let op = match operator.tt {
                    TokenType::Op(op) => op,
                    _ => unreachable!("Expected tt op"),
                };
                let (lhs_st, rhs_st) = (
                    self.visit_expr(left.clone()),
                    self.visit_expr(right.clone()),
                );
                if let (StackType::Literal(lhs), StackType::Literal(rhs)) =
                    (lhs_st.clone(), rhs_st.clone())
                {
                    let res = match op {
                        Op::Add => match (lhs.clone(), rhs.clone()) {
                            (Literal::Str(lhs), Literal::Str(rhs)) => {
                                Some(Literal::Str(lhs + rhs.as_str()))
                            }
                            _ => lhs + rhs,
                        },
                        Op::Sub => lhs - rhs,
                        Op::Mul => lhs * rhs,
                        Op::Div => lhs / rhs,
                        Op::Pow => lhs.pow(rhs),
                        Op::Rem => lhs % rhs,
                        _ => Some(Literal::Bool(match op {
                            Op::Gt => lhs > rhs,
                            Op::Ge => lhs >= rhs,
                            Op::Lt => lhs < rhs,
                            Op::Le => lhs <= rhs,
                            Op::EqEq => lhs == rhs,
                            Op::Ne => lhs != rhs,
                            _ => {
                                self.add_error(Unexpected {
                                    found: Box::new(operator),
                                });
                                report_errors();
                                unreachable!()
                            }
                        })),
                    };
                    if res.is_none() {
                        let kind = InvalidOperands {
                            op: Box::new(operator),
                            lhs: Box::new(left.clone()),
                            rhs: Box::new(right.clone()),
                            span: (left.span.start, right.span.end).into(),
                        };
                        self.add_error(kind);
                        report_errors();
                        unreachable!()
                    }
                    StackType::Literal(res.unwrap())
                } else {
                    match lhs_st {
                        StackType::Literal(..) => {}
                        _ => self.add_error(UnexpectedExpected {
                            expected: "literal".to_owned(),
                            found: Box::new(left),
                        }),
                    }
                    match rhs_st {
                        StackType::Literal(..) => {}
                        _ => self.add_error(UnexpectedExpected {
                            expected: "literal".to_owned(),
                            found: Box::new(right),
                        }),
                    }
                    report_errors();
                    unreachable!()
                }
            }
            ExpressionEnum::UnaryOp { operator, expr } => {
                let literal = match self.visit_expr(expr) {
                    StackType::Literal(literal) => literal,
                    _ => unreachable!("Expected literal"),
                };
                StackType::Literal(
                    match operator.into() {
                        Op::Sub => -literal,
                        Op::Not => !literal,
                        _ => unreachable!(),
                    }
                    .unwrap(),
                )
            }
            ExpressionEnum::Var { ident } => self.env.get(ident.get_str_ident(), &expr).unwrap(),
            ExpressionEnum::Assignment { ident, right } => {
                let value = self.visit_expr(right);
                if self
                    .env
                    .assign(ident.get_str_ident().to_owned(), value.clone(), &expr)
                    .is_err()
                {
                    self.add_error(MismatchedTypes {
                        serialized_tok: Box::new(expr),
                    })
                }
                value
            }
            ExpressionEnum::Call { ident, params } => {
                let params: Vec<StackType> = params
                    .into_iter()
                    .map(|param| self.visit_expr(param))
                    .collect();
                let str_ident = ident.get_str_ident();
                let mut function: Box<dyn Callable> =
                    if let Some(value) = self.global.stack.get(str_ident) {
                        value.clone().into()
                    } else {
                        match self.env.get(ident.get_str_ident(), &expr) {
                            Some(decl) => decl,
                            None => unimplemented!(),
                        }
                        .into()
                    };

                match function.call(self, params) {
                    Ok(returned) => returned,
                    Err(call_err) => match call_err {
                        CallError::MismatchedArity { found, expected } => {
                            self.add_error(MismatchedArity {
                                expected_arity: expected,
                                found_arity: found,
                                called: Box::new(expr),
                            });
                            report_errors();
                            unreachable!()
                        }
                    },
                }
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
                self.env.declare(ident.get_str_ident(), evaluated_expr);
            }
            StatementEnum::Block { stmts } => return self.execute_block(stmts),
            StatementEnum::FnDeclaration {
                ident,
                params,
                stmts,
            } => {
                let env_id = self.env.nodes.len() - 1;
                let function = Function {
                    params: params
                        .iter()
                        .map(|param| param.get_str_ident().to_owned())
                        .collect(),
                    stmts,
                    env_id,
                };
                self.env.declare(
                    ident.get_str_ident(),
                    StackType::Function(Box::new(function)),
                )
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

#[cfg(test)]
mod tests {
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
                        span: Span::default(),
                    },
                },
                Err(StatementErr::Return(StackType::Literal(Literal::Int(1)))),
            ),
            (StatementEnum::Continue, Err(StatementErr::Continue)),
            (
                StatementEnum::Expression(Expression {
                    id: 0,
                    expr_enum: Box::new(ExpressionEnum::Literal(Literal::Int(1))),
                    span: Span::default(),
                }),
                Ok(()),
            ),
        ];

        for case in test_cases {
            assert_eq!(
                evaluator.visit_stmt(Statement {
                    span: Span::default(),
                    stmt_enum: Box::new(case.0)
                }),
                case.1
            )
        }
    }*/
}
