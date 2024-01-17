use super::{CallError, Callable};
use crate::env::StackType;
use crate::eval::{Evaluator, StatementErr};
use ast::Statement;
use dyn_partial_eq::DynPartialEq;
use std::fmt::Debug;

#[derive(Clone, PartialEq, DynPartialEq)]
pub struct Function {
    pub params: Vec<String>,
    pub stmts: Vec<Statement>,
    pub(crate) env_id: usize,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &mut self,
        evaluator: &mut Evaluator,
        params: Vec<StackType>,
    ) -> Result<StackType, CallError> {
        self.check_arity(params.len())?;

        evaluator.env.env_ptr = Some(self.env_id);
        evaluator.env.new_node(); // Begining of execution of fn stmts

        for (ident, value) in self.params.clone().into_iter().zip(params.into_iter()) {
            evaluator.env.declare(ident.as_str(), value)
        }
        let fn_return = evaluator.evaluate(self.stmts.clone());

        evaluator.env.drop(); // End of execution of fn stmts
        evaluator.env.env_ptr = None;

        Ok(match fn_return {
            Ok(()) => StackType::Undefined,
            Err(e) => match e {
                StatementErr::Return(stack_type) => stack_type,
                _ => unreachable!(),
            },
        })
    }

    fn get_env_id(&self) -> Option<usize> {
        Some(self.env_id)
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<lapis fn>")?;
        Ok(())
    }
}
