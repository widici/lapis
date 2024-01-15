use std::fmt::Debug;

use crate::env::StackType;
use crate::eval::{Evaluator, StatementErr};
use ast::Statement;
use dyn_clone::{DynClone, clone_trait_object};
use dyn_partial_eq::{dyn_partial_eq, DynPartialEq};

#[dyn_partial_eq]
pub trait Callable: DynClone + Debug {
    fn arity(&self) -> usize;
    fn call(&mut self, evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType;
    fn get_env_id(&self) -> Option<usize> {
        None
    }
}

clone_trait_object!(Callable);

#[derive(Clone, PartialEq, DynPartialEq)]
pub(crate) struct Function {
    pub params: Vec<String>,
    pub stmts: Vec<Statement>,
    pub(crate) env_id: usize,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&mut self, evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType {
        if self.arity() != params.len() {
            unimplemented!()
        }

        evaluator.env.env_ptr = Some(self.env_id);
        evaluator.env.new_node(); // Begining of execution of fn stmts

        for (ident, value) in self.params.clone().into_iter().zip(params.into_iter()) {
            evaluator.env.declare(ident.as_str(), value)
        }
        let fn_return = evaluator.evaluate(self.stmts.clone());

        evaluator.env.drop(); // End of execution of fn stmts
        evaluator.env.env_ptr = None;

        match fn_return {
            Ok(()) => StackType::Undefined,
            Err(e) => match e {
                StatementErr::Return(stack_type) => stack_type,
                _ => unreachable!(),
            },
        }
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

#[derive(Clone, PartialEq, DynPartialEq)]
pub(crate) struct Puts {}

impl Callable for Puts {
    fn arity(&self) -> usize {
        1
    }

    fn call(&mut self, _evaluator: &mut Evaluator, params:Vec<StackType>) -> StackType {
        print!("{}", params[0]);
        return StackType::Undefined
    }
}

impl Debug for Puts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<puts native fn>")?;
        Ok(())
    }
}

impl From<StackType> for Box<dyn Callable> {
    fn from(value: StackType) -> Self {
        match value {
            StackType::Function(fn_decl) => fn_decl,
            _ => unreachable!()
        }
    }
}