pub(crate) mod function;
pub(crate) mod puts;

pub(crate) use function::Function;
pub(crate) use puts::{Puts, PutsLn};

use crate::env::StackType;
use crate::eval::Evaluator;
use dyn_clone::{clone_trait_object, DynClone};
use dyn_partial_eq::dyn_partial_eq;
use std::fmt::Debug;

#[dyn_partial_eq]
pub trait Callable: DynClone + Debug {
    fn arity(&self) -> usize;
    fn call(
        &mut self,
        evaluator: &mut Evaluator,
        params: Vec<StackType>,
    ) -> Result<StackType, CallError>;
    fn get_env_id(&self) -> Option<usize> {
        None
    }
    fn check_arity(&self, arity: usize) -> Result<(), CallError> {
        if self.arity() != arity {
            return Err(CallError::MismatchedArity {
                found: arity,
                expected: self.arity(),
            });
        }
        Ok(())
    }
}

clone_trait_object!(Callable);

impl From<StackType> for Box<dyn Callable> {
    fn from(value: StackType) -> Self {
        match value {
            StackType::Function(fn_decl) => fn_decl,
            _ => unreachable!(),
        }
    }
}

pub enum CallError {
    MismatchedArity { found: usize, expected: usize },
}
