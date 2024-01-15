pub(crate) mod function;
pub(crate) mod puts;

pub(crate) use function::Function;
pub(crate) use puts::{Puts, PutsLn};

use std::fmt::Debug;
use crate::env::StackType;
use crate::eval::Evaluator;
use dyn_clone::{DynClone, clone_trait_object};
use dyn_partial_eq::dyn_partial_eq;

#[dyn_partial_eq]
pub trait Callable: DynClone + Debug {
    fn arity(&self) -> usize;
    fn call(&mut self, evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType;
    fn get_env_id(&self) -> Option<usize> {
        None
    }
}

clone_trait_object!(Callable);


impl From<StackType> for Box<dyn Callable> {
    fn from(value: StackType) -> Self {
        match value {
            StackType::Function(fn_decl) => fn_decl,
            _ => unreachable!()
        }
    }
}