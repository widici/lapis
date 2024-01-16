use super::Callable;
use crate::env::StackType;
use crate::eval::Evaluator;
use dyn_partial_eq::DynPartialEq;
use std::fmt::Debug;

#[derive(Clone, PartialEq, Eq, DynPartialEq)]
pub struct Puts {}

impl Callable for Puts {
    fn arity(&self) -> usize {
        1
    }

    fn call(&mut self, _evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType {
        print!("{}", params[0]);
        return StackType::Undefined;
    }
}

impl Debug for Puts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<puts native fn>")?;
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, DynPartialEq)]
pub struct PutsLn {}

impl Callable for PutsLn {
    fn arity(&self) -> usize {
        1
    }

    fn call(&mut self, _evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType {
        println!("{}", params[0]);
        return StackType::Undefined;
    }
}

impl Debug for PutsLn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<putsln native fn>")?;
        Ok(())
    }
}
