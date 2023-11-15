use ast::{Statement, StatementEnum};
use lexer::token::Literal;
use crate::env::StackType;
use crate::eval::{Evaluator, StatementErr};

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&mut self, evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType;
}

pub(crate) struct Function {
    pub params: Vec<String>,
    pub block: Statement,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(&mut self, evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType {
        if self.arity() != params.len() {
            unimplemented!()
        }
        
        for (ident, value) in self.params.clone().into_iter().zip(params.into_iter()) {
            evaluator.env.declare(ident, value)
        }
        
        let fn_return = if let StatementEnum::Block { stmts } = *self.block.clone().stmt {
            evaluator.execute_block(stmts)
        } else { unreachable!() };

        return match fn_return {
            Ok(()) => StackType::Void,
            Err(e) => match e {
                StatementErr::Return(stack_type) => stack_type,
                _ => unreachable!()
            }
        }
    }
}

impl From<StackType> for Function {
    fn from(value: StackType) -> Self {
        match value {
            StackType::Function { params, block } => {
                Self { params, block }
            },
            _ => unreachable!()
        }
    }
}

/*
fn f(x) {
    return x += 1
}

fn g(x) {
    <- x += 1
}

*/