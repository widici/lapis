use crate::env::StackType;
use crate::eval::{Evaluator, StatementErr};
use ast::Statement;

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&mut self, evaluator: &mut Evaluator, params: Vec<StackType>) -> StackType;
}

pub(crate) struct Function {
    pub params: Vec<String>,
    pub stmts: Vec<Statement>,
    env_id: usize,
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
            evaluator.env.declare(ident, value)
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
}

impl From<StackType> for Function {
    fn from(value: StackType) -> Self {
        match value {
            StackType::Function {
                params,
                stmts,
                env_id,
            } => Self {
                params,
                stmts,
                env_id,
            },
            _ => unreachable!(),
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
