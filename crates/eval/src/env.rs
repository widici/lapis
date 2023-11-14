use log::info;
use std::collections::{HashMap, hash_map::Entry::{Vacant, Occupied}};
use lexer::token::Literal;
use ast::ast::Statement;
use crate::callable::Function;

#[derive(Clone, Debug, PartialEq)]
pub enum StackType {
    Literal(Literal),
    Function {
         params: Vec<String>,
         block: Statement,
    },
    Void,
}

pub struct Enviroment {
    nodes: Vec<EnviromentNode>,
}

impl Enviroment {
    pub fn new() -> Self {
        Enviroment { nodes: Vec::new() }
    }

    pub(crate) fn new_node(&mut self) {
        self.nodes.push(EnviromentNode::new());
        info!("Enviroment: {:?}", self.nodes)
    }

    pub(crate) fn declare(&mut self, ident: String, value: StackType) {
        if let Some(node) = self.nodes.last_mut() {
            node.declare(ident, value)
        }
        info!("Enviroment: {:?}", self.nodes)
    }

    pub(crate) fn assign(&mut self, ident: String, value: StackType) {
        for node in self.nodes.iter_mut().rev() {
            if node.stack.contains_key(&ident) {
                node.assign(ident, value);
                return;
            }
        }
        unimplemented!()
    }

    pub(crate) fn drop(&mut self) {
        self.nodes.pop();
    }

    pub(crate) fn get(&mut self, ident: String) -> Option<StackType> {
        for node in self.nodes.iter().rev() {
            if let Some(value) = node.stack.get(&ident) {
                return Some(value.to_owned())
            }
        }
        None
    }                                                                                                                                 
}

#[derive(Debug)]
struct EnviromentNode {
    stack: HashMap<String, StackType>
}

impl EnviromentNode {
    pub(crate) fn new() -> Self {
        Self { stack: HashMap::new() }
    }

    pub(crate) fn declare(&mut self, ident: String, value: StackType) {
        match self.stack.entry(ident.clone()) {
            Occupied(..) => unimplemented!(),
            Vacant(entry ) => { entry.insert(value); },
        }
    }

    pub(crate) fn assign(&mut self, ident: String, value: StackType) {
        if self.stack[&ident] != value {
            unimplemented!()
        }
        self.stack.insert(ident, value);
        info!("Stack: {:?}", self.stack)
    } 
}
