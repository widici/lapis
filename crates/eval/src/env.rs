use log::info;
use std::collections::{HashMap, hash_map::Entry::{Vacant, Occupied}};
use lexer::token::Literal;
use ast::{ast::Statement, Expression};
use resolver::Resolver;

#[derive(Clone, Debug, PartialEq)]
pub enum StackType {
    Literal(Literal),
    Function {
         params: Vec<String>,
         block: Statement,
    },
    Undefined,
}

pub struct Enviroment {
    nodes: Vec<EnviromentNode>,
    resolver: Resolver,
}

impl Enviroment {
    pub fn new(resolver: Resolver) -> Self {
        Enviroment { nodes: Vec::new(), resolver }
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

    pub(crate) fn assign(&mut self, ident: String, value: StackType, expr: &Expression) {
        let env_idx = self.get_env_idx(expr);
        let node = match self.nodes.get_mut(env_idx) {
            Some(node) => node,
            None => unimplemented!(),
        };
        if node.stack.contains_key(&ident) {
            node.assign(ident, value);
            return;
        }
        unimplemented!()
    }

    pub(crate) fn drop(&mut self) {
        self.nodes.pop();
    }

    pub(crate) fn get(&mut self, ident: String, expr: &Expression) -> Option<StackType> {
        let env_idx = self.get_env_idx(expr);
        let node = match self.nodes.get_mut(env_idx) {
            Some(node) => node,
            None => unimplemented!(),
        };
        node.stack.get(&ident).cloned()
    }

    fn get_env_idx(&self, expr: &Expression) -> usize {
        match self.resolver.side_table.get(&expr) {
            Some(distance) => {
                let idx = (self.nodes.len() - 1) - distance;
                println!("{:?} {:?}", idx, self.nodes);
                return idx
            },
            None => unimplemented!()
        }
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
