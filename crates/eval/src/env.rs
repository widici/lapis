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
         stmts: Vec<Statement>,
         env_id: usize,
    },
    Undefined,
}

pub struct Enviroment {
    pub(crate) nodes: Vec<EnviromentNode>,
    resolver: Resolver,
    /// This is used in scenarios where the current env id isn't the last node
    /// E.g. during fn calls, where the current scope needs to move to be below the fn decl
    pub(crate) env_ptr: Option<usize>
}

impl Enviroment {
    #[must_use]
    pub const fn new(resolver: Resolver) -> Self {
        Enviroment { nodes: Vec::new(), resolver, env_ptr: None }
    }

    fn get_current_env_id(&self) -> usize {
        match self.env_ptr {
            None => self.nodes.len() - 1,
            Some(env_id) => env_id,
        }
    }

    pub(crate) fn new_node(&mut self) {
        let node = EnviromentNode::new();
        if self.env_ptr.is_none() {
            self.nodes.push(node);
        } else {
            let env_id = self.env_ptr.unwrap() + 1;
            self.nodes.insert(env_id, node);
            self.env_ptr = Some(env_id)
        }
        info!("New node in Enviroment: {:?}", self.nodes)
    }

    pub(crate) fn declare(&mut self, ident: String, value: StackType) {
        let env_id = self.get_current_env_id();
        if let Some(node) = self.nodes.get_mut(env_id) {
            node.declare(ident, value)
        }
        info!("Declared in Enviroment: {:?}", self.nodes)
    }

    pub(crate) fn assign(&mut self, ident: String, value: StackType, expr: &Expression) {
        let env_id = self.get_env_id(expr);
        let node = match self.nodes.get_mut(env_id) {
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
        match self.env_ptr {
            Some(env_id) => { let _ = self.nodes.remove(env_id); },
            None => { let _ = self.nodes.pop(); }
        };
    }

    pub(crate) fn get(&mut self, ident: &String, expr: &Expression) -> Option<StackType> {
        let env_id = self.get_env_id(expr);
        let node = match self.nodes.get_mut(env_id) {
            Some(node) => node,
            None => unimplemented!(),
        };
        node.stack.get(ident).cloned()
    }

    fn get_env_id(&self, expr: &Expression) -> usize {
        match self.resolver.side_table.get(expr) {
            Some(distance) => {
                //log!("Expected from {:?} @ {:?} @ pos {:?}", expr, distance, self.nodes.len() - 1);
                let current_pos = self.env_ptr.unwrap_or(self.nodes.len() - 1);
                
                current_pos - distance
            },
            None => unimplemented!()
        }
    }                                                                                                                              
}

#[derive(Debug)]
pub(crate) struct EnviromentNode {
    stack: HashMap<String, StackType>
}

impl EnviromentNode {
    pub(crate) fn new() -> Self {
        Self { stack: HashMap::new() }
    }

    pub(crate) fn declare(&mut self, ident: String, value: StackType) {
        match self.stack.entry(ident) {
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
