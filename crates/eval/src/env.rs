use ast::Expression;
use lexer::token::Literal;
use log::info;
use std::{
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap,
    },
    fmt::Display,
};

use crate::callable::Callable;

#[derive(Clone, Debug, PartialEq)]
pub enum StackType {
    Literal(Literal),
    Function(Box<dyn Callable>),
    Undefined,
}

pub struct Enviroment {
    pub(crate) nodes: Vec<EnviromentNode>,
    side_table: HashMap<Expression, usize>,
    /// This is used in scenarios where the current env id isn't the last node
    /// E.g. during fn calls, where the current scope needs to move to be below the fn decl
    pub(crate) env_ptr: Option<usize>,
}

impl Enviroment {
    #[must_use]
    pub const fn new(side_table: HashMap<Expression, usize>) -> Self {
        Enviroment {
            nodes: Vec::new(),
            side_table,
            env_ptr: None,
        }
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
        info!(
            "New node in Enviroment with env_id: {}",
            self.get_current_env_id()
        )
    }

    pub(crate) fn declare(&mut self, ident: &str, value: StackType) {
        let env_id = self.get_current_env_id();
        if let Some(node) = self.nodes.get_mut(env_id) {
            node.declare(ident, value)
        }
        //info!("Declared in Enviroment: {:?}", self.nodes)
    }

    pub(crate) fn assign(
        &mut self,
        ident: String,
        value: StackType,
        expr: &Expression,
    ) -> Result<(), ()> {
        let env_id = self.get_env_id(expr);
        let node = match self.nodes.get_mut(env_id) {
            Some(node) => node,
            None => unimplemented!(),
        };
        if node.stack.contains_key(&ident) {
            node.assign(ident, value)?;
            return Ok(());
        }
        unimplemented!()
    }

    pub(crate) fn drop(&mut self) {
        match self.env_ptr {
            Some(env_id) => {
                self.nodes.remove(env_id);
                self.env_ptr = Some(self.env_ptr.unwrap() - 1);
            }
            None => {
                self.nodes.pop();
            }
        };
    }

    pub(crate) fn get(&mut self, ident: &String, expr: &Expression) -> Option<StackType> {
        let env_id = self.get_env_id(expr);
        let node = match self.nodes.get_mut(env_id) {
            Some(node) => node,
            None => unimplemented!("Failed getting: {:?} {}", ident, env_id),
        };
        node.stack.get(ident).cloned()
    }

    fn get_env_id(&self, expr: &Expression) -> usize {
        match self.side_table.get(expr) {
            Some(distance) => {
                //log!("Expected from {:?} @ {:?} @ pos {:?}", expr, distance, self.nodes.len() - 1);
                let current_pos = self.env_ptr.unwrap_or(self.nodes.len() - 1);
                current_pos - distance
            }
            None => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct EnviromentNode {
    pub(crate) stack: HashMap<String, StackType>,
}

impl EnviromentNode {
    pub(crate) fn new() -> Self {
        Self {
            stack: HashMap::new(),
        }
    }

    pub(crate) fn declare(&mut self, ident: &str, value: StackType) {
        match self.stack.entry(ident.to_owned()) {
            Occupied(..) => unimplemented!(),
            Vacant(entry) => {
                info!("Declared {} -> {:?}", ident, value);
                entry.insert(value);
            }
        }
    }

    pub(crate) fn assign(&mut self, ident: String, value: StackType) -> Result<(), ()> {
        if !self.stack[&ident].cmp_type(&value) {
            return Err(());
        }
        info!("Assigned {} -> {:?}", ident, value);
        self.stack.insert(ident, value);
        Ok(())
    }
}

impl StackType {
    fn cmp_type(&self, other: &Self) -> bool {
        match (self, other) {
            (StackType::Literal(self_lit), StackType::Literal(other_lit)) => {
                self_lit.cmp_type(other_lit)
            }
            (StackType::Undefined, StackType::Literal(..) | StackType::Undefined) => true,
            _ => false,
        }
    }
}

impl Display for StackType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackType::Function(function) => write!(f, "{:?}", function)?,
            StackType::Undefined => write!(f, "undefined")?,
            StackType::Literal(literal) => write!(f, "{}", literal)?,
        }
        Ok(())
    }
}
