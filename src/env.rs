use std::collections::HashMap;
use crate::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Envirnoment {
    pub variables: HashMap<String, Object>,
    pub enclosing: Option<Box<Envirnoment>>,
}

impl Envirnoment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with_enclosing(env: Envirnoment) -> Self {
        Self { 
            variables: HashMap::new(),
            enclosing: Some(Box::new(env))
        }
    }

    pub fn define(&mut self, lhs: String, rhs: Object) {
        match self.get_mut(&lhs) {
            Some(val) => *val = rhs,
            None => {
                assert_eq!(self.variables.insert(lhs.to_string(), rhs), None);
            }
        };
    }

    pub fn get_mut(&mut self, lhs: &String) -> Option<&mut Object> {
        match self.variables.get_mut(lhs) {
            Some(value) => Some(value),
            None => {
                if let Some(enclosing) = &mut self.enclosing {
                    return enclosing.get_mut(lhs);
                }
                None
            },
        }
    }

    pub fn get(&self, lhs: &String) -> Option<&Object> {
        match self.variables.get(lhs) {
            Some(value) => Some(value),
            None => {
                if let Some(enclosing) = &self.enclosing {
                    return enclosing.get(lhs);
                }
                None
            },
        }
    }
}
