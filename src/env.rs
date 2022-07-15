use std::collections::HashMap;
use crate::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Envirnoment {
    pub variables: HashMap<String, Object>,
    enclosing: Option<Box<Envirnoment>>,
}

impl<'a> Envirnoment {
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

    pub fn define(&mut self, target: String, value: Object) {
        if self.variables.contains_key(&target) {
            let __value = self.variables.get_mut(&target).unwrap();
            *__value = value;
        } else {
            self.variables.insert(target, value);
        }
    }

    pub fn get(&self, variable: String) -> Option<Object> {
        match self.variables.get(&variable) {
            Some(value) => Some(value.clone()),
            None => {
                if let Some(enclosing) = &self.enclosing {
                    return enclosing.get(variable);
                }
                None
            },
        }
    }
}
