use std::collections::HashMap;
use crate::object::Object;

#[derive(Debug, Clone, PartialEq)]
pub struct Envirnoment<'a> {
    pub variables: HashMap<String, Object<'a>>,
    enclosing: Option<Box<Envirnoment<'a>>>,
}

impl<'a> Envirnoment<'a> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with_enclosing(env: Envirnoment<'a>) -> Self {
        Self { 
            variables: HashMap::new(),
            enclosing: Some(Box::new(env))
        }
    }

    pub fn define(&mut self, target: String, value: Object<'a>) {
        if self.variables.contains_key(&target) {
            let __value = self.variables.get_mut(&target).unwrap();
            *__value = value;
        } else {
            self.variables.insert(target, value);
        }
    }

    pub fn get(&self, variable: String) -> Option<Object<'a>> {
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
