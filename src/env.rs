use std::collections::HashMap;
use crate::object::Object;

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
            None => None,
        }
    }
}
