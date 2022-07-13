use std::collections::HashMap;

pub struct Env {
    locals: HashMap<String, usize>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }
}
