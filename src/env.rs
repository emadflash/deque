use std::collections::{HashMap, VecDeque};

pub struct Env {
    labels: HashMap<String, usize>,
    pub deque: VecDeque<f32>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
            deque: VecDeque::new(),
        }
    }
}
