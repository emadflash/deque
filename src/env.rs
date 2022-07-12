use std::{fmt, fmt::Display};
use std::collections::{HashMap, VecDeque};

// --------------------------------------------------------------------------
//                          - Object -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Object {
    Number { num: f32 },
    String { text: String },
    Boolean { value: bool },
}

impl Object {
    pub fn unwrap_num(&self) -> &f32 {
        match self {
            Object::Number { num } => &num,
            _ => unreachable!(),
        }
    }

    pub fn unwrap_string(&self) -> &String {
        match self {
            Object::String { text } => &text,
            _ => unreachable!(),
        }
    }

    pub fn unwrap_bool(&self) -> &bool {
        match self {
            Object::Boolean { value } => &value,
            _ => unreachable!(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Number { num } => write!(f, "{}", num),
            Object::String { text } => write!(f, "{}", text),
            Object::Boolean { value } => write!(f, "{}", value),
        }
    }
}

// --------------------------------------------------------------------------
//                          - Env -
// --------------------------------------------------------------------------
pub struct Env {
    labels: HashMap<String, usize>,
    pub deque: VecDeque<Object>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            labels: HashMap::new(),
            deque: VecDeque::new(),
        }
    }
}
