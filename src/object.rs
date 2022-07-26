use std::{fmt, fmt::Display};
use crate::ast::Stmt;

////////////////////////////////
// ~ Helper macros
pub mod object {
    macro_rules! number {
        ($a:expr) => {
            Object::Number { num: $a }
        }
    }
    macro_rules! string {
        ($a:expr) => {
            Object::String { text: $a }
        }
    }
    macro_rules! bool {
        ($a:expr) => {
            Object::Bool { value: $a }
        }
    }
    macro_rules! proc {
        ($a:expr) => {
            Object::Bool { value: $a }
        }
    }

    pub(crate) use number;
    pub(crate) use string;
    pub(crate) use bool;
    pub(crate) use proc;
}

// --------------------------------------------------------------------------
//                          - FunctionKind -
// --------------------------------------------------------------------------
enum FunctionKind {
    Normal,
    Lambda
}

// --------------------------------------------------------------------------
//                          - Object -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Number { num: f32 },
    String { text: String },
    Bool { value: bool },
    Fn { name: String, args: Vec<String>, body: Box<Stmt> },
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Number { num } => write!(f, "{}", num),
            Object::String { text } => write!(f, "{}", text),
            Object::Bool { value } => write!(f, "{}", value),
            Object::Fn { name, .. } => write!(f, "#<{}>", name),
        }
    }
}

impl Object {
    pub fn get_num(&self) -> &f32 {
        match self {
            Object::Number { num } => &num,
            _ => unreachable!(),
        }
    }

    pub fn get_string(&self) -> &String {
        match self {
            Object::String { text } => &text,
            _ => unreachable!(),
        }
    }

    pub fn get_bool(&self) -> &bool {
        match self {
            Object::Bool { value } => &value,
            _ => unreachable!(),
        }
    }

    pub fn get_num_mut(&mut self) -> &mut f32 {
        match self {
            Object::Number { ref mut num } => num,
            _ => unreachable!(),
        }
    }

    pub fn get_string_mut(&mut self) -> &mut String {
        match self {
            Object::String { ref mut text } => text,
            _ => unreachable!(),
        }
    }

    pub fn get_bool_mut(&mut self) -> &mut bool {
        match self {
            Object::Bool { ref mut value } => value,
            _ => unreachable!(),
        }
    }
}
