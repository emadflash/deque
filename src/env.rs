use crate::parser::{Expr, ParseError, Parser, Stmt};
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

pub fn eval<'src>(env: &mut Env, src: &'src str) -> anyhow::Result<()> {
    let mut parser = Parser::from(&src).unwrap();
    let stmts = parser.parse().unwrap();
    let mut ip = 0;

    while ip < stmts.len() {
        match &stmts[ip] {
            Stmt::Expr { expr } => {
                match expr {
                    Expr::PushLeft { expr } => {
                        match **expr {
                            Expr::Number { num } => env.deque.push_front(num),
                            Expr::Op { op } => {
                                match op {
                                    "add" => {
                                        let a = env.deque.pop_front().unwrap();
                                        let b = env.deque.pop_front().unwrap();
                                        env.deque.push_front(b + a);
                                    }
                                    "sub" => {
                                        let a = env.deque.pop_front().unwrap();
                                        let b = env.deque.pop_front().unwrap();
                                        env.deque.push_front(b - a);
                                    }
                                    "dup" => {
                                        let dup = env.deque.front().unwrap();
                                        env.deque.push_front(dup.clone());
                                    }
                                    "print" => {
                                        let dup = env.deque.front().unwrap();
                                        print!("{}", dup);
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        }
                    }

                    Expr::PushRight { expr } => {
                        match **expr {
                            Expr::Number { num } => env.deque.push_back(num),
                            Expr::Op { op } => {
                                match op {
                                    "add" => {
                                        let a = env.deque.pop_back().unwrap();
                                        let b = env.deque.pop_back().unwrap();
                                        env.deque.push_back(b + a);
                                    }
                                    "sub" => {
                                        let a = env.deque.pop_back().unwrap();
                                        let b = env.deque.pop_back().unwrap();
                                        env.deque.push_back(b - a);
                                    }
                                    "dup" => {
                                        let dup = env.deque.back().unwrap();
                                        env.deque.push_back(dup.clone());
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        }
                    }

                    _ => unreachable!()
                };
            }

            _ => (),
        }

        ip += 1;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn _eval() {
        let mut env = Env::new();
        assert!(eval(&mut env, "!1 !2 !add !print").is_ok());
        assert_eq!(env.deque, VecDeque::from([3.0]));
    }
}
