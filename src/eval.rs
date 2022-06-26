use crate::env::Env;
use crate::parser::{Expr, ParseError, Parser, Stmt};
use std::collections::VecDeque;

pub struct Eval<'a> {
    env: &'a mut Env,
}

impl<'a, 'src> Eval<'a> {
    pub fn new(env: &'a mut Env) -> Self {
        Self { env }
    }

    fn eval_expr(&mut self, expr: &Expr<'src>) {
        match expr {
            Expr::PushLeft { expr } => match **expr {
                Expr::Number { num } => self.env.deque.push_front(num),
                Expr::Op { op } => match op {
                    "dup" => {
                        let dup = self.env.deque.front().unwrap();
                        self.env.deque.push_front(dup.clone());
                    }
                    "drop" => {
                        let _ = self.env.deque.pop_front().unwrap();
                    }
                    "print" => {
                        let dup = self.env.deque.front().unwrap();
                        print!("{}", dup);
                    }

                    // --------------------------------------------------------------------------
                    //                          - Arithmetic -
                    // --------------------------------------------------------------------------
                    "add" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();
                        self.env.deque.push_front(b + a);
                    }
                    "sub" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();
                        self.env.deque.push_front(b - a);
                    }

                    // --------------------------------------------------------------------------
                    //                          - Comparators -
                    // --------------------------------------------------------------------------
                    "eq" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();

                        if a == b {
                            self.env.deque.push_front(1.0);
                        } else {
                            self.env.deque.push_front(0.0);
                        }
                    }
                    ">" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();

                        if a > b {
                            self.env.deque.push_front(1.0);
                        } else {
                            self.env.deque.push_front(0.0);
                        }
                    }
                    "<" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();

                        if a < b {
                            self.env.deque.push_front(1.0);
                        } else {
                            self.env.deque.push_front(0.0);
                        }
                    }

                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },

            Expr::PushRight { expr } => match **expr {
                Expr::Number { num } => self.env.deque.push_back(num),
                Expr::Op { op } => match op {
                    "dup" => {
                        let dup = self.env.deque.back().unwrap();
                        self.env.deque.push_back(dup.clone());
                    }
                    "drop" => {
                        let _ = self.env.deque.pop_back().unwrap();
                    }

                    // --------------------------------------------------------------------------
                    //                          - Arithmetic -
                    // --------------------------------------------------------------------------
                    "add" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();
                        self.env.deque.push_back(b + a);
                    }
                    "sub" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();
                        self.env.deque.push_back(b - a);
                    }

                    // --------------------------------------------------------------------------
                    //                          - Comparators -
                    // --------------------------------------------------------------------------
                    "eq" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();

                        if a == b {
                            self.env.deque.push_back(1.0);
                        } else {
                            self.env.deque.push_back(0.0);
                        }
                    }
                    ">" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();

                        if a > b {
                            self.env.deque.push_back(1.0);
                        } else {
                            self.env.deque.push_back(0.0);
                        }
                    }
                    "<" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();

                        if a < b {
                            self.env.deque.push_back(1.0);
                        } else {
                            self.env.deque.push_back(0.0);
                        }
                    }

                    _ => unreachable!(),
                },

                _ => unreachable!(),
            },

            _ => todo!(),
        }
    }

    fn eval_if_stmt(&mut self, main: &Expr<'src>, body: &Vec<Stmt<'src>>) -> bool {
        let mut flag = false;

        if matches!(main, Expr::PushLeft { .. }) {
            if self.env.deque.pop_back().unwrap() == 1.0 {
                flag = true;
                for stmt_ in body {
                    self.eval_stmt(&stmt_);
                }
            }
        } else {
            if self.env.deque.pop_back().unwrap() == 1.0 {
                flag = true;
                for stmt_ in body {
                    self.eval_stmt(&stmt_);
                }
            }
        }

        flag
    }

    fn eval_stmt(&mut self, stmt: &Stmt<'src>) {
        match stmt {
            Stmt::Expr { expr } => self.eval_expr(expr),
            Stmt::If { main, body } => {
                let _ = self.eval_if_stmt(main, body);
            }
            Stmt::IfElse { if_block, else_block } => {
                if let Stmt::If { main, body } = &**if_block {
                    if !self.eval_if_stmt(&main, &body) {
                        for stmt_ in else_block {
                            self.eval_stmt(&stmt_);
                        }
                    }
                }
            }
            Stmt::While {
                main,
                condition,
                body,
            } => {
                loop {
                    // E X P R
                    for expr in condition {
                        self.eval_expr(expr);
                    }

                    // T E S T
                    if matches!(main, Expr::PushLeft { .. }) {
                        if self.env.deque.pop_front().unwrap() != 1.0 {
                            break;
                        }
                    } else {
                        if self.env.deque.pop_back().unwrap() != 1.0 {
                            break;
                        }
                    }

                    // B O D Y
                    for stmt_ in body {
                        self.eval_stmt(&stmt_);
                    }
                }
            }

            _ => (),
        }
    }

    pub fn eval(&mut self, src: &'src str) -> anyhow::Result<()> {
        let mut ip = 0;
        let mut parser = Parser::from(&src).unwrap();
        let stmts = parser.parse().unwrap();

        while ip < stmts.len() {
            self.eval_stmt(&stmts[ip]);
            ip += 1;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_arithmetic() {
        let mut env = Env::new();
        let mut eval = Eval::new(&mut env);
        assert!(eval.eval("!1 !2 !add !print").is_ok());
        assert_eq!(env.deque, VecDeque::from([3.0]));
    }

    #[test]
    fn eval_if_stmt() {
        let mut env = Env::new();
        let mut eval = Eval::new(&mut env);
        assert!(eval.eval("!1 !if { !1 !2 !add }").is_ok());
        assert_eq!(env.deque, VecDeque::from([3.0]));
    }

    #[test]
    fn eval_if_else_stmt() {
        let mut env = Env::new();
        let mut eval = Eval::new(&mut env);
        assert!(eval.eval("!0 !if { !1 !2 !add } else { !2 !1 !sub }").is_ok());
        assert_eq!(env.deque, VecDeque::from([1.0]));
    }

    #[test]
    fn eval_while_stmt() {
        // Series from 0 to 10 (including 10)
        let mut env = Env::new();
        let mut eval = Eval::new(&mut env);
        assert!(eval
            .eval(
                "
!1
!while !dup !10 !> {
    !dup !1 !add
}
"
            )
            .is_ok());
        assert_eq!(
            env.deque,
            VecDeque::from([10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0])
        );
    }
}
