use crate::env::{Env, Object};
use crate::parser::{Expr, Parser, Stmt};

// --------------------------------------------------------------------------
//                          - Eval -
// --------------------------------------------------------------------------
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
                ////////////////////////////////
                // ~ Literals
                Expr::Number { num } => self.env.deque.push_front(Object::Number { num }),
                Expr::String { text } => self.env.deque.push_front(Object::String { text: text.to_string() }),
                Expr::Boolean(value) => self.env.deque.push_front(Object::Boolean { value }),

                ////////////////////////////////
                // ~ Ops/Builtins
                Expr::Op { op } => match op {
                    "dup" => {
                        let dup = self.env.deque.front().unwrap();
                        self.env.deque.push_front(dup.clone());
                    }
                    "pud" => {
                        let dup = self.env.deque.front().unwrap();
                        self.env.deque.push_back(dup.clone());
                    }
                    "drop" => {
                        let _ = self.env.deque.pop_front().unwrap();
                    }
                    "print" => {
                        let dup = self.env.deque.pop_front().unwrap();
                        print!("{}", dup);
                    }
                    "println" => {
                        let dup = self.env.deque.pop_front().unwrap();
                        println!("{}", dup);
                    }

                    // --------------------------------------------------------------------------
                    //                          - Arithmetic -
                    // --------------------------------------------------------------------------
                    "+" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();
                        self.env.deque.push_front(Object::Number { num: b.unwrap_num() + a.unwrap_num() });
                    }
                    "-" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();
                        self.env.deque.push_front(Object::Number { num: b.unwrap_num() - a.unwrap_num() });
                    }
                    "%" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();
                        self.env.deque.push_front(Object::Number { num: b.unwrap_num() % a.unwrap_num() });
                    }

                    // --------------------------------------------------------------------------
                    //                          - Comparators -
                    // --------------------------------------------------------------------------
                    "eq" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();

                        if a == b {
                            self.env.deque.push_front(Object::Boolean { value: true });
                        } else {
                            self.env.deque.push_front(Object::Boolean { value: false });
                        }
                    }
                    ">" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();

                        if a > b {
                            self.env.deque.push_front(Object::Boolean { value: true });
                        } else {
                            self.env.deque.push_front(Object::Boolean { value: false });
                        }
                    }
                    "<" => {
                        let a = self.env.deque.pop_front().unwrap();
                        let b = self.env.deque.pop_front().unwrap();

                        if a < b {
                            self.env.deque.push_front(Object::Boolean { value: true });
                        } else {
                            self.env.deque.push_front(Object::Boolean { value: false });
                        }
                    }

                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },

            Expr::PushRight { expr } => match **expr {
                ////////////////////////////////
                // ~ Literals
                Expr::Number { num } => self.env.deque.push_back(Object::Number { num }),
                Expr::String { text } => self.env.deque.push_back(Object::String { text: text.to_string() }),
                Expr::Boolean(value) => self.env.deque.push_back(Object::Boolean { value }),

                ////////////////////////////////
                // ~ Ops/Builtins
                Expr::Op { op } => match op {
                    "dup" => {
                        let dup = self.env.deque.back().unwrap();
                        self.env.deque.push_back(dup.clone());
                    }
                    "pud" => {
                        let dup = self.env.deque.back().unwrap();
                        self.env.deque.push_front(dup.clone());
                    }
                    "drop" => {
                        let _ = self.env.deque.pop_back().unwrap();
                    }
                    "print" => {
                        let dup = self.env.deque.pop_back().unwrap();
                        print!("{}", dup);
                    }
                    "println" => {
                        let dup = self.env.deque.pop_back().unwrap();
                        println!("{}", dup);
                    }

                    // --------------------------------------------------------------------------
                    //                          - Arithmetic -
                    // --------------------------------------------------------------------------
                    "+" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();
                        self.env.deque.push_back(Object::Number { num: b.unwrap_num() + a.unwrap_num() });
                    }
                    "-" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();
                        self.env.deque.push_back(Object::Number { num: b.unwrap_num() - a.unwrap_num() });
                    }
                    "%" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();
                        self.env.deque.push_back(Object::Number { num: b.unwrap_num() % a.unwrap_num() });
                    }

                    // --------------------------------------------------------------------------
                    //                          - Comparators -
                    // --------------------------------------------------------------------------
                    "eq" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();

                        if a == b {
                            self.env.deque.push_front(Object::Boolean { value: true });
                        } else {
                            self.env.deque.push_front(Object::Boolean { value: false });
                        }
                    }
                    ">" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();

                        if a > b {
                            self.env.deque.push_front(Object::Boolean { value: true });
                        } else {
                            self.env.deque.push_front(Object::Boolean { value: false });
                        }
                    }
                    "<" => {
                        let a = self.env.deque.pop_back().unwrap();
                        let b = self.env.deque.pop_back().unwrap();

                        if a < b {
                            self.env.deque.push_front(Object::Boolean { value: true });
                        } else {
                            self.env.deque.push_front(Object::Boolean { value: false });
                        }
                    }

                    _ => unreachable!(),
                },

                _ => unreachable!(),
            },

            _ => todo!(),
        }
    }

    fn eval_if_stmt(&mut self, stmt: &Stmt<'src>) -> bool {
        let (main, conditions, body) = stmt.unwrap_if();
        let mut flag = false;

        if matches!(main, Expr::PushLeft { .. }) {
            conditions.iter().for_each(|condition| self.eval_expr(condition));

            if self.env.deque.pop_front().unwrap().unwrap_bool() == &true { // test
                flag = true;
            }
        } else {
            conditions.iter().for_each(|condition| self.eval_expr(condition));

            if self.env.deque.pop_back().unwrap().unwrap_bool() == &true { // test
                flag = true;
            }
        }

        if flag {
            body.unwrap_body().iter().for_each(|stmt| self.eval_stmt(&stmt));
        }

        flag
    }

    fn eval_while_stmt(&mut self, main: &Expr<'src>, conditions: &Vec<Expr<'src>>, body: &Box<Stmt<'src>>) {
        loop {
            // run condition
            conditions.iter().for_each(|expr| self.eval_expr(expr));

            // test condition
            if matches!(main, Expr::PushLeft { .. }) {
                if self.env.deque.pop_front().unwrap().unwrap_bool() == &false { // test
                    break;
                }
            } else {
                if self.env.deque.pop_back().unwrap().unwrap_bool() == &false { // test
                    break;
                }
            }

            // body
            body.unwrap_body().iter().for_each(|stmt| self.eval_stmt(&stmt));
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt<'src>) {
        match stmt {
            Stmt::Expr { expr } => self.eval_expr(expr),
            Stmt::If { .. } => {
                let _ = self.eval_if_stmt(stmt);
            }
            Stmt::IfElse { master, alternates } => {
                if !self.eval_if_stmt(master) {
                    for alternate in alternates {
                        match alternate {
                            // elif arms
                            Stmt::If { .. } => {
                                if self.eval_if_stmt(alternate) {
                                    break;
                                }
                            }
                            // else arm
                            Stmt::Body { .. } => alternate.unwrap_body().iter().for_each(|stmt| self.eval_stmt(&stmt)),
                            _ => unreachable!()
                        }
                    }
                }
            }
            Stmt::While { main, conditions, body } => self.eval_while_stmt(main, conditions, body),
            _ => (),
        }
    }

    pub fn eval(&mut self, src: &'src str) -> anyhow::Result<()> {
        let mut ip = 0;
        let mut parser = Parser::new(src).unwrap();
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
    use std::collections::VecDeque;

    #[test]
    fn evaluate_arithmetic_exprs() {
        let mut env = Env::new();
        let mut eval = Eval::new(&mut env);
        assert!(eval.eval("!1 !2 !+ !dup !print").is_ok());
        assert_eq!(env.deque, VecDeque::from([Object::Number { num: 3.0 }]));
    }

    #[test]
    fn evaluate_if_statements() {
        {
            let mut env = Env::new();
            let mut eval = Eval::new(&mut env);
            assert!(eval.eval("!if !true { !1 !2 !+ }").is_ok());
            assert_eq!(env.deque, VecDeque::from([Object::Number { num: 3.0 }]));
        }

        {
            let mut env = Env::new();
            let mut eval = Eval::new(&mut env);
            assert!(eval.eval("!0 !if !1 !eq { !1 !2 !+ } else { !2 !1 !- }").is_ok());
            assert_eq!(env.deque, VecDeque::from([Object::Number { num: 1.0 }]));
        }

        {
            let mut env = Env::new();
            let mut eval = Eval::new(&mut env);
            assert!(eval.eval("
                !0
                !if !dup !1 !eq {
                    !1 !2 !+
                } elif !dup !0 !eq {
                    !99 !+
                } else {
                    !2 !1 !-
                }
            ").is_ok());
            assert_eq!(env.deque, VecDeque::from([Object::Number { num: 99.0 }]));
        }
    }

    #[test]
    fn evaluate_while_loop() {
        // Series from 0 to 10 (including 10)
        let mut env = Env::new();
        let mut eval = Eval::new(&mut env);
        assert!(eval.eval(
                "
                !1
                !while !dup !10 !> {
                    !dup !1 !+
                }
                "
            )
            .is_ok());
        assert_eq!(
            env.deque,
            VecDeque::from([Object::Number { num: 10.0 },Object::Number { num:  9.0 },Object::Number { num:  8.0 },Object::Number { num:  7.0 },Object::Number { num:  6.0 },Object::Number { num:  5.0 },Object::Number { num:  4.0 },Object::Number { num:  3.0 },Object::Number { num:  2.0 },Object::Number { num:  1.0 }])
        );
    }
}
