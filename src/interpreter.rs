use std::collections::VecDeque;

use crate::env::Env;
use crate::error::Error;
use crate::object::{object, Object};
use crate::parser::Parser;
use crate::ast::{Expr, Stmt};

// --------------------------------------------------------------------------
//                          - RuntimeError -
// --------------------------------------------------------------------------
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum RuntimeError {
    #[error("missing argument on deque")]
    MissingArgument,
}

// --------------------------------------------------------------------------
//                          - Interpreter -
// --------------------------------------------------------------------------
pub struct Interpreter<'a> {
    env: &'a mut Env,
    deque: VecDeque<Object>,
}

impl<'a, 'src> Interpreter<'a> {
    pub fn new(env: &'a mut Env) -> Self {
        Self { 
            env,
            deque: VecDeque::new(),
        }
    }

    ////////////////////////////////
    // ~ Deque Manupilation (Abstraction)
    #[inline] fn deque_front(&self) -> Result<&Object, RuntimeError> { self.deque.front().ok_or(RuntimeError::MissingArgument) }
    #[inline] fn deque_back(&self) -> Result<&Object, RuntimeError> { self.deque.back().ok_or(RuntimeError::MissingArgument) }
    #[inline] fn deque_front_mut(&mut self) -> Result<&mut Object, RuntimeError> { self.deque.front_mut().ok_or(RuntimeError::MissingArgument) }
    #[inline] fn deque_back_mut(&mut self) -> Result<&mut Object, RuntimeError> { self.deque.back_mut().ok_or(RuntimeError::MissingArgument) }
    #[inline] fn deque_pop_front(&mut self) -> Result<Object, RuntimeError> { self.deque.pop_front().ok_or(RuntimeError::MissingArgument) }
    #[inline] fn deque_pop_back(&mut self) -> Result<Object, RuntimeError> { self.deque.pop_back().ok_or(RuntimeError::MissingArgument) }
    
    fn eval_expr(&mut self, expr: &Expr<'src>) -> Result<(), RuntimeError> {
        match expr {
            Expr::PushLeft { expr } => match **expr {
                ////////////////////////////////
                // ~ Literals
                Expr::Number { num } => self.deque.push_front(object::number!(num)),
                Expr::String { text } => self.deque.push_front(object::string!(text.to_string())),
                Expr::Boolean(value) => self.deque.push_front(object::boolean!(value)),

                ////////////////////////////////
                // ~ Ops/Builtins
                Expr::Op { op } => match op {
                    "dup" => {
                        self.deque.push_front(self.deque_front()?.clone());
                    }
                    "drop" => {
                        let _ = self.deque_pop_front()?;
                    }
                    "inc" => {
                        let a = self.deque_front_mut()?.get_num_mut();
                        *a += 1.0;
                    }
                    "dec" => {
                        let a = self.deque_front_mut()?.get_num_mut();
                        *a -= 1.0;
                    }
                    "print" => {
                        let dup = self.deque_pop_front()?;
                        print!("{}", dup);
                    }
                    "println" => {
                        let dup = self.deque_pop_front()?;
                        println!("{}", dup);
                    }

                    // --------------------------------------------------------------------------
                    //                          - Arithmetic -
                    // --------------------------------------------------------------------------
                    "+" => {
                        let a = self.deque_pop_front()?;
                        let b = self.deque_pop_front()?;
                        self.deque.push_front(object::number!(b.get_num() + a.get_num()));
                    }
                    "-" => {
                        let a = self.deque_pop_front()?;
                        let b = self.deque_pop_front()?;
                        self.deque.push_front(object::number!(b.get_num() - a.get_num()));
                    }
                    "%" => {
                        let a = self.deque_pop_front()?;
                        let b = self.deque_pop_front()?;
                        self.deque.push_front(object::number!(b.get_num() % a.get_num()));
                    }

                    // --------------------------------------------------------------------------
                    //                          - Comparators -
                    // --------------------------------------------------------------------------
                    "eq" => {
                        let a = self.deque_pop_front()?;
                        let b = self.deque_pop_front()?;

                        if a == b {
                            self.deque.push_front(object::boolean!(true));
                        } else {
                            self.deque.push_front(object::boolean!(false));
                        }
                    }
                    ">" => {
                        let a = self.deque_pop_front()?;
                        let b = self.deque_pop_front()?;

                        if a > b {
                            self.deque.push_front(object::boolean!(true));
                        } else {
                            self.deque.push_front(object::boolean!(false));
                        }
                    }
                    "<" => {
                        let a = self.deque_pop_front()?;
                        let b = self.deque_pop_front()?;

                        if a < b {
                            self.deque.push_front(object::boolean!(true));
                        } else {
                            self.deque.push_front(object::boolean!(false));
                        }
                    }

                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },

            Expr::PushRight { expr } => match **expr {
                ////////////////////////////////
                // ~ Literals
                Expr::Number { num } => self.deque.push_back(object::number!(num)),
                Expr::String { text } => self.deque.push_back(object::string!(text.to_string())),
                Expr::Boolean(value) => self.deque.push_back(object::boolean!(value)),

                ////////////////////////////////
                // ~ Ops/Builtins
                Expr::Op { op } => match op {
                    "dup" => {
                        self.deque.push_back(self.deque_back()?.clone());
                    }
                    "drop" => {
                        let _ = self.deque_pop_back()?;
                    }
                    "inc" => {
                        let a = self.deque_back_mut()?.get_num_mut();
                        *a += 1.0;
                    }
                    "dec" => {
                        let a = self.deque_back_mut()?.get_num_mut();
                        *a -= 1.0;
                    }
                    "print" => {
                        let dup = self.deque_pop_back()?;
                        print!("{}", dup);
                    }
                    "println" => {
                        let dup = self.deque_pop_back()?;
                        println!("{}", dup);
                    }

                    // --------------------------------------------------------------------------
                    //                          - Arithmetic -
                    // --------------------------------------------------------------------------
                    "+" => {
                        let a = self.deque_pop_back()?;
                        let b = self.deque_pop_back()?;
                        self.deque.push_back(object::number!(b.get_num() + a.get_num()));
                    }
                    "-" => {
                        let a = self.deque_pop_back()?;
                        let b = self.deque_pop_back()?;
                        self.deque.push_back(object::number!(b.get_num() - a.get_num()));
                    }
                    "%" => {
                        let a = self.deque_pop_back()?;
                        let b = self.deque_pop_back()?;
                        self.deque.push_back(object::number!(b.get_num() % a.get_num()));
                    }

                    // --------------------------------------------------------------------------
                    //                          - Comparators -
                    // --------------------------------------------------------------------------
                    "eq" => {
                        let a = self.deque_pop_back()?;
                        let b = self.deque_pop_back()?;

                        if a == b {
                            self.deque.push_front(object::boolean!(true));
                        } else {
                            self.deque.push_front(object::boolean!(false));
                        }
                    }
                    ">" => {
                        let a = self.deque_pop_back()?;
                        let b = self.deque_pop_back()?;

                        if a > b {
                            self.deque.push_front(object::boolean!(true));
                        } else {
                            self.deque.push_front(object::boolean!(false));
                        }
                    }
                    "<" => {
                        let a = self.deque_pop_back()?;
                        let b = self.deque_pop_back()?;

                        if a < b {
                            self.deque.push_front(object::boolean!(true));
                        } else {
                            self.deque.push_front(object::boolean!(false));
                        }
                    }

                    _ => unreachable!(),
                },

                _ => unreachable!(),
            },

            _ => todo!(),
        };

        Ok(())
    }

    #[inline]
    fn visit_body(&mut self, body: &Stmt<'src>) -> Result<(), RuntimeError> {
        for stmt in body.unwrap_body() {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &Stmt<'src>) -> Result<bool, RuntimeError> {
        let (main, conditions, body) = stmt.unwrap_if();
        let mut flag = false;

        if matches!(main, Expr::PushLeft { .. }) {
            for condition in conditions {
                self.eval_expr(condition)?;
            }

            if self.deque_pop_front()?.get_bool() == &true { // test
                flag = true;
            }
        } else {
            for condition in conditions {
                self.eval_expr(condition)?;
            }

            if self.deque_pop_back()?.get_bool() == &true { // test
                flag = true;
            }
        }

        if flag {
            self.visit_body(body)?;
        }

        Ok(flag)
    }

    fn visit_ifelse_stmt(&mut self, master: &Stmt<'src>, alternates: &Vec<Stmt<'src>>) -> Result<(), RuntimeError> {
        if !self.visit_if_stmt(master)? {
            for alternate in alternates {
                match alternate {
                    // elif arms
                    Stmt::If { .. } => {
                        if self.visit_if_stmt(alternate)? {
                            break;
                        }
                    }
                    // else arm
                    Stmt::Body { .. } => self.visit_body(alternate)?,
                    _ => unreachable!()
                };
            }
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, main: &Expr<'src>, conditions: &Vec<Expr<'src>>, body: &Box<Stmt<'src>>) -> Result<(), RuntimeError> {
        loop {
            // run condition
            for condition in conditions {
                self.eval_expr(condition)?;
            }

            // test condition
            if matches!(main, Expr::PushLeft { .. }) {
                if self.deque_pop_front()?.get_bool() == &false { // test
                    break;
                }
            } else {
                if self.deque_pop_back()?.get_bool() == &false { // test
                    break;
                }
            }

            self.visit_body(body)?;
        }

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Stmt<'src>) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr { expr } => self.eval_expr(expr)?,
            Stmt::If { .. } => {
                let _ = self.visit_if_stmt(stmt)?;
            }
            Stmt::IfElse { master, alternates } => self.visit_ifelse_stmt(master, alternates)?,
            Stmt::While { main, conditions, body } => self.visit_while_stmt(main, conditions, body)?,
            _ => (),
        }

        Ok(())
    }

    pub fn run(&mut self, src: &'src str) -> anyhow::Result<()> {
        let mut parser = Parser::new(src).unwrap();
        parser
            .parse().unwrap()
            .unwrap_program()
            .iter().map(|stmt| self.visit_stmt(stmt)).collect::<Result<Vec<_>, _>>()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interpret_arithmetic_exprs() {
        let mut env = Env::new();
        let mut interpreter = Interpreter::new(&mut env);
        assert!(interpreter.run("!1 !2 !+ !dup !print").is_ok());
        assert_eq!(interpreter.deque, VecDeque::from([object::number!(3.0)]));
    }

    #[test]
    fn interpret_if_statements() {
        {
            let mut env = Env::new();
            let mut interpreter = Interpreter::new(&mut env);
            assert!(interpreter.run("!if !true { !1 !2 !+ }").is_ok());
            assert_eq!(interpreter.deque, VecDeque::from([object::number!(3.0)]));
        }

        {
            let mut env = Env::new();
            let mut interpreter = Interpreter::new(&mut env);
            assert!(interpreter.run("!0 !if !1 !eq { !1 !2 !+ } else { !2 !1 !- }").is_ok());
            assert_eq!(interpreter.deque, VecDeque::from([object::number!(1.0)]));
        }

        {
            let mut env = Env::new();
            let mut interpreter = Interpreter::new(&mut env);
            assert!(interpreter.run("
                !0
                !if !dup !1 !eq {
                    !1 !2 !+
                } elif !dup !0 !eq {
                    !99 !+
                } else {
                    !2 !1 !-
                }
            ").is_ok());
            assert_eq!(interpreter.deque, VecDeque::from([object::number!(99.0)]));
        }
    }

    #[test]
    fn interpret_while_loop() {
        // Series from 0 to 10 (including 10)
        let mut env = Env::new();
        let mut interpreter = Interpreter::new(&mut env);
        assert!(interpreter.run(
                "
                !1
                !while !dup !10 !> {
                    !dup !inc
                }
                "
            )
            .is_ok());
        assert_eq!(
            interpreter.deque,
            VecDeque::from([
                object::number!(10.0), object::number!(9.0), object::number!(8.0), object::number!(7.0), object::number!(6.0), object::number!(5.0),
                object::number!(4.0), object::number!(3.0), object::number!(2.0), object::number!(1.0)])
        );
    }
}