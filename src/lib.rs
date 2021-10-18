use log::trace;

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter {
    stack: Vec<u64>,
    constraints: Vec<ConstraintValued>,
    functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    sig: FunctionSig,
    operations: Vec<Operation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSig {
    name: String,
    constraints: Vec<Constraint>,
}

impl FunctionSig {
    pub fn without_constraints(name: impl Into<String>) -> Self {
        FunctionSig {
            name: name.into(),
            constraints: Vec::new(),
        }
    }
}

impl FunctionSig {
    pub fn new(name: impl Into<String>, constraints: Vec<Constraint>) -> Self {
        FunctionSig {
            name: name.into(),
            constraints,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstraintValued {
    func_name: String,
    args: Vec<u64>,
}

impl ConstraintValued {
    pub fn new(func_name: impl Into<String>, args: Vec<u64>) -> Self {
        ConstraintValued { func_name: func_name.into(), args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    func_name: String,
    args: Vec<ConstraintArg>,
}

impl Constraint {
    pub fn new(func_name: impl Into<String>, args: Vec<ConstraintArg>) -> Self {
        Constraint { func_name: func_name.into(), args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstraintArg {
    Value(u64),
    VarIdx(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    /// Push value to stack.
    Push(u64),
    /// Call function body operations if constraints of sig is satisfied.
    Call(String),
    /// Call function and if it returns 1 so add to constraints this function with these args otherwise do nothing.
    Check(String),
    /// Drop value from stack.
    Drop,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            constraints: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn run_function(&mut self, func: Function) -> Result<(), InterpreterError> {
        trace!("Run function {}", func.sig.name);

        self.check_constraints_satisfied(&func.sig)?;

        for operation in func.operations {
            match operation {
                Operation::Push(value) => self.stack.push(value),
                Operation::Drop => {
                    self.stack.pop();
                },
                Operation::Call(func) => {
                    let function = self
                        .functions
                        .iter()
                        .find(|f| f.sig.name == func)
                        .ok_or_else(|| InterpreterError::FunNotFound(func))?
                        .clone();
                    return self.run_function(function);
                },
                Operation::Check(func) => {
                    match func.as_str() {
                        ">" => {
                            let x = self.stack_get_value(1)?;
                            let y = self.stack_get_value(0)?;
                            if x > y {
                                self.constraints.push(ConstraintValued::new(">".to_string(), vec![x, y]))
                            }
                        }
                        _ => return Err(InterpreterError::FunNotFound(func))
                    }
                }
            }
        }

        Ok(())
    }

    fn check_constraints_satisfied(&self, func: &FunctionSig) -> Result<(), InterpreterError> {
        for constr in &func.constraints {
            let valued_constr = self.constraint_to_valued(constr.clone())?;

            if !self.constraints.contains(&valued_constr) {
                return Err(InterpreterError::ConstraintDoesNotSatisfied(constr.clone()));
            }
        }

        Ok(())
    }

    fn constraint_to_valued(
        &self,
        constr: Constraint,
    ) -> Result<ConstraintValued, InterpreterError> {
        let mut args = Vec::with_capacity(constr.args.len());
        for arg in &constr.args {
            match arg {
                ConstraintArg::Value(v) => args.push(*v),
                ConstraintArg::VarIdx(idx) => args.push(
                    self.stack_get_value(*idx)?,
                ),
            }
        }

        Ok(ConstraintValued::new(constr.func_name, args))
    }

    fn stack_get_value(&self, idx: u32) -> Result<u64, InterpreterError> {
        self.stack.get(self.stack.len() - 1 - idx as usize).cloned().ok_or_else(|| InterpreterError::VarNotFound(idx))
    }
}

#[derive(Debug, PartialEq)]
pub enum InterpreterError {
    ConstraintDoesNotSatisfied(Constraint),
    VarNotFound(u32),
    FunNotFound(String),
}

#[macro_export]
macro_rules! make_fn {
    (fn $name:ident $($constr:expr)*; $($op:ident $($arg:literal)?)+) => {
        Function {
            sig: FunctionSig::new(
                stringify!($name),
                vec![$($constr)*]
            ),
            operations: vec![
                $($crate::make_fn!(@op $op $($arg)?),)*
            ],
        };
    };
    (@op push $value:literal) => { $crate::Operation::Push($value) };
    (@op drop) => { $crate::Operation::Drop };
    (@op check $func:literal) => { $crate::Operation::Check($func.to_string()) };
    (@op call $func:literal) => { $crate::Operation::Call($func.to_string()) };
    (@constr $name:literal, $($arg:expr)*) => {
        Constraint::new($name.to_string(), vec![$($crate::make_fn!(@constr_arg $arg),)*]),
    };
}

#[macro_export]
macro_rules! constrv {
    ($name:literal, $($arg:expr),*) => {
        ConstraintValued::new($name.to_string(), vec![$($arg),*])
    };
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
        let mut inter = Interpreter::new();
        let push_values_and_compare = make_fn! {
            fn push_values_and_compare;
            push 20
            push 10
            check ">"
        };
        inter.run_function(push_values_and_compare).unwrap();

        assert_eq!(*inter.constraints.last().unwrap(), constrv![">", 20, 10]);
    }

    #[test]
    fn test_run_constrained_function() {
        let mut inter = Interpreter::new();
        let constrained_function = make_fn! {
            fn constrained_function
            Constraint::new(">", vec![ConstraintArg::VarIdx(1), ConstraintArg::VarIdx(0)])
            ;
            drop
            drop
        };
        inter.functions.push(constrained_function);

        let push_values_and_compare = make_fn! {
            fn push_values_and_compare;
            push 20
            push 10
            check ">"
            call "constrained_function"
        };
        assert_eq!(inter.clone().run_function(push_values_and_compare), Ok(()));

        let without_constraint = make_fn! {
            fn without_constraint;
            push 20
            push 10
            call "constrained_function"
        };
        assert_eq!(
            inter.clone().run_function(without_constraint),
            Err(InterpreterError::ConstraintDoesNotSatisfied(
                Constraint::new(">", vec![ConstraintArg::VarIdx(1), ConstraintArg::VarIdx(0)])
            ))
        );
    }
}
