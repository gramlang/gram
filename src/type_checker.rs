use crate::{
    ast::{
        Node,
        Variant::{Application, Lambda, Pi, Variable},
    },
    de_bruijn::{open, shift},
    equality::definitionally_equal,
    error::{throw, Error},
    format::CodeStr,
};
use std::{
    borrow::{Borrow, BorrowMut},
    cmp::max,
    path::Path,
    rc::Rc,
};

// The type of all types
pub const TYPE: &str = "type";

// This is the top-level type checking function.
#[allow(clippy::too_many_lines)]
pub fn type_check<'a, T: Borrow<Node<'a>>, U: BorrowMut<Vec<Rc<Node<'a>>>>>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    node: T,
    mut context: U, // The first element is assumed to be `TYPE` [ref:context-starts-with-type].
) -> Result<Rc<Node<'a>>, Error> {
    // Get references to the borrowed data.
    let node = node.borrow();
    // let context = context.borrow_mut(); // Why doesn't this work?

    // Construct `TYPE`.
    let type_type = Node {
        source_range: None,
        group: false,
        variant: Variable(TYPE, context.borrow_mut().len() - 1),
    };

    // The type checking rules are syntax-directed, so here we pattern match on the syntax.
    match &node.variant {
        Variable(_, index) => {
            // Look up the variable in the context to get its type.
            let len = context.borrow().len();
            let variable_type = context.borrow_mut()[max(len - 1, *index) - *index].clone();

            // Shift the type to make it agree with this context.
            Ok(shift(variable_type, 0, *index))
        }
        Lambda(variable, domain, body) => {
            // Temporarily add the variable's type to the context for the purpose of inferring the
            // codomain.
            context.borrow_mut().push(shift(&**domain, 0, 1));

            // Infer the codomain.
            let codomain = type_check(source_path, source_contents, &**body, context.borrow_mut())?;

            // Restore the context.
            context.borrow_mut().pop();

            // Construct the pi type.
            let pi_type = Node {
                source_range: node.source_range,
                group: false,
                variant: Pi(variable, domain.clone(), codomain),
            };

            // Infer the type of the pi type.
            let pi_type_type =
                type_check(source_path, source_contents, &pi_type, context.borrow_mut())?;

            // Check that the type of the pi type is `TYPE`.
            if !definitionally_equal(&*pi_type_type, &type_type) {
                return Err(if let Some(source_range) = pi_type.source_range {
                    throw(
                        format!(
                            "The type of this is {} when {} was expected.",
                            pi_type_type.to_string().code_str(),
                            type_type.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Computed type {} when {} was expected.",
                            pi_type_type.to_string().code_str(),
                            type_type.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            }

            // Construct the pi type.
            Ok(Rc::new(pi_type))
        }
        Pi(_, domain, codomain) => {
            // Infer the type of the domain.
            let domain_type = type_check(
                source_path,
                source_contents,
                &**domain,
                context.borrow_mut(),
            )?;

            // Temporarily add the variable's type to the context for the purpose of inferring the
            // type of the codomain.
            context.borrow_mut().push(shift(&**domain, 0, 1));

            // Infer the type of the codomain.
            let codomain_type = type_check(
                source_path,
                source_contents,
                &**codomain,
                context.borrow_mut(),
            )?;

            // Restore the context.
            context.borrow_mut().pop();

            // Check that the domain has type `TYPE`.
            if !definitionally_equal(&*domain_type, &type_type) {
                return Err(if let Some(source_range) = domain.source_range {
                    throw(
                        format!(
                            "This domain has type {} when {} was expected.",
                            domain_type.to_string().code_str(),
                            type_type.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Found domain of type {} when {} was expected.",
                            domain_type.to_string().code_str(),
                            type_type.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            }

            // Check that the codomain has type `TYPE`.
            if !definitionally_equal(&*codomain_type, shift(&type_type, 0, 1)) {
                return Err(if let Some(source_range) = codomain.source_range {
                    throw(
                        format!(
                            "codomain has type {} when {} was expected.",
                            codomain_type.to_string().code_str(),
                            type_type.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Found codomain of type {} when {} was expected.",
                            codomain_type.to_string().code_str(),
                            type_type.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            }

            // Return `TYPE`.
            Ok(Rc::new(type_type))
        }
        Application(applicand, argument) => {
            // Infer the type of the applicand.
            let applicand_type = type_check(
                source_path,
                source_contents,
                &**applicand,
                context.borrow_mut(),
            )?;

            // Make sure the type of the applicand is a pi type.
            let (domain, codomain) = if let Pi(_, domain, codomain) = &applicand_type.variant {
                (domain, codomain)
            } else {
                return Err(if let Some(source_range) = applicand.source_range {
                    throw(
                        format!(
                            "This has type {} when a pi type was expected.",
                            applicand_type.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Applicand {} has type {} when a pi type was expected.",
                            applicand.to_string().code_str(),
                            applicand_type.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Infer the type of the argument.
            let argument_type = type_check(
                source_path,
                source_contents,
                &**argument,
                context.borrow_mut(),
            )?;

            // Check that the argument type equals the domain.
            if !definitionally_equal(&*argument_type, &**domain) {
                return Err(if let Some(source_range) = argument.source_range {
                    throw(
                        format!(
                            "This has type {} when a pi type was expected.",
                            argument_type.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Argument {} has type {} when a pi type was expected.",
                            argument.to_string().code_str(),
                            argument_type.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            }

            // Construct and return the codomain specialized to the argument.
            Ok(open(&**codomain, 0, &**argument))
        }
    }
}
