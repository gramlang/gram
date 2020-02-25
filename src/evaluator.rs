use crate::{
    de_bruijn::open,
    format::CodeStr,
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::rc::Rc;

// This function evaluates a term using a call-by-value strategy. Assumption: the term has already
// been type-checked. Thus type errors result in panicking rather than returning an error.
pub fn evaluate<'a>(term: Rc<Term<'a>>) -> Rc<Term<'a>> {
    match &term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) => {
            // These cases are already values.
            term
        }
        Variable(variable, _) => {
            // We're stuck!
            panic!(format!(
                "Attempted to evaluate variable {}.",
                variable.to_string().code_str()
            ))
        }
        Application(applicand, argument) => {
            // Evaluate the applicand.
            let evaluated_applicand = evaluate(applicand.clone());

            // Evaluate the argument.
            let evaluated_argument = evaluate(argument.clone());

            // Check if the applicand evaluated to a lambda.
            if let Lambda(_, _, body) = &evaluated_applicand.variant {
                // We got a lambda. Perform beta reduction and continue evaluating.
                evaluate(open(body.clone(), 1, evaluated_argument))
            } else {
                // We didn't get a lambda. We're stuck!
                panic!(format!(
                    "Attempted to apply non-lambda term {} to {}.",
                    evaluated_applicand.to_string().code_str(),
                    evaluated_argument.to_string().code_str()
                ))
            }
        }
        Let(definitions, body) => {
            panic!()
        }
    }
}
