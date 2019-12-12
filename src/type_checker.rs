use crate::{
    de_bruijn::{open, shift},
    equality::definitionally_equal,
    error::{throw, Error},
    format::CodeStr,
    term::{
        Term,
        Variant::{Application, Lambda, Pi, Variable},
    },
};
use std::{path::Path, rc::Rc};

// The type of all types
pub const TYPE: &str = "type";

// This is the top-level type checking function.
#[allow(clippy::too_many_lines)]
pub fn type_check<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &Term<'a>,
    context: &mut Vec<Rc<Term<'a>>>, // `TYPE` is implicitly at the front of the context.
) -> Result<Rc<Term<'a>>, Error> {
    // Construct `TYPE`.
    let type_type = Term {
        source_range: None,
        group: false,
        variant: Variable(TYPE, context.len()),
    };

    // The type checking rules are syntax-directed, so here we pattern match on the syntax.
    match &term.variant {
        Variable(_, index) => {
            // Fetch the length of the context, i.e., the implicit position of `TYPE`.
            let len = context.len();

            // Are we looking up `TYPE`?
            if *index == len {
                // `TYPE` is its own type, so just return it.
                Ok(Rc::new(type_type))
            } else {
                // Look up the type in the context, and shift it such that it's valid in the
                // current context.
                Ok(shift(&*context[len - 1 - *index], 0, *index + 1))
            }
        }
        Lambda(variable, domain, body) => {
            // Temporarily add the variable's type to the context for the purpose of inferring the
            // codomain.
            context.push(domain.clone());

            // Infer the codomain.
            let codomain = type_check(source_path, source_contents, &**body, context)?;

            // Restore the context.
            context.pop();

            // Construct the pi type.
            let pi_type = Term {
                source_range: term.source_range,
                group: false,
                variant: Pi(variable, domain.clone(), codomain),
            };

            // Infer the type of the pi type.
            let pi_type_type = type_check(source_path, source_contents, &pi_type, context)?;

            // Check that the type of the pi type is `TYPE`.
            if !definitionally_equal(&*pi_type_type, &type_type) {
                return Err(if let Some(source_range) = pi_type.source_range {
                    throw(
                        &format!(
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
            let domain_type = type_check(source_path, source_contents, &**domain, context)?;

            // Temporarily add the variable's type to the context for the purpose of inferring the
            // type of the codomain.
            context.push(domain.clone());

            // Infer the type of the codomain.
            let codomain_type = type_check(source_path, source_contents, &**codomain, context)?;

            // Restore the context.
            context.pop();

            // Check that the domain has type `TYPE`.
            if !definitionally_equal(&*domain_type, &type_type) {
                return Err(if let Some(source_range) = domain.source_range {
                    throw(
                        &format!(
                            "This domain has type {:?} when {:?} was expected.",
                            domain_type, type_type,
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
            if !definitionally_equal(&*codomain_type, &shift(&type_type, 0, 1)) {
                return Err(if let Some(source_range) = codomain.source_range {
                    throw(
                        &format!(
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
            let applicand_type = type_check(source_path, source_contents, &**applicand, context)?;

            // Make sure the type of the applicand is a pi type.
            let (domain, codomain) = if let Pi(_, domain, codomain) = &applicand_type.variant {
                (domain, codomain)
            } else {
                return Err(if let Some(source_range) = applicand.source_range {
                    throw(
                        &format!(
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
            let argument_type = type_check(source_path, source_contents, &**argument, context)?;

            // Check that the argument type equals the domain.
            if !definitionally_equal(&*argument_type, &**domain) {
                return Err(if let Some(source_range) = argument.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            argument_type.to_string().code_str(),
                            domain.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Argument {} has type {} when {} was expected.",
                            argument.to_string().code_str(),
                            argument_type.to_string().code_str(),
                            domain.to_string().code_str(),
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

#[cfg(test)]
mod tests {
    use crate::{
        assert_fails,
        equality::definitionally_equal,
        parser::parse,
        term::{Term, Variant::Variable},
        tokenizer::tokenize,
        type_checker::{type_check, TYPE},
    };
    use std::rc::Rc;

    #[test]
    fn type_check_type() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let term_source = "type";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(definitionally_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_variable() {
        let parsing_context = ["a", "x"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable(TYPE, 0),
            }),
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable("a", 0),
            }),
        ];
        let term_source = "x";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(definitionally_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_lambda() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Rc::new(Term {
            source_range: None,
            group: false,
            variant: Variable(TYPE, 0),
        })];
        let term_source = "(x : a) => x";
        let type_source = "(x : a) -> a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(definitionally_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_pi() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Rc::new(Term {
            source_range: None,
            group: false,
            variant: Variable(TYPE, 0),
        })];
        let term_source = "(x : a) -> a";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(definitionally_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_application() {
        let parsing_context = ["a", "y"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable(TYPE, 0),
            }),
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable("a", 0),
            }),
        ];
        let term_source = "((x : a) => x) y";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(definitionally_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_bad_application() {
        let parsing_context = ["a", "b", "y"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable(TYPE, 0),
            }),
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable(TYPE, 1),
            }),
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable("b", 0),
            }),
        ];
        let term_source = "((x : a) => x) y";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();

        assert_fails!(
            type_check(None, term_source, &term_term, &mut typing_context),
            "has type `b` when `a` was expected",
        );
    }

    #[test]
    fn type_check_dependent_apply() {
        let parsing_context = ["int", "y"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable(TYPE, 0),
            }),
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Variable("int", 0),
            }),
        ];
        let term_source = "
          ((a : type) => (P: (x : a) -> type) => (f : (x : a) -> P x) => (x : a) => f x)
            (((t : type) => t) int)
              ((x : int) => int)
                ((x : int) => x)
                  y
        ";
        let type_source = "int";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(definitionally_equal(&term_type_term, &type_term), true);
    }
}
