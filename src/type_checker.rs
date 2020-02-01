use crate::{
    de_bruijn::{open, shift},
    equality::syntactically_equal,
    error::{throw, Error},
    format::CodeStr,
    normalizer::normalize,
    term::{
        Term,
        Variant::{Application, Lambda, Pi, Type, Variable},
        TYPE_TERM,
    },
};
use std::{path::Path, rc::Rc};

// This is the top-level type checking function. Invariants:
// - The types of the variables in the context are normalized.
// - The type returned by this function is normalized.
#[allow(clippy::too_many_lines)]
pub fn type_check<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &Term<'a>,
    context: &mut Vec<Rc<Term<'a>>>,
) -> Result<Rc<Term<'a>>, Error> {
    // The type checking rules are syntax-directed, so here we pattern match on the syntax.
    Ok(normalize(&*match &term.variant {
        Type => Rc::new(TYPE_TERM),
        Variable(_, index) => {
            // Look up the type in the context, and shift it such that it's valid in the
            // current context.
            shift(&*context[context.len() - 1 - *index], 0, *index + 1)
        }
        Lambda(variable, domain, body) => {
            // Infer the type of the domain.
            let domain_type = type_check(source_path, source_contents, &**domain, context)?;

            // Check that the type of the domain is the type of all types.
            if !syntactically_equal(&*domain_type, &TYPE_TERM) {
                return Err(if let Some(source_range) = domain.source_range {
                    throw(
                        &format!(
                            "This domain has type {} when {} was expected.",
                            domain_type.to_string().code_str(),
                            TYPE_TERM.to_string().code_str(),
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
                            TYPE_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            }

            // Temporarily add the variable's type to the context for the purpose of inferring the
            // codomain.
            context.push(domain.clone());

            // Infer the codomain.
            let codomain = type_check(source_path, source_contents, &**body, context)?;

            // Restore the context.
            context.pop();

            // Construct and return the pi type.
            Rc::new(Term {
                source_range: term.source_range,
                group: false,
                variant: Pi(variable, domain.clone(), codomain),
            })
        }
        Pi(_, domain, codomain) => {
            // Infer the type of the domain.
            let domain_type = type_check(source_path, source_contents, &**domain, context)?;

            // Check that the type of the domain is the type of all types.
            if !syntactically_equal(&*domain_type, &TYPE_TERM) {
                return Err(if let Some(source_range) = domain.source_range {
                    throw(
                        &format!(
                            "This domain has type {} when {} was expected.",
                            domain_type.to_string().code_str(),
                            TYPE_TERM.to_string().code_str(),
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
                            TYPE_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            }

            // Temporarily add the variable's type to the context for the purpose of inferring the
            // type of the codomain.
            context.push(domain.clone());

            // Infer the type of the codomain.
            let codomain_type = type_check(source_path, source_contents, &**codomain, context)?;

            // Restore the context.
            context.pop();

            // Check that the type of the codomain is the type of all types.
            if !syntactically_equal(&*codomain_type, &TYPE_TERM) {
                return Err(if let Some(source_range) = codomain.source_range {
                    throw(
                        &format!(
                            "This codomain has type {} when {} was expected.",
                            codomain_type.to_string().code_str(),
                            TYPE_TERM.to_string().code_str(),
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
                            TYPE_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            }

            // The type of a pi type is the type of all types.
            Rc::new(TYPE_TERM)
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
            if !syntactically_equal(&*argument_type, &**domain) {
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
            open(&**codomain, 0, &**argument)
        }
    }))
}

#[cfg(test)]
mod tests {
    use crate::{
        assert_fails,
        equality::syntactically_equal,
        parser::parse,
        term::{
            Term,
            Variant::{Type, Variable},
        },
        tokenizer::tokenize,
        type_checker::type_check,
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

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_variable() {
        let parsing_context = ["a", "x"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Type,
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

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_lambda() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Rc::new(Term {
            source_range: None,
            group: false,
            variant: Type,
        })];
        let term_source = "(x : a) => x";
        let type_source = "(x : a) -> a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_pi() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Rc::new(Term {
            source_range: None,
            group: false,
            variant: Type,
        })];
        let term_source = "(x : a) -> a";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term =
            type_check(None, term_source, &term_term, &mut typing_context).unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_application() {
        let parsing_context = ["a", "y"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Type,
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

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_bad_application() {
        let parsing_context = ["a", "b", "y"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Type,
            }),
            Rc::new(Term {
                source_range: None,
                group: false,
                variant: Type,
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
                variant: Type,
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

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }
}
