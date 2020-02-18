use crate::{
    de_bruijn::{open, shift},
    equality::definitionally_equal,
    error::{throw, Error},
    format::CodeStr,
    normalizer::normalize_weak_head,
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
        TYPE_TERM,
    },
};
use std::{path::Path, rc::Rc};

// This is the top-level type checking function. Invariants:
// - The two contexts have the same length.
// - When this function is finished, the contexts are left unmodified.
#[allow(clippy::too_many_lines)]
pub fn type_check<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &Term<'a>,
    typing_context: &mut Vec<Rc<Term<'a>>>,
    normalization_context: &mut Vec<Option<Rc<Term<'a>>>>,
) -> Result<Rc<Term<'a>>, Error> {
    Ok(match &term.variant {
        Type => Rc::new(TYPE_TERM),
        Variable(_, index) => {
            // Look up the type in the context, and shift it such that it's valid in the
            // current context.
            shift(
                typing_context[typing_context.len() - 1 - *index].clone(),
                0,
                *index + 1,
            )
        }
        Lambda(variable, domain, body) => {
            // Infer the type of the domain.
            let domain_type = type_check(
                source_path,
                source_contents,
                &**domain,
                typing_context,
                normalization_context,
            )?;

            // Check that the type of the domain is the type of all types.
            if !definitionally_equal(
                domain_type.clone(),
                Rc::new(TYPE_TERM),
                normalization_context,
            ) {
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

            // Temporarily add the variable's type to the context for the purpose of inferring
            // the codomain.
            typing_context.push(domain.clone());
            normalization_context.push(None);

            // Infer the codomain.
            let codomain_result = type_check(
                source_path,
                source_contents,
                &**body,
                typing_context,
                normalization_context,
            );

            // Restore the context.
            normalization_context.pop();
            typing_context.pop();

            // Fail if the codomain is not well-typed.
            let codomain = codomain_result?;

            // Construct and return the pi type.
            Rc::new(Term {
                source_range: term.source_range,
                variant: Pi(variable, domain.clone(), codomain),
            })
        }
        Pi(_, domain, codomain) => {
            // Infer the type of the domain.
            let domain_type = type_check(
                source_path,
                source_contents,
                &**domain,
                typing_context,
                normalization_context,
            )?;

            // Check that the type of the domain is the type of all types.
            if !definitionally_equal(
                domain_type.clone(),
                Rc::new(TYPE_TERM),
                normalization_context,
            ) {
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

            // Temporarily add the variable's type to the context for the purpose of inferring
            // the type of the codomain.
            typing_context.push(domain.clone());
            normalization_context.push(None);

            // Infer the type of the codomain.
            let codomain_type_result = type_check(
                source_path,
                source_contents,
                &**codomain,
                typing_context,
                normalization_context,
            );

            // Fail if the type of the codomain is not well-typed.
            let codomain_type = match codomain_type_result {
                Ok(codomain_type) => codomain_type,
                Err(err) => {
                    // Restore the context.
                    normalization_context.pop();
                    typing_context.pop();

                    // Return the error.
                    return Err(err);
                }
            };

            // Check that the type of the codomain is the type of all types.
            let codomain_is_type = definitionally_equal(
                codomain_type.clone(),
                Rc::new(TYPE_TERM),
                normalization_context,
            );

            // Restore the context.
            normalization_context.pop();
            typing_context.pop();

            // If the codomain is not a type, throw a type error.
            if !codomain_is_type {
                // Restore the context.
                normalization_context.pop();
                typing_context.pop();

                // Throw a type error.
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
            let applicand_type = type_check(
                source_path,
                source_contents,
                &**applicand,
                typing_context,
                normalization_context,
            )?;

            // Reduce the type of the applicand to weak head normal form.
            let applicand_type_whnf =
                normalize_weak_head(applicand_type.clone(), normalization_context);

            // Make sure the type of the applicand is a pi type.
            let (domain, codomain) = if let Pi(_, domain, codomain) = &applicand_type_whnf.variant {
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
            let argument_type = type_check(
                source_path,
                source_contents,
                &**argument,
                typing_context,
                normalization_context,
            )?;

            // Check that the argument type equals the domain.
            if !definitionally_equal(argument_type.clone(), domain.clone(), normalization_context) {
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
            open(codomain.clone(), 0, argument.clone())
        }
        Let(_, definition, body) => {
            // Infer the type of the definition.
            let definition_type = type_check(
                source_path,
                source_contents,
                &**definition,
                typing_context,
                normalization_context,
            )?;

            // Temporarily add the definition and its type to the context for the purpose of
            // inferring the codomain.
            typing_context.push(definition_type.clone());
            normalization_context.push(Some(definition.clone()));

            // Infer the type of the body.
            let body_type_result = type_check(
                source_path,
                source_contents,
                &**body,
                typing_context,
                normalization_context,
            );

            // Restore the context.
            normalization_context.pop();
            typing_context.pop();

            // Return the opened type of the body.
            open(body_type_result?, 0, definition.clone())
        }
    })
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
        let mut normalization_context = vec![];
        let term_source = "type";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut normalization_context,
        )
        .unwrap();

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
                variant: Type,
            }),
            Rc::new(Term {
                source_range: None,
                variant: Variable("a", 0),
            }),
        ];
        let mut normalization_context = vec![None, None];
        let term_source = "x";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut normalization_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_lambda() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Rc::new(Term {
            source_range: None,
            variant: Type,
        })];
        let mut normalization_context = vec![None];
        let term_source = "(x : a) => x";
        let type_source = "(x : a) -> a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut normalization_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_pi() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Rc::new(Term {
            source_range: None,
            variant: Type,
        })];
        let mut normalization_context = vec![None];
        let term_source = "(x : a) -> a";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut normalization_context,
        )
        .unwrap();

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
                variant: Type,
            }),
            Rc::new(Term {
                source_range: None,
                variant: Variable("a", 0),
            }),
        ];
        let mut normalization_context = vec![None, None];
        let term_source = "((x : a) => x) y";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut normalization_context,
        )
        .unwrap();

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
                variant: Type,
            }),
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            Rc::new(Term {
                source_range: None,
                variant: Variable("b", 0),
            }),
        ];
        let mut normalization_context = vec![None, None, None];
        let term_source = "((x : a) => x) y";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();

        assert_fails!(
            type_check(
                None,
                term_source,
                &term_term,
                &mut typing_context,
                &mut normalization_context
            ),
            "has type `(b)` when `(a)` was expected",
        );
    }

    #[test]
    fn type_check_let() {
        let parsing_context = ["a", "x"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            Rc::new(Term {
                source_range: None,
                variant: Variable("a", 0),
            }),
        ];
        let mut normalization_context = vec![None, None];
        let term_source = "y = x; y";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut normalization_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_dependent_apply() {
        let parsing_context = ["int", "y"];
        let mut typing_context = vec![
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            Rc::new(Term {
                source_range: None,
                variant: Variable("int", 0),
            }),
        ];
        let mut normalization_context = vec![None, None];
        let term_source = "
          ((a : type) => (P: (x : a) -> type) => (f : (x : a) -> P x) => (x : a) => f x)
            (((t : type) => t) int)
              ((x : int) => int)
                ((x : int) => x)
                  y
        ";
        let type_source = "(((x : int) => int) (y))";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut normalization_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }
}
