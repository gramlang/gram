use crate::{
    de_bruijn::{open, shift},
    equality::definitionally_equal,
    error::{throw, Error},
    format::CodeStr,
    normalizer::normalize_weak_head,
    term::{
        Term,
        Variant::{
            Application, Difference, Integer, IntegerLiteral, Lambda, Let, Pi, Product, Quotient,
            Sum, Type, Variable,
        },
        INTEGER_TERM, TYPE_TERM,
    },
};
use scopeguard::defer;
use std::{cell::RefCell, path::Path, rc::Rc};

// This is the top-level type checking function. Invariants:
// - The two contexts have the same length.
// - When this function is finished, the contexts are left unmodified.
#[allow(clippy::too_many_lines)]
pub fn type_check<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &Term<'a>,
    typing_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
) -> Result<Rc<Term<'a>>, Error> {
    Ok(match &term.variant {
        Type | Integer => Rc::new(TYPE_TERM),
        Variable(variable, index) => {
            // Check if we have a type for this variable.
            match &typing_context[typing_context.len() - 1 - *index] {
                Some((variable_type, offset)) => {
                    // Shift the type such that it's valid in the current context.
                    shift(variable_type.clone(), 0, *index + 1 - offset)
                }
                None => {
                    return Err(if let Some(source_range) = term.source_range {
                        throw(
                            "Unknown type for this variable. You can fix this error by annotating \
                                the variable where it\u{2019}s introduced.",
                            source_path,
                            source_contents,
                            source_range,
                        )
                    } else {
                        Error {
                            message: format!(
                                "Unknown type for variable {} You can fix this error by \
                                    annotating the variable where it\u{2019}s introduced.",
                                (*variable).to_string().code_str(),
                            ),
                            reason: None,
                        }
                    });
                }
            }
        }
        Lambda(variable, domain, body) => {
            // Infer the type of the domain.
            let domain_type = type_check(
                source_path,
                source_contents,
                &**domain,
                typing_context,
                definitions_context,
            )?;

            // Check that the type of the domain is the type of all types.
            if !definitionally_equal(domain_type.clone(), Rc::new(TYPE_TERM), definitions_context) {
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
            typing_context.push(Some((domain.clone(), 0)));
            definitions_context.push(None);

            // Infer the codomain.
            let codomain_result = type_check(
                source_path,
                source_contents,
                &**body,
                typing_context,
                definitions_context,
            );

            // Restore the context.
            definitions_context.pop();
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
                definitions_context,
            )?;

            // Check that the type of the domain is the type of all types.
            if !definitionally_equal(domain_type.clone(), Rc::new(TYPE_TERM), definitions_context) {
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
            typing_context.push(Some((domain.clone(), 0)));
            definitions_context.push(None);

            // Infer the type of the codomain.
            let codomain_type_result = type_check(
                source_path,
                source_contents,
                &**codomain,
                typing_context,
                definitions_context,
            );

            // Fail if the type of the codomain is not well-typed.
            let codomain_type = match codomain_type_result {
                Ok(codomain_type) => codomain_type,
                Err(err) => {
                    // Restore the context.
                    definitions_context.pop();
                    typing_context.pop();

                    // Return the error.
                    return Err(err);
                }
            };

            // Check that the type of the codomain is the type of all types.
            let codomain_is_type = definitionally_equal(
                codomain_type.clone(),
                Rc::new(TYPE_TERM),
                definitions_context,
            );

            // Restore the context.
            definitions_context.pop();
            typing_context.pop();

            // If the codomain is not a type, throw a type error.
            if !codomain_is_type {
                // Restore the context.
                definitions_context.pop();
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
                definitions_context,
            )?;

            // Reduce the type of the applicand to weak head normal form.
            let applicand_type_whnf =
                normalize_weak_head(applicand_type.clone(), definitions_context);

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
                definitions_context,
            )?;

            // Check that the argument type equals the domain.
            if !definitionally_equal(argument_type.clone(), domain.clone(), definitions_context) {
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
        Let(definitions, body) => {
            // When the function returns, remove the variables from the context that we are
            // temporarily adding.
            let context_cell = RefCell::new(((typing_context, definitions_context), 0_usize));
            defer! {{
                let mut guard = context_cell.borrow_mut();
                let (
                    (borrowed_typing_context, borrowed_definitions_context),
                    borrowed_variables_added,
                ) = &mut (*guard);

                for _ in 0..*borrowed_variables_added {
                    borrowed_typing_context.pop();
                    borrowed_definitions_context.pop();
                }
            }};

            // Add the definitions and their types to the context.
            for (i, (_, annotation, definition)) in definitions.iter().enumerate() {
                // Temporarily borrow from the scope guard.
                let mut guard = context_cell.borrow_mut();
                let (
                    (borrowed_typing_context, borrowed_definitions_context),
                    borrowed_variables_added,
                ) = &mut (*guard);

                // Compute this once rather than multiple times.
                let definitions_len_minus_i = definitions.len() - i;

                // Add the definition and its type to the context.
                borrowed_typing_context.push(
                    annotation
                        .as_ref()
                        .map(|annotation| (annotation.clone(), definitions_len_minus_i)),
                );
                borrowed_definitions_context
                    .push(Some((definition.clone(), definitions_len_minus_i)));
                *borrowed_variables_added += 1;
            }

            // Infer/check the types of the definitions.
            for (i, (_, annotation, definition)) in definitions.iter().enumerate() {
                // Temporarily borrow from the scope guard.
                let mut guard = context_cell.borrow_mut();
                let ((borrowed_typing_context, borrowed_definitions_context), _) = &mut (*guard);

                // Infer the type of the definition.
                let definition_type = type_check(
                    source_path,
                    source_contents,
                    &**definition,
                    borrowed_typing_context,
                    borrowed_definitions_context,
                )?;

                // Check if we have an annotation.
                if let Some(annotation) = annotation {
                    // Check the type against the annotation.
                    if !definitionally_equal(
                        definition_type.clone(),
                        annotation.clone(),
                        borrowed_definitions_context,
                    ) {
                        return Err(if let Some(source_range) = definition.source_range {
                            throw(
                                &format!(
                                    "This has type {} but it was annotated as {}.",
                                    definition_type.to_string().code_str(),
                                    annotation.to_string().code_str(),
                                ),
                                source_path,
                                source_contents,
                                source_range,
                            )
                        } else {
                            Error {
                                message: format!(
                                    "Definition {} has type {} but it was annotated as {}.",
                                    definition.to_string().code_str(),
                                    definition_type.to_string().code_str(),
                                    annotation.to_string().code_str(),
                                ),
                                reason: None,
                            }
                        });
                    }
                } else {
                    // Update the type in the context.
                    let definition_index = borrowed_typing_context.len() - definitions.len() + i;
                    borrowed_typing_context[definition_index] =
                        Some((definition_type, definitions.len() - i));
                }
            }

            // Temporarily borrow from the scope guard.
            let mut guard = context_cell.borrow_mut();
            let ((borrowed_typing_context, borrowed_definitions_context), _) = &mut (*guard);

            // Infer the type of the body.
            let body_type = type_check(
                source_path,
                source_contents,
                &**body,
                borrowed_typing_context,
                borrowed_definitions_context,
            )?;

            // Return the opened type of the body.
            (0..definitions.len()).fold(body_type, |acc, i| {
                // Compute this once rather than multiple times.
                let definitions_len_minus_one_minus_i = definitions.len() - 1 - i;

                // Open the body.
                open(
                    acc,
                    0,
                    Rc::new(Term {
                        source_range: None,
                        variant: Let(
                            definitions
                                .iter()
                                .map(|(variable, annotation, definition)| {
                                    (
                                        *variable,
                                        annotation.as_ref().map(|annotation| {
                                            shift(
                                                annotation.clone(),
                                                0,
                                                definitions_len_minus_one_minus_i,
                                            )
                                        }),
                                        shift(
                                            definition.clone(),
                                            0,
                                            definitions_len_minus_one_minus_i,
                                        ),
                                    )
                                })
                                .collect(),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable(
                                    definitions[definitions_len_minus_one_minus_i].0,
                                    i,
                                ),
                            }),
                        ),
                    }),
                )
            })
        }
        IntegerLiteral(_) => Rc::new(INTEGER_TERM),
        Sum(summand1, summand2) => {
            // Infer the type of the left summand.
            let summand1_type = type_check(
                source_path,
                source_contents,
                &**summand1,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the left summand to weak head normal form.
            let summand1_type_whnf =
                normalize_weak_head(summand1_type.clone(), definitions_context);

            // Make sure the type of the left summand is the type of integers.
            if let Integer = &summand1_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = summand1.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            summand1_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Summand {} has type {} when {} was expected.",
                            summand1.to_string().code_str(),
                            summand1_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Infer the type of the right summand.
            let summand2_type = type_check(
                source_path,
                source_contents,
                &**summand2,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the right summand to weak head normal form.
            let summand2_type_whnf =
                normalize_weak_head(summand2_type.clone(), definitions_context);

            // Make sure the type of the right summand is the type of integers.
            if let Integer = &summand2_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = summand2.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            summand2_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Summand {} has type {} when {} was expected.",
                            summand2.to_string().code_str(),
                            summand2_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Return the type of integers.
            Rc::new(INTEGER_TERM)
        }
        Difference(minuend, subtrahend) => {
            // Infer the type of the minuend.
            let minuend_type = type_check(
                source_path,
                source_contents,
                &**minuend,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the minuend to weak head normal form.
            let minuend_type_whnf = normalize_weak_head(minuend_type.clone(), definitions_context);

            // Make sure the type of the minuend is the type of integers.
            if let Integer = &minuend_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = minuend.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            minuend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Minuend {} has type {} when {} was expected.",
                            minuend.to_string().code_str(),
                            minuend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Infer the type of the subtrahend.
            let subtrahend_type = type_check(
                source_path,
                source_contents,
                &**subtrahend,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the subtrahend to weak head normal form.
            let subtrahend_type_whnf =
                normalize_weak_head(subtrahend_type.clone(), definitions_context);

            // Make sure the type of the subtrahend is the type of integers.
            if let Integer = &subtrahend_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = subtrahend.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            subtrahend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Subtrahend {} has type {} when {} was expected.",
                            subtrahend.to_string().code_str(),
                            subtrahend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Return the type of integers.
            Rc::new(INTEGER_TERM)
        }
        Product(factor1, factor2) => {
            // Infer the type of the left factor.
            let factor1_type = type_check(
                source_path,
                source_contents,
                &**factor1,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the left factor to weak head normal form.
            let factor1_type_whnf = normalize_weak_head(factor1_type.clone(), definitions_context);

            // Make sure the type of the left factor is the type of integers.
            if let Integer = &factor1_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = factor1.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            factor1_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Summand {} has type {} when {} was expected.",
                            factor1.to_string().code_str(),
                            factor1_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Infer the type of the right factor.
            let factor2_type = type_check(
                source_path,
                source_contents,
                &**factor2,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the right factor to weak head normal form.
            let factor2_type_whnf = normalize_weak_head(factor2_type.clone(), definitions_context);

            // Make sure the type of the right factor is the type of integers.
            if let Integer = &factor2_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = factor2.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            factor2_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Factor {} has type {} when {} was expected.",
                            factor2.to_string().code_str(),
                            factor2_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Return the type of integers.
            Rc::new(INTEGER_TERM)
        }
        Quotient(dividend, subtrahend) => {
            // Infer the type of the dividend.
            let dividend_type = type_check(
                source_path,
                source_contents,
                &**dividend,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the dividend to weak head normal form.
            let dividend_type_whnf =
                normalize_weak_head(dividend_type.clone(), definitions_context);

            // Make sure the type of the dividend is the type of integers.
            if let Integer = &dividend_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = dividend.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            dividend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Summand {} has type {} when {} was expected.",
                            dividend.to_string().code_str(),
                            dividend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Infer the type of the subtrahend.
            let subtrahend_type = type_check(
                source_path,
                source_contents,
                &**subtrahend,
                typing_context,
                definitions_context,
            )?;

            // Reduce the type of the subtrahend to weak head normal form.
            let subtrahend_type_whnf =
                normalize_weak_head(subtrahend_type.clone(), definitions_context);

            // Make sure the type of the subtrahend is the type of integers.
            if let Integer = &subtrahend_type_whnf.variant {
            } else {
                return Err(if let Some(source_range) = subtrahend.source_range {
                    throw(
                        &format!(
                            "This has type {} when {} was expected.",
                            subtrahend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        source_range,
                    )
                } else {
                    Error {
                        message: format!(
                            "Dividend {} has type {} when {} was expected.",
                            subtrahend.to_string().code_str(),
                            subtrahend_type.to_string().code_str(),
                            INTEGER_TERM.to_string().code_str(),
                        ),
                        reason: None,
                    }
                });
            };

            // Return the type of integers.
            Rc::new(INTEGER_TERM)
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
        let mut definitions_context = vec![];
        let term_source = "type";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
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
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            )),
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("a", 0),
                }),
                0,
            )),
        ];
        let mut definitions_context = vec![None, None];
        let term_source = "x";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_lambda() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Some((
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            0,
        ))];
        let mut definitions_context = vec![None];
        let term_source = "(x : a) => x";
        let type_source = "(x : a) -> a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_pi() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Some((
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            0,
        ))];
        let mut definitions_context = vec![None];
        let term_source = "(x : a) -> a";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
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
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            )),
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("a", 0),
                }),
                0,
            )),
        ];
        let mut definitions_context = vec![None, None];
        let term_source = "((x : a) => x) y";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
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
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            )),
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            )),
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("b", 0),
                }),
                0,
            )),
        ];
        let mut definitions_context = vec![None, None, None];
        let term_source = "((x : a) => x) y";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();

        assert_fails!(
            type_check(
                None,
                term_source,
                &term_term,
                &mut typing_context,
                &mut definitions_context
            ),
            "has type `(b)` when `(a)` was expected",
        );
    }

    #[test]
    fn type_check_dependent_apply() {
        let parsing_context = ["int", "y"];
        let mut typing_context = vec![
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            )),
            Some((
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("int", 0),
                }),
                0,
            )),
        ];
        let mut definitions_context = vec![None, None];
        let term_source = "
          ((a : type) => (P: (x : a) -> type) => (f : (x : a) -> P x) => (x : a) => f x) (
            ((t : type) => t) int) (
              (x : int) => int) (
                (x : int) => x) y
        ";
        let type_source = "(((x : int) => int) (y))";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_let() {
        let parsing_context = ["a"];
        let mut typing_context = vec![Some((
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            0,
        ))];
        let mut definitions_context = vec![None];
        let term_source = "b = a; b";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_sum() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "1 + 2";
        let type_source = "integer";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_difference() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "1 - 2";
        let type_source = "integer";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_product() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "2 * 3";
        let type_source = "integer";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }

    #[test]
    fn type_check_quotient() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "7 / 2";
        let type_source = "integer";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let term_type_term = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(syntactically_equal(&term_type_term, &type_term), true);
    }
}
