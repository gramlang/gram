use crate::{
    de_bruijn::{open, shift},
    equality::definitionally_equal,
    error::{throw, Error},
    format::CodeStr,
    normalizer::normalize_weak_head,
    term::{
        Term,
        Variant::{
            Application, Boolean, Difference, EqualTo, False, GreaterThan, GreaterThanOrEqualTo,
            If, Integer, IntegerLiteral, Lambda, LessThan, LessThanOrEqualTo, Let, Negation, Pi,
            Product, Quotient, Sum, True, Type, Variable,
        },
    },
};
use scopeguard::defer;
use std::{cell::RefCell, path::Path, rc::Rc};

// Construct the type of all types once here rather than constructing it many times later.
pub const TYPE_TERM: Term = Term {
    source_range: None,
    variant: Type,
};

// Construct the type of integers once here rather than constructing it many times later.
pub const INTEGER_TERM: Term = Term {
    source_range: None,
    variant: Integer,
};

// Construct the type of Booleans once here rather than constructing it many times later.
pub const BOOLEAN_TERM: Term = Term {
    source_range: None,
    variant: Boolean,
};

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
        Type | Integer | Boolean => Rc::new(TYPE_TERM),
        Variable(variable, index) => {
            // Check if we have a type for this variable.
            match &typing_context[typing_context.len() - 1 - *index] {
                Some((variable_type, offset)) => {
                    // Shift the type such that it's valid in the current context.
                    shift(variable_type.clone(), 0, *index + 1 - offset)
                }
                None => {
                    return Err(throw(
                        &format!(
                            "Unknown type for {}. You can fix this error by annotating the \
                                variable where it\u{2019}s introduced.",
                            (*variable).to_string().code_str(),
                        ),
                        source_path,
                        term.source_range
                            .map(|source_range| (source_contents, source_range)),
                    ));
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
                return Err(throw(
                    "This is not a type:",
                    source_path,
                    domain
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
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
                return Err(throw(
                    "This is not a type:",
                    source_path,
                    domain
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
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
                return Err(throw(
                    "This is not a type:",
                    source_path,
                    codomain
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
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
                return Err(throw(
                    &format!(
                        "This has type {} when a function was expected:",
                        applicand_type.to_string().code_str(),
                    ),
                    source_path,
                    applicand
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
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
                return Err(throw(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        argument_type.to_string().code_str(),
                        domain.to_string().code_str(),
                    ),
                    source_path,
                    argument
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            }

            // Construct and return the codomain specialized to the argument.
            open(codomain.clone(), 0, argument.clone(), 0)
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
                        return Err(throw(
                            &format!(
                                "This has type {} but it was annotated as {}:",
                                definition_type.to_string().code_str(),
                                annotation.to_string().code_str(),
                            ),
                            source_path,
                            definition
                                .source_range
                                .map(|source_range| (source_contents, source_range)),
                        ));
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
                    0,
                )
            })
        }
        IntegerLiteral(_) => Rc::new(INTEGER_TERM),
        Negation(subterm) => {
            // Infer the type of the subterm.
            let subterm_type = type_check(
                source_path,
                source_contents,
                &**subterm,
                typing_context,
                definitions_context,
            )?;

            // Check that the type of the subterm is the type of integers.
            if !definitionally_equal(
                subterm_type.clone(),
                Rc::new(INTEGER_TERM),
                definitions_context,
            ) {
                return Err(throw(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        subterm_type.to_string().code_str(),
                        INTEGER_TERM.to_string().code_str(),
                    ),
                    source_path,
                    subterm
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            };

            // Return the type of integers.
            Rc::new(INTEGER_TERM)
        }
        Sum(term1, term2)
        | Difference(term1, term2)
        | Product(term1, term2)
        | Quotient(term1, term2) => {
            // Infer the type of the left subterm.
            let term1_type = type_check(
                source_path,
                source_contents,
                &**term1,
                typing_context,
                definitions_context,
            )?;

            // Check that the type of the left subterm is the type of integers.
            if !definitionally_equal(
                term1_type.clone(),
                Rc::new(INTEGER_TERM),
                definitions_context,
            ) {
                return Err(throw(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        INTEGER_TERM.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            };

            // Infer the type of the right subterm.
            let term2_type = type_check(
                source_path,
                source_contents,
                &**term2,
                typing_context,
                definitions_context,
            )?;

            // Check that the type of the right subterm is the type of integers.
            if !definitionally_equal(
                term2_type.clone(),
                Rc::new(INTEGER_TERM),
                definitions_context,
            ) {
                return Err(throw(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        INTEGER_TERM.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            };

            // Return the type of integers.
            Rc::new(INTEGER_TERM)
        }
        LessThan(term1, term2)
        | LessThanOrEqualTo(term1, term2)
        | EqualTo(term1, term2)
        | GreaterThan(term1, term2)
        | GreaterThanOrEqualTo(term1, term2) => {
            // Infer the type of the left subterm.
            let term1_type = type_check(
                source_path,
                source_contents,
                &**term1,
                typing_context,
                definitions_context,
            )?;

            // Check that the type of the left subterm is the type of integers.
            if !definitionally_equal(
                term1_type.clone(),
                Rc::new(INTEGER_TERM),
                definitions_context,
            ) {
                return Err(throw(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        INTEGER_TERM.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            };

            // Infer the type of the right subterm.
            let term2_type = type_check(
                source_path,
                source_contents,
                &**term2,
                typing_context,
                definitions_context,
            )?;

            // Check that the type of the right subterm is the type of integers.
            if !definitionally_equal(
                term2_type.clone(),
                Rc::new(INTEGER_TERM),
                definitions_context,
            ) {
                return Err(throw(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        INTEGER_TERM.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            };

            // Return the type of Booleans.
            Rc::new(BOOLEAN_TERM)
        }
        If(condition, then_branch, else_branch) => {
            // Infer the type of the condition.
            let condition_type = type_check(
                source_path,
                source_contents,
                &**condition,
                typing_context,
                definitions_context,
            )?;

            // Check that the type of the condition is the type of Booleans.
            if !definitionally_equal(
                condition_type.clone(),
                Rc::new(BOOLEAN_TERM),
                definitions_context,
            ) {
                return Err(throw(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        condition_type.to_string().code_str(),
                        BOOLEAN_TERM.to_string().code_str(),
                    ),
                    source_path,
                    condition
                        .source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            };

            // Infer the type of the then branch.
            let then_branch_type = type_check(
                source_path,
                source_contents,
                &**then_branch,
                typing_context,
                definitions_context,
            )?;

            // Infer the type of the else branch.
            let else_branch_type = type_check(
                source_path,
                source_contents,
                &**else_branch,
                typing_context,
                definitions_context,
            )?;

            // Check that the types of the two branches are definitionally equal.
            if !definitionally_equal(
                then_branch_type.clone(),
                else_branch_type.clone(),
                definitions_context,
            ) {
                return Err(throw(
                    &format!(
                        "The two branches of this conditional don\u{2019}t match. The first branch \
                            has type {}, but the second branch has type {}.",
                        then_branch_type.to_string().code_str(),
                        else_branch_type.to_string().code_str(),
                    ),
                    source_path,
                    term.source_range
                        .map(|source_range| (source_contents, source_range)),
                ));
            };

            // Return the type of the branches.
            then_branch_type
        }
        True | False => Rc::new(BOOLEAN_TERM),
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
                &mut definitions_context,
            ),
            "has type `b`, but it should have type `a`",
        );
    }

    #[test]
    fn type_check_dependent_apply() {
        let parsing_context = ["foo", "y"];
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
                    variant: Variable("foo", 0),
                }),
                0,
            )),
        ];
        let mut definitions_context = vec![None, None];
        let term_source = "
          ((a : type) => (P: (x : a) -> type) => (f : (x : a) -> P x) => (x : a) => f x) (
            ((t : type) => t) foo) (
              (x : foo) => foo) (
                (x : foo) => x) y
        ";
        let type_source = "(((x : foo) => foo) (y))";

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
    fn type_check_integer() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "int";
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
    fn type_check_integer_literal() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "42";
        let type_source = "int";

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
    fn type_check_negation() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "-42";
        let type_source = "int";

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
        let type_source = "int";

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
        let type_source = "int";

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
        let type_source = "int";

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
        let type_source = "int";

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
    fn type_check_less_than() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "1 < 2";
        let type_source = "bool";

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
    fn type_check_less_than_or_equal_to() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "1 <= 2";
        let type_source = "bool";

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
    fn type_check_equal_to() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "1 == 2";
        let type_source = "bool";

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
    fn type_check_greater_than() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "1 > 2";
        let type_source = "bool";

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
    fn type_check_greater_than_or_equal_to() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "1 >= 2";
        let type_source = "bool";

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
    fn type_check_boolean() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "bool";
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
    fn type_check_true() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "true";
        let type_source = "bool";

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
    fn type_check_false() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "false";
        let type_source = "bool";

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
    fn type_check_if() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "if true then 3 else 4";
        let type_source = "int";

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
