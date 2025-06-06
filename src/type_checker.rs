use {
    crate::{
        de_bruijn::{open, unsigned_shift},
        error::{Error, listing, throw},
        format::CodeStr,
        parser::PLACEHOLDER_VARIABLE,
        term::{
            Term,
            Variant::{
                Application, Boolean, Difference, EqualTo, False, GreaterThan,
                GreaterThanOrEqualTo, If, Integer, IntegerLiteral, Lambda, LessThan,
                LessThanOrEqualTo, Let, Negation, Pi, Product, Quotient, Sum, True, Type, Unifier,
                Variable,
            },
        },
        unifier::unify,
    },
    scopeguard::defer,
    std::{cell::RefCell, path::Path, rc::Rc},
};

// This is the top-level type checking function. It returns the pair `(elaborated_term, type)`.
// Invariants:
// - The two contexts have the same length.
// - When this function is finished, the contexts are left unmodified.
pub fn type_check<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &Term<'a>,
    typing_context: &mut Vec<(Rc<Term<'a>>, usize)>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
) -> Result<(Term<'a>, Term<'a>), Vec<Error>> {
    let mut errors = vec![];

    let (elaborated_term, term_type) = type_check_rec(
        source_path,
        source_contents,
        term,
        typing_context,
        definitions_context,
        &mut errors,
    );

    if errors.is_empty() {
        Ok((elaborated_term, term_type))
    } else {
        Err(errors)
    }
}

// This helper function is the workhorse of the `type_check` function above.
#[allow(clippy::cognitive_complexity)]
#[allow(clippy::too_many_lines)]
pub fn type_check_rec<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &Term<'a>,
    typing_context: &mut Vec<(Rc<Term<'a>>, usize)>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
    errors: &mut Vec<Error>,
) -> (Term<'a>, Term<'a>) {
    // Construct the type of all types once here rather than constructing it many times later.
    let type_term = Term {
        source_range: None,
        variant: Type,
    };

    // Construct the type of integers once here rather than constructing it many times later.
    let integer_term = Term {
        source_range: None,
        variant: Integer,
    };

    // Construct the type of Booleans once here rather than constructing it many times later.
    let boolean_term = Term {
        source_range: None,
        variant: Boolean,
    };

    // The typing rules are syntax-directed, so we pattern-match on the term.
    let (elaborated_term, elaborated_type) = match &term.variant {
        Unifier(_, _) | Type | Integer | Boolean => (term.clone(), type_term.clone()),
        Variable(_, index) => {
            // Shift the type such that it's valid in the current context.
            let (variable_type, offset) = &typing_context[typing_context.len() - 1 - index];
            (
                term.clone(),
                unsigned_shift(variable_type, 0, index + 1 - offset),
            )
        }
        Lambda(variable, implicit, domain, body) => {
            // Infer the type of the domain.
            let (domain, domain_type) = type_check_rec(
                source_path,
                source_contents,
                domain,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the domain is the type of all types.
            if !unify(&domain_type, &type_term, definitions_context) {
                errors.push(throw::<Error>(
                    "This is not a type:",
                    source_path,
                    domain
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Temporarily add the variable's type to the context for the purpose of inferring the
            // codomain.
            typing_context.push((Rc::new(domain.clone()), 0));
            definitions_context.push(None);

            // Infer the codomain.
            let (body, codomain) = type_check_rec(
                source_path,
                source_contents,
                body,
                typing_context,
                definitions_context,
                errors,
            );

            // Restore the context.
            definitions_context.pop();
            typing_context.pop();

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Lambda(variable, *implicit, Rc::new(domain.clone()), Rc::new(body)),
                },
                Term {
                    source_range: term.source_range,
                    variant: Pi(variable, *implicit, Rc::new(domain), Rc::new(codomain)),
                },
            )
        }
        Pi(variable, implicit, domain, codomain) => {
            // Infer the type of the domain.
            let (domain, domain_type) = type_check_rec(
                source_path,
                source_contents,
                domain,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the domain is the type of all types.
            if !unify(&domain_type, &type_term, definitions_context) {
                errors.push(throw::<Error>(
                    "This is not a type:",
                    source_path,
                    domain
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Temporarily add the variable's type to the context for the purpose of inferring the
            // type of the codomain.
            typing_context.push((Rc::new(domain.clone()), 0));
            definitions_context.push(None);

            // Infer the type of the codomain.
            let (codomain, codomain_type) = type_check_rec(
                source_path,
                source_contents,
                codomain,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the codomain is the type of all types.
            if !unify(&codomain_type, &type_term, definitions_context) {
                errors.push(throw::<Error>(
                    "This is not a type:",
                    source_path,
                    codomain
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Restore the context.
            definitions_context.pop();
            typing_context.pop();

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Pi(variable, *implicit, Rc::new(domain), Rc::new(codomain)),
                },
                type_term.clone(),
            )
        }
        Application(applicand, argument) => {
            // Infer the type of the applicand.
            let (applicand, applicand_type) = type_check_rec(
                source_path,
                source_contents,
                applicand,
                typing_context,
                definitions_context,
                errors,
            );

            // Construct a unification term for the domain.
            let domain = Rc::new(Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None)), 0),
            });

            // Construct a unification term for the codomain.
            let codomain = Rc::new(Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None)), 0),
            });

            // Construct a pi type for unification.
            let pi_type = Rc::new(Term {
                source_range: None,
                variant: Pi(
                    PLACEHOLDER_VARIABLE,
                    false,
                    domain.clone(),
                    codomain.clone(),
                ),
            });

            // Make sure the type of the applicand is a pi type.
            if !unify(&pi_type, &applicand_type, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {} when a function was expected:",
                        applicand_type.to_string().code_str(),
                    ),
                    source_path,
                    applicand
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the argument.
            let (argument, argument_type) = type_check_rec(
                source_path,
                source_contents,
                argument,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the argument type equals the domain.
            if !unify(&domain, &argument_type, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but the function was expecting an argument of type {}:",
                        argument_type.to_string().code_str(),
                        domain.to_string().code_str(),
                    ),
                    source_path,
                    argument
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Application(Rc::new(applicand), Rc::new(argument.clone())),
                },
                open(&codomain, 0, &argument, 0),
            )
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
                borrowed_typing_context.push((annotation.clone(), definitions_len_minus_i));
                borrowed_definitions_context
                    .push(Some((definition.clone(), definitions_len_minus_i)));
                *borrowed_variables_added += 1;
            }

            // Infer/check the types of the definitions.
            let definitions: Vec<_> = definitions
                .iter()
                .map(|(variable, annotation, definition)| {
                    // Temporarily borrow from the scope guard.
                    let mut guard = context_cell.borrow_mut();
                    let ((borrowed_typing_context, borrowed_definitions_context), _) =
                        &mut (*guard);

                    // Infer the type of the definition.
                    let (definition, definition_type) = type_check_rec(
                        source_path,
                        source_contents,
                        definition,
                        borrowed_typing_context,
                        borrowed_definitions_context,
                        errors,
                    );

                    // Check the type against the annotation.
                    if !unify(&definition_type, annotation, borrowed_definitions_context) {
                        errors.push(throw::<Error>(
                            &format!(
                                "This has type {}, but it was expected to have type {}:",
                                definition_type.to_string().code_str(),
                                annotation.to_string().code_str(),
                            ),
                            source_path,
                            definition
                                .source_range
                                .map(|source_range| listing(source_contents, source_range))
                                .as_deref(),
                            None,
                        ));
                    }

                    (*variable, annotation.clone(), Rc::new(definition))
                })
                .collect();

            // Temporarily borrow from the scope guard.
            let mut guard = context_cell.borrow_mut();
            let ((borrowed_typing_context, borrowed_definitions_context), _) = &mut (*guard);

            // Infer the type of the body.
            let (body, body_type) = type_check_rec(
                source_path,
                source_contents,
                body,
                borrowed_typing_context,
                borrowed_definitions_context,
                errors,
            );

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Let(definitions.clone(), Rc::new(body.clone())),
                },
                (0..definitions.len()).fold(body_type, |acc, i| {
                    // Compute this once rather than multiple times.
                    let definitions_len_minus_one_minus_i = definitions.len() - 1 - i;

                    // Open the body.
                    open(
                        &acc,
                        0,
                        &Term {
                            source_range: None,
                            variant: Let(
                                definitions
                                    .iter()
                                    .map(|(variable, annotation, definition)| {
                                        (
                                            *variable,
                                            Rc::new(unsigned_shift(
                                                annotation,
                                                0,
                                                definitions_len_minus_one_minus_i,
                                            )),
                                            Rc::new(unsigned_shift(
                                                definition,
                                                0,
                                                definitions_len_minus_one_minus_i,
                                            )),
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
                        },
                        0,
                    )
                }),
            )
        }
        IntegerLiteral(_) => (term.clone(), integer_term),
        Negation(subterm) => {
            // Infer the type of the subterm.
            let (subterm, subterm_type) = type_check_rec(
                source_path,
                source_contents,
                subterm,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the subterm is the type of integers.
            if !unify(&subterm_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        subterm_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    subterm
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Negation(Rc::new(subterm)),
                },
                integer_term,
            )
        }
        Sum(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Sum(Rc::new(term1), Rc::new(term2)),
                },
                integer_term,
            )
        }
        Difference(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Difference(Rc::new(term1), Rc::new(term2)),
                },
                integer_term,
            )
        }
        Product(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Product(Rc::new(term1), Rc::new(term2)),
                },
                integer_term,
            )
        }
        Quotient(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: Quotient(Rc::new(term1), Rc::new(term2)),
                },
                integer_term,
            )
        }
        LessThan(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: LessThan(Rc::new(term1), Rc::new(term2)),
                },
                boolean_term,
            )
        }
        LessThanOrEqualTo(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: LessThanOrEqualTo(Rc::new(term1), Rc::new(term2)),
                },
                boolean_term,
            )
        }
        EqualTo(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: EqualTo(Rc::new(term1), Rc::new(term2)),
                },
                boolean_term,
            )
        }
        GreaterThan(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: GreaterThan(Rc::new(term1), Rc::new(term2)),
                },
                boolean_term,
            )
        }
        GreaterThanOrEqualTo(term1, term2) => {
            // Infer the type of the left subterm.
            let (term1, term1_type) = type_check_rec(
                source_path,
                source_contents,
                term1,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the left subterm is the type of integers.
            if !unify(&term1_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term1_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term1
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the right subterm.
            let (term2, term2_type) = type_check_rec(
                source_path,
                source_contents,
                term2,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the right subterm is the type of integers.
            if !unify(&term2_type, &integer_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        term2_type.to_string().code_str(),
                        integer_term.to_string().code_str(),
                    ),
                    source_path,
                    term2
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: GreaterThanOrEqualTo(Rc::new(term1), Rc::new(term2)),
                },
                boolean_term,
            )
        }
        If(condition, then_branch, else_branch) => {
            // Infer the type of the condition.
            let (condition, condition_type) = type_check_rec(
                source_path,
                source_contents,
                condition,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the type of the condition is the type of Booleans.
            if !unify(&condition_type, &boolean_term, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "This has type {}, but it should have type {}:",
                        condition_type.to_string().code_str(),
                        boolean_term.to_string().code_str(),
                    ),
                    source_path,
                    condition
                        .source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Infer the type of the then branch.
            let (then_branch, then_branch_type) = type_check_rec(
                source_path,
                source_contents,
                then_branch,
                typing_context,
                definitions_context,
                errors,
            );

            // Infer the type of the else branch.
            let (else_branch, else_branch_type) = type_check_rec(
                source_path,
                source_contents,
                else_branch,
                typing_context,
                definitions_context,
                errors,
            );

            // Check that the types of the two branches are definitionally equal.
            if !unify(&then_branch_type, &else_branch_type, definitions_context) {
                errors.push(throw::<Error>(
                    &format!(
                        "The two branches of this conditional don\u{2019}t match. The first branch \
                            has type {}, but the second branch has type {}.",
                        then_branch_type.to_string().code_str(),
                        else_branch_type.to_string().code_str(),
                    ),
                    source_path,
                    term.source_range
                        .map(|source_range| listing(source_contents, source_range))
                        .as_deref(),
                    None,
                ));
            }

            // Return the new term and type.
            (
                Term {
                    source_range: term.source_range,
                    variant: If(
                        Rc::new(condition),
                        Rc::new(then_branch),
                        Rc::new(else_branch),
                    ),
                },
                then_branch_type,
            )
        }
        True | False => (term.clone(), boolean_term),
    };

    (elaborated_term, elaborated_type)
}

#[cfg(test)]
mod tests {
    use {
        crate::{
            assert_fails,
            equality::syntactically_equal,
            parser::parse,
            term::{
                Term,
                Variant::{Type, Variable},
            },
            tokenizer::tokenize,
            type_checker::type_check,
        },
        std::{fmt::Write, rc::Rc},
    };

    #[test]
    fn type_check_type() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];
        let term_source = "type";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
    }

    #[test]
    fn type_check_variable() {
        let parsing_context = ["a", "x"];
        let mut typing_context = vec![
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            ),
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("a", 0),
                }),
                0,
            ),
        ];
        let mut definitions_context = vec![None, None];
        let term_source = "x";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
    }

    #[test]
    fn type_check_lambda() {
        let parsing_context = ["a"];
        let mut typing_context = vec![(
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            0,
        )];
        let mut definitions_context = vec![None];
        let term_source = "(x : a) => x";
        let type_source = "(x : a) -> a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
    }

    #[test]
    fn type_check_pi() {
        let parsing_context = ["a"];
        let mut typing_context = vec![(
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            0,
        )];
        let mut definitions_context = vec![None];
        let term_source = "(x : a) -> a";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
    }

    #[test]
    fn type_check_application() {
        let parsing_context = ["a", "y"];
        let mut typing_context = vec![
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            ),
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("a", 0),
                }),
                0,
            ),
        ];
        let mut definitions_context = vec![None, None];
        let term_source = "((x : a) => x) y";
        let type_source = "a";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
    }

    #[test]
    fn type_check_bad_application() {
        let parsing_context = ["a", "b", "y"];
        let mut typing_context = vec![
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            ),
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            ),
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("b", 0),
                }),
                0,
            ),
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
            "has type `b`, but the function was expecting an argument of type `a`",
        );
    }

    #[test]
    fn type_check_dependent_apply() {
        let parsing_context = ["foo", "y"];
        let mut typing_context = vec![
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Type,
                }),
                0,
            ),
            (
                Rc::new(Term {
                    source_range: None,
                    variant: Variable("foo", 0),
                }),
                0,
            ),
        ];
        let mut definitions_context = vec![None, None];
        let term_source = "
          (
            (a : type) => (P: a -> type) => (f : (x : a) -> P x) => (x : a) => f x
          ) (
            ((t : type) => t) foo
          ) (
            (x : foo) => foo
          ) (
            (x : foo) => x
          ) y
        ";
        let type_source = "foo";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
    }

    #[test]
    fn type_check_let() {
        let parsing_context = ["a"];
        let mut typing_context = vec![(
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            0,
        )];
        let mut definitions_context = vec![None];
        let term_source = "b = a; b";
        let type_source = "type";

        let term_tokens = tokenize(None, term_source).unwrap();
        let term_term = parse(None, term_source, &term_tokens[..], &parsing_context[..]).unwrap();
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
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
        let (_, term_type_term) = type_check(
            None,
            term_source,
            &term_term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let type_tokens = tokenize(None, type_source).unwrap();
        let type_term = parse(None, type_source, &type_tokens[..], &parsing_context[..]).unwrap();

        assert!(syntactically_equal(&term_type_term, &type_term));
    }
}
