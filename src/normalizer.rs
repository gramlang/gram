use {
    crate::{
        de_bruijn::{open, unsigned_shift},
        term::{
            Term,
            Variant::{
                Application, Boolean, Difference, EqualTo, False, GreaterThan,
                GreaterThanOrEqualTo, If, Integer, IntegerLiteral, Lambda, LessThan,
                LessThanOrEqualTo, Let, Negation, Pi, Product, Quotient, Sum, True, Type, Unifier,
                Variable,
            },
        },
    },
    std::rc::Rc,
};

// This function reduces a term to weak head normal form using normal order reduction. Invariant:
// when this function is finished, the context is left unmodified.
#[allow(clippy::too_many_lines)]
pub fn normalize_weak_head<'a>(
    term: &Term<'a>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
) -> Term<'a> {
    match &term.variant {
        Type
        | Lambda(_, _, _, _)
        | Pi(_, _, _, _)
        | Integer
        | IntegerLiteral(_)
        | Boolean
        | True
        | False => {
            // These cases are already in beta normal form.
            term.clone()
        }
        Unifier(subterm, subterm_shift) => {
            // If the unifier points to something, normalize it. Otherwise, we're stuck. We `clone`
            // the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            { subterm.borrow().clone() }.map_or_else(
                || term.clone(),
                |subterm| {
                    normalize_weak_head(
                        &unsigned_shift(&subterm, 0, *subterm_shift),
                        definitions_context,
                    )
                },
            )
        }
        Variable(_, index) => {
            // Look up the definition in the context.
            match &definitions_context[definitions_context.len() - 1 - index] {
                Some((definition, offset)) => {
                    // Shift the definition so it's valid in the current context and then normalize
                    // it.
                    normalize_weak_head(
                        &unsigned_shift(definition, 0, index + 1 - offset),
                        definitions_context,
                    )
                }
                None => {
                    // The variable doesn't have a definition. Just return it as a "neutral term".
                    term.clone()
                }
            }
        }
        Application(applicand, argument) => {
            // Normalize the applicand.
            let normalized_applicand = normalize_weak_head(applicand, definitions_context);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, _, body) = &normalized_applicand.variant {
                // Perform beta reduction and normalize the result.
                normalize_weak_head(&open(body, 0, argument, 0), definitions_context)
            } else {
                // We didn't get a lambda. We're done here.
                Term {
                    source_range: None,
                    variant: Application(Rc::new(normalized_applicand), argument.clone()),
                }
            }
        }
        Let(definitions, body) => {
            // Substitute the definitions into the body.
            let mut definitions = definitions.clone();
            let mut body = (**body).clone();
            for i in 0..definitions.len() {
                // Compute these once rather than multiple times.
                let i_index = definitions.len() - 1 - i;
                let i_index_plus_one = i_index + 1;
                let (variable, annotation, definition) = &definitions[i];

                // Compute this once rather than multiple times.
                let body_for_unfolding = Rc::new(Term {
                    source_range: None,
                    variant: Variable(variable, 0),
                });

                // Unfold the definition.
                let unfolded_definition = open(
                    definition,
                    i_index,
                    &Term {
                        source_range: None,
                        variant: Let(
                            vec![(
                                variable,
                                Rc::new(open(
                                    &unsigned_shift(annotation, 0, 1),
                                    i_index_plus_one,
                                    &body_for_unfolding,
                                    0,
                                )),
                                Rc::new(open(
                                    &unsigned_shift(definition, 0, 1),
                                    i_index_plus_one,
                                    &body_for_unfolding,
                                    0,
                                )),
                            )],
                            body_for_unfolding,
                        ),
                    },
                    0,
                );

                // Substitute the unfolded definition in subsequent annotations and definitions.
                for (_, annotation, definition) in definitions.iter_mut().skip(i) {
                    *annotation = Rc::new(open(annotation, i_index, &unfolded_definition, 0));
                    *definition = Rc::new(open(definition, i_index, &unfolded_definition, 0));
                }

                // Substitute the unfolded definition in the body.
                body = open(&body, i_index, &unfolded_definition, 0);
            }

            // Normalize the body [tag:let_not_in_weak_head_normal_form].
            normalize_weak_head(&body, definitions_context)
        }
        Negation(subterm) => {
            // Normalize the subterm.
            let normalized_subterm = normalize_weak_head(subterm, definitions_context);

            // Check if the subterm reduced to an integer literal.
            if let IntegerLiteral(integer) = &normalized_subterm.variant {
                // Perform negation.
                Term {
                    source_range: None,
                    variant: IntegerLiteral(-integer),
                }
            } else {
                // We didn't get an integer literal. We're done here.
                Term {
                    source_range: None,
                    variant: Negation(Rc::new(normalized_subterm)),
                }
            }
        }
        Sum(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the subterms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform addition.
                Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 + integer2),
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: Sum(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                }
            }
        }
        Difference(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the subterms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform subtraction.
                Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 - integer2),
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: Difference(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                }
            }
        }
        Product(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the subterms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform multiplication.
                Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 * integer2),
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: Product(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                }
            }
        }
        Quotient(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the subterms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Attempt to perform division.
                integer1.checked_div(integer2).map_or_else(
                    || {
                        // Division by zero!
                        Term {
                            source_range: None,
                            variant: Quotient(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                        }
                    },
                    |quotient| {
                        // The division was successful.
                        Term {
                            source_range: None,
                            variant: IntegerLiteral(quotient),
                        }
                    },
                )
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: Quotient(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                }
            }
        }
        LessThan(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Term {
                    source_range: None,
                    variant: if integer1 < integer2 { True } else { False },
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: LessThan(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                }
            }
        }
        LessThanOrEqualTo(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Term {
                    source_range: None,
                    variant: if integer1 <= integer2 { True } else { False },
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: LessThanOrEqualTo(
                        Rc::new(normalized_term1),
                        Rc::new(normalized_term2),
                    ),
                }
            }
        }
        EqualTo(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Term {
                    source_range: None,
                    variant: if integer1 == integer2 { True } else { False },
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: EqualTo(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                }
            }
        }
        GreaterThan(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Term {
                    source_range: None,
                    variant: if integer1 > integer2 { True } else { False },
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: GreaterThan(Rc::new(normalized_term1), Rc::new(normalized_term2)),
                }
            }
        }
        GreaterThanOrEqualTo(term1, term2) => {
            // Normalize the left subterm.
            let normalized_term1 = normalize_weak_head(term1, definitions_context);

            // Normalize the right subterm.
            let normalized_term2 = normalize_weak_head(term2, definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Term {
                    source_range: None,
                    variant: if integer1 >= integer2 { True } else { False },
                }
            } else {
                // We didn't get integer literals. We're done here.
                Term {
                    source_range: None,
                    variant: GreaterThanOrEqualTo(
                        Rc::new(normalized_term1),
                        Rc::new(normalized_term2),
                    ),
                }
            }
        }
        If(condition, then_branch, else_branch) => {
            // Normalize the condition.
            let normalized_condition = normalize_weak_head(condition, definitions_context);

            // Pattern match on the condition.
            match normalized_condition.variant {
                True => normalize_weak_head(then_branch, definitions_context),
                False => normalize_weak_head(else_branch, definitions_context),
                _ => Term {
                    source_range: None,
                    variant: If(
                        Rc::new(normalized_condition),
                        then_branch.clone(),
                        else_branch.clone(),
                    ),
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        crate::{
            assert_same,
            error::SourceRange,
            normalizer::normalize_weak_head,
            parser::parse,
            term::{
                Term,
                Variant::{
                    Application, Boolean, False, Integer, IntegerLiteral, Lambda, Pi, True, Type,
                    Variable,
                },
            },
            tokenizer::tokenize,
        },
        num_bigint::ToBigInt,
        std::rc::Rc,
    };

    #[test]
    fn normalize_weak_head_type() {
        let parsing_context = [];
        let mut definitions_context = vec![];
        let source = "type";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 4 }),
                variant: Type,
            },
        );
    }

    #[test]
    fn normalize_weak_head_variable_no_definition() {
        let parsing_context = ["x"];
        let mut definitions_context = vec![None];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 1 }),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn normalize_weak_head_variable_definition() {
        let parsing_context = ["x"];
        let mut definitions_context = vec![Some((
            Rc::new(Term {
                source_range: None,
                variant: Type,
            }),
            0,
        ))];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: Type,
            },
        );
    }

    #[test]
    fn normalize_weak_head_redex_under_lambda() {
        let parsing_context = ["p", "q"];
        let mut definitions_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) => ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 48 }),
                variant: Lambda(
                    "x",
                    false,
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 5, end: 24 }),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 5, end: 22 }),
                                variant: Lambda(
                                    "y",
                                    false,
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 11, end: 15 }),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 20, end: 21 }),
                                        variant: Variable("y", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 23, end: 24 }),
                                variant: Variable("p", 1),
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 29, end: 48 }),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 29, end: 46 }),
                                variant: Lambda(
                                    "z",
                                    false,
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 35, end: 39 }),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 44, end: 45 }),
                                        variant: Variable("z", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 47, end: 48 }),
                                variant: Variable("q", 1),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_weak_head_redex_under_pi() {
        let parsing_context = ["p", "q"];
        let mut definitions_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) -> ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 48 }),
                variant: Pi(
                    "x",
                    false,
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 5, end: 24 }),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 5, end: 22 }),
                                variant: Lambda(
                                    "y",
                                    false,
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 11, end: 15 }),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 20, end: 21 }),
                                        variant: Variable("y", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 23, end: 24 }),
                                variant: Variable("p", 1),
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 29, end: 48 }),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 29, end: 46 }),
                                variant: Lambda(
                                    "z",
                                    false,
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 35, end: 39 }),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 44, end: 45 }),
                                        variant: Variable("z", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 47, end: 48 }),
                                variant: Variable("q", 1),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_weak_head_non_redex() {
        let parsing_context = ["y", "w"];
        let mut definitions_context = vec![None, None];
        let source = "(((x : type) => x) y) (((z : type) => z) w)";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: Application(
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 19, end: 20 }),
                        variant: Variable("y", 1),
                    }),
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 23, end: 42 }),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 23, end: 40 }),
                                variant: Lambda(
                                    "z",
                                    false,
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 29, end: 33 }),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 38, end: 39 }),
                                        variant: Variable("z", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 41, end: 42 }),
                                variant: Variable("w", 0),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_weak_head_redex() {
        let parsing_context = ["y"];
        let mut definitions_context = vec![None];
        let source = "((x : type) => x) y";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 18, end: 19 }),
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn normalize_weak_head_let() {
        let parsing_context = ["y"];
        let mut definitions_context = vec![None];
        let source = "x = y; ((z : type) => z) x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 4, end: 5 }),
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn normalize_weak_head_integer() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "int";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 3 }),
                variant: Integer,
            },
        );
    }

    #[test]
    fn normalize_weak_head_integer_literal() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "42";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 2 }),
                variant: IntegerLiteral(ToBigInt::to_bigint(&42_i32).unwrap()),
            },
        );
    }

    #[test]
    fn normalize_weak_head_negation() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "-42";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&-42_i32).unwrap()),
            },
        );
    }

    #[test]
    fn normalize_weak_head_sum() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "1 + 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3_i32).unwrap()),
            },
        );
    }

    #[test]
    fn normalize_weak_head_difference() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "3 - 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&1_i32).unwrap()),
            },
        );
    }

    #[test]
    fn normalize_weak_head_product() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "2 * 3";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&6_i32).unwrap()),
            },
        );
    }

    #[test]
    fn normalize_weak_head_quotient() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "7 / 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3_i32).unwrap()),
            },
        );
    }

    #[test]
    fn normalize_weak_head_less_than() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "1 < 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: True,
            },
        );
    }

    #[test]
    fn normalize_weak_head_less_than_or_equal_to() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "1 <= 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: True,
            },
        );
    }

    #[test]
    fn normalize_weak_head_equal_to() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "1 == 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn normalize_weak_head_greater_than() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "1 >= 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn normalize_weak_head_greater_than_or_equal_to() {
        let parsing_context = [""];
        let mut definitions_context = vec![];
        let source = "1 >= 2";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn normalize_weak_head_boolean() {
        let parsing_context = [];
        let mut definitions_context = vec![];
        let source = "bool";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 4 }),
                variant: Boolean,
            },
        );
    }

    #[test]
    fn normalize_weak_head_true() {
        let parsing_context = [];
        let mut definitions_context = vec![];
        let source = "true";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 4 }),
                variant: True,
            },
        );
    }

    #[test]
    fn normalize_weak_head_false() {
        let parsing_context = [];
        let mut definitions_context = vec![];
        let source = "false";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 0, end: 5 }),
                variant: False,
            },
        );
    }

    #[test]
    fn normalize_weak_head_if_true() {
        let parsing_context = [];
        let mut definitions_context = vec![];
        let source = "if true then 3 else 4";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 13, end: 14 }),
                variant: IntegerLiteral(ToBigInt::to_bigint(&3_i32).unwrap()),
            },
        );
    }

    #[test]
    fn normalize_weak_head_if_false() {
        let parsing_context = [];
        let mut definitions_context = vec![];
        let source = "if false then 3 else 4";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_same!(
            normalize_weak_head(&term, &mut definitions_context),
            Term {
                source_range: Some(SourceRange { start: 21, end: 22 }),
                variant: IntegerLiteral(ToBigInt::to_bigint(&4_i32).unwrap()),
            },
        );
    }
}
