use {
    crate::{
        de_bruijn::{open, unsigned_shift},
        error::Error,
        format::CodeStr,
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
    std::{iter::once, rc::Rc},
};

// This function evaluates a term using a call-by-value strategy.
pub fn evaluate<'a>(term: &Term<'a>) -> Result<Term<'a>, Error> {
    // Repeatedly perform small steps for as long as possible.
    let mut term = term.clone();
    while let Some(stepped_term) = step(&term) {
        term = stepped_term;
    }

    // Check if we got a value.
    if is_value(&term) {
        Ok(term)
    } else {
        Err(Error {
            message: format!("Evaluation of {} is stuck!", term.to_string().code_str()),
            reason: None,
        })
    }
}

// This function implements a call-by-value operational semantics by performing a "small step".
// Call this repeatedly to evaluate a term.
#[allow(clippy::cognitive_complexity)]
#[allow(clippy::too_many_lines)]
pub fn step<'a>(term: &Term<'a>) -> Option<Term<'a>> {
    match &term.variant {
        Type
        | Lambda(_, _, _, _)
        | Pi(_, _, _, _)
        | Variable(_, _)
        | Integer
        | IntegerLiteral(_)
        | Boolean
        | True
        | False => None,
        Unifier(subterm, subterm_shift) => {
            // If the unifier points to something, step to it. Otherwise, we're stuck. We `clone`
            // the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            { subterm.borrow().clone() }.map(|subterm| unsigned_shift(&subterm, 0, *subterm_shift))
        }
        Application(applicand, argument) => {
            // Try to step the applicand.
            if let Some(stepped_applicand) = step(applicand) {
                return Some(Term {
                    source_range: None,
                    variant: Application(Rc::new(stepped_applicand), argument.clone()),
                });
            };

            // Ensure the applicand is a value.
            if !is_value(applicand) {
                return None;
            }

            // Try to step the argument.
            if let Some(stepped_argument) = step(argument) {
                return Some(Term {
                    source_range: None,
                    variant: Application(applicand.clone(), Rc::new(stepped_argument)),
                });
            };

            // Ensure the argument is a value.
            if !is_value(argument) {
                return None;
            }

            // Check if the applicand is a lambda.
            if let Lambda(_, _, _, body) = &applicand.variant {
                // We got a lambda. Perform beta reduction and continue evaluating.
                Some(open(body, 0, argument, 0))
            } else {
                // We didn't get a lambda. We're stuck!
                None
            }
        }
        Let(definitions, body) => {
            // If there are definitions, step the first one and substitute it into the subsequent
            // definitions and body. Otherwise, just return the body.
            if let Some((variable, annotation, definition)) = definitions.first() {
                // Compute this once rather than multiple times.
                let index = definitions.len() - 1;
                let index_plus_one = index + 1;

                // Try to step the definition.
                if let Some(stepped_definition) = step(definition) {
                    return Some(Term {
                        source_range: None,
                        variant: Let(
                            once((*variable, annotation.clone(), Rc::new(stepped_definition)))
                                .chain(definitions.iter().skip(1).map(
                                    |(variable, annotation, definition)| {
                                        (*variable, annotation.clone(), definition.clone())
                                    },
                                ))
                                .collect(),
                            body.clone(),
                        ),
                    });
                };

                // Ensure the definition is a value.
                if !is_value(definition) {
                    return None;
                }

                // Compute this once rather than multiple times.
                let body_for_unfolding = Rc::new(Term {
                    source_range: None,
                    variant: Variable(variable, 0),
                });

                // Unfold the definition.
                let unfolded_definition = open(
                    definition,
                    index,
                    &Term {
                        source_range: None,
                        variant: Let(
                            vec![(
                                variable,
                                Rc::new(open(
                                    &unsigned_shift(annotation, 0, 1),
                                    index_plus_one,
                                    &body_for_unfolding,
                                    0,
                                )),
                                Rc::new(open(
                                    &unsigned_shift(definition, 0, 1),
                                    index_plus_one,
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
                let substituted_definitions = definitions
                    .iter()
                    .skip(1)
                    .map(|(variable, annotation, definition)| {
                        (
                            *variable,
                            Rc::new(open(annotation, index, &unfolded_definition, 0)),
                            Rc::new(open(definition, index, &unfolded_definition, 0)),
                        )
                    })
                    .collect();

                // Substitute the unfolded definition in the body.
                let substituted_body = open(body, index, &unfolded_definition, 0);

                // Return a let with the substituted definitions and body.
                Some(Term {
                    source_range: None,
                    variant: Let(substituted_definitions, Rc::new(substituted_body)),
                })
            } else {
                // There are no definitions. Return the body.
                Some((**body).clone())
            }
        }
        Negation(subterm) => {
            // Try to step the subterm.
            if let Some(stepped_subterm) = step(subterm) {
                return Some(Term {
                    source_range: None,
                    variant: Negation(Rc::new(stepped_subterm)),
                });
            };

            // Ensure the subterm is a value.
            if !is_value(subterm) {
                return None;
            }

            // Check if the subterm is an integer literal.
            if let IntegerLiteral(integer) = &subterm.variant {
                // We got an integer literals. Perform negation and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: IntegerLiteral(-integer),
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        Sum(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: Sum(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: Sum(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the subterms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform addition and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 + integer2),
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        Difference(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: Difference(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: Difference(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the subterms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform subtraction and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 - integer2),
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        Product(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: Product(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: Product(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the subterms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform multiplication and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 * integer2),
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        Quotient(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: Quotient(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: Quotient(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the subterms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Attempt to perform division.
                integer1.checked_div(integer2).map(|quotient| Term {
                    source_range: None,
                    variant: IntegerLiteral(quotient),
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        LessThan(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: LessThan(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: LessThan(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the terms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform the comparison and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: if integer1 < integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        LessThanOrEqualTo(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: LessThanOrEqualTo(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: LessThanOrEqualTo(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the terms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform the comparison and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: if integer1 <= integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        EqualTo(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: EqualTo(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: EqualTo(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the terms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform the comparison and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: if integer1 == integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        GreaterThan(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: GreaterThan(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: GreaterThan(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the terms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform the comparison and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: if integer1 > integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        GreaterThanOrEqualTo(term1, term2) => {
            // Try to step the left subterm.
            if let Some(stepped_term1) = step(term1) {
                return Some(Term {
                    source_range: None,
                    variant: GreaterThanOrEqualTo(Rc::new(stepped_term1), term2.clone()),
                });
            };

            // Ensure the left subterm is a value.
            if !is_value(term1) {
                return None;
            }

            // Try to step the right subterm.
            if let Some(stepped_term2) = step(term2) {
                return Some(Term {
                    source_range: None,
                    variant: GreaterThanOrEqualTo(term1.clone(), Rc::new(stepped_term2)),
                });
            };

            // Ensure the right subterm is a value.
            if !is_value(term2) {
                return None;
            }

            // Check if the terms are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&term1.variant, &term2.variant)
            {
                // We got integer literals. Perform the comparison and continue evaluating.
                Some(Term {
                    source_range: None,
                    variant: if integer1 >= integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        If(condition, then_branch, else_branch) => {
            // Try to step the condition.
            if let Some(stepped_condition) = step(condition) {
                return Some(Term {
                    source_range: None,
                    variant: If(
                        Rc::new(stepped_condition),
                        then_branch.clone(),
                        else_branch.clone(),
                    ),
                });
            };

            // Ensure the condition is a value.
            if !is_value(condition) {
                return None;
            }

            // Pattern match on the condition.
            match &condition.variant {
                True => Some((**then_branch).clone()),
                False => Some((**else_branch).clone()),
                _ => None,
            }
        }
    }
}

// This function returns whether a term is a value. Note that a neutral term (e.g., a variable) is
// not considered a value.
pub fn is_value(term: &Term) -> bool {
    match term.variant {
        Type
        | Lambda(_, _, _, _)
        | Pi(_, _, _, _)
        | Integer
        | IntegerLiteral(_)
        | Boolean
        | True
        | False => true,
        Unifier(_, _)
        | Variable(_, _)
        | Application(_, _)
        | Let(_, _)
        | Negation(_)
        | Sum(_, _)
        | Difference(_, _)
        | Product(_, _)
        | Quotient(_, _)
        | LessThan(_, _)
        | LessThanOrEqualTo(_, _)
        | EqualTo(_, _)
        | GreaterThan(_, _)
        | GreaterThanOrEqualTo(_, _)
        | If(_, _, _) => false,
    }
}

#[cfg(test)]
mod tests {
    use {
        crate::{
            assert_same,
            error::SourceRange,
            evaluator::evaluate,
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
    fn evaluate_type() {
        let source = "type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 4 }),
                variant: Type,
            },
        );
    }

    #[test]
    #[should_panic = "Evaluation of `x` is stuck!"]
    fn evaluate_variable() {
        let context = ["x"];
        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &context[..]).unwrap();

        evaluate(&term).unwrap();
    }

    #[test]
    fn evaluate_lambda() {
        let source = "(f : type -> type) => (x : type) => f x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 39 }),
                variant: Lambda(
                    "f",
                    false,
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 5, end: 17 }),
                        variant: Pi(
                            "_",
                            false,
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 5, end: 9 }),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 13, end: 17 }),
                                variant: Type,
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 22, end: 39 }),
                        variant: Lambda(
                            "x",
                            false,
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 27, end: 31 }),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 36, end: 39 }),
                                variant: Application(
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 36, end: 37 }),
                                        variant: Variable("f", 1),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 38, end: 39 }),
                                        variant: Variable("x", 0),
                                    }),
                                ),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn evaluate_pi() {
        let source = "(f : type -> type) -> (x : type) -> f x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 39 }),
                variant: Pi(
                    "f",
                    false,
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 5, end: 17 }),
                        variant: Pi(
                            "_",
                            false,
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 5, end: 9 }),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 13, end: 17 }),
                                variant: Type,
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some(SourceRange { start: 22, end: 39 }),
                        variant: Pi(
                            "x",
                            false,
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 27, end: 31 }),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some(SourceRange { start: 36, end: 39 }),
                                variant: Application(
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 36, end: 37 }),
                                        variant: Variable("f", 1),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some(SourceRange { start: 38, end: 39 }),
                                        variant: Variable("x", 0),
                                    }),
                                ),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn evaluate_redex() {
        let source = "((x : type) => x) type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 18, end: 22 }),
                variant: Type,
            },
        );
    }

    #[test]
    #[should_panic = "Evaluation of `type type` is stuck!"]
    fn evaluate_non_redex() {
        let source = "type type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        evaluate(&term).unwrap();
    }

    #[test]
    fn evaluate_let() {
        let source = "f = (x : type) => g x; g = (x : type) => x; f type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 46, end: 50 }),
                variant: Type,
            },
        );
    }

    #[test]
    fn evaluate_integer() {
        let source = "int";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 3 }),
                variant: Integer,
            },
        );
    }

    #[test]
    fn evaluate_integer_literal() {
        let source = "42";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 2 }),
                variant: IntegerLiteral(ToBigInt::to_bigint(&42_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_negation() {
        let source = "-42";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&-42_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_sum() {
        let source = "1 + 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_difference() {
        let source = "3 - 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&1_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_product() {
        let source = "2 * 3";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&6_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_quotient() {
        let source = "7 / 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_less_than() {
        let source = "1 < 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: True,
            },
        );
    }

    #[test]
    fn evaluate_less_than_or_equal_to() {
        let source = "1 <= 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: True,
            },
        );
    }

    #[test]
    fn evaluate_equal_to() {
        let source = "1 == 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn evaluate_greater_than() {
        let source = "1 > 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn evaluate_greater_than_or_equal_to() {
        let source = "1 >= 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn evaluate_boolean() {
        let source = "bool";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 4 }),
                variant: Boolean,
            },
        );
    }

    #[test]
    fn evaluate_true() {
        let source = "true";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 4 }),
                variant: True,
            },
        );
    }

    #[test]
    fn evaluate_false() {
        let source = "false";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 0, end: 5 }),
                variant: False,
            },
        );
    }

    #[test]
    fn evaluate_if_true() {
        let source = "if true then 3 else 4";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 13, end: 14 }),
                variant: IntegerLiteral(ToBigInt::to_bigint(&3_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_if_false() {
        let source = "if false then 3 else 4";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: Some(SourceRange { start: 21, end: 22 }),
                variant: IntegerLiteral(ToBigInt::to_bigint(&4_i32).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_factorial() {
        let source = "
            factorial : (int -> int) = (x : int) =>
              if x == 0
              then 1
              else x * factorial (x - 1)

            factorial 5
        ";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_same!(
            evaluate(&term).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&120_i32).unwrap()),
            },
        );
    }
}
