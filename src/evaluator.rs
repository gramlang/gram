use crate::{
    de_bruijn::{open, shift},
    error::Error,
    format::CodeStr,
    term::{
        Term,
        Variant::{
            Application, Difference, Integer, IntegerLiteral, Lambda, Let, Pi, Product, Quotient,
            Sum, Type, Variable,
        },
    },
};
use std::rc::Rc;

// This function evaluates a term using a call-by-value strategy.
pub fn evaluate<'a>(mut term: Rc<Term<'a>>) -> Result<Rc<Term<'a>>, Error> {
    // Repeatedly perform small steps for as long as possible.
    while let Some(stepped_term) = step(&term) {
        term = stepped_term;
    }

    // Check if we got a value.
    if is_value(&*term) {
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
#[allow(clippy::too_many_lines)]
pub fn step<'a>(term: &Rc<Term<'a>>) -> Option<Rc<Term<'a>>> {
    match &term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) | Variable(_, _) | Integer | IntegerLiteral(_) => None,
        Application(applicand, argument) => {
            // Try to step the applicand.
            if let Some(stepped_applicand) = step(applicand) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Application(stepped_applicand, argument.clone()),
                }));
            };

            // Ensure the applicand is a value.
            if !is_value(applicand) {
                return None;
            }

            // Try to step the argument.
            if let Some(stepped_argument) = step(argument) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Application(applicand.clone(), stepped_argument),
                }));
            };

            // Ensure the argument is a value.
            if !is_value(argument) {
                return None;
            }

            // Check if the applicand is a lambda.
            if let Lambda(_, _, body) = &applicand.variant {
                // We got a lambda. Perform beta reduction and continue evaluating.
                Some(open(body.clone(), 0, argument.clone()))
            } else {
                // We didn't get a lambda. We're stuck!
                None
            }
        }
        Let(definitions, body) => {
            // Try to step one of the definitions.
            let mut substituted_definitions = definitions.clone();
            let mut substituted_body = body.clone();
            for i in 0..definitions.len() {
                // Compute this once rather than multiple times.
                let i_index = definitions.len() - 1 - i;

                // Try to step the definition.
                if let Some(stepped_definition) = step(&substituted_definitions[i].2) {
                    return Some(Rc::new(Term {
                        source_range: None,
                        variant: Let(
                            definitions
                                .iter()
                                .enumerate()
                                .map(|(j, (variable, annotation, definition))| {
                                    if i == j {
                                        (*variable, annotation.clone(), stepped_definition.clone())
                                    } else {
                                        (*variable, annotation.clone(), definition.clone())
                                    }
                                })
                                .collect(),
                            body.clone(),
                        ),
                    }));
                };

                // Here is the fully stepped definition. Note that it may have free variables
                // referencing itself or subsequent definitions.
                let definition = substituted_definitions[i].2.clone();

                // Ensure the definition is a value.
                if !is_value(&definition) {
                    return None;
                }

                // Compute this once rather than multiple times.
                let body_for_unfolding = Rc::new(Term {
                    source_range: None,
                    variant: Variable(substituted_definitions[i].0, 0),
                });

                // Unfold the definition.
                let unfolded_definition = open(
                    definition.clone(),
                    i_index,
                    Rc::new(Term {
                        source_range: None,
                        variant: Let(
                            vec![(
                                substituted_definitions[i].0,
                                substituted_definitions[i].1.as_ref().map(|annotation| {
                                    open(
                                        shift(annotation.clone(), 0, 1),
                                        i_index + 1,
                                        body_for_unfolding.clone(),
                                    )
                                }),
                                open(
                                    shift(definition, 0, 1),
                                    i_index + 1,
                                    body_for_unfolding.clone(),
                                ),
                            )],
                            body_for_unfolding,
                        ),
                    }),
                );

                // Substitute the value in subsequent definitions.
                for definition in substituted_definitions.iter_mut().skip(i + 1) {
                    definition.2 = open(definition.2.clone(), i_index, unfolded_definition.clone());
                }

                // Substitute the value in the body.
                substituted_body = open(substituted_body.clone(), i_index, unfolded_definition);
            }

            // Try to step the body.
            if let Some(stepped_body) = step(&substituted_body) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Let(substituted_definitions, stepped_body.clone()),
                }));
            };

            // Ensure the body is a value.
            if !is_value(&substituted_body) {
                return None;
            }

            // Return the substituted body.
            Some(substituted_body)
        }
        Sum(summand1, summand2) => {
            // Try to step the left summand.
            if let Some(stepped_summand1) = step(summand1) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Sum(stepped_summand1, summand2.clone()),
                }));
            };

            // Ensure the left summand is a value.
            if !is_value(summand1) {
                return None;
            }

            // Try to step the right summand.
            if let Some(stepped_summand2) = step(summand2) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Sum(summand1.clone(), stepped_summand2),
                }));
            };

            // Ensure the right summand is a value.
            if !is_value(summand2) {
                return None;
            }

            // Check if the summands are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&summand1.variant, &summand2.variant)
            {
                // We got integer literals. Perform addition and continue evaluating.
                Some(Rc::new(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 + integer2),
                }))
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        Difference(minuend, subtrahend) => {
            // Try to step the minuend.
            if let Some(stepped_minuend) = step(minuend) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Difference(stepped_minuend, subtrahend.clone()),
                }));
            };

            // Ensure the minuend is a value.
            if !is_value(minuend) {
                return None;
            }

            // Try to step the subtrahend.
            if let Some(stepped_subtrahend) = step(subtrahend) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Difference(minuend.clone(), stepped_subtrahend),
                }));
            };

            // Ensure the subtrahend is a value.
            if !is_value(subtrahend) {
                return None;
            }

            // Check if the minuend and subtrahend are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&minuend.variant, &subtrahend.variant)
            {
                // We got integer literals. Perform subtraction and continue evaluating.
                Some(Rc::new(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 - integer2),
                }))
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        Product(factor1, factor2) => {
            // Try to step the left factor.
            if let Some(stepped_factor1) = step(factor1) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Product(stepped_factor1, factor2.clone()),
                }));
            };

            // Ensure the left factor is a value.
            if !is_value(factor1) {
                return None;
            }

            // Try to step the right factor.
            if let Some(stepped_factor2) = step(factor2) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Product(factor1.clone(), stepped_factor2),
                }));
            };

            // Ensure the right factor is a value.
            if !is_value(factor2) {
                return None;
            }

            // Check if the factors are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&factor1.variant, &factor2.variant)
            {
                // We got integer literals. Perform multiplication and continue evaluating.
                Some(Rc::new(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 * integer2),
                }))
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
        Quotient(dividend, divisor) => {
            // Try to step the dividend.
            if let Some(stepped_dividend) = step(dividend) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Quotient(stepped_dividend, divisor.clone()),
                }));
            };

            // Ensure the dividend is a value.
            if !is_value(dividend) {
                return None;
            }

            // Try to step the divisor.
            if let Some(stepped_divisor) = step(divisor) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Quotient(dividend.clone(), stepped_divisor),
                }));
            };

            // Ensure the divisor is a value.
            if !is_value(divisor) {
                return None;
            }

            // Check if the dividend and divisor are integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&dividend.variant, &divisor.variant)
            {
                // We got integer literals. Attempt to perform division.
                if let Some(quotient) = integer1.checked_div(integer2) {
                    // The division was successful.
                    Some(Rc::new(Term {
                        source_range: None,
                        variant: IntegerLiteral(quotient),
                    }))
                } else {
                    // Division by zero!
                    None
                }
            } else {
                // We didn't get integer literals. We're stuck!
                None
            }
        }
    }
}

// This function returns whether a term is a value. Note that a neutral term (e.g., a variable) is
// not considered a value.
pub fn is_value<'a>(term: &Term<'a>) -> bool {
    match term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) | Integer | IntegerLiteral(_) => true,
        Variable(_, _)
        | Application(_, _)
        | Let(_, _)
        | Sum(_, _)
        | Difference(_, _)
        | Product(_, _)
        | Quotient(_, _) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::evaluate,
        parser::parse,
        term::{
            Term,
            Variant::{Application, Integer, IntegerLiteral, Lambda, Pi, Type, Variable},
        },
        tokenizer::tokenize,
    };
    use num_bigint::ToBigInt;
    use std::rc::Rc;

    #[test]
    fn evaluate_type() {
        let source = "type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: Some((0, 4)),
                variant: Type,
            },
        );
    }

    #[test]
    #[should_panic]
    fn evaluate_variable() {
        let context = ["x"];
        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &context[..]).unwrap();

        evaluate(Rc::new(term)).unwrap();
    }

    #[test]
    fn evaluate_lambda() {
        let source = "(f : type -> type) => (x : type) => f x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: Some((0, 39)),
                variant: Lambda(
                    "f",
                    Rc::new(Term {
                        source_range: Some((5, 17)),
                        variant: Pi(
                            "_",
                            Rc::new(Term {
                                source_range: Some((5, 9)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((13, 17)),
                                variant: Type,
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some((22, 39)),
                        variant: Lambda(
                            "x",
                            Rc::new(Term {
                                source_range: Some((27, 31)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((36, 39)),
                                variant: Application(
                                    Rc::new(Term {
                                        source_range: Some((36, 37)),
                                        variant: Variable("f", 1),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((38, 39)),
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

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: Some((0, 39)),
                variant: Pi(
                    "f",
                    Rc::new(Term {
                        source_range: Some((5, 17)),
                        variant: Pi(
                            "_",
                            Rc::new(Term {
                                source_range: Some((5, 9)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((13, 17)),
                                variant: Type,
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some((22, 39)),
                        variant: Pi(
                            "x",
                            Rc::new(Term {
                                source_range: Some((27, 31)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((36, 39)),
                                variant: Application(
                                    Rc::new(Term {
                                        source_range: Some((36, 37)),
                                        variant: Variable("f", 1),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((38, 39)),
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

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: Some((18, 22)),
                variant: Type,
            },
        );
    }

    #[test]
    #[should_panic]
    fn evaluate_non_redex() {
        let source = "type type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        evaluate(Rc::new(term)).unwrap();
    }

    #[test]
    fn evaluate_let() {
        let source = "f = (x : type) => g x; g = (x : type) => x; f type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: Some((46, 50)),
                variant: Type,
            },
        );
    }

    #[test]
    fn evaluate_integer() {
        let source = "integer";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: Some((0, 7)),
                variant: Integer,
            },
        );
    }

    #[test]
    fn evaluate_integer_literal() {
        let source = "42";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: Some((0, 2)),
                variant: IntegerLiteral(ToBigInt::to_bigint(&42).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_sum() {
        let source = "1 + 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_difference() {
        let source = "3 - 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_product() {
        let source = "2 * 3";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&6).unwrap()),
            },
        );
    }

    #[test]
    fn evaluate_quotient() {
        let source = "7 / 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)).unwrap(),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3).unwrap()),
            },
        );
    }
}
