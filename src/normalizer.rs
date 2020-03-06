use crate::{
    de_bruijn::{open, shift},
    term::{
        Term,
        Variant::{
            Application, Boolean, Difference, EqualTo, False, GreaterThan, GreaterThanOrEqualTo,
            If, Integer, IntegerLiteral, Lambda, LessThan, LessThanOrEqualTo, Let, Pi, Product,
            Quotient, Sum, True, Type, Variable,
        },
    },
};
use std::rc::Rc;

// This function reduces a term to weak head normal form using normal order reduction. Invariant:
// when this function is finished, the context is left unmodified.
#[allow(clippy::too_many_lines)]
pub fn normalize_weak_head<'a>(
    term: Rc<Term<'a>>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
) -> Rc<Term<'a>> {
    match &term.variant {
        Type
        | Lambda(_, _, _)
        | Pi(_, _, _)
        | Integer
        | IntegerLiteral(_)
        | Boolean
        | True
        | False => {
            // These cases are already in beta normal form.
            term
        }
        Variable(_, index) => {
            // Look up the definition in the context.
            match &definitions_context[definitions_context.len() - 1 - *index] {
                Some((definition, offset)) => {
                    // Shift the definition so it's valid in the current context and then normalize
                    // it.
                    normalize_weak_head(
                        shift(definition.clone(), 0, *index + 1 - offset),
                        definitions_context,
                    )
                }
                None => {
                    // The variable doesn't have a definition. Just return it as a "neutral term".
                    term
                }
            }
        }
        Application(applicand, argument) => {
            // Normalize the applicand.
            let normalized_applicand = normalize_weak_head(applicand.clone(), definitions_context);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, body) = &normalized_applicand.variant {
                // Perform beta reduction and normalize the result.
                normalize_weak_head(open(body.clone(), 0, argument.clone()), definitions_context)
            } else {
                // We didn't get a lambda. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Application(normalized_applicand, argument.clone()),
                })
            }
        }
        Let(definitions, body) => {
            // Substitute the definitions into the body.
            let mut substituted_definitions = definitions.clone();
            let mut substituted_body = body.clone();
            for i in 0..definitions.len() {
                // Compute these once rather than multiple times.
                let i_index = definitions.len() - 1 - i;
                let i_index_plus_one = i_index + 1;

                // Unfold each remaining definition and substitute it in.
                for j in i..definitions.len() {
                    // Compute this once rather than multiple times.
                    let j_index = definitions.len() - 1 - j;

                    // Unfold the definition.
                    let unfolded_definition = Rc::new(Term {
                        source_range: None,
                        variant: Let(
                            substituted_definitions[i..]
                                .iter()
                                .map(|(variable, annotation, definition)| {
                                    (
                                        *variable,
                                        annotation.as_ref().map(|annotation| {
                                            shift(
                                                annotation.clone(),
                                                definitions.len(),
                                                i_index_plus_one,
                                            )
                                        }),
                                        shift(
                                            definition.clone(),
                                            definitions.len(),
                                            i_index_plus_one,
                                        ),
                                    )
                                })
                                .collect(),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable(substituted_definitions[j].0, 0),
                            }),
                        ),
                    });

                    // Substitute the unfolded definition.
                    substituted_definitions[i].2 = open(
                        substituted_definitions[i].2.clone(),
                        j_index,
                        unfolded_definition,
                    );
                }

                // Remember this for later.
                let substituted_definition = substituted_definitions[i].2.clone();

                // Substitute the value in subsequent definitions.
                for definition in substituted_definitions.iter_mut().skip(i + 1) {
                    definition.2 = open(
                        definition.2.clone(),
                        i_index,
                        substituted_definition.clone(),
                    );
                }

                // Substitute the value in the body.
                substituted_body = open(substituted_body.clone(), i_index, substituted_definition);
            }

            // Normalize the body [tag:let_not_in_weak_head_normal_form].
            normalize_weak_head(substituted_body, definitions_context)
        }
        Sum(summand1, summand2) => {
            // Normalize the left summand.
            let normalized_summand1 = normalize_weak_head(summand1.clone(), definitions_context);

            // Normalize the right summand.
            let normalized_summand2 = normalize_weak_head(summand2.clone(), definitions_context);

            // Check if the summands reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_summand1.variant, &normalized_summand2.variant)
            {
                // Perform addition.
                Rc::new(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 + integer2),
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Sum(normalized_summand1, normalized_summand2),
                })
            }
        }
        Difference(minuend, subtrahend) => {
            // Normalize the minuend.
            let normalized_minuend = normalize_weak_head(minuend.clone(), definitions_context);

            // Normalize the subtrahend.
            let normalized_subtrahend =
                normalize_weak_head(subtrahend.clone(), definitions_context);

            // Check if the minuend and subtrahend reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_minuend.variant, &normalized_subtrahend.variant)
            {
                // Perform subtraction.
                Rc::new(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 - integer2),
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Difference(normalized_minuend, normalized_subtrahend),
                })
            }
        }
        Product(factor1, factor2) => {
            // Normalize the left factor.
            let normalized_factor1 = normalize_weak_head(factor1.clone(), definitions_context);

            // Normalize the right factor.
            let normalized_factor2 = normalize_weak_head(factor2.clone(), definitions_context);

            // Check if the factors reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_factor1.variant, &normalized_factor2.variant)
            {
                // Perform multiplication.
                Rc::new(Term {
                    source_range: None,
                    variant: IntegerLiteral(integer1 * integer2),
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Product(normalized_factor1, normalized_factor2),
                })
            }
        }
        Quotient(dividend, divisor) => {
            // Normalize the dividend.
            let normalized_dividend = normalize_weak_head(dividend.clone(), definitions_context);

            // Normalize the divisor.
            let normalized_divisor = normalize_weak_head(divisor.clone(), definitions_context);

            // Check if the dividend and divisor reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_dividend.variant, &normalized_divisor.variant)
            {
                // Attempt to perform division.
                if let Some(quotient) = integer1.checked_div(integer2) {
                    // The division was successful.
                    Rc::new(Term {
                        source_range: None,
                        variant: IntegerLiteral(quotient),
                    })
                } else {
                    // Division by zero!
                    Rc::new(Term {
                        source_range: None,
                        variant: Quotient(normalized_dividend, normalized_divisor),
                    })
                }
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Quotient(normalized_dividend, normalized_divisor),
                })
            }
        }
        LessThan(term1, term2) => {
            // Normalize the left term.
            let normalized_term1 = normalize_weak_head(term1.clone(), definitions_context);

            // Normalize the right term.
            let normalized_term2 = normalize_weak_head(term2.clone(), definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Rc::new(Term {
                    source_range: None,
                    variant: if integer1 < integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Product(normalized_term1, normalized_term2),
                })
            }
        }
        LessThanOrEqualTo(term1, term2) => {
            // Normalize the left term.
            let normalized_term1 = normalize_weak_head(term1.clone(), definitions_context);

            // Normalize the right term.
            let normalized_term2 = normalize_weak_head(term2.clone(), definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Rc::new(Term {
                    source_range: None,
                    variant: if integer1 <= integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Product(normalized_term1, normalized_term2),
                })
            }
        }
        EqualTo(term1, term2) => {
            // Normalize the left term.
            let normalized_term1 = normalize_weak_head(term1.clone(), definitions_context);

            // Normalize the right term.
            let normalized_term2 = normalize_weak_head(term2.clone(), definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Rc::new(Term {
                    source_range: None,
                    variant: if integer1 == integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Product(normalized_term1, normalized_term2),
                })
            }
        }
        GreaterThan(term1, term2) => {
            // Normalize the left term.
            let normalized_term1 = normalize_weak_head(term1.clone(), definitions_context);

            // Normalize the right term.
            let normalized_term2 = normalize_weak_head(term2.clone(), definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Rc::new(Term {
                    source_range: None,
                    variant: if integer1 > integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Product(normalized_term1, normalized_term2),
                })
            }
        }
        GreaterThanOrEqualTo(term1, term2) => {
            // Normalize the left term.
            let normalized_term1 = normalize_weak_head(term1.clone(), definitions_context);

            // Normalize the right term.
            let normalized_term2 = normalize_weak_head(term2.clone(), definitions_context);

            // Check if the terms reduced to integer literals.
            if let (IntegerLiteral(integer1), IntegerLiteral(integer2)) =
                (&normalized_term1.variant, &normalized_term2.variant)
            {
                // Perform the comparison.
                Rc::new(Term {
                    source_range: None,
                    variant: if integer1 >= integer2 { True } else { False },
                })
            } else {
                // We didn't get integer literals. We're done here.
                Rc::new(Term {
                    source_range: None,
                    variant: Product(normalized_term1, normalized_term2),
                })
            }
        }
        If(condition, then_branch, else_branch) => {
            // Normalize the condition.
            let normalized_condition = normalize_weak_head(condition.clone(), definitions_context);

            // Pattern match on the condition.
            match normalized_condition.variant {
                True => normalize_weak_head(then_branch.clone(), definitions_context),
                False => normalize_weak_head(else_branch.clone(), definitions_context),
                _ => Rc::new(Term {
                    source_range: None,
                    variant: If(
                        normalized_condition,
                        then_branch.clone(),
                        else_branch.clone(),
                    ),
                }),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
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
    };
    use num_bigint::ToBigInt;
    use std::rc::Rc;

    #[test]
    fn normalize_weak_head_type() {
        let parsing_context = [];
        let mut definitions_context = vec![];
        let source = "type";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 4)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 1)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 48)),
                variant: Lambda(
                    "x",
                    Rc::new(Term {
                        source_range: Some((5, 24)),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some((5, 22)),
                                variant: Lambda(
                                    "y",
                                    Rc::new(Term {
                                        source_range: Some((11, 15)),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((20, 21)),
                                        variant: Variable("y", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((23, 24)),
                                variant: Variable("p", 1),
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some((29, 48)),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some((29, 46)),
                                variant: Lambda(
                                    "z",
                                    Rc::new(Term {
                                        source_range: Some((35, 39)),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((44, 45)),
                                        variant: Variable("z", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((47, 48)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 48)),
                variant: Pi(
                    "x",
                    Rc::new(Term {
                        source_range: Some((5, 24)),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some((5, 22)),
                                variant: Lambda(
                                    "y",
                                    Rc::new(Term {
                                        source_range: Some((11, 15)),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((20, 21)),
                                        variant: Variable("y", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((23, 24)),
                                variant: Variable("p", 1),
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some((29, 48)),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some((29, 46)),
                                variant: Lambda(
                                    "z",
                                    Rc::new(Term {
                                        source_range: Some((35, 39)),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((44, 45)),
                                        variant: Variable("z", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((47, 48)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: None,
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((19, 20)),
                        variant: Variable("y", 1),
                    }),
                    Rc::new(Term {
                        source_range: Some((23, 42)),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some((23, 40)),
                                variant: Lambda(
                                    "z",
                                    Rc::new(Term {
                                        source_range: Some((29, 33)),
                                        variant: Type,
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((38, 39)),
                                        variant: Variable("z", 0),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((41, 42)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((18, 19)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((4, 5)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 3)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 2)),
                variant: IntegerLiteral(ToBigInt::to_bigint(&42).unwrap()),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3).unwrap()),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&6).unwrap()),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&3).unwrap()),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 4)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 4)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((0, 5)),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((13, 14)),
                variant: IntegerLiteral(ToBigInt::to_bigint(&3).unwrap()),
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

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut definitions_context),
            Term {
                source_range: Some((21, 22)),
                variant: IntegerLiteral(ToBigInt::to_bigint(&4).unwrap()),
            },
        );
    }
}
