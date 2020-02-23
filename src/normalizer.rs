use crate::{
    de_bruijn::{open, shift},
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::{convert::TryFrom, rc::Rc};

// This function reduces a term to weak head normal form using normal order reduction.
// Invariant: When this function is finished, the context is left unmodified.
pub fn normalize_weak_head<'a>(
    term: Rc<Term<'a>>,
    normalization_context: &mut Vec<Option<Rc<Term<'a>>>>,
) -> Rc<Term<'a>> {
    match &term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) => {
            // These cases are already in beta normal form.
            term
        }
        Variable(_, index) => {
            // Look up the definition in the context. Here we rely on the invariant that `index` is
            // non-negative (otherwise the program will panic).
            match &normalization_context
                [normalization_context.len() - usize::try_from(*index).unwrap()]
            {
                Some(definition) => {
                    // Shift the definition so it's valid in the current context and then normalize
                    // it.
                    normalize_weak_head(shift(definition.clone(), 1, *index), normalization_context)
                }
                None => {
                    // The variable doesn't have a definition. Just return it as a "neutral term".
                    term
                }
            }
        }
        Application(applicand, argument) => {
            // Reduce the applicand.
            let normalized_applicand =
                normalize_weak_head(applicand.clone(), normalization_context);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, body) = &normalized_applicand.variant {
                // Perform beta reduction and normalize the result.
                normalize_weak_head(
                    open(body.clone(), 1, argument.clone()),
                    normalization_context,
                )
            } else {
                // We didn't get a lambda. We're done here.
                Rc::new(Term {
                    source_range: term.source_range,
                    variant: Application(normalized_applicand, argument.clone()),
                })
            }
        }
        Let(_, definition, body) => {
            // Open the body and normalize the result.
            normalize_weak_head(
                open(body.clone(), 1, definition.clone()),
                normalization_context,
            )
        }
    }
}

// This function reduces a term to beta normal form using applicative order reduction.
// Invariant: When this function is finished, the context is left unmodified.
pub fn normalize_beta<'a>(
    term: Rc<Term<'a>>,
    normalization_context: &mut Vec<Option<Rc<Term<'a>>>>,
) -> Rc<Term<'a>> {
    match &term.variant {
        Type => {
            // The type of all types is already in beta normal form.
            term
        }
        Variable(_, index) => {
            // Look up the definition in the context. Here we rely on the invariant that `index` is
            // positive (otherwise the program will panic).
            match &normalization_context
                [normalization_context.len() - usize::try_from(*index).unwrap()]
            {
                Some(definition) => {
                    // Shift the definition so it's valid in the current context and then normalize
                    // it.
                    normalize_beta(shift(definition.clone(), 1, *index), normalization_context)
                }
                None => {
                    // The variable doesn't have a definition. Just return it as a "neutral term".
                    term
                }
            }
        }
        Lambda(variable, domain, body) => {
            // Reduce the domain.
            let normalized_domain = normalize_beta(domain.clone(), normalization_context);

            // Temporarily add the variable to the context for the purpose of normalizing the body.
            normalization_context.push(None);

            // Normalize the body.
            let normalized_body = normalize_beta(body.clone(), normalization_context);

            // Restore the context.
            normalization_context.pop();

            // Construct and return the normalized lambda.
            Rc::new(Term {
                source_range: term.source_range,
                variant: Lambda(variable, normalized_domain, normalized_body),
            })
        }
        Pi(variable, domain, codomain) => {
            // Reduce the domain.
            let normalized_domain = normalize_beta(domain.clone(), normalization_context);

            // Temporarily add the variable the context for the purpose of normalizing the
            // codomain.
            normalization_context.push(None);

            // Normalize the body.
            let normalized_codomain = normalize_beta(codomain.clone(), normalization_context);

            // Restore the context.
            normalization_context.pop();

            // Construct and return the normalized pi type.
            Rc::new(Term {
                source_range: term.source_range,
                variant: Pi(variable, normalized_domain, normalized_codomain),
            })
        }
        Application(applicand, argument) => {
            // Reduce the applicand.
            let normalized_applicand = normalize_beta(applicand.clone(), normalization_context);

            // Reduce the argument.
            let normalized_argument = normalize_beta(argument.clone(), normalization_context);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, body) = &normalized_applicand.variant {
                // We got a lambda. Perform beta reduction.
                normalize_beta(
                    open(body.clone(), 1, normalized_argument),
                    normalization_context,
                )
            } else {
                // We didn't get a lambda. We're done here.
                Rc::new(Term {
                    source_range: term.source_range,
                    variant: Application(normalized_applicand, normalized_argument),
                })
            }
        }
        Let(_, definition, body) => {
            // Eagerly normalize the definition, open the body, and normalize the result.
            normalize_beta(
                open(
                    body.clone(),
                    1,
                    normalize_beta(definition.clone(), normalization_context),
                ),
                normalization_context,
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        normalizer::{normalize_beta, normalize_weak_head},
        parser::parse,
        term::{
            Term,
            Variant::{Application, Lambda, Pi, Type, Variable},
        },
        token::TYPE_KEYWORD,
        tokenizer::tokenize,
    };
    use std::rc::Rc;

    #[test]
    fn normalize_weak_head_type() {
        let parsing_context = [];
        let mut normalization_context = vec![];
        let source = TYPE_KEYWORD;

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, TYPE_KEYWORD.len())),
                variant: Type,
            },
        );
    }

    #[test]
    fn normalize_weak_head_variable_no_definition() {
        let parsing_context = ["x"];
        let mut normalization_context = vec![None];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, 1)),
                variant: Variable("x", 1),
            },
        );
    }

    #[test]
    fn normalize_weak_head_variable_definition() {
        let parsing_context = ["x"];
        let mut normalization_context = vec![Some(Rc::new(Term {
            source_range: None,
            variant: Type,
        }))];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
            Term {
                source_range: None,
                variant: Type,
            },
        );
    }

    #[test]
    fn normalize_weak_head_redex_under_lambda() {
        let parsing_context = ["p", "q"];
        let mut normalization_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) => ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
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
                                        variant: Variable("y", 1),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((23, 24)),
                                variant: Variable("p", 2),
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
                                        variant: Variable("z", 1),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((47, 48)),
                                variant: Variable("q", 2),
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
        let mut normalization_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) -> ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
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
                                        variant: Variable("y", 1),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((23, 24)),
                                variant: Variable("p", 2),
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
                                        variant: Variable("z", 1),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((47, 48)),
                                variant: Variable("q", 2),
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
        let mut normalization_context = vec![None, None];
        let source = "(((x : type) => x) y) (((z : type) => z) w)";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, 43)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((19, 20)),
                        variant: Variable("y", 2),
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
                                        variant: Variable("z", 1),
                                    }),
                                ),
                            }),
                            Rc::new(Term {
                                source_range: Some((41, 42)),
                                variant: Variable("w", 1),
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
        let mut normalization_context = vec![None];
        let source = "((x : type) => x) y";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((18, 19)),
                variant: Variable("y", 1),
            },
        );
    }

    #[test]
    fn normalize_weak_head_let() {
        let parsing_context = ["y"];
        let mut normalization_context = vec![None];
        let source = "x = type; x y";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_weak_head(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((10, 13)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((4, 8)),
                        variant: Type,
                    }),
                    Rc::new(Term {
                        source_range: Some((12, 13)),
                        variant: Variable("y", 1),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_beta_type() {
        let parsing_context = [];
        let mut normalization_context = vec![];
        let source = TYPE_KEYWORD;

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, TYPE_KEYWORD.len())),
                variant: Type,
            },
        );
    }

    #[test]
    fn normalize_beta_variable_no_definition() {
        let parsing_context = ["x"];
        let mut normalization_context = vec![None];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, 1)),
                variant: Variable("x", 1),
            },
        );
    }

    #[test]
    fn normalize_beta_variable_definition() {
        let parsing_context = ["x"];
        let mut normalization_context = vec![Some(Rc::new(Term {
            source_range: None,
            variant: Type,
        }))];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: None,
                variant: Type
            },
        );
    }

    #[test]
    fn normalize_beta_redex_under_lambda() {
        let parsing_context = ["p", "q"];
        let mut normalization_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) => ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, 48)),
                variant: Lambda(
                    "x",
                    Rc::new(Term {
                        source_range: Some((23, 24)),
                        variant: Variable("p", 2),
                    }),
                    Rc::new(Term {
                        source_range: Some((47, 48)),
                        variant: Variable("q", 2),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_beta_redex_under_pi() {
        let parsing_context = ["p", "q"];
        let mut normalization_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) -> ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, 48)),
                variant: Pi(
                    "x",
                    Rc::new(Term {
                        source_range: Some((23, 24)),
                        variant: Variable("p", 2),
                    }),
                    Rc::new(Term {
                        source_range: Some((47, 48)),
                        variant: Variable("q", 2),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_beta_non_redex() {
        let parsing_context = ["y", "w"];
        let mut normalization_context = vec![None, None];
        let source = "(((x : type) => x) y) (((z : type) => z) w)";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((0, 43)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((19, 20)),
                        variant: Variable("y", 2),
                    }),
                    Rc::new(Term {
                        source_range: Some((41, 42)),
                        variant: Variable("w", 1),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_beta_redex() {
        let parsing_context = ["y"];
        let mut normalization_context = vec![None];
        let source = "((x : type) => x) y";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((18, 19)),
                variant: Variable("y", 1),
            },
        );
    }

    #[test]
    fn normalize_beta_let() {
        let parsing_context = ["y"];
        let mut normalization_context = vec![None];
        let source = "x = type; x y";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize_beta(Rc::new(term), &mut normalization_context),
            Term {
                source_range: Some((10, 13)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((4, 8)),
                        variant: Type,
                    }),
                    Rc::new(Term {
                        source_range: Some((12, 13)),
                        variant: Variable("y", 1),
                    }),
                ),
            },
        );
    }
}
