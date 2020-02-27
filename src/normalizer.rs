use crate::{
    de_bruijn::{open, shift},
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::rc::Rc;

// This function reduces a term to weak head normal form using normal order reduction. Invariant:
// when this function is finished, the context is left unmodified.
pub fn normalize_weak_head<'a>(
    term: Rc<Term<'a>>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
) -> Rc<Term<'a>> {
    match &term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) => {
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
                    source_range: term.source_range,
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

            // Normalize the body.
            normalize_weak_head(substituted_body, definitions_context)
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
            Variant::{Application, Lambda, Pi, Type, Variable},
        },
        tokenizer::tokenize,
    };
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
                source_range: Some((0, 43)),
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
}
