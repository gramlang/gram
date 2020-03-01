use crate::term::{
    Term,
    Variant::{Application, Lambda, Let, Pi, Type, Variable},
};
use std::{cmp::Ordering, rc::Rc};

// Shifting refers to increasing the De Bruijn indices of free variables. A cutoff determines which
// variables are considered free. This operation is used to lower a term into a nested scope while
// preserving its meaning.
pub fn shift<'a>(term: Rc<Term<'a>>, cutoff: usize, amount: usize) -> Rc<Term<'a>> {
    match &term.variant {
        Type => term,
        Variable(variable, index) => {
            if *index >= cutoff {
                Rc::new(Term {
                    source_range: term.source_range,
                    variant: Variable(variable, index + amount),
                })
            } else {
                term
            }
        }
        Lambda(variable, domain, body) => Rc::new(Term {
            source_range: term.source_range,
            variant: Lambda(
                variable,
                shift(domain.clone(), cutoff, amount),
                shift(body.clone(), cutoff + 1, amount),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            variant: Pi(
                variable,
                shift(domain.clone(), cutoff, amount),
                shift(codomain.clone(), cutoff + 1, amount),
            ),
        }),
        Application(applicand, argument) => Rc::new(Term {
            source_range: term.source_range,
            variant: Application(
                shift(applicand.clone(), cutoff, amount),
                shift(argument.clone(), cutoff, amount),
            ),
        }),
        Let(definitions, body) => {
            // Compute this once rather than multiple times.
            let new_cutoff = cutoff + definitions.len();

            // Shift definitions, annotations, and the body by the new index.
            Rc::new(Term {
                source_range: term.source_range,
                variant: Let(
                    definitions
                        .iter()
                        .map(|(variable, annotation, definition)| {
                            (
                                *variable,
                                annotation.as_ref().map(|annotation| {
                                    shift(annotation.clone(), new_cutoff, amount)
                                }),
                                shift(definition.clone(), new_cutoff, amount),
                            )
                        })
                        .collect(),
                    shift(body.clone(), new_cutoff, amount),
                ),
            })
        }
    }
}

// Opening is the act of replacing a free variable by a term and decrementing the De Bruijn indices
// of the variables corresponding to entries in the context appearing earlier than the one
// corresponding to the variable being substituted. This operation is used to perform beta
// reduction.
pub fn open<'a>(
    term_to_open: Rc<Term<'a>>,
    index_to_replace: usize,
    term_to_insert: Rc<Term<'a>>,
) -> Rc<Term<'a>> {
    match &term_to_open.variant {
        Type => term_to_open,
        Variable(variable, index) => match index.cmp(&index_to_replace) {
            Ordering::Greater => Rc::new(Term {
                source_range: term_to_open.source_range,
                variant: Variable(variable, index - 1),
            }),
            Ordering::Less => term_to_open,
            Ordering::Equal => shift(term_to_insert, 0, index_to_replace),
        },
        Lambda(variable, domain, body) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Lambda(
                variable,
                open(domain.clone(), index_to_replace, term_to_insert.clone()),
                open(body.clone(), index_to_replace + 1, term_to_insert),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Pi(
                variable,
                open(domain.clone(), index_to_replace, term_to_insert.clone()),
                open(codomain.clone(), index_to_replace + 1, term_to_insert),
            ),
        }),
        Application(applicand, argument) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Application(
                open(applicand.clone(), index_to_replace, term_to_insert.clone()),
                open(argument.clone(), index_to_replace, term_to_insert),
            ),
        }),
        Let(definitions, body) => {
            // Compute this once rather than multiple times.
            let new_index_to_replace = index_to_replace + definitions.len();

            // Open definitions, annotations, and the body at the new index to replace.
            Rc::new(Term {
                source_range: term_to_open.source_range,
                variant: Let(
                    definitions
                        .iter()
                        .map(|(variable, annotation, definition)| {
                            (
                                *variable,
                                annotation.as_ref().map(|annotation| {
                                    open(
                                        annotation.clone(),
                                        new_index_to_replace,
                                        term_to_insert.clone(),
                                    )
                                }),
                                open(
                                    definition.clone(),
                                    new_index_to_replace,
                                    term_to_insert.clone(),
                                ),
                            )
                        })
                        .collect(),
                    open(body.clone(), new_index_to_replace, term_to_insert),
                ),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        de_bruijn::{open, shift},
        term::{
            Term,
            Variant::{Application, Lambda, Let, Pi, Type, Variable},
        },
        token::TYPE_KEYWORD,
    };
    use std::rc::Rc;

    #[test]
    fn shift_type() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, TYPE_KEYWORD.len())),
                    variant: Type,
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((0, TYPE_KEYWORD.len())),
                variant: Type,
            },
        );
    }

    #[test]
    fn shift_variable_free() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, 1)),
                    variant: Variable("x", 0),
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((0, 1)),
                variant: Variable("x", 42),
            },
        );
    }

    #[test]
    fn shift_variable_bound() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, 1)),
                    variant: Variable("x", 0),
                }),
                1,
                42,
            ),
            Term {
                source_range: Some((0, 1)),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn shift_lambda() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Lambda(
                        "a",
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                    ),
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Lambda(
                    "a",
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 42),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_pi() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Pi(
                        "a",
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                    ),
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Pi(
                    "a",
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 42),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_application() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Application(
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                    ),
                }),
                1,
                42,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_let() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, 29)),
                    variant: Let(
                        vec![
                            (
                                "x",
                                Some(Rc::new(Term {
                                    source_range: Some((4, 8)),
                                    variant: Type,
                                })),
                                Rc::new(Term {
                                    source_range: Some((11, 12)),
                                    variant: Variable("y", 0),
                                }),
                            ),
                            (
                                "y",
                                Some(Rc::new(Term {
                                    source_range: Some((18, 22)),
                                    variant: Type,
                                })),
                                Rc::new(Term {
                                    source_range: Some((25, 26)),
                                    variant: Variable("z", 3),
                                }),
                            ),
                        ],
                        Rc::new(Term {
                            source_range: Some((28, 29)),
                            variant: Variable("w", 4),
                        }),
                    ),
                }),
                1,
                42,
            ),
            Term {
                source_range: Some((0, 29)),
                variant: Let(
                    vec![
                        (
                            "x",
                            Some(Rc::new(Term {
                                source_range: Some((4, 8)),
                                variant: Type,
                            })),
                            Rc::new(Term {
                                source_range: Some((11, 12)),
                                variant: Variable("y", 0),
                            }),
                        ),
                        (
                            "y",
                            Some(Rc::new(Term {
                                source_range: Some((18, 22)),
                                variant: Type,
                            })),
                            Rc::new(Term {
                                source_range: Some((25, 26)),
                                variant: Variable("z", 45),
                            }),
                        ),
                    ],
                    Rc::new(Term {
                        source_range: Some((28, 29)),
                        variant: Variable("w", 46),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_type() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, TYPE_KEYWORD.len())),
                    variant: Type,
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
            ),
            Term {
                source_range: Some((0, TYPE_KEYWORD.len())),
                variant: Type,
            },
        );
    }

    #[test]
    fn open_variable_match() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, 1)),
                    variant: Variable("x", 0),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
            ),
            Term {
                source_range: Some((3, 4)),
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn open_variable_free() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, 1)),
                    variant: Variable("x", 1),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
            ),
            Term {
                source_range: Some((0, 1)),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn open_variable_bound() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, 1)),
                    variant: Variable("x", 0),
                }),
                1,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
            ),
            Term {
                source_range: Some((0, 1)),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn open_lambda() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Lambda(
                        "a",
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                    ),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("x", 4),
                }),
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Lambda(
                    "a",
                    Rc::new(Term {
                        source_range: Some((3, 4)),
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: Some((3, 4)),
                        variant: Variable("x", 5),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_pi() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Pi(
                        "a",
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                    ),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("x", 4),
                }),
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Pi(
                    "a",
                    Rc::new(Term {
                        source_range: Some((3, 4)),
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: Some((3, 4)),
                        variant: Variable("x", 5),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_application() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Application(
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                    ),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("x", 4),
                }),
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((3, 4)),
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_let() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, 29)),
                    variant: Let(
                        vec![
                            (
                                "x",
                                Some(Rc::new(Term {
                                    source_range: Some((4, 8)),
                                    variant: Type,
                                })),
                                Rc::new(Term {
                                    source_range: Some((11, 12)),
                                    variant: Variable("y", 0),
                                }),
                            ),
                            (
                                "y",
                                Some(Rc::new(Term {
                                    source_range: Some((18, 22)),
                                    variant: Type,
                                })),
                                Rc::new(Term {
                                    source_range: Some((25, 26)),
                                    variant: Variable("z", 2),
                                }),
                            ),
                        ],
                        Rc::new(Term {
                            source_range: Some((28, 29)),
                            variant: Variable("w", 3),
                        }),
                    ),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("x", 4),
                }),
            ),
            Term {
                source_range: Some((0, 29)),
                variant: Let(
                    vec![
                        (
                            "x",
                            Some(Rc::new(Term {
                                source_range: Some((4, 8)),
                                variant: Type,
                            })),
                            Rc::new(Term {
                                source_range: Some((11, 12)),
                                variant: Variable("y", 0),
                            }),
                        ),
                        (
                            "y",
                            Some(Rc::new(Term {
                                source_range: Some((18, 22)),
                                variant: Type,
                            })),
                            Rc::new(Term {
                                source_range: Some((3, 4)),
                                variant: Variable("x", 6),
                            }),
                        ),
                    ],
                    Rc::new(Term {
                        source_range: Some((28, 29)),
                        variant: Variable("w", 2),
                    }),
                ),
            },
        );
    }
}
