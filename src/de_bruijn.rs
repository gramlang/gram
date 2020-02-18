use crate::term::{
    Term,
    Variant::{Application, Lambda, Let, Pi, Type, Variable},
};
use std::{cmp::Ordering, rc::Rc};

// Shifting refers to increasing the De Bruijn indices of free variables greater than or equal to a
// given index.
pub fn shift<'a>(term: Rc<Term<'a>>, min_index: usize, amount: usize) -> Rc<Term<'a>> {
    match &term.variant {
        Type => term,
        Variable(variable, index) => {
            if *index >= min_index {
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
                shift(domain.clone(), min_index, amount),
                shift(body.clone(), min_index + 1, amount),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            variant: Pi(
                variable,
                shift(domain.clone(), min_index, amount),
                shift(codomain.clone(), min_index + 1, amount),
            ),
        }),
        Application(applicand, argument) => Rc::new(Term {
            source_range: term.source_range,
            variant: Application(
                shift(applicand.clone(), min_index, amount),
                shift(argument.clone(), min_index, amount),
            ),
        }),
        Let(variable, definition, body) => Rc::new(Term {
            source_range: term.source_range,
            variant: Let(
                variable,
                shift(definition.clone(), min_index, amount),
                shift(body.clone(), min_index + 1, amount),
            ),
        }),
    }
}

// Opening is the act of replacing a free variable by a term and decrementing the De Bruijn indices
// of the free variables with higher indices than that of the one being replaced.
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
        Let(variable, definition, body) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Let(
                variable,
                open(definition.clone(), index_to_replace, term_to_insert.clone()),
                open(body.clone(), index_to_replace + 1, term_to_insert),
            ),
        }),
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
                    source_range: Some((97, 112)),
                    variant: Let(
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
                variant: Let(
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
                    source_range: Some((97, 112)),
                    variant: Let(
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
                variant: Let(
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
}
