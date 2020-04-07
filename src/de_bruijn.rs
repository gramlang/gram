use crate::term::{
    Term,
    Variant::{
        Application, Boolean, Difference, EqualTo, False, GreaterThan, GreaterThanOrEqualTo, If,
        Integer, IntegerLiteral, Lambda, LessThan, LessThanOrEqualTo, Let, Negation, Pi, Product,
        Quotient, Sum, True, Type, Unifier, Variable,
    },
};
use std::{cmp::Ordering, collections::HashSet, rc::Rc};

// Shifting refers to increasing the De Bruijn indices of free variables. A cutoff determines which
// variables are considered free. This operation is used to lower a term into a nested scope while
// preserving its meaning.
#[allow(clippy::too_many_lines)]
pub fn shift<'a>(term: &Term<'a>, cutoff: usize, amount: usize) -> Term<'a> {
    match &term.variant {
        Unifier(subterm) => {
            if let Some(subterm) = &*subterm.borrow() {
                shift(subterm, cutoff, amount)
            } else {
                term.clone()
            }
        }
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => term.clone(),
        Variable(variable, index) => {
            if *index >= cutoff {
                Term {
                    source_range: term.source_range,
                    variant: Variable(variable, index + amount),
                }
            } else {
                term.clone()
            }
        }
        Lambda(variable, domain, body) => Term {
            source_range: term.source_range,
            variant: Lambda(
                variable,
                Rc::new(shift(domain, cutoff, amount)),
                Rc::new(shift(body, cutoff + 1, amount)),
            ),
        },
        Pi(variable, domain, codomain) => Term {
            source_range: term.source_range,
            variant: Pi(
                variable,
                Rc::new(shift(domain, cutoff, amount)),
                Rc::new(shift(codomain, cutoff + 1, amount)),
            ),
        },
        Application(applicand, argument) => Term {
            source_range: term.source_range,
            variant: Application(
                Rc::new(shift(applicand, cutoff, amount)),
                Rc::new(shift(argument, cutoff, amount)),
            ),
        },
        Let(definitions, body) => {
            let new_cutoff = cutoff + definitions.len();

            Term {
                source_range: term.source_range,
                variant: Let(
                    definitions
                        .iter()
                        .map(|(variable, annotation, definition)| {
                            (
                                *variable,
                                Rc::new(shift(annotation, new_cutoff, amount)),
                                Rc::new(shift(definition, new_cutoff, amount)),
                            )
                        })
                        .collect(),
                    Rc::new(shift(body, new_cutoff, amount)),
                ),
            }
        }
        Negation(subterm) => Term {
            source_range: term.source_range,
            variant: Negation(Rc::new(shift(subterm, cutoff, amount))),
        },
        Sum(term1, term2) => Term {
            source_range: term.source_range,
            variant: Sum(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        Difference(term1, term2) => Term {
            source_range: term.source_range,
            variant: Difference(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        Product(term1, term2) => Term {
            source_range: term.source_range,
            variant: Product(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        Quotient(term1, term2) => Term {
            source_range: term.source_range,
            variant: Quotient(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        LessThan(term1, term2) => Term {
            source_range: term.source_range,
            variant: LessThan(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        LessThanOrEqualTo(term1, term2) => Term {
            source_range: term.source_range,
            variant: LessThanOrEqualTo(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        EqualTo(term1, term2) => Term {
            source_range: term.source_range,
            variant: EqualTo(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        GreaterThan(term1, term2) => Term {
            source_range: term.source_range,
            variant: GreaterThan(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        GreaterThanOrEqualTo(term1, term2) => Term {
            source_range: term.source_range,
            variant: GreaterThanOrEqualTo(
                Rc::new(shift(term1, cutoff, amount)),
                Rc::new(shift(term2, cutoff, amount)),
            ),
        },
        If(condition, then_branch, else_branch) => Term {
            source_range: term.source_range,
            variant: If(
                Rc::new(shift(condition, cutoff, amount)),
                Rc::new(shift(then_branch, cutoff, amount)),
                Rc::new(shift(else_branch, cutoff, amount)),
            ),
        },
    }
}

// Opening is the act of replacing a free variable by a term and decrementing the De Bruijn indices
// of the variables corresponding to entries in the context appearing earlier than the one
// corresponding to the variable being substituted. This function can also shift the term to insert
// by a given amount, though the shift will usually be zero in the outermost call. This operation is
// used to perform beta reduction.
#[allow(clippy::too_many_lines)]
pub fn open<'a>(
    term_to_open: &Term<'a>,
    index_to_replace: usize,
    term_to_insert: &Term<'a>,
    shift_amount: usize,
) -> Term<'a> {
    match &term_to_open.variant {
        Unifier(subterm) => {
            if let Some(subterm) = &*subterm.borrow() {
                open(subterm, index_to_replace, term_to_insert, shift_amount)
            } else {
                term_to_open.clone()
            }
        }
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => term_to_open.clone(),
        Variable(variable, index) => match index.cmp(&index_to_replace) {
            Ordering::Greater => Term {
                source_range: term_to_open.source_range,
                variant: Variable(variable, index - 1),
            },
            Ordering::Less => term_to_open.clone(),
            Ordering::Equal => shift(term_to_insert, 0, shift_amount),
        },
        Lambda(variable, domain, body) => Term {
            source_range: term_to_open.source_range,
            variant: Lambda(
                variable,
                Rc::new(open(domain, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(
                    body,
                    index_to_replace + 1,
                    term_to_insert,
                    shift_amount + 1,
                )),
            ),
        },
        Pi(variable, domain, codomain) => Term {
            source_range: term_to_open.source_range,
            variant: Pi(
                variable,
                Rc::new(open(domain, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(
                    codomain,
                    index_to_replace + 1,
                    term_to_insert,
                    shift_amount + 1,
                )),
            ),
        },
        Application(applicand, argument) => Term {
            source_range: term_to_open.source_range,
            variant: Application(
                Rc::new(open(
                    applicand,
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                )),
                Rc::new(open(
                    argument,
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                )),
            ),
        },
        Let(definitions, body) => {
            let new_index_to_replace = index_to_replace + definitions.len();
            let new_shift_amount = shift_amount + definitions.len();

            Term {
                source_range: term_to_open.source_range,
                variant: Let(
                    definitions
                        .iter()
                        .map(|(variable, annotation, definition)| {
                            (
                                *variable,
                                Rc::new(open(
                                    annotation,
                                    new_index_to_replace,
                                    term_to_insert,
                                    new_shift_amount,
                                )),
                                Rc::new(open(
                                    definition,
                                    new_index_to_replace,
                                    term_to_insert,
                                    new_shift_amount,
                                )),
                            )
                        })
                        .collect(),
                    Rc::new(open(
                        body,
                        new_index_to_replace,
                        term_to_insert,
                        new_shift_amount,
                    )),
                ),
            }
        }
        Negation(subterm) => Term {
            source_range: term_to_open.source_range,
            variant: Negation(Rc::new(open(
                subterm,
                index_to_replace,
                term_to_insert,
                shift_amount,
            ))),
        },
        Sum(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: Sum(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        Difference(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: Difference(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        Product(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: Product(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        Quotient(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: Quotient(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        LessThan(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: LessThan(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        LessThanOrEqualTo(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: LessThanOrEqualTo(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        EqualTo(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: EqualTo(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        GreaterThan(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: GreaterThan(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        GreaterThanOrEqualTo(term1, term2) => Term {
            source_range: term_to_open.source_range,
            variant: GreaterThanOrEqualTo(
                Rc::new(open(term1, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(term2, index_to_replace, term_to_insert, shift_amount)),
            ),
        },
        If(condition, then_branch, else_branch) => Term {
            source_range: term_to_open.source_range,
            variant: If(
                Rc::new(open(
                    condition,
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                )),
                Rc::new(open(
                    then_branch,
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                )),
                Rc::new(open(
                    else_branch,
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                )),
            ),
        },
    }
}

// Compute the free variables of a term. A cutoff determines which variables are considered free.
// This function includes free variables in type annotations.
pub fn free_variables<'a>(term: &Term<'a>, cutoff: usize, variables: &mut HashSet<usize>) {
    match &term.variant {
        Unifier(subterm) => {
            if let Some(subterm) = &*subterm.borrow() {
                free_variables(subterm, cutoff, variables);
            }
        }
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => {}
        Variable(_, index) => {
            if *index >= cutoff {
                variables.insert(*index - cutoff);
            }
        }
        Lambda(_, domain, body) => {
            free_variables(domain, cutoff, variables);
            free_variables(body, cutoff + 1, variables);
        }
        Pi(_, domain, codomain) => {
            free_variables(domain, cutoff, variables);
            free_variables(codomain, cutoff + 1, variables);
        }
        Application(applicand, argument) => {
            free_variables(applicand, cutoff, variables);
            free_variables(argument, cutoff, variables);
        }
        Let(definitions, body) => {
            for (_, annotation, definition) in definitions {
                free_variables(annotation, cutoff + definitions.len(), variables);
                free_variables(definition, cutoff + definitions.len(), variables);
            }

            free_variables(body, cutoff + definitions.len(), variables);
        }
        Negation(subterm) => free_variables(subterm, cutoff, variables),
        Sum(term1, term2)
        | Difference(term1, term2)
        | Product(term1, term2)
        | Quotient(term1, term2)
        | LessThan(term1, term2)
        | LessThanOrEqualTo(term1, term2)
        | EqualTo(term1, term2)
        | GreaterThan(term1, term2)
        | GreaterThanOrEqualTo(term1, term2) => {
            free_variables(term1, cutoff, variables);
            free_variables(term2, cutoff, variables);
        }
        If(condition, then_branch, else_branch) => {
            free_variables(condition, cutoff, variables);
            free_variables(then_branch, cutoff, variables);
            free_variables(else_branch, cutoff, variables);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        assert_same,
        de_bruijn::{free_variables, open, shift},
        term::{
            Term,
            Variant::{
                Application, Boolean, Difference, EqualTo, False, GreaterThan,
                GreaterThanOrEqualTo, If, Integer, IntegerLiteral, Lambda, LessThan,
                LessThanOrEqualTo, Let, Negation, Pi, Product, Quotient, Sum, True, Type, Unifier,
                Variable,
            },
        },
    };
    use num_bigint::ToBigInt;
    use std::{cell::RefCell, collections::HashSet, rc::Rc};

    #[test]
    fn shift_unifier_none() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Unifier(Rc::new(RefCell::new(None))),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None))),
            },
        );
    }

    #[test]
    fn shift_unifier_some() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Unifier(Rc::new(RefCell::new(Some(Term {
                        source_range: None,
                        variant: Variable("x", 0),
                    })))),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Variable("x", 42),
            },
        );
    }

    #[test]
    fn shift_type() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Type,
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Type,
            },
        );
    }

    #[test]
    fn shift_variable_free() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Variable("x", 0),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Variable("x", 42),
            },
        );
    }

    #[test]
    fn shift_variable_bound() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Variable("x", 0),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn shift_lambda() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Lambda(
                        "a",
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Lambda(
                    "a",
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 42),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_pi() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Pi(
                        "a",
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Pi(
                    "a",
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 42),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_application() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Application(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: Application(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_let() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Let(
                        vec![
                            (
                                "x",
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Type,
                                }),
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Variable("y", 0),
                                }),
                            ),
                            (
                                "y",
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Type,
                                }),
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Variable("z", 3),
                                }),
                            ),
                        ],
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("w", 4),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: Let(
                    vec![
                        (
                            "x",
                            Rc::new(Term {
                                source_range: None,
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable("y", 0),
                            }),
                        ),
                        (
                            "y",
                            Rc::new(Term {
                                source_range: None,
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable("z", 45),
                            }),
                        ),
                    ],
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("w", 46),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_integer() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Integer,
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Integer,
            },
        );
    }

    #[test]
    fn shift_integer_literal() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
            },
        );
    }

    #[test]
    fn shift_negation() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Negation(Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    })),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Negation(Rc::new(Term {
                    source_range: None,
                    variant: Variable("a", 42),
                })),
            },
        );
    }

    #[test]
    fn shift_sum() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Sum(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: Sum(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_difference() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Difference(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: Difference(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_product() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Product(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: Product(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_quotient() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Quotient(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: Quotient(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_less_than() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: LessThan(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: LessThan(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_less_than_or_equal_to() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: LessThanOrEqualTo(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: LessThanOrEqualTo(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_equal_to() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: EqualTo(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: EqualTo(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_greater_than() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: GreaterThan(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: GreaterThan(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_greater_than_or_equal_to() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: GreaterThanOrEqualTo(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: GreaterThanOrEqualTo(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_boolean() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: Boolean,
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Boolean,
            },
        );
    }

    #[test]
    fn shift_true() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: True,
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: True,
            },
        );
    }

    #[test]
    fn shift_false() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: False,
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn shift_if() {
        assert_same!(
            shift(
                &Term {
                    source_range: None,
                    variant: If(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("c", 2),
                        }),
                    ),
                },
                1,
                42,
            ),
            Term {
                source_range: None,
                variant: If(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 43),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("c", 44),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_unifier_none() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Unifier(Rc::new(RefCell::new(None))),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None))),
            },
        );
    }

    #[test]
    fn open_unifier_some() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Unifier(Rc::new(RefCell::new(Some(Term {
                        source_range: None,
                        variant: Variable("x", 0),
                    })))),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn open_type() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Type,
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Type,
            },
        );
    }

    #[test]
    fn open_variable_match() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Variable("x", 0),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn open_variable_free() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Variable("x", 1),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn open_variable_bound() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Variable("x", 0),
                },
                1,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn open_lambda() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Lambda(
                        "a",
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Lambda(
                    "a",
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 5),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_pi() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Pi(
                        "a",
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Pi(
                    "a",
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 5),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_application() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Application(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Application(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_let() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Let(
                        vec![
                            (
                                "x",
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Type,
                                }),
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Variable("y", 0),
                                }),
                            ),
                            (
                                "y",
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Type,
                                }),
                                Rc::new(Term {
                                    source_range: None,
                                    variant: Variable("z", 2),
                                }),
                            ),
                        ],
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("w", 3),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Let(
                    vec![
                        (
                            "x",
                            Rc::new(Term {
                                source_range: None,
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable("y", 0),
                            }),
                        ),
                        (
                            "y",
                            Rc::new(Term {
                                source_range: None,
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable("x", 6),
                            }),
                        ),
                    ],
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("w", 2),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_integer() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Integer,
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Integer,
            },
        );
    }

    #[test]
    fn open_integer_literal() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
            },
        );
    }

    #[test]
    fn open_negation() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Negation(Rc::new(Term {
                        source_range: None,
                        variant: Variable("a", 0),
                    })),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Negation(Rc::new(Term {
                    source_range: None,
                    variant: Variable("y", 0),
                })),
            },
        );
    }

    #[test]
    fn open_sum() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Sum(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Sum(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_difference() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Difference(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Difference(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_product() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Product(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Product(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_quotient() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Quotient(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Quotient(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_less_than() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: LessThan(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: LessThan(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_less_than_or_equal_to() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: LessThanOrEqualTo(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: LessThanOrEqualTo(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_equal_to() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: EqualTo(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: EqualTo(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_greater_than() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: GreaterThan(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: GreaterThan(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_greater_than_or_equal_to() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: GreaterThanOrEqualTo(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: GreaterThanOrEqualTo(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_boolean() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Boolean,
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: Boolean,
            },
        );
    }

    #[test]
    fn open_true() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: True,
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: True,
            },
        );
    }

    #[test]
    fn open_false() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: False,
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("y", 0),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: False,
            },
        );
    }

    #[test]
    fn open_if() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: If(
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("b", 1),
                        }),
                        Rc::new(Term {
                            source_range: None,
                            variant: Variable("c", 2),
                        }),
                    ),
                },
                0,
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                0,
            ),
            Term {
                source_range: None,
                variant: If(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 0),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("c", 1),
                    }),
                ),
            },
        );
    }

    #[test]
    fn free_variables_unifier_none() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None))),
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_unifier_some() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(Some(Term {
                    source_range: None,
                    variant: Variable("x", 15),
                })))),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
    }

    #[test]
    fn free_variables_type() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Type,
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_variable_free() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Variable("x", 15),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
    }

    #[test]
    fn free_variables_variable_bound() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Variable("x", 5),
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_lambda() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Lambda(
                    "a",
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&4));
        assert!(variables.contains(&5));
    }

    #[test]
    fn free_variables_pi() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Pi(
                    "a",
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&4));
        assert!(variables.contains(&5));
    }

    #[test]
    fn free_variables_application() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Application(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 16),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
        assert!(variables.contains(&6));
    }

    #[test]
    fn free_variables_let() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Let(
                    vec![
                        (
                            "x",
                            Rc::new(Term {
                                source_range: None,
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable("y", 15),
                            }),
                        ),
                        (
                            "y",
                            Rc::new(Term {
                                source_range: None,
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable("z", 16),
                            }),
                        ),
                    ],
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("w", 17),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&3));
        assert!(variables.contains(&4));
        assert!(variables.contains(&5));
    }

    #[test]
    fn free_variables_integer() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Integer,
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_integer_literal() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_negation() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Negation(Rc::new(Term {
                    source_range: None,
                    variant: Variable("b", 15),
                })),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
    }

    #[test]
    fn free_variables_sum() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Sum(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 16),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
        assert!(variables.contains(&6));
    }

    #[test]
    fn free_variables_difference() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Difference(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 16),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
        assert!(variables.contains(&6));
    }

    #[test]
    fn free_variables_product() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Product(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 16),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
        assert!(variables.contains(&6));
    }

    #[test]
    fn free_variables_quotient() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Quotient(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 16),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
        assert!(variables.contains(&6));
    }

    #[test]
    fn free_variables_boolean() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Boolean,
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_true() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: True,
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_false() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: False,
            },
            10,
            &mut variables,
        );

        assert!(variables.is_empty());
    }

    #[test]
    fn free_variables_if() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: If(
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("b", 16),
                    }),
                    Rc::new(Term {
                        source_range: None,
                        variant: Variable("c", 17),
                    }),
                ),
            },
            10,
            &mut variables,
        );

        assert!(variables.contains(&5));
        assert!(variables.contains(&6));
        assert!(variables.contains(&7));
    }
}
