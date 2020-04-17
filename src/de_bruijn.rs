use crate::term::{
    Term,
    Variant::{
        Application, Boolean, Difference, EqualTo, False, GreaterThan, GreaterThanOrEqualTo, If,
        Integer, IntegerLiteral, Lambda, LessThan, LessThanOrEqualTo, Let, Negation, Pi, Product,
        Quotient, Sum, True, Type, Unifier, Variable,
    },
};
use std::{cmp::Ordering, convert::TryFrom, rc::Rc};

// Shifting refers to adjusting the De Bruijn indices of free variables. A cutoff determines which
// variables are considered free. This function is used to raise or lower a term into a different
// scope while preserving its meaning. This is necessarily a partial operation. If the term has any
// free variables in the range `cutoff <= variable < cutoff - amount`, this operation will fail.
#[allow(clippy::too_many_lines)]
pub fn signed_shift<'a>(term: &Term<'a>, cutoff: usize, amount: isize) -> Option<Term<'a>> {
    match &term.variant {
        Unifier(subterm, subterm_shift) => {
            // We `clone` the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            let borrow = { subterm.borrow().clone() };

            if let Some(subterm) = borrow {
                signed_shift(&unsigned_shift(&subterm, 0, *subterm_shift), cutoff, amount)
            } else if *subterm_shift >= cutoff {
                // This `unwrap` is "virtually safe", unless the conversion overflows.
                let signed_subterm_shift = isize::try_from(*subterm_shift).unwrap();

                // Adjust the shift if it results in a non-negative quantity.
                if signed_subterm_shift >= -amount {
                    Some(Term {
                        source_range: term.source_range,
                        variant: Unifier(
                            subterm.clone(),
                            // This `unwrap` is safe due to the check above.
                            usize::try_from(signed_subterm_shift + amount).unwrap(),
                        ),
                    })
                } else {
                    None
                }
            } else {
                Some(term.clone())
            }
        }
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => Some(term.clone()),
        Variable(variable, index) => {
            if *index >= cutoff {
                // These two `unwrap`s are "virtually safe", unless the conversions overflow.
                if isize::try_from(*index).unwrap() + amount >= isize::try_from(cutoff).unwrap() {
                    Some(Term {
                        source_range: term.source_range,
                        variant: Variable(
                            variable,
                            usize::try_from(isize::try_from(*index).unwrap() + amount).unwrap(),
                        ),
                    })
                } else {
                    None
                }
            } else {
                Some(term.clone())
            }
        }
        Lambda(variable, implicit, domain, body) => Some(Term {
            source_range: term.source_range,
            variant: Lambda(
                variable,
                *implicit,
                Rc::new(signed_shift(domain, cutoff, amount)?),
                Rc::new(signed_shift(body, cutoff + 1, amount)?),
            ),
        }),
        Pi(variable, implicit, domain, codomain) => Some(Term {
            source_range: term.source_range,
            variant: Pi(
                variable,
                *implicit,
                Rc::new(signed_shift(domain, cutoff, amount)?),
                Rc::new(signed_shift(codomain, cutoff + 1, amount)?),
            ),
        }),
        Application(applicand, argument) => Some(Term {
            source_range: term.source_range,
            variant: Application(
                Rc::new(signed_shift(applicand, cutoff, amount)?),
                Rc::new(signed_shift(argument, cutoff, amount)?),
            ),
        }),
        Let(definitions, body) => {
            let new_cutoff = cutoff + definitions.len();
            let mut new_definitions = vec![];
            for (variable, annotation, definition) in definitions {
                new_definitions.push((
                    *variable,
                    Rc::new(signed_shift(annotation, new_cutoff, amount)?),
                    Rc::new(signed_shift(definition, new_cutoff, amount)?),
                ));
            }

            Some(Term {
                source_range: term.source_range,
                variant: Let(
                    new_definitions,
                    Rc::new(signed_shift(body, new_cutoff, amount)?),
                ),
            })
        }
        Negation(subterm) => Some(Term {
            source_range: term.source_range,
            variant: Negation(Rc::new(signed_shift(subterm, cutoff, amount)?)),
        }),
        Sum(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: Sum(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        Difference(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: Difference(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        Product(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: Product(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        Quotient(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: Quotient(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        LessThan(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: LessThan(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        LessThanOrEqualTo(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: LessThanOrEqualTo(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        EqualTo(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: EqualTo(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        GreaterThan(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: GreaterThan(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        GreaterThanOrEqualTo(term1, term2) => Some(Term {
            source_range: term.source_range,
            variant: GreaterThanOrEqualTo(
                Rc::new(signed_shift(term1, cutoff, amount)?),
                Rc::new(signed_shift(term2, cutoff, amount)?),
            ),
        }),
        If(condition, then_branch, else_branch) => Some(Term {
            source_range: term.source_range,
            variant: If(
                Rc::new(signed_shift(condition, cutoff, amount)?),
                Rc::new(signed_shift(then_branch, cutoff, amount)?),
                Rc::new(signed_shift(else_branch, cutoff, amount)?),
            ),
        }),
    }
}

// This is an unsigned version of the `signed_shift` function above. Unlike that function, this one
// is total.
pub fn unsigned_shift<'a>(term: &Term<'a>, cutoff: usize, amount: usize) -> Term<'a> {
    // The inner `unwrap` is "essentially safe" in that it can only fail in the virtually
    // impossible case of the conversion overflowing. The outer `unwrap` is safe because `amount`
    // is non-negative.
    signed_shift(term, cutoff, isize::try_from(amount).unwrap()).unwrap()
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
        Unifier(subterm, subterm_shift) => {
            // We `clone` the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            let borrow = { subterm.borrow().clone() };

            if let Some(subterm) = borrow {
                open(
                    &unsigned_shift(&subterm, 0, *subterm_shift),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                )
            } else {
                // The `unwrap` is NOT justified!
                signed_shift(term_to_open, 0, -1).unwrap()
            }
        }
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => term_to_open.clone(),
        Variable(variable, index) => match index.cmp(&index_to_replace) {
            Ordering::Greater => Term {
                source_range: term_to_open.source_range,
                variant: Variable(variable, index - 1),
            },
            Ordering::Less => term_to_open.clone(),
            Ordering::Equal => unsigned_shift(term_to_insert, 0, shift_amount),
        },
        Lambda(variable, implicit, domain, body) => Term {
            source_range: term_to_open.source_range,
            variant: Lambda(
                variable,
                *implicit,
                Rc::new(open(domain, index_to_replace, term_to_insert, shift_amount)),
                Rc::new(open(
                    body,
                    index_to_replace + 1,
                    term_to_insert,
                    shift_amount + 1,
                )),
            ),
        },
        Pi(variable, implicit, domain, codomain) => Term {
            source_range: term_to_open.source_range,
            variant: Pi(
                variable,
                *implicit,
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

#[cfg(test)]
mod tests {
    use crate::{
        assert_same,
        de_bruijn::{open, signed_shift, unsigned_shift},
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
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn signed_shift_unifier_none_valid() {
        assert_same!(
            signed_shift(
                &Term {
                    source_range: None,
                    variant: Unifier(Rc::new(RefCell::new(None)), 10),
                },
                0,
                -4,
            ),
            Some(Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None)), 6),
            }),
        );
    }

    #[test]
    fn signed_shift_unifier_none_invalid() {
        assert_same!(
            signed_shift(
                &Term {
                    source_range: None,
                    variant: Unifier(Rc::new(RefCell::new(None)), 0),
                },
                0,
                -42,
            ),
            None,
        );
    }

    #[test]
    fn signed_shift_unifier_some() {
        assert_same!(
            signed_shift(
                &Term {
                    source_range: None,
                    variant: Unifier(
                        Rc::new(RefCell::new(Some(Term {
                            source_range: None,
                            variant: Variable("x", 10),
                        }))),
                        0,
                    ),
                },
                5,
                -3,
            ),
            Some(Term {
                source_range: None,
                variant: Variable("x", 7),
            }),
        );
    }

    #[test]
    fn signed_shift_variable_free() {
        assert_same!(
            signed_shift(
                &Term {
                    source_range: None,
                    variant: Variable("x", 10),
                },
                5,
                -3,
            ),
            Some(Term {
                source_range: None,
                variant: Variable("x", 7),
            }),
        );
    }

    #[test]
    fn signed_shift_variable_bound() {
        assert_same!(
            signed_shift(
                &Term {
                    source_range: None,
                    variant: Variable("x", 4),
                },
                5,
                -3,
            ),
            Some(Term {
                source_range: None,
                variant: Variable("x", 4),
            }),
        );
    }

    #[test]
    fn signed_shift_variable_conflict() {
        assert_same!(
            signed_shift(
                &Term {
                    source_range: None,
                    variant: Variable("x", 10),
                },
                5,
                -7,
            ),
            None,
        );
    }

    #[test]
    fn unsigned_shift_unifier_none() {
        assert_same!(
            unsigned_shift(
                &Term {
                    source_range: None,
                    variant: Unifier(Rc::new(RefCell::new(None)), 0),
                },
                0,
                42,
            ),
            Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None)), 42),
            },
        );
    }

    #[test]
    fn unsigned_shift_unifier_some() {
        assert_same!(
            unsigned_shift(
                &Term {
                    source_range: None,
                    variant: Unifier(
                        Rc::new(RefCell::new(Some(Term {
                            source_range: None,
                            variant: Variable("x", 0),
                        }))),
                        0,
                    ),
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
    fn unsigned_shift_type() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_variable_free() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_variable_bound() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_lambda() {
        assert_same!(
            unsigned_shift(
                &Term {
                    source_range: None,
                    variant: Lambda(
                        "a",
                        false,
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
                    false,
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
    fn unsigned_shift_pi() {
        assert_same!(
            unsigned_shift(
                &Term {
                    source_range: None,
                    variant: Pi(
                        "a",
                        false,
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
                    false,
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
    fn unsigned_shift_application() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_let() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_integer() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_integer_literal() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_negation() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_sum() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_difference() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_product() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_quotient() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_less_than() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_less_than_or_equal_to() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_equal_to() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_greater_than() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_greater_than_or_equal_to() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_boolean() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_true() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_false() {
        assert_same!(
            unsigned_shift(
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
    fn unsigned_shift_if() {
        assert_same!(
            unsigned_shift(
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
                    variant: Unifier(Rc::new(RefCell::new(None)), 10),
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
                variant: Unifier(Rc::new(RefCell::new(None)), 9),
            },
        );
    }

    #[test]
    fn open_unifier_some() {
        assert_same!(
            open(
                &Term {
                    source_range: None,
                    variant: Unifier(
                        Rc::new(RefCell::new(Some(Term {
                            source_range: None,
                            variant: Variable("x", 0),
                        }))),
                        0,
                    ),
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
                        false,
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
                    false,
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
                        false,
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
                    false,
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
}
