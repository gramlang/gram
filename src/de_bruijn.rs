use crate::term::{
    Term,
    Variant::{
        Application, Boolean, Difference, EqualTo, False, GreaterThan, GreaterThanOrEqualTo, If,
        Integer, IntegerLiteral, Lambda, LessThan, LessThanOrEqualTo, Let, Negation, Pi, Product,
        Quotient, Sum, True, Type, Variable,
    },
};
use std::{cmp::Ordering, collections::HashSet, rc::Rc};

// Shifting refers to increasing the De Bruijn indices of free variables. A cutoff determines which
// variables are considered free. This operation is used to lower a term into a nested scope while
// preserving its meaning.
#[allow(clippy::too_many_lines)]
pub fn shift<'a>(term: Rc<Term<'a>>, cutoff: usize, amount: usize) -> Rc<Term<'a>> {
    match &term.variant {
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => term,
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
                domain
                    .as_ref()
                    .map(|domain| shift(domain.clone(), cutoff, amount)),
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
        Negation(subterm) => Rc::new(Term {
            source_range: term.source_range,
            variant: Negation(shift(subterm.clone(), cutoff, amount)),
        }),
        Sum(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: Sum(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        Difference(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: Difference(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        Product(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: Product(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        Quotient(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: Quotient(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        LessThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: LessThan(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        LessThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: LessThanOrEqualTo(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        EqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: EqualTo(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        GreaterThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: GreaterThan(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        GreaterThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            variant: GreaterThanOrEqualTo(
                shift(term1.clone(), cutoff, amount),
                shift(term2.clone(), cutoff, amount),
            ),
        }),
        If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term.source_range,
            variant: If(
                shift(condition.clone(), cutoff, amount),
                shift(then_branch.clone(), cutoff, amount),
                shift(else_branch.clone(), cutoff, amount),
            ),
        }),
    }
}

// Opening is the act of replacing a free variable by a term and decrementing the De Bruijn indices
// of the variables corresponding to entries in the context appearing earlier than the one
// corresponding to the variable being substituted. This function can also shift the term to insert
// by a given amount, though the shift will usually be zero in the outermost call. This operation is
// used to perform beta reduction.
#[allow(clippy::too_many_lines)]
pub fn open<'a>(
    term_to_open: Rc<Term<'a>>,
    index_to_replace: usize,
    term_to_insert: Rc<Term<'a>>,
    shift_amount: usize,
) -> Rc<Term<'a>> {
    match &term_to_open.variant {
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => term_to_open,
        Variable(variable, index) => match index.cmp(&index_to_replace) {
            Ordering::Greater => Rc::new(Term {
                source_range: term_to_open.source_range,
                variant: Variable(variable, index - 1),
            }),
            Ordering::Less => term_to_open,
            Ordering::Equal => shift(term_to_insert, 0, shift_amount),
        },
        Lambda(variable, domain, body) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Lambda(
                variable,
                domain.as_ref().map(|domain| {
                    open(
                        domain.clone(),
                        index_to_replace,
                        term_to_insert.clone(),
                        shift_amount,
                    )
                }),
                open(
                    body.clone(),
                    index_to_replace + 1,
                    term_to_insert,
                    shift_amount + 1,
                ),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Pi(
                variable,
                open(
                    domain.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    codomain.clone(),
                    index_to_replace + 1,
                    term_to_insert,
                    shift_amount + 1,
                ),
            ),
        }),
        Application(applicand, argument) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Application(
                open(
                    applicand.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    argument.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        Let(definitions, body) => {
            // Compute these once rather than multiple times.
            let new_index_to_replace = index_to_replace + definitions.len();
            let new_shift_amount = shift_amount + definitions.len();

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
                                        new_shift_amount,
                                    )
                                }),
                                open(
                                    definition.clone(),
                                    new_index_to_replace,
                                    term_to_insert.clone(),
                                    new_shift_amount,
                                ),
                            )
                        })
                        .collect(),
                    open(
                        body.clone(),
                        new_index_to_replace,
                        term_to_insert,
                        new_shift_amount,
                    ),
                ),
            })
        }
        Negation(subterm) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Negation(open(
                subterm.clone(),
                index_to_replace,
                term_to_insert.clone(),
                shift_amount,
            )),
        }),
        Sum(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Sum(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        Difference(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Difference(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        Product(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Product(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        Quotient(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: Quotient(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        LessThan(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: LessThan(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        LessThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: LessThanOrEqualTo(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        EqualTo(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: EqualTo(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        GreaterThan(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: GreaterThan(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        GreaterThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: GreaterThanOrEqualTo(
                open(
                    term1.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    term2.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
        If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term_to_open.source_range,
            variant: If(
                open(
                    condition.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    then_branch.clone(),
                    index_to_replace,
                    term_to_insert.clone(),
                    shift_amount,
                ),
                open(
                    else_branch.clone(),
                    index_to_replace,
                    term_to_insert,
                    shift_amount,
                ),
            ),
        }),
    }
}

// Compute the free variables of a term. A cutoff determines which variables are considered free.
// This function includes free variables in type annotations.
pub fn free_variables<'a>(term: &Term<'a>, cutoff: usize, variables: &mut HashSet<usize>) {
    match &term.variant {
        Type | Integer | IntegerLiteral(_) | Boolean | True | False => {}
        Variable(_, index) => {
            if *index >= cutoff {
                variables.insert(*index - cutoff);
            }
        }
        Lambda(_, domain, body) => {
            if let Some(domain) = domain {
                free_variables(domain, cutoff, variables);
            }

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
                if let Some(annotation) = annotation {
                    free_variables(annotation, cutoff + definitions.len(), variables);
                }

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
        de_bruijn::{free_variables, open, shift},
        term::{
            Term,
            Variant::{
                Application, Boolean, Difference, EqualTo, False, GreaterThan,
                GreaterThanOrEqualTo, If, Integer, IntegerLiteral, Lambda, LessThan,
                LessThanOrEqualTo, Let, Negation, Pi, Product, Quotient, Sum, True, Type, Variable,
            },
        },
        token::{BOOLEAN_KEYWORD, FALSE_KEYWORD, INTEGER_KEYWORD, TRUE_KEYWORD, TYPE_KEYWORD},
    };
    use num_bigint::ToBigInt;
    use std::{collections::HashSet, rc::Rc};

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
                        Some(Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("b", 0),
                        })),
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
                    Some(Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 42),
                    })),
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
    fn shift_integer() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, INTEGER_KEYWORD.len())),
                    variant: Integer,
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((0, INTEGER_KEYWORD.len())),
                variant: Integer,
            },
        );
    }

    #[test]
    fn shift_integer_literal() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, 2)),
                    variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((0, 2)),
                variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
            },
        );
    }

    #[test]
    fn shift_negation() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Negation(Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("a", 0),
                    })),
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Negation(Rc::new(Term {
                    source_range: Some((102, 106)),
                    variant: Variable("a", 42),
                })),
            },
        );
    }

    #[test]
    fn shift_sum() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Sum(
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
                variant: Sum(
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
    fn shift_difference() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Difference(
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
                variant: Difference(
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
    fn shift_product() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Product(
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
                variant: Product(
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
    fn shift_quotient() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Quotient(
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
                variant: Quotient(
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
    fn shift_less_than() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: LessThan(
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
                variant: LessThan(
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
    fn shift_less_than_or_equal_to() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: LessThanOrEqualTo(
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
                variant: LessThanOrEqualTo(
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
    fn shift_equal_to() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: EqualTo(
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
                variant: EqualTo(
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
    fn shift_greater_than() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: GreaterThan(
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
                variant: GreaterThan(
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
    fn shift_greater_than_or_equal_to() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: GreaterThanOrEqualTo(
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
                variant: GreaterThanOrEqualTo(
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
    fn shift_boolean() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, BOOLEAN_KEYWORD.len())),
                    variant: Boolean,
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((0, BOOLEAN_KEYWORD.len())),
                variant: Boolean,
            },
        );
    }

    #[test]
    fn shift_true() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, TRUE_KEYWORD.len())),
                    variant: True,
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((0, TRUE_KEYWORD.len())),
                variant: True,
            },
        );
    }

    #[test]
    fn shift_false() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((0, FALSE_KEYWORD.len())),
                    variant: False,
                }),
                0,
                42,
            ),
            Term {
                source_range: Some((0, FALSE_KEYWORD.len())),
                variant: False,
            },
        );
    }

    #[test]
    fn shift_if() {
        assert_eq!(
            *shift(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: If(
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                        Rc::new(Term {
                            source_range: Some((121, 122)),
                            variant: Variable("c", 2),
                        }),
                    ),
                }),
                1,
                42,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: If(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
                        variant: Variable("b", 43),
                    }),
                    Rc::new(Term {
                        source_range: Some((121, 122)),
                        variant: Variable("c", 44),
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
                0,
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
                0,
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
                0,
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
                0,
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
                        Some(Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("b", 0),
                        })),
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Lambda(
                    "a",
                    Some(Rc::new(Term {
                        source_range: Some((3, 4)),
                        variant: Variable("x", 4),
                    })),
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
                0,
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
                0,
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
                0,
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

    #[test]
    fn open_integer() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, INTEGER_KEYWORD.len())),
                    variant: Integer,
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
                0,
            ),
            Term {
                source_range: Some((0, INTEGER_KEYWORD.len())),
                variant: Integer,
            },
        );
    }

    #[test]
    fn open_integer_literal() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, 2)),
                    variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
                0,
            ),
            Term {
                source_range: Some((0, 2)),
                variant: IntegerLiteral(ToBigInt::to_bigint(&84).unwrap()),
            },
        );
    }

    #[test]
    fn open_negation() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Negation(Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("a", 0),
                    })),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Negation(Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                })),
            },
        );
    }

    #[test]
    fn open_sum() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Sum(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Sum(
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
    fn open_difference() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Difference(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Difference(
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
    fn open_product() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Product(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Product(
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
    fn open_quotient() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: Quotient(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: Quotient(
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
    fn open_less_than() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: LessThan(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: LessThan(
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
    fn open_less_than_or_equal_to() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: LessThanOrEqualTo(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: LessThanOrEqualTo(
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
    fn open_equal_to() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: EqualTo(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: EqualTo(
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
    fn open_greater_than() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: GreaterThan(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: GreaterThan(
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
    fn open_greater_than_or_equal_to() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: GreaterThanOrEqualTo(
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
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: GreaterThanOrEqualTo(
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
    fn open_boolean() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, BOOLEAN_KEYWORD.len())),
                    variant: Boolean,
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
                0,
            ),
            Term {
                source_range: Some((0, BOOLEAN_KEYWORD.len())),
                variant: Boolean,
            },
        );
    }

    #[test]
    fn open_true() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, TRUE_KEYWORD.len())),
                    variant: True,
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
                0,
            ),
            Term {
                source_range: Some((0, TRUE_KEYWORD.len())),
                variant: True,
            },
        );
    }

    #[test]
    fn open_false() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((0, FALSE_KEYWORD.len())),
                    variant: False,
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("y", 0),
                }),
                0,
            ),
            Term {
                source_range: Some((0, FALSE_KEYWORD.len())),
                variant: False,
            },
        );
    }

    #[test]
    fn open_if() {
        assert_eq!(
            *open(
                Rc::new(Term {
                    source_range: Some((97, 112)),
                    variant: If(
                        Rc::new(Term {
                            source_range: Some((102, 106)),
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Term {
                            source_range: Some((111, 112)),
                            variant: Variable("b", 1),
                        }),
                        Rc::new(Term {
                            source_range: Some((121, 122)),
                            variant: Variable("c", 2),
                        }),
                    ),
                }),
                0,
                Rc::new(Term {
                    source_range: Some((3, 4)),
                    variant: Variable("x", 4),
                }),
                0,
            ),
            Term {
                source_range: Some((97, 112)),
                variant: If(
                    Rc::new(Term {
                        source_range: Some((3, 4)),
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
                        variant: Variable("b", 0),
                    }),
                    Rc::new(Term {
                        source_range: Some((121, 122)),
                        variant: Variable("c", 1),
                    }),
                ),
            },
        );
    }

    #[test]
    fn free_variables_type() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: Some((0, TYPE_KEYWORD.len())),
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
                source_range: Some((0, 1)),
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
                source_range: Some((0, 1)),
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
                source_range: Some((97, 112)),
                variant: Lambda(
                    "a",
                    Some(Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    })),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
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
                source_range: Some((97, 112)),
                variant: Pi(
                    "a",
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
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
                source_range: Some((97, 112)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
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
                                variant: Variable("y", 15),
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
                                variant: Variable("z", 16),
                            }),
                        ),
                    ],
                    Rc::new(Term {
                        source_range: Some((28, 29)),
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
                source_range: Some((0, INTEGER_KEYWORD.len())),
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
                source_range: Some((0, 2)),
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
                source_range: Some((97, 112)),
                variant: Negation(Rc::new(Term {
                    source_range: Some((102, 106)),
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
                source_range: Some((97, 112)),
                variant: Sum(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
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
                source_range: Some((97, 112)),
                variant: Difference(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
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
                source_range: Some((97, 112)),
                variant: Product(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
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
                source_range: Some((97, 112)),
                variant: Quotient(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
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
                source_range: Some((0, BOOLEAN_KEYWORD.len())),
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
                source_range: Some((0, TRUE_KEYWORD.len())),
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
                source_range: Some((0, FALSE_KEYWORD.len())),
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
                source_range: Some((97, 112)),
                variant: If(
                    Rc::new(Term {
                        source_range: Some((102, 106)),
                        variant: Variable("b", 15),
                    }),
                    Rc::new(Term {
                        source_range: Some((111, 112)),
                        variant: Variable("b", 16),
                    }),
                    Rc::new(Term {
                        source_range: Some((121, 122)),
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
