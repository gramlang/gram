use crate::term::{
    Term,
    Variant::{Application, Lambda, Let, Pi, Type, Variable},
};
use std::{cmp::Ordering, convert::TryFrom, rc::Rc};

// Shifting refers to increasing the De Bruijn indices of free variables greater than or equal to a
// given index. Invariant: `min_index` should be greater than 0.
pub fn shift<'a>(term: Rc<Term<'a>>, min_index: isize, amount: isize) -> Rc<Term<'a>> {
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
        Let(definitions, body) => {
            Rc::new(Term {
                source_range: term.source_range,
                variant: Let(
                    definitions
                        .into_iter()
                        .enumerate()
                        .map(|(i, (variable, definition, annotation))| {
                            (
                                *variable,
                                shift(
                                    definition.clone(),
                                    // This will panic if `i` cannot be converted into an `isize`.
                                    min_index + isize::try_from(i).unwrap(),
                                    amount,
                                ),
                                annotation.as_ref().map(|ascription| {
                                    shift(
                                        ascription.clone(),
                                        // This will panic if `i` cannot be converted into an
                                        // `isize`.
                                        min_index + isize::try_from(i).unwrap(),
                                        amount,
                                    )
                                }),
                            )
                        })
                        .collect(),
                    // This will panic if `definitions.len()` cannot be converted into an `isize`.
                    shift(
                        body.clone(),
                        min_index + isize::try_from(definitions.len()).unwrap(),
                        amount,
                    ),
                ),
            })
        }
    }
}

// Opening is the act of replacing a free variable by a term and decrementing the De Bruijn indices
// of the free variables with higher indices than that of the one being replaced. Invariant:
// `index_to_replace` should be greater than 0.
pub fn open<'a>(
    term_to_open: Rc<Term<'a>>,
    index_to_replace: isize,
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
            Ordering::Equal => shift(term_to_insert, 1, index_to_replace - 1),
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
            Rc::new(Term {
                source_range: term_to_open.source_range,
                variant: Let(
                    definitions
                        .into_iter()
                        .enumerate()
                        .map(|(i, (variable, definition, annotation))| {
                            (
                                *variable,
                                open(
                                    definition.clone(),
                                    // This will panic if `i` cannot be converted into an `isize`.
                                    index_to_replace + isize::try_from(i).unwrap(),
                                    term_to_insert.clone(),
                                ),
                                annotation.as_ref().map(|ascription| {
                                    open(
                                        ascription.clone(),
                                        // This will panic if `i` cannot be converted into an
                                        // `isize`.
                                        index_to_replace + isize::try_from(i).unwrap(),
                                        term_to_insert.clone(),
                                    )
                                }),
                            )
                        })
                        .collect(),
                    // This will panic if `definitions.len()` cannot be converted into an `isize`.
                    open(
                        body.clone(),
                        index_to_replace + isize::try_from(definitions.len()).unwrap(),
                        term_to_insert,
                    ),
                ),
            })
        }
    }
}
