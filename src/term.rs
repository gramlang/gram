use crate::{
    de_bruijn::unsigned_shift,
    token::{BOOLEAN_KEYWORD, FALSE_KEYWORD, INTEGER_KEYWORD, TRUE_KEYWORD, TYPE_KEYWORD},
};
use num_bigint::BigInt;
use std::{
    cell::RefCell,
    collections::HashSet,
    fmt::{Display, Formatter},
    rc::Rc,
};

// The token stream is parsed into an abstract syntax tree (AST) [tag:ast] [ref:bison_grammar]. This
// struct represents a node in an AST.
#[derive(Clone, Debug)]
pub struct Term<'a> {
    pub source_range: Option<(usize, usize)>, // Inclusive on the left and exclusive on the right
    pub variant: Variant<'a>,
}

// Each term has a "variant" describing what kind of term it is.
#[derive(Clone, Debug)]
pub enum Variant<'a> {
    Unifier(Rc<RefCell<Option<Term<'a>>>>, usize), // (subterm, subterm_shift)
    Type,
    Variable(&'a str, usize),
    Lambda(&'a str, bool, Rc<Term<'a>>, Rc<Term<'a>>), // (variable, implicit, domain, body)
    Pi(&'a str, bool, Rc<Term<'a>>, Rc<Term<'a>>),     // (variable, implicit, domain, codomain)
    Application(Rc<Term<'a>>, Rc<Term<'a>>),
    #[allow(clippy::type_complexity)]
    Let(Vec<(&'a str, Rc<Term<'a>>, Rc<Term<'a>>)>, Rc<Term<'a>>),
    Integer,
    IntegerLiteral(BigInt),
    Negation(Rc<Term<'a>>),
    Sum(Rc<Term<'a>>, Rc<Term<'a>>),
    Difference(Rc<Term<'a>>, Rc<Term<'a>>),
    Product(Rc<Term<'a>>, Rc<Term<'a>>),
    Quotient(Rc<Term<'a>>, Rc<Term<'a>>),
    LessThan(Rc<Term<'a>>, Rc<Term<'a>>),
    LessThanOrEqualTo(Rc<Term<'a>>, Rc<Term<'a>>),
    EqualTo(Rc<Term<'a>>, Rc<Term<'a>>),
    GreaterThan(Rc<Term<'a>>, Rc<Term<'a>>),
    GreaterThanOrEqualTo(Rc<Term<'a>>, Rc<Term<'a>>),
    Boolean,
    True,
    False,
    If(Rc<Term<'a>>, Rc<Term<'a>>, Rc<Term<'a>>),
}

impl<'a> Display for Term<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.variant)?;
        Ok(())
    }
}

impl<'a> Display for Variant<'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Unifier(subterm_rc, _) => {
                // We `clone` the borrowed `subterm` to avoid holding the dynamic borrow for too
                // long.
                if let Some(subterm) = { subterm_rc.borrow().clone() } {
                    write!(f, "{}", subterm)
                } else {
                    write!(f, "_")
                }
            }
            Self::Type => write!(f, "{}", TYPE_KEYWORD),
            Self::Variable(variable, _) => write!(f, "{}", variable),
            Self::Lambda(variable, implicit, domain, body) => {
                if *implicit {
                    write!(f, "{{{} : {}}} => {}", variable, domain, body)
                } else {
                    write!(f, "({} : {}) => {}", variable, domain, body)
                }
            }
            Self::Pi(variable, implicit, domain, codomain) => {
                let mut variables = HashSet::new();
                free_variables(codomain, 0, &mut variables);

                if variables.contains(&0) {
                    if *implicit {
                        write!(f, "{{{} : {}}} -> {}", variable, domain, codomain)
                    } else {
                        write!(f, "({} : {}) -> {}", variable, domain, codomain)
                    }
                } else if *implicit {
                    write!(f, "{{{}}} -> {}", domain, codomain)
                } else {
                    match domain.variant {
                        Self::Application(_, _) => write!(f, "{} -> {}", domain, codomain),
                        _ => write!(f, "{} -> {}", group(domain), codomain),
                    }
                }
            }
            Self::Application(applicand, argument) => match applicand.variant {
                Self::Application(_, _) => write!(f, "{} {}", applicand, group(argument)),
                _ => write!(f, "{} {}", group(applicand), group(argument)),
            },
            Self::Let(definitions, body) => {
                for (variable, annotation, definition) in definitions {
                    write!(
                        f,
                        "{} : {} = {}; ",
                        variable,
                        group(annotation),
                        group(definition),
                    )?;
                }

                write!(f, "{}", body)
            }
            Self::Integer => write!(f, "{}", INTEGER_KEYWORD),
            Self::IntegerLiteral(integer) => write!(f, "{}", integer),
            Self::Negation(subterm) => write!(f, "-{}", group(subterm)),
            Self::Sum(term1, term2) => write!(f, "{} + {}", group(term1), group(term2)),
            Self::Difference(term1, term2) => write!(f, "{} - {}", group(term1), group(term2)),
            Self::Product(term1, term2) => write!(f, "{} * {}", group(term1), group(term2)),
            Self::Quotient(term1, term2) => write!(f, "{} / {}", group(term1), group(term2)),
            Self::LessThan(term1, term2) => write!(f, "{} < {}", group(term1), group(term2)),
            Self::LessThanOrEqualTo(term1, term2) => {
                write!(f, "{} <= {}", group(term1), group(term2))
            }
            Self::EqualTo(term1, term2) => write!(f, "{} == {}", group(term1), group(term2)),
            Self::GreaterThan(term1, term2) => write!(f, "{} > {}", group(term1), group(term2)),
            Self::GreaterThanOrEqualTo(term1, term2) => {
                write!(f, "{} >= {}", group(term1), group(term2))
            }
            Self::Boolean => write!(f, "{}", BOOLEAN_KEYWORD),
            Self::True => write!(f, "{}", TRUE_KEYWORD),
            Self::False => write!(f, "{}", FALSE_KEYWORD),
            Self::If(condition, then_branch, else_branch) => write!(
                f,
                "if {} then {} else {}",
                condition, then_branch, else_branch,
            ),
        }
    }
}

// Convert a term to a string with surrounding parentheses, except for simple terms that cause no
// parsing ambiguities in any context.
fn group<'a>(term: &Term<'a>) -> String {
    match &term.variant {
        Variant::Unifier(subterm, _) => {
            // We `clone` the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            if let Some(subterm) = { subterm.borrow().clone() } {
                group(&subterm)
            } else {
                format!("{}", term)
            }
        }
        Variant::Type
        | Variant::Variable(_, _)
        | Variant::Integer
        | Variant::IntegerLiteral(_)
        | Variant::Boolean
        | Variant::True
        | Variant::False => format!("{}", term),
        Variant::Lambda(_, _, _, _)
        | Variant::Pi(_, _, _, _)
        | Variant::Application(_, _)
        | Variant::Let(_, _)
        | Variant::Negation(_)
        | Variant::Sum(_, _)
        | Variant::Difference(_, _)
        | Variant::Product(_, _)
        | Variant::Quotient(_, _)
        | Variant::LessThan(_, _)
        | Variant::LessThanOrEqualTo(_, _)
        | Variant::EqualTo(_, _)
        | Variant::GreaterThan(_, _)
        | Variant::GreaterThanOrEqualTo(_, _)
        | Variant::If(_, _, _) => format!("({})", term),
    }
}

// Compute the free variables of a term. A cutoff determines which variables are considered free.
// This function includes free variables in type annotations.
pub fn free_variables<'a>(term: &Term<'a>, cutoff: usize, variables: &mut HashSet<usize>) {
    match &term.variant {
        Variant::Unifier(subterm, subterm_shift) => {
            // We `clone` the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            if let Some(subterm) = { subterm.borrow().clone() } {
                free_variables(
                    &unsigned_shift(&subterm, 0, *subterm_shift),
                    cutoff,
                    variables,
                );
            }
        }
        Variant::Type
        | Variant::Integer
        | Variant::IntegerLiteral(_)
        | Variant::Boolean
        | Variant::True
        | Variant::False => {}
        Variant::Variable(_, index) => {
            if *index >= cutoff {
                variables.insert(index - cutoff);
            }
        }
        Variant::Lambda(_, _, domain, body) => {
            free_variables(domain, cutoff, variables);
            free_variables(body, cutoff + 1, variables);
        }
        Variant::Pi(_, _, domain, codomain) => {
            free_variables(domain, cutoff, variables);
            free_variables(codomain, cutoff + 1, variables);
        }
        Variant::Application(applicand, argument) => {
            free_variables(applicand, cutoff, variables);
            free_variables(argument, cutoff, variables);
        }
        Variant::Let(definitions, body) => {
            for (_, annotation, definition) in definitions {
                free_variables(annotation, cutoff + definitions.len(), variables);
                free_variables(definition, cutoff + definitions.len(), variables);
            }

            free_variables(body, cutoff + definitions.len(), variables);
        }
        Variant::Negation(subterm) => free_variables(subterm, cutoff, variables),
        Variant::Sum(term1, term2)
        | Variant::Difference(term1, term2)
        | Variant::Product(term1, term2)
        | Variant::Quotient(term1, term2)
        | Variant::LessThan(term1, term2)
        | Variant::LessThanOrEqualTo(term1, term2)
        | Variant::EqualTo(term1, term2)
        | Variant::GreaterThan(term1, term2)
        | Variant::GreaterThanOrEqualTo(term1, term2) => {
            free_variables(term1, cutoff, variables);
            free_variables(term2, cutoff, variables);
        }
        Variant::If(condition, then_branch, else_branch) => {
            free_variables(condition, cutoff, variables);
            free_variables(then_branch, cutoff, variables);
            free_variables(else_branch, cutoff, variables);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        term::free_variables,
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
    fn free_variables_unifier_none() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: Unifier(Rc::new(RefCell::new(None)), 0),
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
                variant: Unifier(
                    Rc::new(RefCell::new(Some(Term {
                        source_range: None,
                        variant: Variable("x", 15),
                    }))),
                    0,
                ),
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
                    false,
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
                    false,
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
    fn free_variables_less_than() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: LessThan(
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
    fn free_variables_less_than_or_equal_to() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: LessThanOrEqualTo(
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
    fn free_variables_equal_to() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: EqualTo(
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
    fn free_variables_greater_than() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: GreaterThan(
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
    fn free_variables_greater_than_or_equal_to() {
        let mut variables = HashSet::new();

        free_variables(
            &Term {
                source_range: None,
                variant: GreaterThanOrEqualTo(
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
