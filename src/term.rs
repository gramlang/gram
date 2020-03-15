use crate::{
    de_bruijn::free_variables,
    token::{BOOLEAN_KEYWORD, FALSE_KEYWORD, INTEGER_KEYWORD, TRUE_KEYWORD, TYPE_KEYWORD},
};
use num_bigint::BigInt;
use std::{
    collections::HashSet,
    fmt::{Display, Formatter, Result},
    rc::Rc,
};

// The token stream is parsed into an abstract syntax tree (AST). This struct represents a node in
// an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Term<'a> {
    // Inclusive on the left and exclusive on the right
    pub source_range: Option<(usize, usize)>,

    pub variant: Variant<'a>,
}

// Each term has a "variant" describing what kind of term it is.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variant<'a> {
    Type,
    Variable(&'a str, usize),
    Lambda(&'a str, Rc<Term<'a>>, Rc<Term<'a>>),
    Pi(&'a str, Rc<Term<'a>>, Rc<Term<'a>>),
    Application(Rc<Term<'a>>, Rc<Term<'a>>),
    #[allow(clippy::type_complexity)]
    Let(
        Vec<(&'a str, Option<Rc<Term<'a>>>, Rc<Term<'a>>)>,
        Rc<Term<'a>>,
    ),
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
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.variant)?;
        Ok(())
    }
}

impl<'a> Display for Variant<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Type => write!(f, "{}", TYPE_KEYWORD),
            Self::Variable(variable, _) => write!(f, "{}", variable),
            Self::Lambda(variable, domain, body) => {
                write!(f, "({} : {}) => {}", variable, domain, body)
            }
            Self::Pi(variable, domain, codomain) => {
                let mut variables = HashSet::new();
                free_variables(codomain, 0, &mut variables);

                if variables.contains(&0) {
                    write!(f, "({} : {}) -> {}", variable, domain, codomain)
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
                    match annotation {
                        Some(annotation) => {
                            write!(
                                f,
                                "{} : {} = {}; ",
                                variable,
                                group(annotation),
                                group(definition),
                            )?;
                        }
                        None => {
                            write!(f, "{} = {}; ", variable, definition)?;
                        }
                    }
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
    match term.variant {
        Variant::Type
        | Variant::Variable(_, _)
        | Variant::Integer
        | Variant::IntegerLiteral(_)
        | Variant::Boolean
        | Variant::True
        | Variant::False => format!("{}", term),
        Variant::Lambda(_, _, _)
        | Variant::Pi(_, _, _)
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
