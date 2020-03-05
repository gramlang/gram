use crate::{
    parser::PLACEHOLDER_VARIABLE,
    token::{BOOLEAN_KEYWORD, FALSE_KEYWORD, INTEGER_KEYWORD, TRUE_KEYWORD, TYPE_KEYWORD},
};
use num_bigint::BigInt;
use std::{
    fmt::{Display, Formatter, Result},
    rc::Rc,
};

// The token stream is parsed into an abstract syntax tree (AST). This struct represents a node in
// an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Term<'a> {
    pub source_range: Option<(usize, usize)>, // [start, end)
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
    Sum(Rc<Term<'a>>, Rc<Term<'a>>),
    Difference(Rc<Term<'a>>, Rc<Term<'a>>),
    Product(Rc<Term<'a>>, Rc<Term<'a>>),
    Quotient(Rc<Term<'a>>, Rc<Term<'a>>),
    Boolean,
    True,
    False,
    If(Rc<Term<'a>>, Rc<Term<'a>>, Rc<Term<'a>>),
}

impl<'a> Display for Term<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({})", self.variant)?;
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
                if *variable == PLACEHOLDER_VARIABLE {
                    write!(f, "{} -> {}", domain, codomain)
                } else {
                    write!(f, "({} : {}) -> {}", variable, domain, codomain)
                }
            }
            Self::Application(applicand, argument) => write!(f, "{} {}", applicand, argument),
            Self::Let(definitions, body) => {
                for (variable, annotation, definition) in definitions {
                    match annotation {
                        Some(annotation) => {
                            write!(f, "{} : {} = {}; ", variable, annotation, definition)?;
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
            Self::Sum(summand1, summand2) => write!(f, "{} + {}", summand1, summand2),
            Self::Difference(minuend, subtrahend) => write!(f, "{} - {}", minuend, subtrahend),
            Self::Product(factor1, factor2) => write!(f, "{} * {}", factor1, factor2),
            Self::Quotient(dividend, divisor) => write!(f, "{} / {}", dividend, divisor),
            Self::Boolean => write!(f, "{}", BOOLEAN_KEYWORD),
            Self::True => write!(f, "{}", TRUE_KEYWORD),
            Self::False => write!(f, "{}", FALSE_KEYWORD),
            Self::If(condition, then_branch, else_branch) => write!(
                f,
                "if {} then {} else {}",
                condition, then_branch, else_branch
            ),
        }
    }
}
