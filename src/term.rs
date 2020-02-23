use crate::{parser::PLACEHOLDER_VARIABLE, token::TYPE_KEYWORD};
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
    // This is the type of all types, including itself.
    Type,

    // A variable is a placeholder bound by a lambda, pi type, or let. The integer is the De Bruijn
    // index for the variable. Positive indices refer to variables earlier in the context. Zero is
    // a recursive reference. Negative indices refer to variables later in the context, which
    // happens in let bindings with forward references.
    Variable(&'a str, isize),

    // A lambda, or dependent function, is a computable function.
    Lambda(&'a str, Rc<Term<'a>>, Rc<Term<'a>>),

    // Pi types, or dependent function types, are the types ascribed to lambdas.
    Pi(&'a str, Rc<Term<'a>>, Rc<Term<'a>>),

    // An application is the act of applying a lambda to an argument.
    Application(Rc<Term<'a>>, Rc<Term<'a>>),

    // A let is a local variable definition.
    Let(&'a str, Rc<Term<'a>>, Rc<Term<'a>>),
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
            Self::Let(variable, definition, body) => {
                write!(f, "{} = {}; {}", variable, definition, body)
            }
        }
    }
}

// Construct the type of all types once here rather than constructing it many times later.
pub const TYPE_TERM: Term = Term {
    source_range: None,
    variant: Variant::Type,
};
