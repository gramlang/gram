use std::{
    fmt::{Display, Formatter, Result},
    rc::Rc,
};

// The token stream is parsed into an abstract syntax tree (AST). This struct represents a node in
// an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Node<'a> {
    pub source_range: Option<(usize, usize)>, // [start, end)
    pub group: bool, // For an explanation of this field, see [ref:group-flag].
    pub variant: Variant<'a>,
}

// We assign each node a "variant" describing what kind of node it is.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variant<'a> {
    // A variable is a placeholder bound by a lambda or a pi type. The integer is the De Bruijn
    // index for the variable.
    Variable(&'a str, usize),

    // A lambda, or dependent function, is a computable function.
    Lambda(&'a str, Rc<Node<'a>>, Rc<Node<'a>>),

    // Pi types, or dependent function types, are the types ascribed to lambdas.
    Pi(&'a str, Rc<Node<'a>>, Rc<Node<'a>>),

    // An application is the act of applying a lambda to an argument.
    Application(Rc<Node<'a>>, Rc<Node<'a>>),
}

impl<'a> Display for Node<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.variant)
    }
}

impl<'a> Display for Variant<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Variable(variable, _) => write!(f, "{}", variable),
            Self::Lambda(variable, domain, body) => {
                write!(f, "({} : {}) => ({})", variable, domain, body)
            }
            Self::Pi(variable, domain, codomain) => {
                write!(f, "({} : {}) -> ({})", variable, domain, codomain)
            }
            Self::Application(applicand, argument) => write!(f, "({}) ({})", applicand, argument),
        }
    }
}
