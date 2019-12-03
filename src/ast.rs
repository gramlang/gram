use std::rc::Rc;

// The token stream is parsed into an abstract syntax tree (AST). This struct represents a node in
// an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Node<'a> {
    pub source_range: Option<(usize, usize)>, // [start, end)
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
