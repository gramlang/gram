use crate::{
    ast::{
        Node,
        Variant::{Application, Lambda, Pi, Variable},
    },
    error::Error,
};
use std::{
    borrow::{Borrow, BorrowMut},
    path::Path,
    rc::Rc,
};

// The type of all types
pub const TYPE: &str = "type";

// This is the top-level type checking function.
pub fn type_check<'a, T: Borrow<Node<'a>>, U: BorrowMut<Vec<Rc<Node<'a>>>>>(
    _source_path: Option<&'a Path>,
    _source_contents: &'a str,
    node: T,
    context: U,
) -> Result<Rc<Node<'a>>, Error> {
    // Get references to the borrowed data.
    let node = node.borrow();
    let context = context.borrow();

    // The type checking rules are syntax-directed, so here we pattern match on the syntax.
    match &node.variant {
        Variable(_variable, index) => Ok(context[*index].clone()),
        Lambda(_variable, _domain, _body) => panic!("NOT IMPLEMENTED: LAMBDA"),
        Pi(_variable, _domain, _codomain) => panic!("NOT IMPLEMENTED: PI"),
        Application(_applicand, _argument) => panic!("NOT IMPLEMENTED: APPLICATION"),
    }
}
