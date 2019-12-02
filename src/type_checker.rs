use crate::{
    ast::{Node, Variant},
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
        Variant::Pi(_name, _domain, _codomain) => panic!("NOT IMPLEMENTED: PI"),
        Variant::Lambda(_name, _domain, _body) => panic!("NOT IMPLEMENTED: LAMBDA"),
        Variant::Variable(_name, index) => Ok(context[*index].clone()),
        Variant::Application(_applicand, _argument) => panic!("NOT IMPLEMENTED: APPLICATION"),
    }
}
