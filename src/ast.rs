use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Node<'a> {
    pub source_range: (usize, usize), // [start, end)
    pub variant: Variant<'a>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variant<'a> {
    Lambda(&'a str, Rc<Node<'a>>, Rc<Node<'a>>),
    Pi(&'a str, Rc<Node<'a>>, Rc<Node<'a>>),
    Variable(&'a str),
    Application(Rc<Node<'a>>, Rc<Node<'a>>),
}
