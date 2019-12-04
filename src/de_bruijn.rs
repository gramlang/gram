use crate::ast::{
    Node,
    Variant::{Application, Lambda, Pi, Variable},
};
use std::{borrow::Borrow, rc::Rc};

// Check if a term contains a particular free variable.
pub fn occurs<'a, T: Borrow<Node<'a>>>(node: T, index_to_check: usize) -> bool {
    // Get references to the borrowed data.
    let node = node.borrow();

    // Recursively check sub-nodes.
    match &node.variant {
        Variable(_, index) => index == &index_to_check,
        Lambda(_, domain, body) => {
            occurs(&**domain, index_to_check) || occurs(&**body, index_to_check + 1)
        }
        Pi(_, domain, codomain) => {
            occurs(&**domain, index_to_check) || occurs(&**codomain, index_to_check + 1)
        }
        Application(applicand, argument) => {
            occurs(&**applicand, index_to_check) || occurs(&**argument, index_to_check)
        }
    }
}

// Shifting refers to increasing the De Bruijn indices of all free variables.
pub fn shift<'a, T: Borrow<Node<'a>>>(node: T, depth: usize, amount: usize) -> Rc<Node<'a>> {
    // Get references to the borrowed data.
    let node = node.borrow();

    // Recursively shift sub-nodes.
    match &node.variant {
        Variable(variable, index) => {
            if *index >= depth {
                Rc::new(Node {
                    source_range: node.source_range,
                    group: node.group,
                    variant: Variable(variable, index + amount),
                })
            } else {
                Rc::new(node.clone())
            }
        }
        Lambda(variable, domain, body) => Rc::new(Node {
            source_range: node.source_range,
            group: node.group,
            variant: Lambda(
                variable,
                shift(&**domain, depth, amount),
                shift(&**body, depth + 1, amount),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Node {
            source_range: node.source_range,
            group: node.group,
            variant: Pi(
                variable,
                shift(&**domain, depth, amount),
                shift(&**codomain, depth + 1, amount),
            ),
        }),
        Application(applicand, argument) => Rc::new(Node {
            source_range: node.source_range,
            group: node.group,
            variant: Application(
                shift(&**applicand, depth, amount),
                shift(&**argument, depth, amount),
            ),
        }),
    }
}

// Opening is the act of replacing a variable by a term and decrementing the De Bruijn indices of
// all free variables.
pub fn open<'a, T: Borrow<Node<'a>>, U: Borrow<Node<'a>>>(
    node_to_open: T,
    index_to_replace: usize,
    node_to_insert: U,
) -> Rc<Node<'a>> {
    // Get references to the borrowed data.
    let node_to_open = node_to_open.borrow();
    let node_to_insert = node_to_insert.borrow();

    // Recursively open sub-nodes.
    match &node_to_open.variant {
        Variable(variable, index) => {
            if *index > index_to_replace {
                Rc::new(Node {
                    source_range: node_to_open.source_range,
                    group: node_to_open.group,
                    variant: Variable(variable, index - 1),
                })
            } else if *index < index_to_replace {
                Rc::new(node_to_open.clone())
            } else {
                shift(node_to_insert, 0, index_to_replace)
            }
        }
        Lambda(variable, domain, body) => Rc::new(Node {
            source_range: node_to_open.source_range,
            group: node_to_open.group,
            variant: Lambda(
                variable,
                open(&**domain, index_to_replace, node_to_insert),
                open(&**body, index_to_replace + 1, node_to_insert),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Node {
            source_range: node_to_open.source_range,
            group: node_to_open.group,
            variant: Pi(
                variable,
                open(&**domain, index_to_replace, node_to_insert),
                open(&**codomain, index_to_replace + 1, node_to_insert),
            ),
        }),
        Application(applicand, argument) => Rc::new(Node {
            source_range: node_to_open.source_range,
            group: node_to_open.group,
            variant: Application(
                open(&**applicand, index_to_replace, node_to_insert),
                open(&**argument, index_to_replace, node_to_insert),
            ),
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Node,
            Variant::{Application, Lambda, Pi, Variable},
        },
        de_bruijn::{occurs, open, shift},
    };
    use std::rc::Rc;

    #[test]
    fn occurs_variable_match() {
        assert_eq!(
            occurs(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                0,
            ),
            true,
        );
    }

    #[test]
    fn occurs_variable_less_free() {
        assert_eq!(
            occurs(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                1,
            ),
            false,
        );
    }

    #[test]
    fn occurs_variable_more_free() {
        assert_eq!(
            occurs(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 1),
                },
                0,
            ),
            false,
        );
    }

    #[test]
    fn occurs_lambda() {
        assert_eq!(
            occurs(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Lambda(
                        "a",
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
            ),
            true,
        );
    }

    #[test]
    fn occurs_pi() {
        assert_eq!(
            occurs(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Pi(
                        "a",
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
            ),
            true,
        );
    }

    #[test]
    fn occurs_application() {
        assert_eq!(
            occurs(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Application(
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
            ),
            true,
        );
    }

    #[test]
    fn shift_variable_free() {
        assert_eq!(
            *shift(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                0,
                42,
            ),
            Node {
                source_range: Some((0, 1)),
                group: false,
                variant: Variable("x", 42),
            },
        );
    }

    #[test]
    fn shift_variable_bound() {
        assert_eq!(
            *shift(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                1,
                42,
            ),
            Node {
                source_range: Some((0, 1)),
                group: false,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn shift_lambda() {
        assert_eq!(
            *shift(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Lambda(
                        "a",
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                42,
            ),
            Node {
                source_range: Some((97, 112)),
                group: false,
                variant: Lambda(
                    "a",
                    Rc::new(Node {
                        source_range: Some((102, 106)),
                        group: false,
                        variant: Variable("b", 42),
                    }),
                    Rc::new(Node {
                        source_range: Some((111, 112)),
                        group: false,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_pi() {
        assert_eq!(
            *shift(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Pi(
                        "a",
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                42,
            ),
            Node {
                source_range: Some((97, 112)),
                group: false,
                variant: Pi(
                    "a",
                    Rc::new(Node {
                        source_range: Some((102, 106)),
                        group: false,
                        variant: Variable("b", 42),
                    }),
                    Rc::new(Node {
                        source_range: Some((111, 112)),
                        group: false,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn shift_application() {
        assert_eq!(
            *shift(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Application(
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                1,
                42,
            ),
            Node {
                source_range: Some((97, 112)),
                group: false,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((102, 106)),
                        group: false,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Node {
                        source_range: Some((111, 112)),
                        group: false,
                        variant: Variable("b", 43),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_variable_match() {
        assert_eq!(
            *open(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                0,
                Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("y", 0),
                },
            ),
            Node {
                source_range: Some((3, 4)),
                group: false,
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn open_variable_free() {
        assert_eq!(
            *open(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 1),
                },
                0,
                Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("y", 0),
                },
            ),
            Node {
                source_range: Some((0, 1)),
                group: false,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn open_variable_bound() {
        assert_eq!(
            *open(
                Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                1,
                Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("y", 0),
                },
            ),
            Node {
                source_range: Some((0, 1)),
                group: false,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn open_lambda() {
        assert_eq!(
            *open(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Lambda(
                        "a",
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("x", 4),
                },
            ),
            Node {
                source_range: Some((97, 112)),
                group: false,
                variant: Lambda(
                    "a",
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: false,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: false,
                        variant: Variable("x", 5),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_pi() {
        assert_eq!(
            *open(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Pi(
                        "a",
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("b", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("x", 4),
                },
            ),
            Node {
                source_range: Some((97, 112)),
                group: false,
                variant: Pi(
                    "a",
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: false,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: false,
                        variant: Variable("x", 5),
                    }),
                ),
            },
        );
    }

    #[test]
    fn open_application() {
        assert_eq!(
            *open(
                Node {
                    source_range: Some((97, 112)),
                    group: false,
                    variant: Application(
                        Rc::new(Node {
                            source_range: Some((102, 106)),
                            group: false,
                            variant: Variable("a", 0),
                        }),
                        Rc::new(Node {
                            source_range: Some((111, 112)),
                            group: false,
                            variant: Variable("b", 1),
                        }),
                    ),
                },
                0,
                Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("x", 4),
                },
            ),
            Node {
                source_range: Some((97, 112)),
                group: false,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: false,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Node {
                        source_range: Some((111, 112)),
                        group: false,
                        variant: Variable("b", 0),
                    }),
                ),
            },
        );
    }
}
