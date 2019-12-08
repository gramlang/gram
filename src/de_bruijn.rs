use crate::ast::{
    Node,
    Variant::{Application, Lambda, Pi, Variable},
};
use std::rc::Rc;

// Shifting refers to increasing the De Bruijn indices of free variables greater than or equal to a
// given index.
pub fn shift<'a>(node: &Node<'a>, min_index: usize, amount: usize) -> Rc<Node<'a>> {
    // Recursively shift sub-nodes.
    match &node.variant {
        Variable(variable, index) => {
            if *index >= min_index {
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
                shift(&**domain, min_index, amount),
                shift(&**body, min_index + 1, amount),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Node {
            source_range: node.source_range,
            group: node.group,
            variant: Pi(
                variable,
                shift(&**domain, min_index, amount),
                shift(&**codomain, min_index + 1, amount),
            ),
        }),
        Application(applicand, argument) => Rc::new(Node {
            source_range: node.source_range,
            group: node.group,
            variant: Application(
                shift(&**applicand, min_index, amount),
                shift(&**argument, min_index, amount),
            ),
        }),
    }
}

// Opening is the act of replacing a free variable by a term and decrementing the De Bruijn indices
// of the free variables with higher indices than that of the one being replaced.
pub fn open<'a>(
    node_to_open: &Node<'a>,
    index_to_replace: usize,
    node_to_insert: &Node<'a>,
) -> Rc<Node<'a>> {
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
                let shifted_node = shift(node_to_insert, 0, index_to_replace);

                Rc::new(Node {
                    source_range: shifted_node.source_range,
                    group: true, // To ensure the resulting node is still parse-able when printed
                    variant: shifted_node.variant.clone(),
                })
            }
        }
        Lambda(variable, domain, body) => Rc::new(Node {
            source_range: node_to_open.source_range,
            group: true, // To ensure the resulting node is still parse-able when printed
            variant: Lambda(
                variable,
                open(&**domain, index_to_replace, node_to_insert),
                open(&**body, index_to_replace + 1, node_to_insert),
            ),
        }),
        Pi(variable, domain, codomain) => Rc::new(Node {
            source_range: node_to_open.source_range,
            group: true, // To ensure the resulting node is still parse-able when printed
            variant: Pi(
                variable,
                open(&**domain, index_to_replace, node_to_insert),
                open(&**codomain, index_to_replace + 1, node_to_insert),
            ),
        }),
        Application(applicand, argument) => Rc::new(Node {
            source_range: node_to_open.source_range,
            group: true, // To ensure the resulting node is still parse-able when printed
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
        de_bruijn::{open, shift},
    };
    use std::rc::Rc;

    #[test]
    fn shift_variable_free() {
        assert_eq!(
            *shift(
                &Node {
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
                &Node {
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
                &Node {
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
                &Node {
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
                &Node {
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
                &Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                0,
                &Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("y", 0),
                },
            ),
            Node {
                source_range: Some((3, 4)),
                group: true,
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn open_variable_free() {
        assert_eq!(
            *open(
                &Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 1),
                },
                0,
                &Node {
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
                &Node {
                    source_range: Some((0, 1)),
                    group: false,
                    variant: Variable("x", 0),
                },
                1,
                &Node {
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
                &Node {
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
                &Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("x", 4),
                },
            ),
            Node {
                source_range: Some((97, 112)),
                group: true,
                variant: Lambda(
                    "a",
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: true,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: true,
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
                &Node {
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
                &Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("x", 4),
                },
            ),
            Node {
                source_range: Some((97, 112)),
                group: true,
                variant: Pi(
                    "a",
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: true,
                        variant: Variable("x", 4),
                    }),
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: true,
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
                &Node {
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
                &Node {
                    source_range: Some((3, 4)),
                    group: false,
                    variant: Variable("x", 4),
                },
            ),
            Node {
                source_range: Some((97, 112)),
                group: true,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((3, 4)),
                        group: true,
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
