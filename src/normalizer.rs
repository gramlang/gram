use crate::{
    ast::{
        Node,
        Variant::{Application, Lambda, Pi, Variable},
    },
    de_bruijn::{occurs, open},
};
use std::{borrow::Borrow, path::Path, rc::Rc};

// This function reduces a term to beta-eta normal form using applicative order reduction.
pub fn normalize<'a, T: Borrow<Node<'a>>>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    node: T,
) -> Rc<Node<'a>> {
    // Get references to the borrowed data.
    let node = node.borrow();

    // Recursively normalize sub-nodes.
    match &node.variant {
        Variable(_, _) => {
            // Variables are already in beta-eta normal form.
            Rc::new(node.clone())
        }
        Lambda(variable, domain, body) => {
            // Normalize the domain.
            let normalized_domain = normalize(source_path, source_contents, &**domain);

            // Normalize the body.
            let normalized_body = normalize(source_path, source_contents, &**body);

            // Perform eta reduction, if applicable.
            if let Application(applicand, argument) = &normalized_body.variant {
                if !occurs(&**applicand, 0) {
                    if let Variable(_, 0) = &argument.variant {
                        return applicand.clone();
                    }
                }
            }

            // If we reached this point, there is no applicable eta reduction to perform. Just
            // return the lambda with its domain and body reduced.
            Rc::new(Node {
                source_range: node.source_range,
                group: node.group,
                variant: Lambda(variable, normalized_domain, normalized_body),
            })
        }
        Pi(variable, domain, codomain) => {
            // For pi types, we simply reduce the domain and codomain.
            Rc::new(Node {
                source_range: node.source_range,
                group: node.group,
                variant: Pi(
                    variable,
                    normalize(source_path, source_contents, &**domain),
                    normalize(source_path, source_contents, &**codomain),
                ),
            })
        }
        Application(applicand, argument) => {
            // Reduce the applicand.
            let normalized_applicand = normalize(source_path, source_contents, &**applicand);

            // Reduce the argument. This means we're doing applicative order reduction.
            let normalized_argument = normalize(source_path, source_contents, &**argument);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, body) = &normalized_applicand.variant {
                // We got a lambda. Perform beta reduction.
                normalize(
                    source_path,
                    source_contents,
                    open(&**body, 0, normalized_argument),
                )
            } else {
                // We didn't get a lambda. Just reduce the argument.
                Rc::new(Node {
                    source_range: node.source_range,
                    group: node.group,
                    variant: Application(normalized_applicand, normalized_argument),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Node,
            Variant::{Application, Lambda, Pi, Variable},
        },
        normalizer::normalize,
        parser::parse,
        tokenizer::tokenize,
        type_checker::TYPE,
    };
    use std::{collections::HashMap, rc::Rc};

    #[test]
    fn normalize_variable() {
        let mut context = HashMap::<&str, usize>::new();
        context.insert("x", 0);

        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let node = parse(None, source, &tokens[..], &mut context).unwrap();

        assert_eq!(
            *normalize(None, "", node),
            Node {
                source_range: Some((0, 1)),
                group: false,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn normalize_redex_under_lambda() {
        let mut context = HashMap::<&str, usize>::new();
        context.insert(TYPE, 0);
        context.insert("p", 1);
        context.insert("q", 2);

        let source = "(x : ((y : type) => y) p) => ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let node = parse(None, source, &tokens[..], &mut context).unwrap();

        assert_eq!(
            *normalize(None, "", node),
            Node {
                source_range: Some((0, 48)),
                group: false,
                variant: Lambda(
                    "x",
                    Rc::new(Node {
                        source_range: Some((23, 24)),
                        group: false,
                        variant: Variable("p", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((47, 48)),
                        group: false,
                        variant: Variable("q", 1),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_eta() {
        let mut context = HashMap::<&str, usize>::new();
        context.insert(TYPE, 0);
        context.insert("f", 1);

        let source = "(x : type) => f x";

        let tokens = tokenize(None, source).unwrap();
        let node = parse(None, source, &tokens[..], &mut context).unwrap();

        assert_eq!(
            *normalize(None, "", node),
            Node {
                source_range: Some((14, 15)),
                group: false,
                variant: Variable("f", 1),
            },
        );
    }

    #[test]
    fn normalize_non_eta() {
        let mut context = HashMap::<&str, usize>::new();
        context.insert(TYPE, 0);
        context.insert("f", 1);

        let source = "(x : type) => x x";

        let tokens = tokenize(None, source).unwrap();
        let node = parse(None, source, &tokens[..], &mut context).unwrap();

        assert_eq!(
            *normalize(None, "", node),
            Node {
                source_range: Some((0, 17)),
                group: false,
                variant: Lambda(
                    "x",
                    Rc::new(Node {
                        source_range: Some((5, 9)),
                        group: false,
                        variant: Variable("type", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((14, 17)),
                        group: false,
                        variant: Application(
                            Rc::new(Node {
                                source_range: Some((14, 15)),
                                group: false,
                                variant: Variable("x", 0),
                            }),
                            Rc::new(Node {
                                source_range: Some((16, 17)),
                                group: false,
                                variant: Variable("x", 0),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_redex_under_pi() {
        let mut context = HashMap::<&str, usize>::new();
        context.insert(TYPE, 0);
        context.insert("p", 1);
        context.insert("q", 2);

        let source = "(x : ((y : type) => y) p) -> ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let node = parse(None, source, &tokens[..], &mut context).unwrap();

        assert_eq!(
            *normalize(None, "", node),
            Node {
                source_range: Some((0, 48)),
                group: false,
                variant: Pi(
                    "x",
                    Rc::new(Node {
                        source_range: Some((23, 24)),
                        group: false,
                        variant: Variable("p", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((47, 48)),
                        group: false,
                        variant: Variable("q", 1),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_non_redex() {
        let mut context = HashMap::<&str, usize>::new();
        context.insert(TYPE, 0);
        context.insert("y", 1);
        context.insert("w", 2);

        let source = "(((x : type) => x) y) (((z : type) => z) w)";

        let tokens = tokenize(None, source).unwrap();
        let node = parse(None, source, &tokens[..], &mut context).unwrap();

        assert_eq!(
            *normalize(None, "", node),
            Node {
                source_range: Some((2, 42)),
                group: false,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((19, 20)),
                        group: false,
                        variant: Variable("y", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((41, 42)),
                        group: false,
                        variant: Variable("w", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn normalize_redex() {
        let mut context = HashMap::<&str, usize>::new();
        context.insert(TYPE, 0);
        context.insert("y", 1);

        let source = "((x : type) => x) y";

        let tokens = tokenize(None, source).unwrap();
        let node = parse(None, source, &tokens[..], &mut context).unwrap();

        assert_eq!(
            *normalize(None, "", node),
            Node {
                source_range: Some((18, 19)),
                group: false,
                variant: Variable("y", 0),
            },
        );
    }
}
