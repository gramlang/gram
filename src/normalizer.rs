use crate::{
    ast::{
        Node,
        Variant::{Application, Lambda, Pi, Variable},
    },
    de_bruijn::open,
};
use std::{borrow::Borrow, rc::Rc};

// This function reduces a term to beta normal form using applicative order reduction.
pub fn normalize<'a, T: Borrow<Node<'a>>>(node: T) -> Rc<Node<'a>> {
    // Get references to the borrowed data.
    let node = node.borrow();

    // Recursively normalize sub-nodes.
    match &node.variant {
        Variable(_, _) => {
            // Variables are already in beta normal form.
            Rc::new(node.clone())
        }
        Lambda(variable, domain, body) => {
            // For lambdas, we simply reduce the domain and body.
            Rc::new(Node {
                source_range: node.source_range,
                group: true, // To ensure the resulting node is still parse-able when printed
                variant: Lambda(variable, normalize(&**domain), normalize(&**body)),
            })
        }
        Pi(variable, domain, codomain) => {
            // For pi types, we simply reduce the domain and codomain.
            Rc::new(Node {
                source_range: node.source_range,
                group: true, // To ensure the resulting node is still parse-able when printed
                variant: Pi(variable, normalize(&**domain), normalize(&**codomain)),
            })
        }
        Application(applicand, argument) => {
            // Reduce the applicand.
            let normalized_applicand = normalize(&**applicand);

            // Reduce the argument. This means we're doing applicative order reduction.
            let normalized_argument = normalize(&**argument);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, body) = &normalized_applicand.variant {
                // We got a lambda. Perform beta reduction.
                normalize(open(&**body, 0, normalized_argument))
            } else {
                // We didn't get a lambda. Just reduce the argument.
                Rc::new(Node {
                    source_range: node.source_range,
                    group: true, // To ensure the resulting node is still parse-able when printed
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
            *normalize(node),
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
            *normalize(node),
            Node {
                source_range: Some((0, 48)),
                group: true,
                variant: Lambda(
                    "x",
                    Rc::new(Node {
                        source_range: Some((23, 24)),
                        group: true,
                        variant: Variable("p", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((47, 48)),
                        group: true,
                        variant: Variable("q", 1),
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
            *normalize(node),
            Node {
                source_range: Some((0, 48)),
                group: true,
                variant: Pi(
                    "x",
                    Rc::new(Node {
                        source_range: Some((23, 24)),
                        group: true,
                        variant: Variable("p", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((47, 48)),
                        group: true,
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
            *normalize(node),
            Node {
                source_range: Some((2, 42)),
                group: true,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((19, 20)),
                        group: true,
                        variant: Variable("y", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((41, 42)),
                        group: true,
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
            *normalize(node),
            Node {
                source_range: Some((18, 19)),
                group: true,
                variant: Variable("y", 0),
            },
        );
    }
}
