use crate::{
    ast::{
        Node,
        Variant::{Application, Lambda, Pi, Variable},
    },
    normalizer::normalize,
};
use std::borrow::Borrow;

// Check if two terms are equal up to alpha renaming.
pub fn syntactically_equal<'a, T: Borrow<Node<'a>>, U: Borrow<Node<'a>>>(
    node1: T,
    node2: U,
) -> bool {
    // Get references to the borrowed data.
    let node1 = node1.borrow();
    let node2 = node2.borrow();

    // Recursively check sub-nodes.
    match (&node1.variant, &node2.variant) {
        (Variable(_, index1), Variable(_, index2)) => index1 == index2,
        (Lambda(_, domain1, body1), Lambda(_, domain2, body2)) => {
            syntactically_equal(&**domain1, &**domain2) && syntactically_equal(&**body1, &**body2)
        }
        (Pi(_, domain1, codomain1), Pi(_, domain2, codomain2)) => {
            syntactically_equal(&**domain1, &**domain2)
                && syntactically_equal(&**codomain1, &**codomain2)
        }
        (Application(applicand1, argument1), Application(applicand2, argument2)) => {
            syntactically_equal(&**applicand1, &**applicand2)
                && syntactically_equal(&**argument1, &**argument2)
        }
        _ => false,
    }
}

// Check if two terms are equal up to alpha renaming and beta/eta equivalence.
pub fn definitionally_equal<'a, T: Borrow<Node<'a>>, U: Borrow<Node<'a>>>(
    node1: T,
    node2: U,
) -> bool {
    // Get references to the borrowed data.
    let node1 = node1.borrow();
    let node2 = node2.borrow();

    // Check if the normalized terms are equal.
    syntactically_equal(normalize(node1), normalize(node2))
}

#[cfg(test)]
mod tests {
    use crate::{
        equality::{definitionally_equal, syntactically_equal},
        parser::parse,
        tokenizer::tokenize,
        type_checker::TYPE,
    };
    use std::collections::HashMap;

    #[test]
    fn syntactically_equal_alpha() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert("x", 0);

        let source1 = "x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert("y", 0);

        let source2 = "y";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), true);
    }

    #[test]
    fn syntactically_inequal_variable() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert("x", 0);

        let source1 = "x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert("x", 0);
        context2.insert("y", 1);

        let source2 = "x";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), false);
    }

    #[test]
    fn syntactically_equal_lambda() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);

        let source1 = "(x : type) => x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert(TYPE, 0);

        let source2 = "(x : type) => x";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), true);
    }

    #[test]
    fn syntactically_inequal_lambda_domain() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);

        let source1 = "(x : type) => x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert(TYPE, 0);

        let source2 = "(x : (type type)) => x";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), false);
    }

    #[test]
    fn syntactically_inequal_lambda_body() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);

        let source1 = "(x : type) => x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert(TYPE, 0);

        let source2 = "(x : type) => type";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), false);
    }

    #[test]
    fn syntactically_equal_pi() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);

        let source1 = "(x : type) -> x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert(TYPE, 0);

        let source2 = "(x : type) -> x";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), true);
    }

    #[test]
    fn syntactically_inequal_pi_domain() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);

        let source1 = "(x : type) -> x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert(TYPE, 0);

        let source2 = "(x : (type type)) -> x";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), false);
    }

    #[test]
    fn syntactically_inequal_pi() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);

        let source1 = "(x : type) -> x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert(TYPE, 0);

        let source2 = "(x : type) -> type";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), false);
    }

    #[test]
    fn syntactically_equal_application() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert("f", 0);
        context1.insert("x", 1);

        let source1 = "f x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert("f", 0);
        context2.insert("x", 1);

        let source2 = "f x";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), true);
    }

    #[test]
    fn syntactically_inequal_application_applicand() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert("f", 0);
        context1.insert("x", 1);

        let source1 = "f x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert("f", 0);
        context2.insert("x", 1);

        let source2 = "x x";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), false);
    }

    #[test]
    fn syntactically_equal_argument() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert("f", 0);
        context1.insert("x", 1);

        let source1 = "f x";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert("f", 0);
        context2.insert("x", 1);

        let source2 = "f f";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(syntactically_equal(node1, node2), false);
    }

    #[test]
    fn definitionally_equal_beta() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);
        context1.insert("y", 1);

        let source1 = "((x : type) => x x) y";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert("y", 0);

        let source2 = "y y";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(definitionally_equal(node1, node2), true);
    }

    #[test]
    fn definitionally_inequal_beta() {
        let mut context1 = HashMap::<&str, usize>::new();
        context1.insert(TYPE, 0);
        context1.insert("y", 1);

        let source1 = "((x : type) => x x) y";

        let tokens1 = tokenize(None, source1).unwrap();
        let node1 = parse(None, source1, &tokens1[..], &mut context1).unwrap();

        let mut context2 = HashMap::<&str, usize>::new();
        context2.insert("y", 0);

        let source2 = "y";

        let tokens2 = tokenize(None, source2).unwrap();
        let node2 = parse(None, source2, &tokens2[..], &mut context2).unwrap();

        assert_eq!(definitionally_equal(node1, node2), false);
    }
}
