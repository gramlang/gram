use crate::{
    normalizer::normalize_weak_head,
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::rc::Rc;

// Check if two terms are equal up to alpha conversion. Type annotations are not checked.
pub fn syntactically_equal<'a>(term1: &Term<'a>, term2: &Term<'a>) -> bool {
    // Due to the catch-all case at the bottom of this `match`, the compiler will not complain if a
    // new syntactic form is added and this `match` is not updated. Be sure to update it when
    // adding new syntactic forms!
    match (&term1.variant, &term2.variant) {
        (Type, Type) => true,
        (Variable(_, index1), Variable(_, index2)) => index1 == index2,
        (Lambda(_, _, body1), Lambda(_, _, body2)) => syntactically_equal(&**body1, &**body2),
        (Pi(_, domain1, codomain1), Pi(_, domain2, codomain2)) => {
            syntactically_equal(&**domain1, &**domain2)
                && syntactically_equal(&**codomain1, &**codomain2)
        }
        (Application(applicand1, argument1), Application(applicand2, argument2)) => {
            syntactically_equal(&**applicand1, &**applicand2)
                && syntactically_equal(&**argument1, &**argument2)
        }
        (Let(_, definition1, body1), Let(_, definition2, body2)) => {
            syntactically_equal(&**definition1, &**definition2)
                && syntactically_equal(&**body1, &**body2)
        }
        _ => false,
    }
}

// Check if two terms are convertible up to beta normalization. Type annotations are not checked.
pub fn definitionally_equal<'a>(
    term1: Rc<Term<'a>>,
    term2: Rc<Term<'a>>,
    normalization_context: &mut Vec<Option<Rc<Term<'a>>>>,
) -> bool {
    // The two terms might not have normal forms, but if they are syntactically equal then we can
    // still consider them definitionally equal. So we check for that first.
    if syntactically_equal(&*term1, &*term2) {
        return true;
    }

    // Reduce both terms to weak head normal form and recursively check for convertibility. Note
    // that there is no case for lets because lets are never in weak head normal form. Due to the
    // catch-all case at the bottom of this `match`, the compiler will not complain if a new
    // syntactic form is added and this `match` is not updated. Be sure to update it when adding
    // new syntactic forms!
    match (
        &normalize_weak_head(term1, normalization_context).variant,
        &normalize_weak_head(term2, normalization_context).variant,
    ) {
        (Type, Type) => true,
        (Variable(_, index1), Variable(_, index2)) => index1 == index2,
        (Lambda(_, _, body1), Lambda(_, _, body2)) => {
            // Temporarily add the variable to the context for the purpose of normalizing the body.
            normalization_context.push(None);

            // Check if the bodies are definitionally equal.
            let bodies_definitionally_equal =
                definitionally_equal(body1.clone(), body2.clone(), normalization_context);

            // Restore the context.
            normalization_context.pop();

            // Return the result.
            bodies_definitionally_equal
        }
        (Pi(_, domain1, codomain1), Pi(_, domain2, codomain2)) => {
            definitionally_equal(domain1.clone(), domain2.clone(), normalization_context) && {
                // Temporarily add the variable to the context for the purpose of normalizing the body.
                normalization_context.push(None);

                // Check if the codomains are definitionally equal.
                let codomains_definitionally_equal = definitionally_equal(
                    codomain1.clone(),
                    codomain2.clone(),
                    normalization_context,
                );

                // Restore the context.
                normalization_context.pop();

                // Return the result.
                codomains_definitionally_equal
            }
        }
        (Application(applicand1, argument1), Application(applicand2, argument2)) => {
            definitionally_equal(
                applicand1.clone(),
                applicand2.clone(),
                normalization_context,
            ) && definitionally_equal(argument1.clone(), argument2.clone(), normalization_context)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        equality::{definitionally_equal, syntactically_equal},
        parser::parse,
        token::TYPE_KEYWORD,
        tokenizer::tokenize,
    };
    use std::rc::Rc;

    #[test]
    fn syntactically_equal_type() {
        let context = [];

        let source1 = TYPE_KEYWORD;
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = TYPE_KEYWORD;
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_equal_variable() {
        let context = ["x"];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_variable() {
        let context = ["x", "y"];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "y";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_lambda() {
        let context = [];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_equal_lambda_inequal_domain() {
        let context = [];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : (type type)) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_lambda_body() {
        let context = [];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) => type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_pi() {
        let context = [];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_pi_domain() {
        let context = [];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : (type type)) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_pi_codomain() {
        let context = [];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) -> type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_application() {
        let context = ["f", "x"];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "f x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_application_applicand() {
        let context = ["f", "x"];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_application_argument() {
        let context = ["f", "x"];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "f f";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_let() {
        let context = [];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_let_definition() {
        let context = [];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = type type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_let_body() {
        let context = [];

        let source1 = "x = type type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = type; type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn definitionally_equal_type() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = TYPE_KEYWORD;
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = TYPE_KEYWORD;
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            true
        );
    }

    #[test]
    fn definitionally_equal_variable() {
        let context = ["x"];
        let mut normalization_context = vec![None, None];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_variable() {
        let context = ["x", "y"];
        let mut normalization_context = vec![None, None];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "y";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_lambda() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            true
        );
    }

    #[test]
    fn definitionally_equal_lambda_inequal_domain() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : (type type)) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_lambda_body() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) => type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_pi() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_pi_domain() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : (type type)) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_pi_codomain() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "(x : type) -> type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_application() {
        let context = ["f", "x"];
        let mut normalization_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "f x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_application_applicand() {
        let context = ["f", "x"];
        let mut normalization_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_application_argument() {
        let context = ["f", "x"];
        let mut normalization_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "f f";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_let() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_let_definition() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = type type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_let_body() {
        let context = [];
        let mut normalization_context = vec![];

        let source1 = "x = type type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = type; type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut normalization_context),
            false
        );
    }
}
