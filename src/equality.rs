use crate::{
    normalizer::normalize,
    term::{
        Term,
        Variant::{Application, Lambda, Pi, Type, Variable},
    },
};

// Check if two terms are equal up to alpha renaming.
pub fn syntactically_equal<'a>(term1: &Term<'a>, term2: &Term<'a>) -> bool {
    // Recursively check sub-terms.
    match (&term1.variant, &term2.variant) {
        (Type, Type) => true,
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

// Check if two terms are equal up to alpha renaming and beta equivalence.
pub fn definitionally_equal<'a>(term1: &Term<'a>, term2: &Term<'a>) -> bool {
    // Check if the normalized terms are equal.
    syntactically_equal(&normalize(term1), &normalize(term2))
}

#[cfg(test)]
mod tests {
    use crate::{
        equality::{definitionally_equal, syntactically_equal},
        parser::parse,
        token::TYPE,
        tokenizer::tokenize,
    };

    #[test]
    fn syntactically_equal_type() {
        let context1 = [];
        let source1 = TYPE;

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = TYPE;

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_equal_alpha() {
        let context1 = ["x"];
        let source1 = "x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = ["y"];
        let source2 = "y";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_variable() {
        let context1 = ["x"];
        let source1 = "x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = ["x", "y"];
        let source2 = "x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_lambda() {
        let context1 = [];
        let source1 = "(x : type) => x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "(x : type) => x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_lambda_domain() {
        let context1 = [];
        let source1 = "(x : type) => x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "(x : (type type)) => x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_lambda_body() {
        let context1 = [];
        let source1 = "(x : type) => x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "(x : type) => type";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_pi() {
        let context1 = [];
        let source1 = "(x : type) -> x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "(x : type) -> x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_pi_domain() {
        let context1 = [];
        let source1 = "(x : type) -> x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "(x : (type type)) -> x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_pi() {
        let context1 = [];
        let source1 = "(x : type) -> x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "(x : type) -> type";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_application() {
        let context1 = ["f", "x"];
        let source1 = "f x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = ["f", "x"];
        let source2 = "f x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_application_applicand() {
        let context1 = ["f", "x"];
        let source1 = "f x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = ["f", "x"];
        let source2 = "x x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_application_argument() {
        let context1 = ["f", "x"];
        let source1 = "f x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = ["f", "x"];
        let source2 = "f f";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn definitionally_equal_beta() {
        let context1 = ["y"];
        let source1 = "((x : type) => x x) y";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = ["y"];
        let source2 = "y y";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(definitionally_equal(&term1, &term2), true);
    }

    #[test]
    fn definitionally_inequal_beta() {
        let context1 = ["y"];
        let source1 = "((x : type) => x x) y";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = ["y"];
        let source2 = "y";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(definitionally_equal(&term1, &term2), false);
    }
}
