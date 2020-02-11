use crate::term::{
    Term,
    Variant::{Application, Lambda, Let, Pi, Type, Variable},
};

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

#[cfg(test)]
mod tests {
    use crate::{
        equality::syntactically_equal, parser::parse, token::TYPE_KEYWORD, tokenizer::tokenize,
    };

    #[test]
    fn syntactically_equal_type() {
        let context1 = [];
        let source1 = TYPE_KEYWORD;

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = TYPE_KEYWORD;

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
    fn syntactically_equal_lambda_inequal_domain() {
        let context1 = [];
        let source1 = "(x : type) => x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "(x : (type type)) => x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
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
    fn syntactically_inequal_pi_codomain() {
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
    fn syntactically_equal_let() {
        let context1 = [];
        let source1 = "x = type; x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "x = type; x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_let_definition() {
        let context1 = [];
        let source1 = "x = type; x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "x = type type; x";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_let_body() {
        let context1 = [];
        let source1 = "x = type type; x";

        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context1[..]).unwrap();

        let context2 = [];
        let source2 = "x = type; type";

        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context2[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }
}
