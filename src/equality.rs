use crate::{
    normalizer::normalize_weak_head,
    term::{
        Term,
        Variant::{
            Application, Difference, Integer, IntegerLiteral, Lambda, Let, Pi, Sum, Type, Variable,
        },
    },
};
use std::rc::Rc;

// Check if two terms are equal up to alpha conversion. Type annotations are not checked.
pub fn syntactically_equal<'a>(term1: &Term<'a>, term2: &Term<'a>) -> bool {
    match (&term1.variant, &term2.variant) {
        (Type, Type) | (Integer, Integer) => true,
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
        (Let(definitions1, body1), Let(definitions2, body2)) => {
            definitions1.len() == definitions2.len()
                && definitions1.iter().zip(definitions2.iter()).fold(
                    true,
                    |acc, ((_, annotation1, definition1), (_, annotation2, definition2))| {
                        acc && (match (annotation1, annotation2) {
                            (Some(annotation1), Some(annotation2)) => {
                                syntactically_equal(&**annotation1, &**annotation2)
                            }
                            (None, None) => true,
                            _ => false,
                        }) && syntactically_equal(&**definition1, &**definition2)
                    },
                )
                && syntactically_equal(&**body1, &**body2)
        }
        (IntegerLiteral(integer1), IntegerLiteral(integer2)) => integer1 == integer2,
        (Sum(summand11, summand12), Sum(summand21, summand22)) => {
            syntactically_equal(&**summand11, &**summand21)
                && syntactically_equal(&**summand12, &**summand22)
        }
        (Difference(minuend1, subtrahend1), Difference(minuend2, subtrahend2)) => {
            syntactically_equal(&**minuend1, &**minuend2)
                && syntactically_equal(&**subtrahend1, &**subtrahend2)
        }
        (Variable(_, _), _)
        | (_, Variable(_, _))
        | (Lambda(_, _, _), _)
        | (_, Lambda(_, _, _))
        | (Pi(_, _, _), _)
        | (_, Pi(_, _, _))
        | (Application(_, _), _)
        | (_, Application(_, _))
        | (Let(_, _), _)
        | (_, Let(_, _))
        | (Integer, _)
        | (_, Integer)
        | (IntegerLiteral(_), _)
        | (_, IntegerLiteral(_))
        | (Sum(_, _), _)
        | (_, Sum(_, _))
        | (Difference(_, _), _)
        | (_, Difference(_, _)) => false,
    }
}

// Check if two terms are convertible up to beta normalization. Type annotations are not checked.
pub fn definitionally_equal<'a>(
    term1: Rc<Term<'a>>,
    term2: Rc<Term<'a>>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
) -> bool {
    // The two terms might not have normal forms, but if they are syntactically equal then we can
    // still consider them definitionally equal. So we check for that first.
    if syntactically_equal(&*term1, &*term2) {
        return true;
    }

    // Reduce both terms to weak head normal form and recursively check for convertibility.
    match (
        &normalize_weak_head(term1, definitions_context).variant,
        &normalize_weak_head(term2, definitions_context).variant,
    ) {
        (Type, Type) | (Integer, Integer) => true,
        (Variable(_, index1), Variable(_, index2)) => index1 == index2,
        (Lambda(_, _, body1), Lambda(_, _, body2)) => {
            // Temporarily add the variable to the context for the purpose of normalizing the body.
            definitions_context.push(None);

            // Check if the bodies are definitionally equal.
            let bodies_definitionally_equal =
                definitionally_equal(body1.clone(), body2.clone(), definitions_context);

            // Restore the context.
            definitions_context.pop();

            // Return the result.
            bodies_definitionally_equal
        }
        (Pi(_, domain1, codomain1), Pi(_, domain2, codomain2)) => {
            definitionally_equal(domain1.clone(), domain2.clone(), definitions_context) && {
                // Temporarily add the variable to the context for the purpose of normalizing the
                // codomain.
                definitions_context.push(None);

                // Check if the codomains are definitionally equal.
                let codomains_definitionally_equal =
                    definitionally_equal(codomain1.clone(), codomain2.clone(), definitions_context);

                // Restore the context.
                definitions_context.pop();

                // Return the result.
                codomains_definitionally_equal
            }
        }
        (Application(applicand1, argument1), Application(applicand2, argument2)) => {
            definitionally_equal(applicand1.clone(), applicand2.clone(), definitions_context)
                && definitionally_equal(argument1.clone(), argument2.clone(), definitions_context)
        }
        (IntegerLiteral(integer1), IntegerLiteral(integer2)) => integer1 == integer2,
        (Sum(summand11, summand12), Sum(summand21, summand22)) => {
            definitionally_equal(summand11.clone(), summand21.clone(), definitions_context)
                && definitionally_equal(summand12.clone(), summand22.clone(), definitions_context)
        }
        (Difference(minuend1, subtrahend1), Difference(minuend2, subtrahend2)) => {
            definitionally_equal(minuend1.clone(), minuend2.clone(), definitions_context)
                && definitionally_equal(
                    subtrahend1.clone(),
                    subtrahend2.clone(),
                    definitions_context,
                )
        }
        (Variable(_, _), _)
        | (_, Variable(_, _))
        | (Lambda(_, _, _), _)
        | (_, Lambda(_, _, _))
        | (Pi(_, _, _), _)
        | (_, Pi(_, _, _))
        | (Application(_, _), _)
        | (_, Application(_, _))
        | (Integer, _)
        | (_, Integer)
        | (IntegerLiteral(_), _)
        | (_, IntegerLiteral(_))
        | (Sum(_, _), _)
        | (_, Sum(_, _))
        | (Difference(_, _), _)
        | (_, Difference(_, _)) => false,
        (Let(_, _), _) | (_, Let(_, _)) => {
            // [ref:let_not_in_weak_head_normal_form]
            panic!("Encountered a let after conversion to weak head normal form.")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        equality::{definitionally_equal, syntactically_equal},
        parser::parse,
        tokenizer::tokenize,
    };
    use std::rc::Rc;

    #[test]
    fn syntactically_equal_type() {
        let context = [];

        let source1 = "type";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "type";
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
        let context = ["f"];

        let source1 = "x = f; y = x; f y";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = f; y = x; f y";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_let_definition() {
        let context = ["f"];

        let source1 = "x = f; y = x; f y";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = f; y = f; f y";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_let_body() {
        let context = ["f"];

        let source1 = "x = f; y = x; f y";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "x = f; y = x; f";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_integer() {
        let context = [];

        let source1 = "integer";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "integer";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_equal_integer_literal() {
        let context = [];

        let source1 = "42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "42";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_integer() {
        let context = [];

        let source1 = "42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "43";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_sum() {
        let context = [];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 + 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_sum_summand1() {
        let context = [];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 + 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_sum_summand2() {
        let context = [];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 + 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_equal_difference() {
        let context = [];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 - 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), true);
    }

    #[test]
    fn syntactically_inequal_difference_minuend() {
        let context = [];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 - 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn syntactically_inequal_difference_subtrahend() {
        let context = [];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 - 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert_eq!(syntactically_equal(&term1, &term2), false);
    }

    #[test]
    fn definitionally_equal_type() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "type";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_equal_variable() {
        let parsing_context = ["x"];
        let mut definitions_context = vec![None, None];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_variable() {
        let parsing_context = ["x", "y"];
        let mut definitions_context = vec![None, None];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "y";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_lambda() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_equal_lambda_inequal_domain() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : (type type)) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_lambda_body() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) => type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_pi() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_pi_domain() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : (type type)) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_pi_codomain() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) -> type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_application() {
        let parsing_context = ["f", "x"];
        let mut definitions_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "f x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_application_applicand() {
        let parsing_context = ["f", "x"];
        let mut definitions_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_application_argument() {
        let parsing_context = ["f", "x"];
        let mut definitions_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "f f";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_let() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x = type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_let_definition() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x = type type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_let_body() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x = type; type type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_integer() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "integer";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "integer";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_equal_integer_literal() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "42";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_integer_literal() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "43";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_sum() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_sum_summand1() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3 + 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_sum_summand2() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "1 + 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_equal_difference() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "1";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true
        );
    }

    #[test]
    fn definitionally_inequal_difference_minuend() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3 - 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }

    #[test]
    fn definitionally_inequal_difference_subtrahend() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "1 - 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            definitionally_equal(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false
        );
    }
}
