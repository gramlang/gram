use crate::{
    de_bruijn::signed_shift,
    term::{
        Term,
        Variant::{
            Application, Boolean, Difference, EqualTo, False, GreaterThan, GreaterThanOrEqualTo,
            If, Integer, IntegerLiteral, Lambda, LessThan, LessThanOrEqualTo, Let, Negation, Pi,
            Product, Quotient, Sum, True, Type, Unifier, Variable,
        },
    },
};
use std::rc::Rc;

// Check if two terms are equal up to alpha conversion. Type annotations are not checked.
#[allow(clippy::similar_names)]
#[allow(clippy::too_many_lines)]
pub fn syntactically_equal<'a>(term1: &Term<'a>, term2: &Term<'a>) -> bool {
    // Follow unifiers in the first term.
    let mut term1 = term1.clone();
    loop {
        term1 = if let Unifier(subterm, subterm_shift) = &term1.variant {
            // We `clone` the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            let borrow = { subterm.borrow().clone() };

            if let Ok(subterm) = borrow {
                if let Some(shifted_term) = signed_shift(&subterm, 0, *subterm_shift) {
                    shifted_term
                } else {
                    // The `signed_shift` failed. This means the term is malformed. The error will
                    // be reported during type checking.
                    return false;
                }
            } else {
                break;
            }
        } else {
            break;
        };
    }

    // Follow unifiers in the second term.
    let mut term2 = term2.clone();
    loop {
        term2 = if let Unifier(subterm, subterm_shift) = &term2.variant {
            // We `clone` the borrowed `subterm` to avoid holding the dynamic borrow for too long.
            let borrow = { subterm.borrow().clone() };

            if let Ok(subterm) = borrow {
                if let Some(shifted_term) = signed_shift(&subterm, 0, *subterm_shift) {
                    shifted_term
                } else {
                    // The `signed_shift` failed. This means the term is malformed. The error will
                    // be reported during type checking.
                    return false;
                }
            } else {
                break;
            }
        } else {
            break;
        };
    }

    // Compare the two terms structurally.
    match (&term1.variant, &term2.variant) {
        (Unifier(subterm1, subterm_shift1), Unifier(subterm2, subterm_shift2)) => {
            Rc::ptr_eq(subterm1, subterm2) && subterm_shift1 == subterm_shift2
        }
        (Type, Type) | (Integer, Integer) | (Boolean, Boolean) | (True, True) | (False, False) => {
            true
        }
        (Variable(_, index1), Variable(_, index2)) => index1 == index2,
        (Lambda(_, _, body1), Lambda(_, _, body2)) => syntactically_equal(body1, body2),
        (Pi(_, domain1, codomain1), Pi(_, domain2, codomain2)) => {
            syntactically_equal(domain1, domain2) && syntactically_equal(codomain1, codomain2)
        }
        (Application(applicand1, argument1), Application(applicand2, argument2)) => {
            syntactically_equal(applicand1, applicand2) && syntactically_equal(argument1, argument2)
        }
        (Let(definitions1, body1), Let(definitions2, body2)) => {
            definitions1.len() == definitions2.len()
                && definitions1.iter().zip(definitions2.iter()).fold(
                    true,
                    |acc, ((_, _, definition1), (_, _, definition2))| {
                        acc && syntactically_equal(definition1, definition2)
                    },
                )
                && syntactically_equal(body1, body2)
        }
        (IntegerLiteral(integer1), IntegerLiteral(integer2)) => integer1 == integer2,
        (Negation(subterm1), Negation(subterm2)) => syntactically_equal(subterm1, subterm2),
        (Sum(term11, term21), Sum(term12, term22))
        | (Difference(term11, term21), Difference(term12, term22))
        | (Product(term11, term21), Product(term12, term22))
        | (Quotient(term11, term21), Quotient(term12, term22))
        | (LessThan(term11, term21), LessThan(term12, term22))
        | (LessThanOrEqualTo(term11, term21), LessThanOrEqualTo(term12, term22))
        | (EqualTo(term11, term21), EqualTo(term12, term22))
        | (GreaterThan(term11, term21), GreaterThan(term12, term22))
        | (GreaterThanOrEqualTo(term11, term21), GreaterThanOrEqualTo(term12, term22)) => {
            syntactically_equal(term11, term12) && syntactically_equal(term21, term22)
        }
        (
            If(condition1, then_branch1, else_branch1),
            If(condition2, then_branch2, else_branch2),
        ) => {
            syntactically_equal(condition1, condition2)
                && syntactically_equal(then_branch1, then_branch2)
                && syntactically_equal(else_branch1, else_branch2)
        }
        (Unifier(_, _), _)
        | (_, Unifier(_, _))
        | (Variable(_, _), _)
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
        | (Negation(_), _)
        | (_, Negation(_))
        | (Sum(_, _), _)
        | (_, Sum(_, _))
        | (Difference(_, _), _)
        | (_, Difference(_, _))
        | (Product(_, _), _)
        | (_, Product(_, _))
        | (Quotient(_, _), _)
        | (_, Quotient(_, _))
        | (LessThan(_, _), _)
        | (_, LessThan(_, _))
        | (LessThanOrEqualTo(_, _), _)
        | (_, LessThanOrEqualTo(_, _))
        | (EqualTo(_, _), _)
        | (_, EqualTo(_, _))
        | (GreaterThan(_, _), _)
        | (_, GreaterThan(_, _))
        | (GreaterThanOrEqualTo(_, _), _)
        | (_, GreaterThanOrEqualTo(_, _))
        | (Boolean, _)
        | (_, Boolean)
        | (True, _)
        | (_, True)
        | (False, _)
        | (_, False)
        | (If(_, _, _), _)
        | (_, If(_, _, _)) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        equality::syntactically_equal,
        parser::parse,
        term::{
            Term,
            Variant::{Unifier, Variable},
        },
        tokenizer::tokenize,
    };
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn syntactically_equal_unifier_left() {
        let term1 = Term {
            source_range: None,
            variant: Unifier(
                Rc::new(RefCell::new(Ok(Term {
                    source_range: None,
                    variant: Variable("x", 0),
                }))),
                5,
            ),
        };

        let term2 = Term {
            source_range: None,
            variant: Variable("x", 5),
        };

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_unifier_right() {
        let term1 = Term {
            source_range: None,
            variant: Variable("x", 5),
        };

        let term2 = Term {
            source_range: None,
            variant: Unifier(
                Rc::new(RefCell::new(Ok(Term {
                    source_range: None,
                    variant: Variable("x", 0),
                }))),
                5,
            ),
        };

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_unifier_same_pointer_same_shift() {
        let rc = Rc::new(RefCell::new(Err(0)));

        let term1 = Term {
            source_range: None,
            variant: Unifier(rc.clone(), 5),
        };

        let term2 = Term {
            source_range: None,
            variant: Unifier(rc, 5),
        };

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_unifier_same_pointer_different_shift() {
        let rc = Rc::new(RefCell::new(Err(0)));

        let term1 = Term {
            source_range: None,
            variant: Unifier(rc.clone(), 5),
        };

        let term2 = Term {
            source_range: None,
            variant: Unifier(rc, 6),
        };

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_unifier_different_pointer_same_shift() {
        let term1 = Term {
            source_range: None,
            variant: Unifier(Rc::new(RefCell::new(Err(0))), 5),
        };

        let term2 = Term {
            source_range: None,
            variant: Unifier(Rc::new(RefCell::new(Err(0))), 5),
        };

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_unifier_explicit() {
        let context = [];

        let source1 = "_";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "_";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_type() {
        let context = [];

        let source1 = "type";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_integer() {
        let context = [];

        let source1 = "int";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "int";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_integer_literal() {
        let context = [];

        let source1 = "42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "43";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_negation() {
        let context = [];

        let source1 = "-42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "-42";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_negation() {
        let context = [];

        let source1 = "-42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "-43";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_sum_term1() {
        let context = [];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 + 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_sum_term2() {
        let context = [];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 + 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
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

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_difference_term1() {
        let context = [];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 - 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_difference_term2() {
        let context = [];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 - 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_product() {
        let context = [];

        let source1 = "2 * 3";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "2 * 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_product_term1() {
        let context = [];

        let source1 = "1 * 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 * 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_product_term2() {
        let context = [];

        let source1 = "1 * 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 * 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_quotient() {
        let context = [];

        let source1 = "1 / 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 / 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_quotient_term1() {
        let context = [];

        let source1 = "1 / 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 / 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_quotient_term2() {
        let context = [];

        let source1 = "4 / 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "4 / 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_less_than() {
        let context = [];

        let source1 = "2 < 3";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "2 < 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_less_than_term1() {
        let context = [];

        let source1 = "1 < 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 < 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_less_than_term2() {
        let context = [];

        let source1 = "1 < 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 < 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_less_than_or_equal_to() {
        let context = [];

        let source1 = "2 <= 3";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "2 <= 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_less_than_or_equal_to_term1() {
        let context = [];

        let source1 = "1 <= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 <= 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_less_than_or_equal_to_term2() {
        let context = [];

        let source1 = "1 <= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 <= 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_equal_to() {
        let context = [];

        let source1 = "2 == 3";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "2 == 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_equal_to_term1() {
        let context = [];

        let source1 = "1 == 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 == 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_equal_to_term2() {
        let context = [];

        let source1 = "1 == 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 == 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_greater_than() {
        let context = [];

        let source1 = "2 > 3";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "2 > 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_greater_than_term1() {
        let context = [];

        let source1 = "1 > 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 > 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_greater_than_term2() {
        let context = [];

        let source1 = "1 > 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 > 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_greater_than_or_equal_to() {
        let context = [];

        let source1 = "2 >= 3";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "2 >= 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_greater_than_or_equal_to_term1() {
        let context = [];

        let source1 = "1 >= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "3 >= 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_greater_than_or_equal_to_term2() {
        let context = [];

        let source1 = "1 >= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "1 >= 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_boolean() {
        let context = [];

        let source1 = "bool";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "bool";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_true() {
        let context = [];

        let source1 = "true";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "true";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_false() {
        let context = [];

        let source1 = "false";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_equal_if() {
        let context = [];

        let source1 = "if true then 1 else 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "if true then 1 else 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_if_condition() {
        let context = [];

        let source1 = "if true then 1 else 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "if false then 1 else 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_if_then_branch() {
        let context = [];

        let source1 = "if true then 1 else 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "if true then 3 else 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }

    #[test]
    fn syntactically_inequal_if_else_branch() {
        let context = [];

        let source1 = "if true then 1 else 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &context[..]).unwrap();

        let source2 = "if true then 1 else 3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &context[..]).unwrap();

        assert!(!syntactically_equal(&term1, &term2));
    }
}
