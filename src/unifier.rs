use crate::{
    equality::syntactically_equal,
    format::CodeStr,
    normalizer::normalize_weak_head,
    term::{
        Term,
        Variant::{
            Application, Boolean, Difference, EqualTo, False, GreaterThan, GreaterThanOrEqualTo,
            If, Integer, IntegerLiteral, Lambda, LessThan, LessThanOrEqualTo, Let, Negation, Pi,
            Product, Quotient, Sum, True, Type, Unifier, Variable,
        },
    },
};
use std::{
    cell::RefCell,
    collections::HashSet,
    hash::{Hash, Hasher},
    ptr,
    rc::Rc,
};

// This struct is a "newtype" for `Rc` that implements `Eq` and `Hash` based on the underlying
// pointer, rather than the value being pointed to.
struct HashableRc<T>(Rc<T>);

impl<T> Hash for HashableRc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(&*self.0, state);
    }
}

impl<T> PartialEq for HashableRc<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T> Eq for HashableRc<T> {}

// Unify two terms by updating nested unifiers. Terms are equated up to beta normalization. Type
// annotations are not checked.
#[allow(clippy::similar_names)]
#[allow(clippy::too_many_lines)]
pub fn unify<'a>(
    term1: Rc<Term<'a>>,
    term2: Rc<Term<'a>>,
    definitions_context: &mut Vec<Option<(Rc<Term<'a>>, usize)>>,
) -> bool {
    // The two terms might not have normal forms, but if they are syntactically equal then we can
    // still consider them definitionally equal. So we check for that first.
    if syntactically_equal(&*term1, &*term2) {
        return true;
    }

    // Reduce both terms to weak head normal form.
    let whnf1 = normalize_weak_head(term1, definitions_context);
    let whnf2 = normalize_weak_head(term2, definitions_context);

    // Unify the weak head normal forms.
    match (&whnf1.variant, &whnf2.variant) {
        (Unifier(subterm1), Unifier(subterm2)) if Rc::ptr_eq(subterm1, subterm2) => true,
        (Unifier(subterm1), _) => {
            if subterm1.borrow().is_none() {
                // Occurs check
                let mut unifiers = vec![];
                let mut visited = HashSet::new();
                collect_unifiers(&*whnf2, &mut unifiers, &mut visited);
                if unifiers.iter().any(|unifier| Rc::ptr_eq(unifier, subterm1)) {
                    return false;
                }

                // Unify
                *subterm1.borrow_mut() = Some((*whnf2).clone());

                // We did it!
                true
            } else {
                panic!(
                    "Encountered a non-{} unifier after reduction to weak-head normal form",
                    "None".code_str(),
                );
            }
        }
        (_, Unifier(subterm2)) => {
            if subterm2.borrow().is_none() {
                // Occurs check
                let mut unifiers = vec![];
                let mut visited = HashSet::new();
                collect_unifiers(&*whnf1, &mut unifiers, &mut visited);
                if unifiers.iter().any(|unifier| Rc::ptr_eq(unifier, subterm2)) {
                    return false;
                }

                // Unify
                *subterm2.borrow_mut() = Some((*whnf1).clone());

                // We did it!
                true
            } else {
                panic!(
                    "Encountered a non-{} unifier after reduction to weak-head normal form",
                    "None".code_str(),
                );
            }
        }
        (Type, Type) | (Integer, Integer) | (Boolean, Boolean) | (True, True) | (False, False) => {
            true
        }
        (Variable(_, index1), Variable(_, index2)) => index1 == index2,
        (Lambda(_, _, body1), Lambda(_, _, body2)) => {
            // Temporarily add the variable to the context for the purpose of normalizing the body.
            definitions_context.push(None);

            // Check if the bodies are definitionally equal.
            let bodies_unify = unify(body1.clone(), body2.clone(), definitions_context);

            // Restore the context.
            definitions_context.pop();

            // Return the result.
            bodies_unify
        }
        (Pi(_, domain1, codomain1), Pi(_, domain2, codomain2)) => {
            unify(domain1.clone(), domain2.clone(), definitions_context) && {
                // Temporarily add the variable to the context for the purpose of normalizing the
                // codomain.
                definitions_context.push(None);

                // Check if the codomains are definitionally equal.
                let codomains_unify =
                    unify(codomain1.clone(), codomain2.clone(), definitions_context);

                // Restore the context.
                definitions_context.pop();

                // Return the result.
                codomains_unify
            }
        }
        (Application(applicand1, argument1), Application(applicand2, argument2)) => {
            unify(applicand1.clone(), applicand2.clone(), definitions_context)
                && unify(argument1.clone(), argument2.clone(), definitions_context)
        }
        (IntegerLiteral(integer1), IntegerLiteral(integer2)) => integer1 == integer2,
        (Negation(subterm1), Negation(subterm2)) => {
            unify(subterm1.clone(), subterm2.clone(), definitions_context)
        }
        (Sum(term11, term21), Sum(term12, term22))
        | (Difference(term11, term21), Difference(term12, term22))
        | (Product(term11, term21), Product(term12, term22))
        | (Quotient(term11, term21), Quotient(term12, term22))
        | (LessThan(term11, term21), LessThan(term12, term22))
        | (LessThanOrEqualTo(term11, term21), LessThanOrEqualTo(term12, term22))
        | (EqualTo(term11, term21), EqualTo(term12, term22))
        | (GreaterThan(term11, term21), GreaterThan(term12, term22))
        | (GreaterThanOrEqualTo(term11, term21), GreaterThanOrEqualTo(term12, term22)) => {
            unify(term11.clone(), term12.clone(), definitions_context)
                && unify(term21.clone(), term22.clone(), definitions_context)
        }
        (
            If(condition1, then_branch1, else_branch1),
            If(condition2, then_branch2, else_branch2),
        ) => {
            unify(condition1.clone(), condition2.clone(), definitions_context)
                && unify(
                    then_branch1.clone(),
                    then_branch2.clone(),
                    definitions_context,
                )
                && unify(
                    else_branch1.clone(),
                    else_branch2.clone(),
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
        (Let(_, _), _) | (_, Let(_, _)) => {
            // [ref:let_not_in_weak_head_normal_form]
            panic!("Encountered a let after conversion to weak head normal form.")
        }
    }
}

// This function collects all the unresolved unifiers in a term. The unifiers are deduplicated and
// returned in the order they are first encountered in the term.
fn collect_unifiers<'a>(
    term: &Term<'a>,
    unifiers: &mut Vec<Rc<RefCell<Option<Term<'a>>>>>,
    visited: &mut HashSet<HashableRc<RefCell<Option<Term<'a>>>>>,
) {
    match &term.variant {
        Unifier(unifier) => {
            if let Some(subterm) = &*unifier.borrow() {
                collect_unifiers(subterm, unifiers, visited);
            } else if visited.insert(HashableRc(unifier.clone())) {
                unifiers.push(unifier.clone());
            }
        }
        Type | Variable(_, _) | Integer | IntegerLiteral(_) | Boolean | True | False => {}
        Lambda(_, domain, body) => {
            collect_unifiers(domain, unifiers, visited);
            collect_unifiers(body, unifiers, visited);
        }
        Pi(_, domain, codomain) => {
            collect_unifiers(domain, unifiers, visited);
            collect_unifiers(codomain, unifiers, visited);
        }
        Application(applicand, argument) => {
            collect_unifiers(applicand, unifiers, visited);
            collect_unifiers(argument, unifiers, visited);
        }
        Let(definitions, body) => {
            for (_, annotation, definition) in definitions {
                collect_unifiers(annotation, unifiers, visited);
                collect_unifiers(definition, unifiers, visited);
            }

            collect_unifiers(body, unifiers, visited);
        }
        Negation(subterm) => collect_unifiers(subterm, unifiers, visited),
        Sum(term1, term2)
        | Difference(term1, term2)
        | Product(term1, term2)
        | Quotient(term1, term2)
        | LessThan(term1, term2)
        | LessThanOrEqualTo(term1, term2)
        | EqualTo(term1, term2)
        | GreaterThan(term1, term2)
        | GreaterThanOrEqualTo(term1, term2) => {
            collect_unifiers(term1, unifiers, visited);
            collect_unifiers(term2, unifiers, visited);
        }
        If(condition, then_branch, else_branch) => {
            collect_unifiers(condition, unifiers, visited);
            collect_unifiers(then_branch, unifiers, visited);
            collect_unifiers(else_branch, unifiers, visited);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::parse,
        tokenizer::tokenize,
        type_checker::type_check,
        unifier::{collect_unifiers, unify},
    };
    use std::{collections::HashSet, rc::Rc};

    #[test]
    fn unify_unifier_left() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "_";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_unifier_right() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "type";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "_";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_type() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "type";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_variable() {
        let parsing_context = ["x"];
        let mut definitions_context = vec![None, None];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_variable() {
        let parsing_context = ["x", "y"];
        let mut definitions_context = vec![None, None];

        let source1 = "x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "y";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_lambda() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_lambda_inequal_domain() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : (type type)) => x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_lambda_body() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) => x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) => type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_pi() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_pi_domain() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : (type type)) -> x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_failure_pi_codomain() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "(x : type) -> x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "(x : type) -> type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_application() {
        let parsing_context = ["f", "x"];
        let mut definitions_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "f x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_application_applicand() {
        let parsing_context = ["f", "x"];
        let mut definitions_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_failure_application_argument() {
        let parsing_context = ["f", "x"];
        let mut definitions_context = vec![None, None];

        let source1 = "f x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "f f";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_let() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x = type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_let_definition() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x = type type; x";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_failure_let_body() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "x = type; x";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "x = type; type type";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_integer() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "int";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "int";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_integer_literal() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "42";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_integer_literal() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "43";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_negation() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "-42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "-42";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_negation() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "-42";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "-43";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_sum() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_sum() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 + 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3 + 4";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_difference() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "1";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_difference() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 - 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3 - 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_product() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "2 * 3";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "6";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_product() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 * 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3 * 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_quotient() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 / 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "1";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_quotient() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 / 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3 / 2";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_less_than() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 < 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_less_than() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 < 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_less_than_or_equal_to() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 <= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_less_than_or_equal_to() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 <= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_equal_to() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 == 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_equal_to() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 == 1";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_greater_than() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 > 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "true";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_greater_than() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 > 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "true";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_greater_than_or_equal_to() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "3 >= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "true";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_failure_greater_than_or_equal_to() {
        let parsing_context = [];
        let mut definitions_context = vec![None, None];

        let source1 = "1 >= 2";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "true";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            false,
        );
    }

    #[test]
    fn unify_boolean() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "bool";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "bool";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_true() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "true";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "true";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_false() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "false";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "false";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_if_true() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "if true then 1 + 2 else 3 + 4";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "3";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn unify_if_false() {
        let parsing_context = [];
        let mut definitions_context = vec![];

        let source1 = "if false then 1 + 2 else 3 + 4";
        let tokens1 = tokenize(None, source1).unwrap();
        let term1 = parse(None, source1, &tokens1[..], &parsing_context[..]).unwrap();

        let source2 = "7";
        let tokens2 = tokenize(None, source2).unwrap();
        let term2 = parse(None, source2, &tokens2[..], &parsing_context[..]).unwrap();

        assert_eq!(
            unify(Rc::new(term1), Rc::new(term2), &mut definitions_context),
            true,
        );
    }

    #[test]
    fn collect_unifiers_unifier_no_deduplication() {
        let parsing_context = [];

        let source = "(x => x) _";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert_eq!(unifiers.len(), 2);
    }

    #[test]
    fn collect_unifiers_unifier_deduplication() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];

        let source = "(x => x) _";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();
        let _ = type_check(
            None,
            source,
            &term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert_eq!(unifiers.len(), 1);
    }

    #[test]
    fn collect_unifiers_unresolved() {
        let parsing_context = [];

        let source = "x => x + 3";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert_eq!(unifiers.len(), 1);
    }

    #[test]
    fn collect_unifiers_resolved() {
        let parsing_context = [];
        let mut typing_context = vec![];
        let mut definitions_context = vec![];

        let source = "x => x + 3";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();
        let _ = type_check(
            None,
            source,
            &term,
            &mut typing_context,
            &mut definitions_context,
        )
        .unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert_eq!(unifiers.len(), 0);
    }

    #[test]
    fn collect_unifiers_type() {
        let parsing_context = [];

        let source = "type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_variable() {
        let parsing_context = ["x"];

        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_lambda() {
        let parsing_context = [];

        let source = "x => x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert_eq!(unifiers.len(), 1);
    }

    #[test]
    fn collect_unifiers_annotated_lambda() {
        let parsing_context = [];

        let source = "(x : type) => x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_pi() {
        let parsing_context = [];

        let source = "(x : type) -> x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_application() {
        let parsing_context = ["f", "x"];

        let source = "f x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_let() {
        let parsing_context = [];

        let source = "x = type; x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert_eq!(unifiers.len(), 1);
    }

    #[test]
    fn collect_unifiers_integer() {
        let parsing_context = [];

        let source = "int";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_integer_literal() {
        let parsing_context = [];

        let source = "42";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_negation() {
        let parsing_context = [];

        let source = "-2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_sum() {
        let parsing_context = [];

        let source = "1 + 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_difference() {
        let parsing_context = [];

        let source = "1 - 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_product() {
        let parsing_context = [];

        let source = "1 * 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_quotient() {
        let parsing_context = [];

        let source = "1 / 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_less_than() {
        let parsing_context = [];

        let source = "1 < 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_less_than_or_equal_to() {
        let parsing_context = [];

        let source = "1 <= 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_equal_to() {
        let parsing_context = [];

        let source = "1 == 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_greater_than() {
        let parsing_context = [];

        let source = "1 > 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_greater_than_or_equal_to() {
        let parsing_context = [];

        let source = "1 >= 2";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_boolean() {
        let parsing_context = [];

        let source = "bool";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_true() {
        let parsing_context = [];

        let source = "true";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_false() {
        let parsing_context = [];

        let source = "false";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }

    #[test]
    fn collect_unifiers_if() {
        let parsing_context = [];

        let source = "if true then 0 else 1";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        let mut unifiers = vec![];
        let mut visited = HashSet::new();
        collect_unifiers(&term, &mut unifiers, &mut visited);

        assert!(unifiers.is_empty());
    }
}