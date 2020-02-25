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
        (Let(definitions1, body1), Let(definitions2, body2)) => {
            definitions1.len() == definitions2.len()
                && definitions1.iter().zip(definitions2.iter()).fold(
                    true,
                    |acc, ((_, definition1, _), (_, definition2, _))| {
                        acc && syntactically_equal(&**definition1, &**definition2)
                    },
                )
                && syntactically_equal(&**body1, &**body2)
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
        | (_, Let(_, _)) => false,
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

    // Reduce both terms to weak head normal form and recursively check for convertibility.
    match (
        &normalize_weak_head(term1, normalization_context).variant,
        &normalize_weak_head(term2, normalization_context).variant,
    ) {
        (Type, Type) => true,
        (Variable(_, index1), Variable(_, index2)) => index1 == index2,
        (Lambda(_, _, body1), Lambda(_, _, body2)) => {
            // Temporarily add the variable to the context.
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
                // Temporarily add the variable to the context.
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
        (Let(definitions1, body1), Let(definitions2, body2)) => {
            if definitions1.len() == definitions2.len() {
                // Check if the definitions are definitionally equal.
                let mut definitions_equal = true;
                for ((_, definition1, _), (_, definition2, _)) in
                    definitions1.iter().zip(definitions2.iter())
                {
                    // Check if the definitions are definitionally equal.
                    definitions_equal = definitions_equal
                        && definitionally_equal(
                            definition1.clone(),
                            definition2.clone(),
                            normalization_context,
                        );

                    if !definitions_equal {
                        break;
                    }

                    // Temporarily add the variable to the context.
                    normalization_context.push(Some(definition1.clone()));
                }

                // Check if the bodies are definitionally equal.
                let bodies_definitionally_equal =
                    definitionally_equal(body1.clone(), body2.clone(), normalization_context);

                // Restore the context.
                for _ in definitions1 {
                    normalization_context.pop();
                }

                definitions_equal && bodies_definitionally_equal
            } else {
                false
            }
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
        | (_, Let(_, _)) => false,
    }
}
