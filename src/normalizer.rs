use crate::{
    de_bruijn::{open, shift},
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::{convert::TryFrom, rc::Rc};

// This function reduces a term to weak head normal form using normal order reduction. Invariant:
// When this function is finished, the context is left unmodified.
pub fn normalize_weak_head<'a>(
    term: Rc<Term<'a>>,
    normalization_context: &mut Vec<Option<Rc<Term<'a>>>>,
) -> Rc<Term<'a>> {
    match &term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) => {
            // These cases are already in beta normal form.
            term
        }
        Variable(_, index) => {
            // Look up the definition in the context. Here we rely on the invariant that `index` is
            // non-negative (otherwise the program will panic).
            match &normalization_context
                [normalization_context.len() - usize::try_from(*index).unwrap()]
            {
                Some(definition) => {
                    // Shift the definition so it's valid in the current context and then normalize
                    // it.
                    normalize_weak_head(shift(definition.clone(), 1, *index), normalization_context)
                }
                None => {
                    // The variable doesn't have a definition. Just return it as a "neutral term".
                    term
                }
            }
        }
        Application(applicand, argument) => {
            // Reduce the applicand.
            let normalized_applicand =
                normalize_weak_head(applicand.clone(), normalization_context);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, body) = &normalized_applicand.variant {
                // Perform beta reduction and normalize the result.
                normalize_weak_head(
                    open(body.clone(), 1, argument.clone()),
                    normalization_context,
                )
            } else {
                // We didn't get a lambda. We're done here.
                Rc::new(Term {
                    source_range: term.source_range,
                    variant: Application(normalized_applicand, argument.clone()),
                })
            }
        }
        Let(definitions, body) => {
            // Add the definitions to the context.
            for (_, definition, _) in definitions {
                normalization_context.push(Some(definition.clone()));
            }

            // Reduce the body.
            let normalized_body = normalize_weak_head(body.clone(), normalization_context);

            // Restore the context.
            for _ in definitions {
                normalization_context.pop();
            }

            // Return the result.
            Rc::new(Term {
                source_range: term.source_range,
                variant: Let(definitions.clone(), normalized_body),
            })
        }
    }
}
