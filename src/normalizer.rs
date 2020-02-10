use crate::{
    de_bruijn::{open, shift},
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::rc::Rc;

// This function reduces a term to normal form using applicative order reduction. Invariants:
// - The types and definitions of the variables in the context are normalized.
// - When this function is finished, the context is left unmodified.
pub fn normalize<'a>(
    term: &Term<'a>,
    normalization_context: &mut Vec<Option<Rc<Term<'a>>>>,
) -> Rc<Term<'a>> {
    // Recursively normalize sub-terms.
    match &term.variant {
        Type => {
            // The type of all types is already in beta normal form.
            Rc::new(term.clone())
        }
        Variable(_, index) => {
            // Look up the definition in the context.
            match &normalization_context[normalization_context.len() - 1 - *index] {
                Some(definition) => {
                    // Shift the definition so it's valid in the current context.
                    shift(&*definition, 0, *index + 1)
                }
                None => {
                    // The variable doesn't have a definition. Just return it as a "neutral term".
                    Rc::new(term.clone())
                }
            }
        }
        Lambda(variable, domain, body) => {
            // Reduce the domain.
            let normalized_domain = normalize(&**domain, normalization_context);

            // Temporarily add the variable's type to the context for the purpose of normalizing
            // the body.
            normalization_context.push(None);

            // Normalize the body.
            let normalized_body = normalize(&**body, normalization_context);

            // Restore the context.
            normalization_context.pop();

            // For lambdas, we simply reduce the domain and body.
            Rc::new(Term {
                source_range: term.source_range,
                group: true, // To ensure the resulting term is still parse-able when printed
                variant: Lambda(variable, normalized_domain, normalized_body),
            })
        }
        Pi(variable, domain, codomain) => {
            // Reduce the domain.
            let normalized_domain = normalize(&**domain, normalization_context);

            // Temporarily add the variable's type to the context for the purpose of normalizing
            // the codomain.
            normalization_context.push(None);

            // Normalize the body.
            let normalized_codomain = normalize(&**codomain, normalization_context);

            // Restore the context.
            normalization_context.pop();

            // For pi types, we simply reduce the domain and codomain.
            Rc::new(Term {
                source_range: term.source_range,
                group: true, // To ensure the resulting term is still parse-able when printed
                variant: Pi(variable, normalized_domain, normalized_codomain),
            })
        }
        Application(applicand, argument) => {
            // Reduce the applicand.
            let normalized_applicand = normalize(&**applicand, normalization_context);

            // Reduce the argument. This means we're doing applicative order reduction.
            let normalized_argument = normalize(&**argument, normalization_context);

            // Check if the applicand reduced to a lambda.
            if let Lambda(_, _, body) = &normalized_applicand.variant {
                // We got a lambda. Perform beta reduction.
                normalize(
                    &open(&**body, 0, &normalized_argument),
                    normalization_context,
                )
            } else {
                // We didn't get a lambda. We're done here.
                Rc::new(Term {
                    source_range: term.source_range,
                    group: true, // To ensure the resulting term is still parse-able when printed
                    variant: Application(normalized_applicand, normalized_argument),
                })
            }
        }
        Let(_, definition, body) => {
            // Reduce the definition.
            let normalized_definition = normalize(&**definition, normalization_context);

            // Temporarily add the variable's type to the context for the purpose of normalizing
            // the body.
            normalization_context.push(Some(normalized_definition.clone()));

            // Normalize the body.
            let normalized_body = normalize(&**body, normalization_context);

            // Restore the context.
            normalization_context.pop();

            // Return the opened body. Since the body has already been normalized, any references
            // to the definition should have already been unfolded. This, opening merely decrements
            // the indices.
            open(&*normalized_body, 0, definition)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        normalizer::normalize,
        parser::parse,
        term::{
            Term,
            Variant::{Application, Lambda, Pi, Type, Variable},
        },
        token::TYPE_KEYWORD,
        tokenizer::tokenize,
    };
    use std::rc::Rc;

    #[test]
    fn normalize_type() {
        let parsing_context = [];
        let mut normalization_context = vec![];
        let source = TYPE_KEYWORD;

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: Some((0, TYPE_KEYWORD.len())),
                group: false,
                variant: Type,
            },
        );
    }

    #[test]
    fn normalize_variable_no_definition() {
        let parsing_context = ["x"];
        let mut normalization_context = vec![None];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: Some((0, 1)),
                group: false,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn normalize_variable_definition() {
        let parsing_context = ["x"];
        let mut normalization_context = vec![Some(Rc::new(Term {
            source_range: None,
            group: false,
            variant: Type,
        }))];
        let source = "x";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: None,
                group: false,
                variant: Type
            },
        );
    }

    #[test]
    fn normalize_redex_under_lambda() {
        let parsing_context = ["p", "q"];
        let mut normalization_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) => ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: Some((0, 48)),
                group: true,
                variant: Lambda(
                    "x",
                    Rc::new(Term {
                        source_range: Some((23, 24)),
                        group: true,
                        variant: Variable("p", 1),
                    }),
                    Rc::new(Term {
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
        let parsing_context = ["p", "q"];
        let mut normalization_context = vec![None, None];
        let source = "(x : ((y : type) => y) p) -> ((z : type) => z) q";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: Some((0, 48)),
                group: true,
                variant: Pi(
                    "x",
                    Rc::new(Term {
                        source_range: Some((23, 24)),
                        group: true,
                        variant: Variable("p", 1),
                    }),
                    Rc::new(Term {
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
        let parsing_context = ["y", "w"];
        let mut normalization_context = vec![None, None];
        let source = "(((x : type) => x) y) (((z : type) => z) w)";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: Some((2, 42)),
                group: true,
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((19, 20)),
                        group: true,
                        variant: Variable("y", 1),
                    }),
                    Rc::new(Term {
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
        let parsing_context = ["y"];
        let mut normalization_context = vec![None];
        let source = "((x : type) => x) y";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: Some((18, 19)),
                group: true,
                variant: Variable("y", 0),
            },
        );
    }

    #[test]
    fn normalize_let() {
        let parsing_context = ["y"];
        let mut normalization_context = vec![None];
        let source = "x = type; x y";

        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &parsing_context[..]).unwrap();

        assert_eq!(
            *normalize(&term, &mut normalization_context),
            Term {
                source_range: Some((10, 13)),
                group: true,
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((4, 8)),
                        group: false,
                        variant: Type,
                    }),
                    Rc::new(Term {
                        source_range: Some((12, 13)),
                        group: false,
                        variant: Variable("y", 0),
                    }),
                ),
            },
        );
    }
}
