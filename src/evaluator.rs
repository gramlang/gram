use crate::{
    de_bruijn::{open, shift},
    format::CodeStr,
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::rc::Rc;

// This function evaluates a term using a call-by-value strategy. The term is assumed to be
// well-typed in the empty context. Runtime type errors will result in panicking.
pub fn evaluate<'a>(term: Rc<Term<'a>>) -> Rc<Term<'a>> {
    match &term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) => {
            // These cases are already values.
            term
        }
        Variable(variable, _) => {
            // We're stuck!
            panic!(format!(
                "Attempted to evaluate variable {}.",
                (*variable).to_string().code_str()
            ))
        }
        Application(applicand, argument) => {
            // Evaluate the applicand.
            let evaluated_applicand = evaluate(applicand.clone());

            // Evaluate the argument.
            let evaluated_argument = evaluate(argument.clone());

            // Check if the applicand evaluated to a lambda.
            if let Lambda(_, _, body) = &evaluated_applicand.variant {
                // We got a lambda. Perform beta reduction and continue evaluating.
                evaluate(open(body.clone(), 0, evaluated_argument))
            } else {
                // We didn't get a lambda. We're stuck!
                panic!(format!(
                    "Attempted to apply non-lambda term {} to {}.",
                    evaluated_applicand.to_string().code_str(),
                    evaluated_argument.to_string().code_str()
                ))
            }
        }
        Let(definitions, body) => {
            // Evaluate the definitions and substitute them into the body.
            let mut substituted_definitions = definitions.clone();
            let mut substituted_body = body.clone();
            for i in 0..definitions.len() {
                // Compute these once rather than multiple times.
                let i_index = definitions.len() - 1 - i;
                let i_index_plus_1 = i_index + 1;

                // Unfold each remaining definition and substitute it in.
                for j in i..definitions.len() {
                    // Compute this once rather than multiple times.
                    let j_index = definitions.len() - 1 - j;

                    // Unfold the definition.
                    let unfolded_definition = Rc::new(Term {
                        source_range: None,
                        variant: Let(
                            substituted_definitions[i..]
                                .iter()
                                .map(|(variable, annotation, definition)| {
                                    (
                                        *variable,
                                        annotation.as_ref().map(|annotation| {
                                            shift(
                                                annotation.clone(),
                                                definitions.len(),
                                                i_index_plus_1,
                                            )
                                        }),
                                        shift(
                                            definition.clone(),
                                            definitions.len(),
                                            i_index_plus_1,
                                        ),
                                    )
                                })
                                .collect(),
                            Rc::new(Term {
                                source_range: None,
                                variant: Variable(substituted_definitions[j].0, 0),
                            }),
                        ),
                    });

                    // Substitute the unfolded definition.
                    substituted_definitions[i].2 = open(
                        substituted_definitions[i].2.clone(),
                        j_index,
                        unfolded_definition,
                    );
                }

                // Evaluate the definition.
                let evaluated_definition = evaluate(substituted_definitions[i].2.clone());

                // Substitute the value in subsequent definitions.
                for definition in substituted_definitions.iter_mut().skip(i + 1) {
                    definition.2 =
                        open(definition.2.clone(), i_index, evaluated_definition.clone());
                }

                // Substitute the value in the body.
                substituted_body = open(
                    substituted_body.clone(),
                    i_index,
                    evaluated_definition.clone(),
                );
            }

            // Evaluate the body.
            evaluate(substituted_body)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        evaluator::evaluate,
        parser::parse,
        term::{
            Term,
            Variant::{Application, Lambda, Pi, Type, Variable},
        },
        tokenizer::tokenize,
    };
    use std::rc::Rc;

    #[test]
    fn evaluate_type() {
        let source = "type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)),
            Term {
                source_range: Some((0, 4)),
                variant: Type,
            },
        );
    }

    #[test]
    #[should_panic]
    fn evaluate_variable() {
        let context = ["x"];
        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &context[..]).unwrap();

        evaluate(Rc::new(term));
    }

    #[test]
    fn evaluate_lambda() {
        let source = "(f : type -> type) => (x : type) => f x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)),
            Term {
                source_range: Some((0, 39)),
                variant: Lambda(
                    "f",
                    Rc::new(Term {
                        source_range: Some((5, 17)),
                        variant: Pi(
                            "_",
                            Rc::new(Term {
                                source_range: Some((5, 9)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((13, 17)),
                                variant: Type,
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some((22, 39)),
                        variant: Lambda(
                            "x",
                            Rc::new(Term {
                                source_range: Some((27, 31)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((36, 39)),
                                variant: Application(
                                    Rc::new(Term {
                                        source_range: Some((36, 37)),
                                        variant: Variable("f", 1),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((38, 39)),
                                        variant: Variable("x", 0),
                                    }),
                                ),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn evaluate_pi() {
        let source = "(f : type -> type) -> (x : type) -> f x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)),
            Term {
                source_range: Some((0, 39)),
                variant: Pi(
                    "f",
                    Rc::new(Term {
                        source_range: Some((5, 17)),
                        variant: Pi(
                            "_",
                            Rc::new(Term {
                                source_range: Some((5, 9)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((13, 17)),
                                variant: Type,
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some((22, 39)),
                        variant: Pi(
                            "x",
                            Rc::new(Term {
                                source_range: Some((27, 31)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((36, 39)),
                                variant: Application(
                                    Rc::new(Term {
                                        source_range: Some((36, 37)),
                                        variant: Variable("f", 1),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((38, 39)),
                                        variant: Variable("x", 0),
                                    }),
                                ),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn evaluate_redex() {
        let source = "((x : type) => x) type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)),
            Term {
                source_range: Some((18, 22)),
                variant: Type,
            },
        );
    }

    #[test]
    #[should_panic]
    fn evaluate_non_redex() {
        let source = "type type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        evaluate(Rc::new(term));
    }

    #[test]
    fn evaluate_let() {
        let source = "f = (x : type) => g x; g = (x : type) => x; f type";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)),
            Term {
                source_range: Some((46, 50)),
                variant: Type
            },
        );
    }
}
