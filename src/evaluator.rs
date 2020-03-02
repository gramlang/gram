use crate::{
    de_bruijn::{open, shift},
    format::CodeStr,
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::{collections::HashSet, rc::Rc};

// This function evaluates a term using a call-by-value strategy. The term is assumed to be
// well-typed in the empty context. Runtime type errors will result in panicking.
pub fn evaluate<'a>(mut term: Rc<Term<'a>>) -> Rc<Term<'a>> {
    // Repeatedly perform small steps for as long as possible.
    while let Some(stepped_term) = step(&term) {
        term = stepped_term;
    }

    // Check if we got a value.
    if is_value(&*term) {
        term
    } else {
        panic!(format!(
            "Evaluation of {} is stuck!",
            term.to_string().code_str(),
        ))
    }
}

// This function implements a call-by-value operational semantics by performing a "small step".
// Call this repeatedly to evaluate a term. The term is assumed to be well-typed in the empty
// context. Runtime type errors will result in panicking.
#[allow(clippy::too_many_lines)]
fn step<'a>(term: &Rc<Term<'a>>) -> Option<Rc<Term<'a>>> {
    match &term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) | Variable(_, _) => None,
        Application(applicand, argument) => {
            // Try to step the applicand.
            if let Some(stepped_applicand) = step(applicand) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Application(stepped_applicand, argument.clone()),
                }));
            };

            // Ensure the applicand is a value.
            if !is_value(applicand) {
                return None;
            }

            // Try to step the argument.
            if let Some(stepped_argument) = step(argument) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Application(applicand.clone(), stepped_argument),
                }));
            };

            // Ensure the argument is a value.
            if !is_value(argument) {
                return None;
            }

            // Check if the applicand is a lambda.
            if let Lambda(_, _, body) = &applicand.variant {
                // We got a lambda. Perform beta reduction and continue evaluating.
                Some(open(body.clone(), 0, argument.clone()))
            } else {
                // We didn't get a lambda. We're stuck!
                None
            }
        }
        Let(definitions, body) => {
            // Try to step one of the definitions.
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

                    // Determine which definitions are actually needed.
                    let mut live_definitions = HashSet::new();
                    get_live_definitions(
                        &substituted_definitions,
                        &Rc::new(Term {
                            source_range: None,
                            variant: Variable(substituted_definitions[j].0, 0),
                        }),
                        substituted_definitions.len(),
                        &mut live_definitions,
                    );

                    // Unfold the definition.
                    let unfolded_definition = Rc::new(Term {
                        source_range: None,
                        variant: Let(
                            substituted_definitions[i..]
                                .iter()
                                .enumerate()
                                .map(|(k, (variable, annotation, definition))| {
                                    if live_definitions.contains(&(i + k)) {
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
                                    } else {
                                        // The definition isn't needed. Replace it with an
                                        // arbitrary value to avoid wasting (potentially infinite)
                                        // time evaluating it.
                                        (
                                            *variable,
                                            None,
                                            Rc::new(Term {
                                                source_range: None,
                                                variant: Type,
                                            }),
                                        )
                                    }
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

                // Try to step the definition.
                if let Some(stepped_definition) = step(&substituted_definitions[i].2) {
                    return Some(Rc::new(Term {
                        source_range: None,
                        variant: Let(
                            definitions
                                .iter()
                                .enumerate()
                                .map(|(j, (variable, annotation, definition))| {
                                    if i == j {
                                        (*variable, annotation.clone(), stepped_definition.clone())
                                    } else {
                                        (*variable, annotation.clone(), definition.clone())
                                    }
                                })
                                .collect(),
                            body.clone(),
                        ),
                    }));
                };

                // Here is the fully stepped definition.
                let stepped_definition = substituted_definitions[i].2.clone();

                // Ensure the definition is a value.
                if !is_value(&stepped_definition) {
                    return None;
                }

                // Substitute the value in subsequent definitions.
                for definition in substituted_definitions.iter_mut().skip(i + 1) {
                    definition.2 = open(definition.2.clone(), i_index, stepped_definition.clone());
                }

                // Substitute the value in the body.
                substituted_body = open(substituted_body.clone(), i_index, stepped_definition);
            }

            // Try to step the body.
            if let Some(stepped_body) = step(&substituted_body) {
                return Some(Rc::new(Term {
                    source_range: None,
                    variant: Let(substituted_definitions, stepped_body.clone()),
                }));
            };

            // Ensure the body is a value.
            if !is_value(&substituted_body) {
                return None;
            }

            // Return the substituted body.
            Some(substituted_body)
        }
    }
}

// This function returns whether a term is a value. Note that a neutral term (e.g., a variable) is
// not considered a value.
fn is_value<'a>(term: &Term<'a>) -> bool {
    match term.variant {
        Type | Lambda(_, _, _) | Pi(_, _, _) => true,
        Variable(_, _) | Application(_, _) | Let(_, _) => false,
    }
}

// Given a set of definitions and a term, this function determines which of the definitions are
// actually needed to evaluate the term.
#[allow(clippy::type_complexity)]
fn get_live_definitions<'a>(
    definitions: &[(&'a str, Option<Rc<Term<'a>>>, Rc<Term<'a>>)],
    term: &Rc<Term<'a>>,
    term_depth: usize,
    live_definitions: &mut HashSet<usize>,
) {
    match &term.variant {
        Type | Pi(_, _, _) => {
            // We ignore the contents of pi types since they are not needed for evaluation.
        }
        Variable(_, index) => {
            // Compute this once rather than multiple times.
            let definition_position = term_depth - 1 - index;

            // Check if the variable points to one of the definitions.
            if definition_position < definitions.len() {
                // Check and update `live_definitions`.
                if live_definitions.insert(definition_position) {
                    // Recurse on the definition of the variable.
                    get_live_definitions(
                        definitions,
                        &definitions[definition_position].2,
                        definitions.len(),
                        live_definitions,
                    );
                }
            }
        }
        Lambda(_, _, body) => {
            // Recurse on the body.
            get_live_definitions(definitions, body, definitions.len() + 1, live_definitions);
        }
        Application(applicand, argument) => {
            // Recurse on the applicand.
            get_live_definitions(definitions, applicand, definitions.len(), live_definitions);

            // Recurse on the argument.
            get_live_definitions(definitions, argument, definitions.len(), live_definitions);
        }
        Let(let_definitions, body) => {
            // Recurse on the definitions.
            for (_, _, definition) in let_definitions {
                get_live_definitions(
                    definitions,
                    definition,
                    definitions.len() + let_definitions.len(),
                    live_definitions,
                );
            }

            // Recurse on the body.
            get_live_definitions(
                definitions,
                body,
                definitions.len() + let_definitions.len(),
                live_definitions,
            );
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
