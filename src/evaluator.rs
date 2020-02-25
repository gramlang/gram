use crate::{
    de_bruijn::open,
    format::CodeStr,
    term::{
        Term,
        Variant::{Application, Lambda, Let, Pi, Type, Variable},
    },
};
use std::rc::Rc;

// This function evaluates a term using a call-by-value strategy. The term is assumed to be
// well-typed. Runtime type errors will result in panicking.
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
        Let(_, definition, body) => {
            // Eagerly evaluate the definition, open the body, and continue evaluating.
            evaluate(open(body.clone(), 0, evaluate(definition.clone())))
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
        token::TYPE_KEYWORD,
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
                source_range: Some((0, TYPE_KEYWORD.len())),
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
        let source = "x = type; ((y : type) => y) x";
        let tokens = tokenize(None, source).unwrap();
        let term = parse(None, source, &tokens[..], &[]).unwrap();

        assert_eq!(
            *evaluate(Rc::new(term)),
            Term {
                source_range: Some((4, 8)),
                variant: Type,
            },
        );
    }
}
