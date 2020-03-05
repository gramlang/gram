use crate::{
    de_bruijn::free_variables,
    error::{throw, Error},
    evaluator::is_value,
    format::CodeStr,
    term,
    token::{self, TerminatorType, Token},
};
use num_bigint::BigInt;
use scopeguard::defer;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::Path,
    rc::Rc,
};

// Gram uses a packrat parser, i.e., a recursive descent parser with memoization. This guarantees
// linear-time parsing.
//
// If we naively interpret the abstract syntax as a grammar, we face two problems:
//
//   1. The grammar is left-recursive, and left recursion is not supported by the packrat parsing
//      technique.
//   2. The grammar is ambiguous. For example, the associativity of application has not been
//      specified. Packrat parsers resolve ambiguities using the order of the alternatives in the
//      grammar, but we'd prefer to have an unambiguous grammar in the first place.
//
// To address the ambiguity issue (2), we could encode the associativity of various constructs into
// the grammar. This works well for right-associative operators, but for left-associative
// operators it would exacerbate the left-recursion problem (1). To resolve this, we employ a
// trick: we parse all binary operators as right-associative, but then we reassociate the relevant
// constructs in a post-processing step to achieve the desired left-associativity. For example, we
// parse `f x y` as `f [x y]`, then transform it into `[f x] y`. We have to be careful not to
// reassociate operations in which the associativity has already been specified by explicit
// grouping. For example, we should not reassociate `f (x y)`. Thus, when parsing, we record
// whether a term was parsed as group [tag:group_flag].
//
// After resolving ambiguities, we end up with the grammar located in `grammar.y`. This grammar has
// been verified to be unambiguous by Bison. [tag:grammar] [ref:bison_grammar]
//
// There is one more concern to consider: packrat parsers are greedy, so the order in which we try
// the productions matters in some cases. We address this by attempting productions that result in
// potentially longer matches first. For example, when parsing a term, we try to parse it as an
// application before trying to parse it as a variable, since an application may start with a
// variable. The grammar has been manually verified to be amenable to greedy parsing in this way.

// This represents a fresh variable name. It's never added to the context.
pub const PLACEHOLDER_VARIABLE: &str = "_";

// The token stream is parsed into an abstract syntax tree (AST). This struct represents a node in
// an AST. This is similar to `term::Term`, except:
// - It doesn't contain De Bruijn indices.
// - It has a `group` field (see see [ref:group_flag]).
// - It has source ranges for variables.
#[derive(Clone, Debug, Eq, PartialEq)]
struct Term<'a> {
    source_range: (usize, usize), // [start, end)
    group: bool,                  // For an explanation of this field, see [ref:group_flag].
    variant: Variant<'a>,
}

// Each term has a "variant" describing what kind of term it is.
#[derive(Clone, Debug, Eq, PartialEq)]
enum Variant<'a> {
    Type,
    Variable(SourceVariable<'a>),
    Lambda(SourceVariable<'a>, Rc<Term<'a>>, Rc<Term<'a>>),
    Pi(SourceVariable<'a>, Rc<Term<'a>>, Rc<Term<'a>>),
    Application(Rc<Term<'a>>, Rc<Term<'a>>),
    Let(
        SourceVariable<'a>,
        Option<Rc<Term<'a>>>,
        Rc<Term<'a>>,
        Rc<Term<'a>>,
    ),
    Integer,
    IntegerLiteral(BigInt),
    Sum(Rc<Term<'a>>, Rc<Term<'a>>),
    Difference(Rc<Term<'a>>, Rc<Term<'a>>),
    Product(Rc<Term<'a>>, Rc<Term<'a>>),
    Quotient(Rc<Term<'a>>, Rc<Term<'a>>),
    Boolean,
    True,
    False,
    If(Rc<Term<'a>>, Rc<Term<'a>>, Rc<Term<'a>>),
}

// For variables, we store the name of the variable and its source range. The source range allows
// us to report nice errors when there are multiple variables with the same name.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceVariable<'a> {
    source_range: (usize, usize), // [start, end)
    name: &'a str,
}

// When reassociating sums and differences to be left-associative, this enum is used to record how
// each term is used.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum SumOrDifference {
    Sum,
    Difference,
}

// When reassociating products and quotients to be left-associative, this enum is used to record how
// each term is used.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum ProductOrQuotient {
    Product,
    Quotient,
}

// When memoizing a function, we'll use this enum to identify which function is being memoized.
#[derive(Debug, Eq, Hash, PartialEq)]
enum CacheType {
    Term,
    Type,
    Variable,
    Lambda,
    Pi,
    NonDependentPi,
    Application,
    Let,
    Integer,
    IntegerLiteral,
    Sum,
    Difference,
    Product,
    Quotient,
    Boolean,
    True,
    False,
    If,
    Group,
    Atom,
    SmallTerm,
    MediumTerm,
    LargeTerm,
    HugeTerm,
}

// A cache key consists of a `CacheType` indicating which function is being memoized together
// with a position in the input stream.
type CacheKey = (CacheType, usize);

// An `ErrorFactory` is a function which takes a source path and contents and produces an `Error`.
// It's cheaper to generate a closure that produces the `Error` than to generate the actual
// `Error`, which may contain a long string message.
type ErrorFactory<'a, 'b> = Rc<dyn Fn(Option<&'a Path>, &'a str) -> Error + 'b>;

// An `ErrorConfidenceLevel` describes how likely an error is to be the correct error to show to the
// user.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum ErrorConfidenceLevel {
    Low,
    High,
}

// An `ParseError` is a triple containing:
// 1. An `ErrorFactory`, if there is a more useful error message than the default one.
// 2. The position of the token that caused the error. This position will be used to rank errors
//    and choose the "best" one.
// 3. An ErrorConfidenceLevel`. This will be used to rank errors with equal positions.
type ParseError<'a, 'b> = (Option<ErrorFactory<'a, 'b>>, usize, ErrorConfidenceLevel);

// The result of a cache lookup is a pair consisting of the term that was parsed and the position
// of the next unconsumed token, if the parse was successful. Otherwise, the result is `None`.
type CacheResult<'a, 'b> = Option<(Term<'a>, usize)>;

// A cache is a map from cache key to result.
type Cache<'a, 'b> = HashMap<CacheKey, CacheResult<'a, 'b>>;

// This macro should be called at the beginning of every parsing function to do a cache lookup and
// return early on cache hit.
macro_rules! cache_check {
    ($cache:ident, $type:ident, $start:expr, $error:ident $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;

        // Do the cache lookup.
        if let Some(result) = $cache.get(&(CacheType::$type, start)) {
            return (*result).clone();
        }
    }};
}

// This macro caches a value and returns it. In parsing functions, this should always be used
// instead of `return` to ensure results are memoized.
macro_rules! cache_return {
    ($cache:ident, $type:ident, $start:expr, $value:expr $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let value = $value;

        // Cache and return the value.
        $cache.insert((CacheType::$type, start), value.clone());
        return value;
    }};
}

// This macro will return early if `$value` isn't `None`.
macro_rules! try_return {
    ($cache:ident, $type:ident, $start:expr, $value:expr $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let value = $value;

        // Record the match if one hasn't already been found.
        if value.is_some() {
            cache_return!($cache, $type, start, value)
        }
    }};
}

// This macro should be used instead of the `x?` syntax to return early upon the failure of
// `$expr` and cache the result. If `$expr` succeeds, this macro evaluates to its value.
macro_rules! try_eval {
    ($cache:ident, $type:ident, $start:expr, $value:expr $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let value = $value;

        // Extract the result or fail fast.
        if let Some(result) = value {
            result
        } else {
            cache_return!($cache, $type, start, value)
        }
    }};
}

// This macro consumes a single token and evaluates to the position of the next token.
macro_rules! consume_token {
    (
        $cache:ident,
        $type:ident,
        $start:expr,
        $tokens:expr,
        $variant:ident,
        $next:expr,
        $error:ident,
        $confidence:ident $(,)?
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let tokens = $tokens;
        let next = $next;

        // Fail if there are no more tokens to parse.
        if next == tokens.len() {
            if next > $error.1 || (next == $error.1 && ErrorConfidenceLevel::$confidence > $error.2)
            {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            &format!(
                                "Expected {} after {}.",
                                token::Variant::$variant.to_string().code_str(),
                                tokens[next - 1].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next - 1].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                    ErrorConfidenceLevel::$confidence,
                );
            }

            cache_return!($cache, $type, start, None)
        }

        // Check if the token was the expected one.
        if let token::Variant::$variant = tokens[next].variant {
            next + 1
        } else {
            if next > $error.1 || (next == $error.1 && ErrorConfidenceLevel::$confidence > $error.2)
            {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            &format!(
                                "Expected {} but encountered {}.",
                                token::Variant::$variant.to_string().code_str(),
                                tokens[next].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                    ErrorConfidenceLevel::$confidence,
                );
            }

            cache_return!($cache, $type, start, None)
        }
    }};
}

// This macro consumes a single terminator token and evaluates to the position of the next token.
// This is similar to the `consume_token!` macro above, except that macro doesn't work for
// terminator tokens since the terminator variant constructor takes an argument.
macro_rules! consume_terminator {
    (
        $cache:ident,
        $type:ident,
        $start:expr,
        $tokens:expr,
        $next:expr,
        $error:ident,
        $confidence:ident $(,)?
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let tokens = $tokens;
        let next = $next;

        // Fail if there are no more tokens to parse.
        if next == tokens.len() {
            if next > $error.1 || (next == $error.1 && ErrorConfidenceLevel::$confidence > $error.2)
            {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            &format!(
                                "Expected {} after {}.",
                                token::Variant::Terminator(TerminatorType::Semicolon)
                                    .to_string()
                                    .code_str(),
                                tokens[next - 1].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next - 1].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                    ErrorConfidenceLevel::$confidence,
                );
            }

            cache_return!($cache, $type, start, None)
        }

        // Check if the token was the expected one.
        if let token::Variant::Terminator(_) = tokens[next].variant {
            next + 1
        } else {
            if next > $error.1 || (next == $error.1 && ErrorConfidenceLevel::$confidence > $error.2)
            {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            &format!(
                                "Expected {} but encountered {}.",
                                token::Variant::Terminator(TerminatorType::Semicolon)
                                    .to_string()
                                    .code_str(),
                                tokens[next].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                    ErrorConfidenceLevel::$confidence,
                );
            }

            cache_return!($cache, $type, start, None)
        }
    }};
}

// This macro consumes an identifier and evaluates to the identifier paired with the position of
// the next token.
macro_rules! consume_identifier {
    (
        $cache:ident,
        $type:ident,
        $start:expr,
        $tokens:expr,
        $next:expr,
        $error:ident,
        $confidence:ident $(,)?
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let tokens = $tokens;
        let next = $next;

        // Fail if there are no more tokens to parse.
        if next == tokens.len() {
            if next > $error.1 || (next == $error.1 && ErrorConfidenceLevel::$confidence > $error.2)
            {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            &format!(
                                "Expected an identifier after {}.",
                                tokens[next - 1].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next - 1].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                    ErrorConfidenceLevel::$confidence,
                );
            }

            cache_return!($cache, $type, start, None)
        }

        // Check if the token is actually an identifier.
        if let token::Variant::Identifier(identifier) = tokens[next].variant {
            (identifier, next + 1)
        } else {
            if next > $error.1 || (next == $error.1 && ErrorConfidenceLevel::$confidence > $error.2)
            {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            &format!(
                                "Expected an identifier but encountered {}.",
                                tokens[next].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                    ErrorConfidenceLevel::$confidence,
                );
            }

            cache_return!($cache, $type, start, None)
        }
    }};
}

// When an attempt to parse several alternatives fails, none of the errors returned from those
// failed parses are relevant. Instead, we use this function to set a generic error.
fn set_generic_error<'a, 'b>(
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) {
    if error.1 == start && error.2 == ErrorConfidenceLevel::Low {
        // Check if there are any tokens to complain about.
        error.0 = Some(Rc::new(move |source_path, source_contents| {
            if start == tokens.len() {
                throw(
                    "Unexpected end of file.",
                    source_path,
                    source_contents,
                    tokens.last().map_or((0, 0), |token| token.source_range),
                )
            } else {
                throw(
                    &format!("Unexpected {}.", tokens[start].to_string().code_str()),
                    source_path,
                    source_contents,
                    tokens[start].source_range,
                )
            }
        }) as ErrorFactory);
    }
}

// This is the top-level parsing function. All the parsed terms are guaranteed to have a non-`None`
// `source_range`. The parser also guarantees that all variables are bound, except of course the
// ones in the initial context. Variable shadowing is not allowed.
pub fn parse<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    tokens: &[Token<'a>],
    context: &[&'a str],
) -> Result<term::Term<'a>, Error> {
    // Construct a hash table to memoize parsing results.
    let mut cache = Cache::new();

    // Construct a default error in case none of the tokens can be parsed.
    let mut error: ParseError = (None, 0, ErrorConfidenceLevel::Low);

    // Parse the term.
    let result = parse_term(&mut cache, tokens, 0, &mut error);

    // Check if we managed to parse something.
    let first_unparsed_token = if let Some((term, next)) = result {
        // We parsed something, but did we parse everything?
        if next == tokens.len() {
            // We parsed everything. Flip the associativity of applications with non-grouped
            // arguments from right to left.
            let reassociated_term = reassociate_sums_and_differences(
                None,
                reassociate_products_and_quotients(
                    None,
                    reassociate_applications(None, Rc::new(term)),
                ),
            );

            // Construct a mutable context.
            let mut context: HashMap<&'a str, usize> = context
                .iter()
                .enumerate()
                .map(|(i, variable)| (*variable, i))
                .collect();

            // Resolve variables and return the term.
            return Ok(resolve_variables(
                source_path,
                source_contents,
                &*reassociated_term,
                context.len(),
                &mut context,
            )?);
        } else {
            // We didn't parse all the tokens. Remember which one we stopped at.
            next
        }
    } else {
        // The parse failed. Remember which token caused the parse to fail.
        error.1
    };

    // If we made it this far, the parse failed. If none of the parse attempts resulted in a high-
    // confidence error, employ a generic error message.
    set_generic_error(tokens, first_unparsed_token, &mut error);

    // If we made it this far, something went wrong. See if we have an error factory.
    if let (Some(factory), _, _) = error {
        // We have one. Use it to generate the error.
        Err(factory(source_path, source_contents))
    } else {
        panic!(format!(
            "Apparently {} didn't provide an error factory.",
            "set_generic_error".code_str(),
        ));
    }
}

// Flip the associativity of applications from right to left.
#[allow(clippy::too_many_lines)]
fn reassociate_applications<'a>(acc: Option<Rc<Term<'a>>>, term: Rc<Term<'a>>) -> Rc<Term<'a>> {
    // In every case except the application case, if we have a value for the accumulator, we want
    // to construct an application with the accumulator as the applicand and the reduced term as
    // the argument. In the application case, we build up the accumulator.
    let reduced = match &term.variant {
        Variant::Type
        | Variant::Variable(_)
        | Variant::Integer
        | Variant::IntegerLiteral(_)
        | Variant::Boolean
        | Variant::True
        | Variant::False => term,
        Variant::Lambda(variable, domain, body) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Lambda(
                *variable,
                reassociate_applications(None, domain.clone()),
                reassociate_applications(None, body.clone()),
            ),
        }),
        Variant::Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Pi(
                *variable,
                reassociate_applications(None, domain.clone()),
                reassociate_applications(None, codomain.clone()),
            ),
        }),
        Variant::Application(applicand, argument) => {
            return if argument.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: (acc.source_range.0, argument.source_range.1),
                        group: true,
                        variant: Variant::Application(
                            reassociate_applications(Some(acc), applicand.clone()),
                            reassociate_applications(None, argument.clone()),
                        ),
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Application(
                            reassociate_applications(None, applicand.clone()),
                            reassociate_applications(None, argument.clone()),
                        ),
                    })
                }
            } else {
                reassociate_applications(
                    Some(if let Some(acc) = acc {
                        Rc::new(Term {
                            source_range: (acc.source_range.0, applicand.source_range.1),
                            group: true,
                            variant: Variant::Application(
                                acc,
                                reassociate_applications(None, applicand.clone()),
                            ),
                        })
                    } else {
                        reassociate_applications(None, applicand.clone())
                    }),
                    argument.clone(),
                )
            };
        }
        Variant::Let(variable, annotation, definition, body) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Let(
                *variable,
                annotation
                    .as_ref()
                    .map(|annotation| reassociate_applications(None, annotation.clone())),
                reassociate_applications(None, definition.clone()),
                reassociate_applications(None, body.clone()),
            ),
        }),
        Variant::Sum(summand1, summand2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Sum(
                reassociate_applications(None, summand1.clone()),
                reassociate_applications(None, summand2.clone()),
            ),
        }),
        Variant::Difference(minuend, subtrahend) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Difference(
                reassociate_applications(None, minuend.clone()),
                reassociate_applications(None, subtrahend.clone()),
            ),
        }),
        Variant::Product(factor1, factor2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Product(
                reassociate_applications(None, factor1.clone()),
                reassociate_applications(None, factor2.clone()),
            ),
        }),
        Variant::Quotient(dividend, divisor) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Quotient(
                reassociate_applications(None, dividend.clone()),
                reassociate_applications(None, divisor.clone()),
            ),
        }),
        Variant::If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::If(
                reassociate_applications(None, condition.clone()),
                reassociate_applications(None, then_branch.clone()),
                reassociate_applications(None, else_branch.clone()),
            ),
        }),
    };

    // We end up here as long as `term` isn't an application. If we have an accumulator, construct
    // an application as described above. Otherwise, just return the reduced term.
    if let Some(acc) = acc {
        Rc::new(Term {
            source_range: (acc.source_range.0, reduced.source_range.1),
            group: true,
            variant: Variant::Application(acc, reduced),
        })
    } else {
        reduced
    }
}

// Flip the associativity of sums and differences from right to left.
#[allow(clippy::too_many_lines)]
fn reassociate_sums_and_differences<'a>(
    acc: Option<(Rc<Term<'a>>, SumOrDifference)>,
    term: Rc<Term<'a>>,
) -> Rc<Term<'a>> {
    // In every case except the sum and difference cases, if we have a value for the accumulator,
    // we want to construct a sum or difference with the accumulator as the left subterm and the
    // reduced term as the right subterm. In the sum and difference cases, we build up the
    // accumulator.
    let reduced = match &term.variant {
        Variant::Type
        | Variant::Variable(_)
        | Variant::Integer
        | Variant::IntegerLiteral(_)
        | Variant::Boolean
        | Variant::True
        | Variant::False => term,
        Variant::Lambda(variable, domain, body) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Lambda(
                *variable,
                reassociate_sums_and_differences(None, domain.clone()),
                reassociate_sums_and_differences(None, body.clone()),
            ),
        }),
        Variant::Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Pi(
                *variable,
                reassociate_sums_and_differences(None, domain.clone()),
                reassociate_sums_and_differences(None, codomain.clone()),
            ),
        }),
        Variant::Application(applicand, argument) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Application(
                reassociate_sums_and_differences(None, applicand.clone()),
                reassociate_sums_and_differences(None, argument.clone()),
            ),
        }),
        Variant::Let(variable, annotation, definition, body) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Let(
                *variable,
                annotation
                    .as_ref()
                    .map(|annotation| reassociate_sums_and_differences(None, annotation.clone())),
                reassociate_sums_and_differences(None, definition.clone()),
                reassociate_sums_and_differences(None, body.clone()),
            ),
        }),
        Variant::Sum(summand1, summand2) => {
            return if summand2.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: (acc.0.source_range.0, summand2.source_range.1),
                        group: true,
                        variant: Variant::Sum(
                            reassociate_sums_and_differences(Some(acc), summand1.clone()),
                            reassociate_sums_and_differences(None, summand2.clone()),
                        ),
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Sum(
                            reassociate_sums_and_differences(None, summand1.clone()),
                            reassociate_sums_and_differences(None, summand2.clone()),
                        ),
                    })
                }
            } else {
                reassociate_sums_and_differences(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: (acc.source_range.0, summand1.source_range.1),
                                group: true,
                                variant: match operator {
                                    SumOrDifference::Sum => Variant::Sum(
                                        acc,
                                        reassociate_sums_and_differences(None, summand1.clone()),
                                    ),
                                    SumOrDifference::Difference => Variant::Difference(
                                        acc,
                                        reassociate_sums_and_differences(None, summand1.clone()),
                                    ),
                                },
                            })
                        } else {
                            reassociate_sums_and_differences(None, summand1.clone())
                        },
                        SumOrDifference::Sum,
                    )),
                    summand2.clone(),
                )
            };
        }
        Variant::Difference(minuend, subtrahend) => {
            return if subtrahend.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: (acc.0.source_range.0, subtrahend.source_range.1),
                        group: true,
                        variant: Variant::Difference(
                            reassociate_sums_and_differences(Some(acc), minuend.clone()),
                            reassociate_sums_and_differences(None, subtrahend.clone()),
                        ),
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Difference(
                            reassociate_sums_and_differences(None, minuend.clone()),
                            reassociate_sums_and_differences(None, subtrahend.clone()),
                        ),
                    })
                }
            } else {
                reassociate_sums_and_differences(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: (acc.source_range.0, minuend.source_range.1),
                                group: true,
                                variant: match operator {
                                    SumOrDifference::Sum => Variant::Sum(
                                        acc,
                                        reassociate_sums_and_differences(None, minuend.clone()),
                                    ),
                                    SumOrDifference::Difference => Variant::Difference(
                                        acc,
                                        reassociate_sums_and_differences(None, minuend.clone()),
                                    ),
                                },
                            })
                        } else {
                            reassociate_sums_and_differences(None, minuend.clone())
                        },
                        SumOrDifference::Difference,
                    )),
                    subtrahend.clone(),
                )
            };
        }
        Variant::Product(factor1, factor2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Product(
                reassociate_sums_and_differences(None, factor1.clone()),
                reassociate_sums_and_differences(None, factor2.clone()),
            ),
        }),
        Variant::Quotient(dividend, divisor) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Quotient(
                reassociate_sums_and_differences(None, dividend.clone()),
                reassociate_sums_and_differences(None, divisor.clone()),
            ),
        }),
        Variant::If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::If(
                reassociate_sums_and_differences(None, condition.clone()),
                reassociate_sums_and_differences(None, then_branch.clone()),
                reassociate_sums_and_differences(None, else_branch.clone()),
            ),
        }),
    };

    // We end up here as long as `term` isn't a sum or difference. If we have an accumulator,
    // construct a sum or difference as described above. Otherwise, just return the reduced term.
    if let Some((acc, operator)) = acc {
        Rc::new(Term {
            source_range: (acc.source_range.0, reduced.source_range.1),
            group: true,
            variant: match operator {
                SumOrDifference::Sum => Variant::Sum(acc, reduced),
                SumOrDifference::Difference => Variant::Difference(acc, reduced),
            },
        })
    } else {
        reduced
    }
}

// Flip the associativity of products and quotients from right to left.
#[allow(clippy::too_many_lines)]
fn reassociate_products_and_quotients<'a>(
    acc: Option<(Rc<Term<'a>>, ProductOrQuotient)>,
    term: Rc<Term<'a>>,
) -> Rc<Term<'a>> {
    // In every case except the product and quotient cases, if we have a value for the accumulator,
    // we want to construct a product or quotient with the accumulator as the left subterm and the
    // reduced term as the right subterm. In the product and quotient cases, we build up the
    // accumulator.
    let reduced = match &term.variant {
        Variant::Type
        | Variant::Variable(_)
        | Variant::Integer
        | Variant::IntegerLiteral(_)
        | Variant::Boolean
        | Variant::True
        | Variant::False => term,
        Variant::Lambda(variable, domain, body) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Lambda(
                *variable,
                reassociate_products_and_quotients(None, domain.clone()),
                reassociate_products_and_quotients(None, body.clone()),
            ),
        }),
        Variant::Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Pi(
                *variable,
                reassociate_products_and_quotients(None, domain.clone()),
                reassociate_products_and_quotients(None, codomain.clone()),
            ),
        }),
        Variant::Application(applicand, argument) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Application(
                reassociate_products_and_quotients(None, applicand.clone()),
                reassociate_products_and_quotients(None, argument.clone()),
            ),
        }),
        Variant::Let(variable, annotation, definition, body) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Let(
                *variable,
                annotation
                    .as_ref()
                    .map(|annotation| reassociate_products_and_quotients(None, annotation.clone())),
                reassociate_products_and_quotients(None, definition.clone()),
                reassociate_products_and_quotients(None, body.clone()),
            ),
        }),
        Variant::Sum(summand1, summand2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Sum(
                reassociate_products_and_quotients(None, summand1.clone()),
                reassociate_products_and_quotients(None, summand2.clone()),
            ),
        }),
        Variant::Difference(minuend, subtrahend) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Difference(
                reassociate_products_and_quotients(None, minuend.clone()),
                reassociate_products_and_quotients(None, subtrahend.clone()),
            ),
        }),
        Variant::Product(factor1, factor2) => {
            return if factor2.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: (acc.0.source_range.0, factor2.source_range.1),
                        group: true,
                        variant: Variant::Product(
                            reassociate_products_and_quotients(Some(acc), factor1.clone()),
                            reassociate_products_and_quotients(None, factor2.clone()),
                        ),
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Product(
                            reassociate_products_and_quotients(None, factor1.clone()),
                            reassociate_products_and_quotients(None, factor2.clone()),
                        ),
                    })
                }
            } else {
                reassociate_products_and_quotients(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: (acc.source_range.0, factor1.source_range.1),
                                group: true,
                                variant: match operator {
                                    ProductOrQuotient::Product => Variant::Product(
                                        acc,
                                        reassociate_products_and_quotients(None, factor1.clone()),
                                    ),
                                    ProductOrQuotient::Quotient => Variant::Quotient(
                                        acc,
                                        reassociate_products_and_quotients(None, factor1.clone()),
                                    ),
                                },
                            })
                        } else {
                            reassociate_products_and_quotients(None, factor1.clone())
                        },
                        ProductOrQuotient::Product,
                    )),
                    factor2.clone(),
                )
            };
        }
        Variant::Quotient(dividend, divisor) => {
            return if divisor.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: (acc.0.source_range.0, divisor.source_range.1),
                        group: true,
                        variant: Variant::Quotient(
                            reassociate_products_and_quotients(Some(acc), dividend.clone()),
                            reassociate_products_and_quotients(None, divisor.clone()),
                        ),
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Quotient(
                            reassociate_products_and_quotients(None, dividend.clone()),
                            reassociate_products_and_quotients(None, divisor.clone()),
                        ),
                    })
                }
            } else {
                reassociate_products_and_quotients(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: (acc.source_range.0, dividend.source_range.1),
                                group: true,
                                variant: match operator {
                                    ProductOrQuotient::Product => Variant::Product(
                                        acc,
                                        reassociate_products_and_quotients(None, dividend.clone()),
                                    ),
                                    ProductOrQuotient::Quotient => Variant::Quotient(
                                        acc,
                                        reassociate_products_and_quotients(None, dividend.clone()),
                                    ),
                                },
                            })
                        } else {
                            reassociate_products_and_quotients(None, dividend.clone())
                        },
                        ProductOrQuotient::Quotient,
                    )),
                    divisor.clone(),
                )
            };
        }
        Variant::If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::If(
                reassociate_products_and_quotients(None, condition.clone()),
                reassociate_products_and_quotients(None, then_branch.clone()),
                reassociate_products_and_quotients(None, else_branch.clone()),
            ),
        }),
    };

    // We end up here as long as `term` isn't a product or quotient. If we have an accumulator,
    // construct a product or quotient as described above. Otherwise, just return the reduced term.
    if let Some((acc, operator)) = acc {
        Rc::new(Term {
            source_range: (acc.source_range.0, reduced.source_range.1),
            group: true,
            variant: match operator {
                ProductOrQuotient::Product => Variant::Product(acc, reduced),
                ProductOrQuotient::Quotient => Variant::Quotient(acc, reduced),
            },
        })
    } else {
        reduced
    }
}

// Resolve variables into De Bruijn indices.
#[allow(clippy::too_many_lines)]
fn resolve_variables<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &Term<'a>,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
) -> Result<term::Term<'a>, Error> {
    Ok(match &term.variant {
        Variant::Type => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Type,
            }
        }
        Variant::Variable(variable) => {
            // Make sure the variable is in scope and calculate its De Bruijn index.
            let index = if let Some(variable_depth) = context.get(variable.name) {
                depth - 1 - variable_depth
            } else {
                return Err(throw(
                    &format!("Variable {} not in scope.", variable.name.code_str()),
                    source_path,
                    source_contents,
                    variable.source_range,
                ));
            };

            // Construct the variable.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Variable(variable.name, index),
            }
        }
        Variant::Lambda(variable, domain, body) => {
            // Resolve variables in the domain.
            let resolved_domain =
                resolve_variables(source_path, source_contents, &*domain, depth, context)?;

            // If the variable is `PLACEHOLDER_VARIABLE`, don't check for naming conflicts, and
            // don't add it to the context.
            if variable.name != PLACEHOLDER_VARIABLE {
                // Fail if the variable is already in the context.
                if context.contains_key(variable.name) {
                    return Err(throw(
                        &format!("Variable {} already exists.", variable.name.code_str()),
                        source_path,
                        source_contents,
                        variable.source_range,
                    ));
                }

                // Add the variable to the context.
                context.insert(variable.name, depth);
            }

            // Remove the variable from the context (if it was added) when the function
            // returns.
            let context_cell = RefCell::new(context);
            defer! {{ context_cell.borrow_mut().remove(variable.name); }};

            // Construct the lambda.
            let mut guard = context_cell.borrow_mut();
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Lambda(
                    variable.name,
                    Rc::new(resolved_domain),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*body,
                        depth + 1,
                        &mut *guard,
                    )?),
                ),
            }
        }
        Variant::Pi(variable, domain, codomain) => {
            // Resolve variables in the domain.
            let resolved_domain =
                resolve_variables(source_path, source_contents, &*domain, depth, context)?;

            // If the variable is `PLACEHOLDER_VARIABLE`, don't check for naming conflicts, and
            // don't add it to the context.
            if variable.name != PLACEHOLDER_VARIABLE {
                // Fail if the variable is already in the context.
                if context.contains_key(variable.name) {
                    return Err(throw(
                        &format!("Variable {} already exists.", variable.name.code_str()),
                        source_path,
                        source_contents,
                        variable.source_range,
                    ));
                }

                // Add the variable to the context.
                context.insert(variable.name, depth);
            }

            // Remove the variable from the context (if it was added) when the function
            // returns.
            let context_cell = RefCell::new(context);
            defer! {{ context_cell.borrow_mut().remove(variable.name); }};

            // Construct the pi type.
            let mut guard = context_cell.borrow_mut();
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Pi(
                    variable.name,
                    Rc::new(resolved_domain),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*codomain,
                        depth + 1,
                        &mut *guard,
                    )?),
                ),
            }
        }
        Variant::Application(applicand, argument) => {
            // Just resolve variables in the applicand and the argument.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Application(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*applicand,
                        depth,
                        context,
                    )?),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*argument,
                        depth,
                        context,
                    )?),
                ),
            }
        }
        Variant::Let(_, _, _, _) => {
            // Collect the definitions from nested lets.
            let mut definitions = vec![];
            let innermost_body = collect_definitions(&mut definitions, Rc::new(term.clone()));

            // When the function returns, remove the variables from the context that we are
            // temporarily adding.
            let context_cell = RefCell::new((context, vec![]));
            defer! {{
                let mut guard = context_cell.borrow_mut();
                let (borrowed_context, borrowed_variables_added) = &mut (*guard);

                for variable_added in borrowed_variables_added {
                    borrowed_context.remove(*variable_added);
                }
            }};

            // Add the definitions to the context.
            for (i, (inner_variable, _, _)) in definitions.iter().enumerate() {
                // If the variable is `PLACEHOLDER_VARIABLE`, don't check for naming conflicts,
                // and don't add it to the context.
                if inner_variable.name != PLACEHOLDER_VARIABLE {
                    // Temporarily borrow from the scope guard.
                    let mut guard = context_cell.borrow_mut();
                    let (borrowed_context, borrowed_variables_added) = &mut (*guard);

                    // Fail if the variable is already in the context.
                    if borrowed_context.contains_key(inner_variable.name) {
                        return Err(throw(
                            &format!(
                                "Variable {} already exists.",
                                inner_variable.name.code_str(),
                            ),
                            source_path,
                            source_contents,
                            inner_variable.source_range,
                        ));
                    }

                    // Add the variable to the context.
                    borrowed_context.insert(inner_variable.name, depth + i);
                    borrowed_variables_added.push(inner_variable.name);
                }
            }

            // The depth for the annotations, definitions, and innermost body will be as follows:
            let new_depth = depth + definitions.len();

            // Resolve variables in the definitions and annotations.
            let mut resolved_definitions = vec![];
            for (inner_variable, inner_annotation, inner_definition) in &definitions {
                // Temporarily borrow from the scope guard.
                let mut guard = context_cell.borrow_mut();
                let (borrowed_context, _) = &mut (*guard);

                // Resolve variables in the annotation.
                let resolved_annotation = match inner_annotation {
                    Some(annotation) => Some(Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*annotation,
                        new_depth,
                        borrowed_context,
                    )?)),
                    None => None,
                };

                // Resolve variables in the definition.
                let resolved_definition = resolve_variables(
                    source_path,
                    source_contents,
                    &*inner_definition,
                    new_depth,
                    borrowed_context,
                )?;

                // Add the definition to the vector.
                resolved_definitions.push((
                    inner_variable.name,
                    resolved_annotation,
                    Rc::new(resolved_definition),
                ));
            }

            // Ensure forward references make semantic sense.
            for i in 0..resolved_definitions.len() {
                if !is_value(&resolved_definitions[i].2) {
                    let mut visited = HashSet::new();

                    check_references(
                        source_path,
                        source_contents,
                        &resolved_definitions,
                        new_depth,
                        i,
                        i,
                        &mut visited,
                    )?;
                }
            }

            // Temporarily borrow from the scope guard.
            let mut guard = context_cell.borrow_mut();
            let (borrowed_context, _) = &mut (*guard);

            // Resolve definitions in the body and construct the let.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Let(
                    resolved_definitions,
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &innermost_body,
                        new_depth,
                        borrowed_context,
                    )?),
                ),
            }
        }
        Variant::Integer => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Integer,
            }
        }
        Variant::IntegerLiteral(integer) => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::IntegerLiteral(integer.clone()),
            }
        }
        Variant::Sum(summand1, summand2) => {
            // Just resolve variables in the summands.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Sum(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*summand1,
                        depth,
                        context,
                    )?),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*summand2,
                        depth,
                        context,
                    )?),
                ),
            }
        }
        Variant::Difference(minuend, subtrahend) => {
            // Just resolve variables in the minuend and subtrahend.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Difference(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*minuend,
                        depth,
                        context,
                    )?),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*subtrahend,
                        depth,
                        context,
                    )?),
                ),
            }
        }
        Variant::Product(factor1, factor2) => {
            // Just resolve variables in the factors.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Product(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*factor1,
                        depth,
                        context,
                    )?),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*factor2,
                        depth,
                        context,
                    )?),
                ),
            }
        }
        Variant::Quotient(dividend, divisor) => {
            // Just resolve variables in the dividend and divisor.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Quotient(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*dividend,
                        depth,
                        context,
                    )?),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*divisor,
                        depth,
                        context,
                    )?),
                ),
            }
        }
        Variant::Boolean => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::Boolean,
            }
        }
        Variant::True => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::True,
            }
        }
        Variant::False => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::False,
            }
        }
        Variant::If(condition, then_branch, else_branch) => {
            // Just resolve variables in the condition and branches.
            term::Term {
                source_range: Some(term.source_range),
                variant: term::Variant::If(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*condition,
                        depth,
                        context,
                    )?),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*then_branch,
                        depth,
                        context,
                    )?),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*else_branch,
                        depth,
                        context,
                    )?),
                ),
            }
        }
    })
}

// Recurse into nested lets to collect their definitions and return the innermost body.
#[allow(clippy::type_complexity)]
fn collect_definitions<'a>(
    definitions: &mut Vec<(SourceVariable<'a>, Option<Rc<Term<'a>>>, Rc<Term<'a>>)>,
    term: Rc<Term<'a>>,
) -> Rc<Term<'a>> {
    match &term.variant {
        Variant::Let(variable, annotation, definition, body) => {
            definitions.push((*variable, annotation.clone(), definition.clone()));
            collect_definitions(definitions, body.clone())
        }
        _ => term,
    }
}

// This function is used to ensure forward references in lets make semantic sense.
#[allow(clippy::type_complexity)]
fn check_references<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    definitions: &[(&'a str, Option<Rc<term::Term<'a>>>, Rc<term::Term<'a>>)],
    definitions_depth: usize, // Assumes the definitions have already been added to the context
    start_index: usize,       // [0, definitions.len())
    current_index: usize,     // [0, definitions.len())
    visited: &mut HashSet<usize>,
) -> Result<(), Error> {
    // Collect the free variables of the definition.
    let mut variables = HashSet::new();
    free_variables(&definitions[current_index].2, 0, &mut variables);

    // For each free variable bound by the let, check the corresponding definition.
    for variable in variables {
        if variable < definitions.len() {
            // Compute this once rather than multiple times.
            let definition_index = definitions.len() - 1 - variable;

            // Check if we've visited this definition before.
            if visited.insert(definition_index) {
                // If the definition is a value, recurse on it. Otherwise, ensure the definition
                // will be evaluated before the original definition.
                if is_value(&definitions[definition_index].2) {
                    check_references(
                        source_path,
                        source_contents,
                        definitions,
                        definitions_depth,
                        start_index,
                        definition_index,
                        visited,
                    )?;
                } else if definition_index >= start_index {
                    return Err(
                        if let Some(source_range) = definitions[start_index].2.source_range {
                            throw(
                                &format!(
                                    "The definition of {} references {} (directly or indirectly), \
                                        which will not be available in time during evaluation.",
                                    definitions[start_index].0.code_str(),
                                    definitions[definition_index].0.code_str(),
                                ),
                                source_path,
                                source_contents,
                                source_range,
                            )
                        } else {
                            Error {
                                message: format!(
                                    "The definition of {} references {} (directly or indirectly), \
                                        which will not be available in time during evaluation.",
                                    definitions[start_index].0.code_str(),
                                    definitions[definition_index].0.code_str(),
                                ),
                                reason: None,
                            }
                        },
                    );
                }
            }
        }
    }

    Ok(())
}

// Parse a term.
fn parse_term<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Term, start, error);

    // Try to parse a let.
    try_return!(cache, Term, start, parse_let(cache, tokens, start, error));

    // Try to parse a huge term.
    try_return!(
        cache,
        Term,
        start,
        parse_huge_term(cache, tokens, start, error)
    );

    // If we made it this far, the parse failed. If none of the parse attempts resulted in a high-
    // confidence error, employ a generic error message.
    set_generic_error(tokens, start, error);

    // Return `None` since the parse failed.
    cache_return!(cache, Term, start, None)
}

// Parse the type of all types.
fn parse_type<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Type, start, error);

    // Consume the keyword.
    let next = consume_token!(cache, Type, start, tokens, Type, start, error, Low);

    // Construct and return the value.
    cache_return!(
        cache,
        Type,
        start,
        Some((
            Term {
                source_range: tokens[start].source_range,
                group: false,
                variant: Variant::Type,
            },
            next,
        )),
    )
}

// Parse a variable.
fn parse_variable<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Variable, start, error);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Variable, start, tokens, start, error, Low);

    // Construct and return the variable.
    cache_return!(
        cache,
        Variable,
        start,
        Some((
            Term {
                source_range: tokens[start].source_range,
                group: false,
                variant: Variant::Variable(SourceVariable {
                    source_range: tokens[start].source_range,
                    name: variable
                }),
            },
            next,
        )),
    )
}

// Parse a lambda.
fn parse_lambda<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Lambda, start, error);

    // Consume the left parenthesis.
    let variable_pos = consume_token!(cache, Lambda, start, tokens, LeftParen, start, error, Low);

    // Consume the variable.
    let (variable, next) =
        consume_identifier!(cache, Lambda, start, tokens, variable_pos, error, Low);

    // Consume the colon.
    let next = consume_token!(cache, Lambda, start, tokens, Colon, next, error, Low);

    // Parse the domain.
    let (domain, next) = try_eval!(
        cache,
        Lambda,
        start,
        parse_huge_term(cache, tokens, next, error)
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Lambda, start, tokens, RightParen, next, error, Low);

    // Consume the arrow.
    let next = consume_token!(cache, Lambda, start, tokens, ThickArrow, next, error, Low);

    // Parse the body.
    let (body, next) = try_eval!(cache, Lambda, start, parse_term(cache, tokens, next, error));

    // Construct and return the lambda.
    cache_return!(
        cache,
        Lambda,
        start,
        Some((
            Term {
                source_range: (tokens[start].source_range.0, body.source_range.1),
                group: false,
                variant: Variant::Lambda(
                    SourceVariable {
                        source_range: tokens[variable_pos].source_range,
                        name: variable,
                    },
                    Rc::new(domain),
                    Rc::new(body),
                ),
            },
            next,
        )),
    )
}

// Parse a pi type.
fn parse_pi<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Pi, start, error);

    // Consume the left parenthesis.
    let variable_pos = consume_token!(cache, Pi, start, tokens, LeftParen, start, error, Low);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Pi, start, tokens, variable_pos, error, Low);

    // Consume the colon.
    let next = consume_token!(cache, Pi, start, tokens, Colon, next, error, Low);

    // Parse the domain.
    let (domain, next) = try_eval!(
        cache,
        Pi,
        start,
        parse_huge_term(cache, tokens, next, error)
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Pi, start, tokens, RightParen, next, error, Low);

    // Consume the arrow.
    let next = consume_token!(cache, Pi, start, tokens, ThinArrow, next, error, Low);

    // Parse the codomain.
    let (codomain, next) = try_eval!(cache, Pi, start, parse_term(cache, tokens, next, error));

    // Construct and return the pi type.
    cache_return!(
        cache,
        Pi,
        start,
        Some((
            Term {
                source_range: (tokens[start].source_range.0, codomain.source_range.1),
                group: false,
                variant: Variant::Pi(
                    SourceVariable {
                        source_range: tokens[variable_pos].source_range,
                        name: variable,
                    },
                    Rc::new(domain),
                    Rc::new(codomain),
                ),
            },
            next,
        )),
    )
}

// Parse a non-dependent pi type.
fn parse_non_dependent_pi<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, NonDependentPi, start, error);

    // Parse the domain.
    let (domain, next) = try_eval!(
        cache,
        NonDependentPi,
        start,
        parse_small_term(cache, tokens, start, error),
    );

    // Consume the arrow.
    let next = consume_token!(
        cache,
        NonDependentPi,
        start,
        tokens,
        ThinArrow,
        next,
        error,
        Low,
    );

    // Parse the codomain.
    let (codomain, next) = {
        try_eval!(
            cache,
            NonDependentPi,
            start,
            parse_term(cache, tokens, next, error),
        )
    };

    // The source range for the implicit placeholder variable will be the empty range at this
    // location.
    let placeholder_variable_location = tokens[start].source_range.0;

    // Construct and return the pi type.
    cache_return!(
        cache,
        NonDependentPi,
        start,
        Some((
            Term {
                source_range: (tokens[start].source_range.0, codomain.source_range.1),
                group: false,
                variant: Variant::Pi(
                    SourceVariable {
                        source_range: (
                            placeholder_variable_location,
                            placeholder_variable_location,
                        ),
                        name: PLACEHOLDER_VARIABLE,
                    },
                    Rc::new(domain),
                    Rc::new(codomain),
                ),
            },
            next
        )),
    )
}

// Parse an application.
fn parse_application<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Application, start, error);

    // Parse the applicand.
    let (applicand, next) = try_eval!(
        cache,
        Application,
        start,
        parse_atom(cache, tokens, start, error)
    );

    // Parse the argument.
    let (argument, next) = try_eval!(
        cache,
        Application,
        start,
        parse_small_term(cache, tokens, next, error),
    );

    // Construct and return the application.
    cache_return!(
        cache,
        Application,
        start,
        Some((
            Term {
                source_range: (applicand.source_range.0, argument.source_range.1),
                group: false,
                variant: Variant::Application(Rc::new(applicand), Rc::new(argument)),
            },
            next,
        )),
    )
}

// Parse a let.
fn parse_let<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Let, start, error);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Let, start, tokens, start, error, Low);

    // Parse the annotation, if there is one.
    let (annotation, next) = if next < tokens.len() {
        if let token::Variant::Colon = tokens[next].variant {
            // Consume the colon.
            let next = consume_token!(cache, Let, start, tokens, Colon, next, error, Low);

            // Parse the annotation.
            let (annotation, next) = try_eval!(
                cache,
                Let,
                start,
                parse_small_term(cache, tokens, next, error)
            );

            // Package up the annotation in the right form.
            (Some(Rc::new(annotation)), next)
        } else {
            // There is no annotation.
            (None, next)
        }
    } else {
        // There is no annotation because we're at the end of the token stream.
        (None, next)
    };

    // Consume the equals sign.
    let next = consume_token!(cache, Let, start, tokens, Equals, next, error, Low);

    // Parse the definition.
    let (definition, next) = try_eval!(cache, Let, start, parse_term(cache, tokens, next, error));

    // Consume the terminator.
    let next = consume_terminator!(cache, Let, start, tokens, next, error, High);

    // Parse the body.
    let (body, next) = try_eval!(cache, Let, start, parse_term(cache, tokens, next, error));

    // Construct and return the let.
    cache_return!(
        cache,
        Let,
        start,
        Some((
            Term {
                source_range: (tokens[start].source_range.0, body.source_range.1),
                group: false,
                variant: Variant::Let(
                    SourceVariable {
                        source_range: tokens[start].source_range,
                        name: variable,
                    },
                    annotation,
                    Rc::new(definition),
                    Rc::new(body),
                ),
            },
            next,
        )),
    )
}

// Parse the type of integers.
fn parse_integer<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Integer, start, error);

    // Consume the keyword.
    let next = consume_token!(cache, Integer, start, tokens, Integer, start, error, Low);

    // Construct and return the variable.
    cache_return!(
        cache,
        Integer,
        start,
        Some((
            Term {
                source_range: tokens[start].source_range,
                group: false,
                variant: Variant::Integer,
            },
            next,
        )),
    )
}

// Parse an integer literal.
fn parse_integer_literal<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, IntegerLiteral, start, error);

    // Fail if there are no more tokens to parse.
    if start == tokens.len() {
        if start > error.1 || (start == error.1 && ErrorConfidenceLevel::Low > error.2) {
            *error = (
                Some(Rc::new(move |source_path, source_contents| {
                    throw(
                        &format!(
                            "Expected an integer literal after {}.",
                            tokens[start - 1].to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        tokens[start - 1].source_range,
                    )
                }) as ErrorFactory),
                start,
                ErrorConfidenceLevel::Low,
            );
        }

        cache_return!(cache, IntegerLiteral, start, None)
    }

    // Check if the token was the expected one.
    let (next, integer) = if let token::Variant::IntegerLiteral(integer) = &tokens[start].variant {
        (start + 1, integer.clone())
    } else {
        if start > error.1 || (start == error.1 && ErrorConfidenceLevel::Low > error.2) {
            *error = (
                Some(Rc::new(move |source_path, source_contents| {
                    throw(
                        &format!(
                            "Expected an integer literal but encountered {}.",
                            tokens[start].to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        tokens[start].source_range,
                    )
                }) as ErrorFactory),
                start,
                ErrorConfidenceLevel::Low,
            );
        }

        cache_return!(cache, IntegerLiteral, start, None)
    };

    // Construct and return the variable.
    cache_return!(
        cache,
        IntegerLiteral,
        start,
        Some((
            Term {
                source_range: tokens[start].source_range,
                group: false,
                variant: Variant::IntegerLiteral(integer),
            },
            next,
        )),
    )
}

// Parse a sum.
fn parse_sum<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Sum, start, error);

    // Parse the left summand.
    let (summand1, next) = try_eval!(
        cache,
        Sum,
        start,
        parse_medium_term(cache, tokens, start, error),
    );

    // Consume the plus symbol.
    let next = consume_token!(cache, Sum, start, tokens, Plus, next, error, Low);

    // Parse the right summand.
    let (summand2, next) = {
        try_eval!(
            cache,
            Sum,
            start,
            parse_large_term(cache, tokens, next, error),
        )
    };

    // Construct and return the sum.
    cache_return!(
        cache,
        Sum,
        start,
        Some((
            Term {
                source_range: (summand1.source_range.0, summand2.source_range.1),
                group: false,
                variant: Variant::Sum(Rc::new(summand1), Rc::new(summand2)),
            },
            next
        )),
    )
}

// Parse a difference.
fn parse_difference<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Difference, start, error);

    // Parse the minuend.
    let (minuend, next) = try_eval!(
        cache,
        Difference,
        start,
        parse_medium_term(cache, tokens, start, error),
    );

    // Consume the plus symbol.
    let next = consume_token!(cache, Difference, start, tokens, Minus, next, error, Low);

    // Parse the subtrahend.
    let (subtrahend, next) = {
        try_eval!(
            cache,
            Difference,
            start,
            parse_large_term(cache, tokens, next, error),
        )
    };

    // Construct and return the sum.
    cache_return!(
        cache,
        Difference,
        start,
        Some((
            Term {
                source_range: (minuend.source_range.0, subtrahend.source_range.1),
                group: false,
                variant: Variant::Difference(Rc::new(minuend), Rc::new(subtrahend)),
            },
            next
        )),
    )
}

// Parse a product.
fn parse_product<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Product, start, error);

    // Parse the left factor.
    let (factor1, next) = try_eval!(
        cache,
        Product,
        start,
        parse_small_term(cache, tokens, start, error),
    );

    // Consume the asterisk.
    let next = consume_token!(cache, Product, start, tokens, Asterisk, next, error, Low);

    // Parse the right factor.
    let (factor2, next) = {
        try_eval!(
            cache,
            Product,
            start,
            parse_medium_term(cache, tokens, next, error),
        )
    };

    // Construct and return the product.
    cache_return!(
        cache,
        Product,
        start,
        Some((
            Term {
                source_range: (factor1.source_range.0, factor2.source_range.1),
                group: false,
                variant: Variant::Product(Rc::new(factor1), Rc::new(factor2)),
            },
            next
        )),
    )
}

// Parse a quotient.
fn parse_quotient<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Quotient, start, error);

    // Parse the dividend.
    let (dividend, next) = try_eval!(
        cache,
        Quotient,
        start,
        parse_small_term(cache, tokens, start, error),
    );

    // Consume the slash.
    let next = consume_token!(cache, Quotient, start, tokens, Slash, next, error, Low);

    // Parse the divisor.
    let (divisor, next) = {
        try_eval!(
            cache,
            Quotient,
            start,
            parse_medium_term(cache, tokens, next, error),
        )
    };

    // Construct and return the quotient.
    cache_return!(
        cache,
        Quotient,
        start,
        Some((
            Term {
                source_range: (dividend.source_range.0, divisor.source_range.1),
                group: false,
                variant: Variant::Quotient(Rc::new(dividend), Rc::new(divisor)),
            },
            next
        )),
    )
}

// Parse the type of Booleans.
fn parse_boolean<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Boolean, start, error);

    // Consume the keyword.
    let next = consume_token!(cache, Boolean, start, tokens, Boolean, start, error, Low);

    // Construct and return the value.
    cache_return!(
        cache,
        Boolean,
        start,
        Some((
            Term {
                source_range: tokens[start].source_range,
                group: false,
                variant: Variant::Boolean,
            },
            next,
        )),
    )
}

// Parse the logical true value.
fn parse_true<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, True, start, error);

    // Consume the keyword.
    let next = consume_token!(cache, True, start, tokens, True, start, error, Low);

    // Construct and return the value.
    cache_return!(
        cache,
        True,
        start,
        Some((
            Term {
                source_range: tokens[start].source_range,
                group: false,
                variant: Variant::True,
            },
            next,
        )),
    )
}

// Parse the logical false value.
fn parse_false<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, False, start, error);

    // Consume the keyword.
    let next = consume_token!(cache, False, start, tokens, False, start, error, Low);

    // Construct and return the value.
    cache_return!(
        cache,
        False,
        start,
        Some((
            Term {
                source_range: tokens[start].source_range,
                group: false,
                variant: Variant::False,
            },
            next,
        )),
    )
}

// Parse an if expression.
fn parse_if<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, If, start, error);

    // Consume the `if` keyword.
    let next = consume_token!(cache, If, start, tokens, If, start, error, Low);

    // Parse the conditional.
    let (conditional, next) = try_eval!(cache, If, start, parse_term(cache, tokens, next, error));

    // Consume the `then` keyword.
    let next = consume_token!(cache, If, start, tokens, Then, next, error, Low);

    // Parse the then branch.
    let (then_branch, next) = try_eval!(cache, If, start, parse_term(cache, tokens, next, error));

    // Consume the `else` keyword.
    let next = consume_token!(cache, If, start, tokens, Else, next, error, Low);

    // Parse the else branch.
    let (else_branch, next) = try_eval!(cache, If, start, parse_term(cache, tokens, next, error));

    // Construct and return the if expression.
    cache_return!(
        cache,
        If,
        start,
        Some((
            Term {
                source_range: (tokens[start].source_range.0, else_branch.source_range.1),
                group: false,
                variant: Variant::If(
                    Rc::new(conditional),
                    Rc::new(then_branch),
                    Rc::new(else_branch),
                ),
            },
            next,
        )),
    )
}

// Parse a group.
fn parse_group<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Group, start, error);

    // Consume the left parenthesis.
    let next = consume_token!(cache, Group, start, tokens, LeftParen, start, error, Low);

    // Parse the inner term.
    let (term, next) = try_eval!(cache, Group, start, parse_term(cache, tokens, next, error));

    // Consume the right parenthesis.
    let next = consume_token!(cache, Group, start, tokens, RightParen, next, error, Low);

    // If we made it this far, we successfully parsed the group. Return the inner term.
    cache_return!(
        cache,
        Group,
        start,
        Some((
            Term {
                // It's important for this source range to include the parentheses, because parent
                // nodes might use this source range for their own source ranges. We want to avoid
                // a situation in which a parent node's source range includes one of these
                // parentheses but not both.
                source_range: (
                    tokens[start].source_range.0,
                    tokens[next - 1].source_range.1
                ),
                group: true,
                variant: term.variant,
            },
            next,
        ))
    )
}

// Parse an applicand (the left part of an application).
fn parse_atom<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, Atom, start, error);

    // Try to parse the type of all types.
    try_return!(cache, Atom, start, parse_type(cache, tokens, start, error));

    // Try to parse a variable.
    try_return!(
        cache,
        Atom,
        start,
        parse_variable(cache, tokens, start, error),
    );

    // Try to parse the type of integers.
    try_return!(
        cache,
        Term,
        start,
        parse_integer(cache, tokens, start, error)
    );

    // Try to parse an integer literal.
    try_return!(
        cache,
        Term,
        start,
        parse_integer_literal(cache, tokens, start, error)
    );

    // Try to parse the type of Booleans.
    try_return!(
        cache,
        Atom,
        start,
        parse_boolean(cache, tokens, start, error)
    );

    // Try to parse the logical true value.
    try_return!(cache, Atom, start, parse_true(cache, tokens, start, error));

    // Try to parse the logical false value.
    try_return!(cache, Atom, start, parse_false(cache, tokens, start, error));

    // Try to parse a group.
    try_return!(cache, Atom, start, parse_group(cache, tokens, start, error));

    // If we made it this far, the parse failed. If none of the parse attempts resulted in a high-
    // confidence error, employ a generic error message.
    set_generic_error(tokens, start, error);

    // Return `None` since the parse failed.
    cache_return!(cache, Atom, start, None)
}

// Parse a small term.
fn parse_small_term<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, SmallTerm, start, error);

    // Try to parse an application.
    try_return!(
        cache,
        SmallTerm,
        start,
        parse_application(cache, tokens, start, error),
    );

    // Try to parse an atom.
    try_return!(
        cache,
        SmallTerm,
        start,
        parse_atom(cache, tokens, start, error)
    );

    // If we made it this far, the parse failed. If none of the parse attempts resulted in a high-
    // confidence error, employ a generic error message.
    set_generic_error(tokens, start, error);

    // Return `None` since the parse failed.
    cache_return!(cache, SmallTerm, start, None)
}

// Parse a medium term.
fn parse_medium_term<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, MediumTerm, start, error);

    // Try to parse a product.
    try_return!(
        cache,
        MediumTerm,
        start,
        parse_product(cache, tokens, start, error)
    );

    // Try to parse a quotient.
    try_return!(
        cache,
        MediumTerm,
        start,
        parse_quotient(cache, tokens, start, error)
    );

    // Try to parse a small term.
    try_return!(
        cache,
        MediumTerm,
        start,
        parse_small_term(cache, tokens, start, error),
    );

    // If we made it this far, the parse failed. If none of the parse attempts resulted in a high-
    // confidence error, employ a generic error message.
    set_generic_error(tokens, start, error);

    // Return `None` since the parse failed.
    cache_return!(cache, MediumTerm, start, None)
}

// Parse a large term.
fn parse_large_term<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, LargeTerm, start, error);

    // Try to parse a sum.
    try_return!(
        cache,
        LargeTerm,
        start,
        parse_sum(cache, tokens, start, error)
    );

    // Try to parse a difference.
    try_return!(
        cache,
        LargeTerm,
        start,
        parse_difference(cache, tokens, start, error)
    );

    // Try to parse a medium term.
    try_return!(
        cache,
        LargeTerm,
        start,
        parse_medium_term(cache, tokens, start, error),
    );

    // If we made it this far, the parse failed. If none of the parse attempts resulted in a high-
    // confidence error, employ a generic error message.
    set_generic_error(tokens, start, error);

    // Return `None` since the parse failed.
    cache_return!(cache, LargeTerm, start, None)
}

// Parse a huge term.
fn parse_huge_term<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache.
    cache_check!(cache, HugeTerm, start, error);

    // Try to parse a non-dependent pi type.
    try_return!(
        cache,
        HugeTerm,
        start,
        parse_non_dependent_pi(cache, tokens, start, error),
    );

    // Try to parse a lambda.
    try_return!(
        cache,
        HugeTerm,
        start,
        parse_lambda(cache, tokens, start, error),
    );

    // Try to parse a pi type.
    try_return!(
        cache,
        HugeTerm,
        start,
        parse_pi(cache, tokens, start, error)
    );

    // Try to parse an if expression.
    try_return!(
        cache,
        HugeTerm,
        start,
        parse_if(cache, tokens, start, error)
    );

    // Try to parse a large term.
    try_return!(
        cache,
        HugeTerm,
        start,
        parse_large_term(cache, tokens, start, error),
    );

    // If we made it this far, the parse failed. If none of the parse attempts resulted in a high-
    // confidence error, employ a generic error message.
    set_generic_error(tokens, start, error);

    // Return `None` since the parse failed.
    cache_return!(cache, HugeTerm, start, None)
}

#[cfg(test)]
mod tests {
    use crate::{
        assert_fails,
        parser::parse,
        term::{
            Term,
            Variant::{
                Application, Boolean, Difference, False, If, Integer, IntegerLiteral, Lambda, Let,
                Pi, Product, Quotient, Sum, True, Type, Variable,
            },
        },
        tokenizer::tokenize,
    };
    use num_bigint::ToBigInt;
    use std::rc::Rc;

    #[test]
    fn parse_empty() {
        let source = "";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_fails!(
            parse(None, source, &tokens[..], &context[..]),
            "Unexpected end of file",
        );
    }

    #[test]
    fn parse_type() {
        let source = "type";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 4)),
                variant: Type,
            },
        );
    }

    #[test]
    fn parse_variable() {
        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["x"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 1)),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn parse_variable_missing() {
        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_fails!(
            parse(None, source, &tokens[..], &context[..]),
            "not in scope",
        );
    }

    #[test]
    fn parse_lambda() {
        let source = "(x : a) => x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 12)),
                variant: Lambda(
                    "x",
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: Some((11, 12)),
                        variant: Variable("x", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_lambda_shadowing() {
        let source = "(x : a) => x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a", "x"];

        assert_fails!(
            parse(None, source, &tokens[..], &context[..]),
            "already exists"
        );
    }

    #[test]
    fn parse_lambda_placeholder_variable() {
        let source = "(_ : a) => (_ : a) => a";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 23)),
                variant: Lambda(
                    "_",
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: Some((11, 23)),
                        variant: Lambda(
                            "_",
                            Rc::new(Term {
                                source_range: Some((16, 17)),
                                variant: Variable("a", 1),
                            }),
                            Rc::new(Term {
                                source_range: Some((22, 23)),
                                variant: Variable("a", 2),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_pi() {
        let source = "(x : a) -> x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 12)),
                variant: Pi(
                    "x",
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: Some((11, 12)),
                        variant: Variable("x", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_pi_shadowing() {
        let source = "(x : a) -> x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a", "x"];

        assert_fails!(
            parse(None, source, &tokens[..], &context[..]),
            "already exists",
        );
    }

    #[test]
    fn parse_pi_placeholder_variable() {
        let source = "(_ : a) -> (_ : a) -> a";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 23)),
                variant: Pi(
                    "_",
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Term {
                        source_range: Some((11, 23)),
                        variant: Pi(
                            "_",
                            Rc::new(Term {
                                source_range: Some((16, 17)),
                                variant: Variable("a", 1),
                            }),
                            Rc::new(Term {
                                source_range: Some((22, 23)),
                                variant: Variable("a", 2),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_non_dependent_pi() {
        let source = "a -> b";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a", "b"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 6)),
                variant: Pi(
                    "_",
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: Variable("a", 1),
                    }),
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("b", 1),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_non_dependent_pi_associativity() {
        let source = "a -> b -> c";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a", "b", "c"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 11)),
                variant: Pi(
                    "_",
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: Variable("a", 2),
                    }),
                    Rc::new(Term {
                        source_range: Some((5, 11)),
                        variant: Pi(
                            "_",
                            Rc::new(Term {
                                source_range: Some((5, 6)),
                                variant: Variable("b", 2),
                            }),
                            Rc::new(Term {
                                source_range: Some((10, 11)),
                                variant: Variable("c", 2),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_application() {
        let source = "f x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["f", "x"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 3)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: Variable("f", 1),
                    }),
                    Rc::new(Term {
                        source_range: Some((2, 3)),
                        variant: Variable("x", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_application_associativity() {
        let source = "f x y";
        let tokens = tokenize(None, source).unwrap();
        let context = ["f", "x", "y"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((0, 3)),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some((0, 1)),
                                variant: Variable("f", 2),
                            }),
                            Rc::new(Term {
                                source_range: Some((2, 3)),
                                variant: Variable("x", 1),
                            }),
                        ),
                    }),
                    Rc::new(Term {
                        source_range: Some((4, 5)),
                        variant: Variable("y", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_application_grouped_argument() {
        let source = "f (x y)";
        let tokens = tokenize(None, source).unwrap();
        let context = ["f", "x", "y"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 7)),
                variant: Application(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: Variable("f", 2),
                    }),
                    Rc::new(Term {
                        source_range: Some((3, 6)),
                        variant: Application(
                            Rc::new(Term {
                                source_range: Some((3, 4)),
                                variant: Variable("x", 1),
                            }),
                            Rc::new(Term {
                                source_range: Some((5, 6)),
                                variant: Variable("y", 0),
                            }),
                        ),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_let() {
        let source = "x = ((_ : type) => y) type; y : type = type; x";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 46)),
                variant: Let(
                    vec![
                        (
                            "x",
                            None,
                            Rc::new(Term {
                                source_range: Some((4, 26)),
                                variant: Application(
                                    Rc::new(Term {
                                        source_range: Some((4, 21)),
                                        variant: Lambda(
                                            "_",
                                            Rc::new(Term {
                                                source_range: Some((10, 14)),
                                                variant: Type,
                                            }),
                                            Rc::new(Term {
                                                source_range: Some((19, 20)),
                                                variant: Variable("y", 1),
                                            }),
                                        ),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((22, 26)),
                                        variant: Type,
                                    }),
                                ),
                            }),
                        ),
                        (
                            "y",
                            Some(Rc::new(Term {
                                source_range: Some((32, 36)),
                                variant: Type,
                            })),
                            Rc::new(Term {
                                source_range: Some((39, 43)),
                                variant: Type,
                            }),
                        ),
                    ],
                    Rc::new(Term {
                        source_range: Some((45, 46)),
                        variant: Variable("x", 1),
                    })
                ),
            },
        );
    }

    #[test]
    fn parse_integer() {
        let source = "integer";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 7)),
                variant: Integer,
            },
        );
    }

    #[test]
    fn parse_integer_literal() {
        let source = "42";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 2)),
                variant: IntegerLiteral(ToBigInt::to_bigint(&42).unwrap()),
            },
        );
    }

    #[test]
    fn parse_sum() {
        let source = "1 + 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: Sum(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((4, 5)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_difference() {
        let source = "1 - 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: Difference(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((4, 5)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_product() {
        let source = "2 * 3";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: Product(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((4, 5)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&3).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_quotient() {
        let source = "7 / 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: Quotient(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&7).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((4, 5)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_arithmetic() {
        let source = "1 + 2 * (3 - 4 / 5)";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 19)),
                variant: Sum(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((4, 19)),
                        variant: Product(
                            Rc::new(Term {
                                source_range: Some((4, 5)),
                                variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                            }),
                            Rc::new(Term {
                                source_range: Some((8, 19)),
                                variant: Difference(
                                    Rc::new(Term {
                                        source_range: Some((9, 10)),
                                        variant: IntegerLiteral(ToBigInt::to_bigint(&3).unwrap()),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((13, 18)),
                                        variant: Quotient(
                                            Rc::new(Term {
                                                source_range: Some((13, 14)),
                                                variant: IntegerLiteral(
                                                    ToBigInt::to_bigint(&4).unwrap(),
                                                ),
                                            }),
                                            Rc::new(Term {
                                                source_range: Some((17, 18)),
                                                variant: IntegerLiteral(
                                                    ToBigInt::to_bigint(&5).unwrap(),
                                                ),
                                            }),
                                        ),
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
    fn parse_boolean() {
        let source = "boolean";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 7)),
                variant: Boolean,
            },
        );
    }

    #[test]
    fn parse_true() {
        let source = "true";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 4)),
                variant: True,
            },
        );
    }

    #[test]
    fn parse_false() {
        let source = "false";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: False,
            },
        );
    }

    #[test]
    fn parse_if() {
        let source = "if true then 0 else 1";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 21)),
                variant: If(
                    Rc::new(Term {
                        source_range: Some((3, 7)),
                        variant: True,
                    }),
                    Rc::new(Term {
                        source_range: Some((13, 14)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&0).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((20, 21)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_group() {
        let source = "(x)";
        let tokens = tokenize(None, source).unwrap();
        let context = ["x"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 3)),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn parse_identity_function() {
        let source = "(a : type) => (b : type) => (f : a -> b) => (x : a) => f x";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 58)),
                variant: Lambda(
                    "a",
                    Rc::new(Term {
                        source_range: Some((5, 9)),
                        variant: Type,
                    }),
                    Rc::new(Term {
                        source_range: Some((14, 58)),
                        variant: Lambda(
                            "b",
                            Rc::new(Term {
                                source_range: Some((19, 23)),
                                variant: Type,
                            }),
                            Rc::new(Term {
                                source_range: Some((28, 58)),
                                variant: Lambda(
                                    "f",
                                    Rc::new(Term {
                                        source_range: Some((33, 39)),
                                        variant: Pi(
                                            "_",
                                            Rc::new(Term {
                                                source_range: Some((33, 34)),
                                                variant: Variable("a", 1),
                                            }),
                                            Rc::new(Term {
                                                source_range: Some((38, 39)),
                                                variant: Variable("b", 1),
                                            }),
                                        ),
                                    }),
                                    Rc::new(Term {
                                        source_range: Some((44, 58)),
                                        variant: Lambda(
                                            "x",
                                            Rc::new(Term {
                                                source_range: Some((49, 50)),
                                                variant: Variable("a", 2),
                                            }),
                                            Rc::new(Term {
                                                source_range: Some((55, 58)),
                                                variant: Application(
                                                    Rc::new(Term {
                                                        source_range: Some((55, 56)),
                                                        variant: Variable("f", 1),
                                                    }),
                                                    Rc::new(Term {
                                                        source_range: Some((57, 58)),
                                                        variant: Variable("x", 0),
                                                    }),
                                                ),
                                            }),
                                        ),
                                    }),
                                ),
                            }),
                        ),
                    }),
                ),
            },
        );
    }
}
