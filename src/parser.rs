use crate::{
    de_bruijn::free_variables,
    error::{listing, throw, Error},
    evaluator::is_value,
    format::CodeStr,
    term,
    token::{self, TerminatorType, Token},
};
use colored::Colorize;
use num_bigint::BigInt;
use scopeguard::defer;
use std::{
    cell::RefCell,
    cmp::{max, min},
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
// been verified to be unambiguous by Bison. [ref:bison_grammar]
//
// There is one more concern to consider: packrat parsers are greedy, so the order in which we try
// the productions matters in some cases. We address this by attempting productions that result in
// potentially longer matches first. For example, when parsing a term, we try to parse it as an
// application before trying to parse it as a variable, since an application may start with a
// variable. The grammar has been manually verified to be amenable to greedy parsing in this way.

// This represents a fresh variable name. It's never added to the context.
pub const PLACEHOLDER_VARIABLE: &str = "_";

// An `ErrorFactory` is a closure which takes a source path and contents and produces an `Error`.
// It's cheaper to generate a closure that produces the `Error` than to generate the actual
// `Error`, which may contain a long string message.
type ErrorFactory<'a> = Rc<dyn Fn(Option<&'a Path>, &'a str) -> Error + 'a>;

// The token stream is parsed into an abstract syntax tree (AST). This struct represents a node in
// an AST. This is similar to `term::Term`, except:
// - It doesn't contain De Bruijn indices.
// - It has a `group` field (see [ref:group_flag]).
// - It has source ranges for variable bindings.
// - `Variant<'a>` has an extra case for parse errors.
#[derive(Clone)]
struct Term<'a> {
    source_range: SourceRange,
    group: bool, // For an explanation of this field, see [ref:group_flag].
    variant: Variant<'a>,
    errors: Vec<ErrorFactory<'a>>,
}

// Each term has a "variant" describing what kind of term it is.
#[derive(Clone)]
enum Variant<'a> {
    ParseError,
    Type,
    Variable(SourceVariable<'a>),
    Lambda(SourceVariable<'a>, Option<Rc<Term<'a>>>, Rc<Term<'a>>),
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
    Negation(Rc<Term<'a>>),
    Sum(Rc<Term<'a>>, Rc<Term<'a>>),
    Difference(Rc<Term<'a>>, Rc<Term<'a>>),
    Product(Rc<Term<'a>>, Rc<Term<'a>>),
    Quotient(Rc<Term<'a>>, Rc<Term<'a>>),
    LessThan(Rc<Term<'a>>, Rc<Term<'a>>),
    LessThanOrEqualTo(Rc<Term<'a>>, Rc<Term<'a>>),
    EqualTo(Rc<Term<'a>>, Rc<Term<'a>>),
    GreaterThan(Rc<Term<'a>>, Rc<Term<'a>>),
    GreaterThanOrEqualTo(Rc<Term<'a>>, Rc<Term<'a>>),
    Boolean,
    True,
    False,
    If(Rc<Term<'a>>, Rc<Term<'a>>, Rc<Term<'a>>),
}

// For variables, we store the name of the variable and its source range. The source range allows
// us to report nice errors when there are multiple variables with the same name.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct SourceVariable<'a> {
    source_range: SourceRange,
    name: &'a str,
}

// For extra type safety, we use the "newtype" pattern here to introduce a new type for source
// ranges. The goal is to prevent source ranges from accidentally including token indices.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct SourceRange((usize, usize)); // Inclusive on the left and exclusive on the right

// This function constructs a `SourceRange` that spans two given `SourceRange`s.
fn span(x: SourceRange, y: SourceRange) -> SourceRange {
    SourceRange {
        0: (min((x.0).0, (y.0).0), max((x.0).1, (y.0).1)),
    }
}

// This function computes the source range for a token, or the empty range at the end of the source
// file in the case where the given position is at the end of the token stream.
fn token_source_range<'a>(tokens: &'a [Token<'a>], position: usize) -> SourceRange {
    if position == tokens.len() {
        SourceRange {
            0: tokens
                .last()
                .map_or((0, 0), |token| (token.source_range.1, token.source_range.1)),
        }
    } else {
        SourceRange {
            0: tokens[position].source_range,
        }
    }
}

// This function computes an empty source range at the beginning of a token, or the empty range at
// the end of the source file in the case where the given position is at the end of the token
// stream.
fn empty_source_range<'a>(tokens: &'a [Token<'a>], position: usize) -> SourceRange {
    let range_start = (token_source_range(tokens, position).0).0;

    SourceRange {
        0: (range_start, range_start),
    }
}

// When memoizing a function, we'll use this enum to identify which function is being memoized.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Nonterminal {
    Term,
    Type,
    Variable,
    Lambda,
    AnnotatedLambda,
    Pi,
    NonDependentPi,
    Application,
    Let,
    Integer,
    IntegerLiteral,
    Negation,
    Sum,
    Difference,
    Product,
    Quotient,
    LessThan,
    LessThanOrEqualTo,
    EqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
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
    GiantTerm,
    JumboTerm,
}

// A cache is a map from nonterminal and position to term, a new position, and a boolean indicating
// whether we're confident in the new position. If the term is either the `Missing` case or the
// `ParseError` case, the new position should equal the given position.
type Cache<'a> = HashMap<(Nonterminal, usize), (Term<'a>, usize, bool)>;

// This macro should be called at the beginning of every parsing function to do a cache lookup and
// return early on cache hit. It returns the cache key for use by subsequent macro invocations.
macro_rules! cache_check {
    ($cache:ident, $nonterminal:ident, $start:expr $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;

        // Do the cache lookup.
        let cache_key = (Nonterminal::$nonterminal, start);
        if let Some(result) = $cache.get(&cache_key) {
            return result.clone();
        }

        // Return the cache key.
        cache_key
    }};
}

// This macro caches a value and returns it. In parsing functions, this should always be used
// instead of `return` to ensure results are memoized. It evaluates to `()`.
macro_rules! cache_return {
    ($cache:ident, $cache_key:expr, $value:expr $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let cache_key = $cache_key;
        let value = $value;

        // Cache and return the value.
        $cache.insert(cache_key, value.clone());
        return value;
    }};
}

// This macro will return early if `$value` is not a `ParseError`. It evaluates to `()`.
macro_rules! try_return {
    ($cache:ident, $cache_key:expr, $value:expr $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let cache_key = $cache_key;
        let value = $value;

        // Check if the value is an error.
        if let Variant::ParseError = value.0.variant {
        } else {
            // We got a successful parse. Return early.
            cache_return!($cache, cache_key, value)
        }
    }};
}

// This macro is analogous to the `x?` syntax in that it returns early if `$value` is a
// `ParseError`. If `$value` isn't a `ParseError`, this macro evaluates to it.
macro_rules! try_eval {
    ($cache:ident, $cache_key:expr, $value:expr $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let cache_key = $cache_key;
        let value = $value;

        // Extract the result or fail fast.
        if let Variant::ParseError = value.0.variant {
            cache_return!($cache, cache_key, value)
        } else {
            value
        }
    }};
}

// This function constructs a generic error factory that just complains about a particular token or
// the end of the source file.
fn error_factory<'a>(
    tokens: &'a [Token<'a>],
    position: usize,
    expectation: &str,
) -> ErrorFactory<'a> {
    let source_range = token_source_range(tokens, position);
    let expectation = expectation.to_owned();

    Rc::new(move |source_path, source_contents| {
        if tokens.is_empty() {
            throw(
                &format!("Expected {}, but the file is empty.", expectation),
                source_path,
                Some((source_contents, source_range.0)),
            )
        } else if position == tokens.len() {
            throw(
                &format!("Expected {} at the end of the file.", expectation),
                source_path,
                Some((source_contents, source_range.0)),
            )
        } else {
            throw(
                &if tokens[position].variant
                    == token::Variant::Terminator(TerminatorType::LineBreak)
                {
                    format!("Expected {} at the end of this line:", expectation)
                } else {
                    format!(
                        "Expected {}, but encountered {}.",
                        expectation,
                        tokens[position].to_string().code_str(),
                    )
                },
                source_path,
                Some((source_contents, source_range.0)),
            )
        }
    })
}

// This function constructs an error term that just complains about a particular token or the end of
// the source file.
fn error_term<'a>(tokens: &'a [Token<'a>], position: usize, expectation: &str) -> Term<'a> {
    Term {
        source_range: empty_source_range(tokens, position),
        group: false,
        variant: Variant::ParseError,
        errors: vec![error_factory(tokens, position, expectation)],
    }
}

// This macro consumes a single token (with no arguments) and evaluates to the position of the next
// token. It returns early if the token isn't there.
macro_rules! consume_token0 {
    (
        $cache:ident,
        $cache_key:expr,
        $tokens:expr,
        $next:expr,
        $variant:ident,
        $expectation:expr $(,)? // This comma is needed to satisfy the trailing commas check: ,
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let cache_key = $cache_key;
        let tokens = $tokens;
        let next = $next;
        let expectation = $expectation;

        // Fail if there are no more tokens to parse.
        if next == tokens.len() {
            cache_return!(
                $cache,
                cache_key,
                (error_term(tokens, next, expectation), next, false),
            )
        }

        // Check if the token was the expected one.
        if let token::Variant::$variant = tokens[next].variant {
            next + 1
        } else {
            cache_return!(
                $cache,
                cache_key,
                (error_term(tokens, next, expectation), next, false),
            )
        }
    }};
}

// This macro consumes a single token (with one argument) and evaluates to the argument paired with
// the position of the next token. It returns early if the token isn't there.
macro_rules! consume_token1 {
    (
        $cache:ident,
        $cache_key:expr,
        $tokens:expr,
        $next:expr,
        $variant:ident,
        $expectation:expr $(,)? // This comma is needed to satisfy the trailing commas check: ,
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let cache_key = $cache_key;
        let tokens = $tokens;
        let next = $next;
        let expectation = $expectation;

        // Fail if there are no more tokens to parse.
        if next == tokens.len() {
            cache_return!(
                $cache,
                cache_key,
                (error_term(tokens, next, expectation), next, false),
            )
        }

        // Check if the token was the expected one.
        if let token::Variant::$variant(argument) = &tokens[next].variant {
            (argument.clone(), next + 1)
        } else {
            cache_return!(
                $cache,
                cache_key,
                (error_term(tokens, next, expectation), next, false),
            )
        }
    }};
}

// This macro consumes a single token (with no arguments). If the token isn't found at the given
// position, the remaining tokens will be scanned until the token is found (skipping over
// nested parenthesized groupings) or a terminator or the end of the current parenthesized group is
// reached. This macro evaluates to a pair consisting of a Boolean indicating whether the token was
// found and the position of the subsequent token.
macro_rules! expect_token0 {
    (
        $tokens:expr,
        $next:expr,
        $errors:ident,
        $variant:ident,
        $expectation:expr,
        $report_error:expr $(,)? // This comma is needed to satisfy the trailing commas check: ,
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let tokens = $tokens;
        let next = $next;
        let expectation = $expectation;
        let report_error = $report_error;

        // Report an error if the next token is not the expected token.
        if report_error {
            if next == tokens.len() {
                $errors.push(error_factory(tokens, next, expectation));
            } else if let token::Variant::$variant = tokens[next].variant {
            } else {
                $errors.push(error_factory(tokens, next, expectation));
            }
        }

        // Find the `then` keyword and skip past it.
        let mut next = next;
        let mut found = false;
        let mut depth = 0_usize;
        while next < tokens.len() {
            match tokens[next].variant {
                token::Variant::$variant if depth == 0 => {
                    next += 1;
                    found = true;
                    break;
                }
                token::Variant::LeftParen => {
                    depth += 1;
                }
                token::Variant::RightParen => {
                    if depth > 0 {
                        depth -= 1;
                    } else {
                        break;
                    }
                }
                token::Variant::Terminator(_) if depth == 0 => {
                    break;
                }
                _ => {}
            }

            next += 1;
        }

        (found, next)
    }};
}

// This macro consumes a single token (with one argument). If the token isn't found at the given
// position, the remaining tokens will be scanned until the token is found (skipping over
// nested parenthesized groupings) or a terminator or the end of the current parenthesized group is
// reached. This macro evaluates to a pair consisting of a Boolean indicating whether the token was
// found and the position of the subsequent token.
macro_rules! expect_token1 {
    (
        $tokens:expr,
        $next:expr,
        $errors:ident,
        $variant:ident,
        $expectation:expr,
        $report_error:expr $(,)? // This comma is needed to satisfy the trailing commas check: ,
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let tokens = $tokens;
        let next = $next;
        let expectation = $expectation;
        let report_error = $report_error;

        // Report an error if the next token is not the expected token.
        if report_error {
            if next == tokens.len() {
                $errors.push(error_factory(tokens, next, expectation));
            } else if let token::Variant::$variant(_) = tokens[next].variant {
            } else {
                $errors.push(error_factory(tokens, next, expectation));
            }
        }

        // Find the `then` keyword and skip past it.
        let mut next = next;
        let mut argument_option = None;
        let mut depth = 0_usize;
        while next < tokens.len() {
            match &tokens[next].variant {
                token::Variant::$variant(argument) if depth == 0 => {
                    next += 1;
                    argument_option = Some(argument.clone());
                    break;
                }
                token::Variant::LeftParen => {
                    depth += 1;
                }
                token::Variant::RightParen => {
                    if depth > 0 {
                        depth -= 1;
                    } else {
                        break;
                    }
                }
                token::Variant::Terminator(_) if depth == 0 => {
                    break;
                }
                _ => {}
            }

            next += 1;
        }

        (argument_option, next)
    }};
}

// Parse a term.
fn parse_term<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Term, start);

    // Try to parse a let.
    try_return!(cache, cache_key, parse_let(cache, tokens, start));

    // Try to parse a jumbo term.
    try_return!(cache, cache_key, parse_jumbo_term(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// Parse the type of all types.
fn parse_type<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Type, start);

    // Consume the keyword.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        Type,
        &format!("{}", token::Variant::Type.to_string().code_str()),
    );

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: token_source_range(tokens, start),
                group: false,
                variant: Variant::Type,
                errors: vec![],
            },
            next,
            true,
        ),
    )
}

// Parse a variable.
fn parse_variable<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Variable, start);

    // Consume the variable.
    let variable_source_range = token_source_range(tokens, start);
    let (variable, next) =
        consume_token1!(cache, cache_key, tokens, start, Identifier, "a variable");

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: variable_source_range,
                group: false,
                variant: Variant::Variable(SourceVariable {
                    source_range: variable_source_range,
                    name: variable
                }),
                errors: vec![],
            },
            next,
            true,
        ),
    )
}

// Parse a lambda.
fn parse_lambda<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Lambda, start);

    // Consume the variable.
    let (variable, next) =
        consume_token1!(cache, cache_key, tokens, start, Identifier, "a variable");

    // Consume the arrow.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        ThickArrow,
        &format!("{}", token::Variant::ThickArrow.to_string().code_str()),
    );

    // Parse the body.
    let (body, next, confident_next) = parse_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(token_source_range(tokens, start), body.source_range),
                group: false,
                variant: Variant::Lambda(
                    SourceVariable {
                        source_range: token_source_range(tokens, start),
                        name: variable,
                    },
                    None,
                    Rc::new(body),
                ),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse an annotated lambda.
fn parse_annotated_lambda<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, AnnotatedLambda, start);

    // Consume the left parenthesis.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        LeftParen,
        &format!("{}", token::Variant::LeftParen.to_string().code_str()),
    );

    // Consume the variable.
    let variable_source_range = token_source_range(tokens, next);
    let (variable, next) =
        consume_token1!(cache, cache_key, tokens, next, Identifier, "a variable");

    // Consume the colon.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        Colon,
        &format!("{}", token::Variant::Colon.to_string().code_str()),
    );

    // Parse the domain.
    let (domain, next, _) = try_eval!(cache, cache_key, parse_jumbo_term(cache, tokens, next));

    // Consume the right parenthesis.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        RightParen,
        &format!("{}", token::Variant::RightParen.to_string().code_str()),
    );

    // Consume the arrow.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        ThickArrow,
        &format!("{}", token::Variant::ThickArrow.to_string().code_str()),
    );

    // Parse the body.
    let (body, next, confident_next) = parse_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(token_source_range(tokens, start), body.source_range),
                group: false,
                variant: Variant::Lambda(
                    SourceVariable {
                        source_range: variable_source_range,
                        name: variable,
                    },
                    Some(Rc::new(domain)),
                    Rc::new(body),
                ),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a pi type.
fn parse_pi<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Pi, start);

    // Consume the left parenthesis.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        LeftParen,
        &format!("{}", token::Variant::LeftParen.to_string().code_str()),
    );

    // Consume the variable.
    let variable_source_range = token_source_range(tokens, next);
    let (variable, next) =
        consume_token1!(cache, cache_key, tokens, next, Identifier, "a variable");

    // Consume the colon.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        Colon,
        &format!("{}", token::Variant::Colon.to_string().code_str()),
    );

    // Parse the domain.
    let (domain, next, _) = try_eval!(cache, cache_key, parse_jumbo_term(cache, tokens, next));

    // Consume the right parenthesis.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        RightParen,
        &format!("{}", token::Variant::RightParen.to_string().code_str()),
    );

    // Consume the arrow.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        ThinArrow,
        &format!("{}", token::Variant::ThinArrow.to_string().code_str()),
    );

    // Parse the codomain.
    let (codomain, next, confident_next) = parse_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(token_source_range(tokens, start), codomain.source_range),
                group: false,
                variant: Variant::Pi(
                    SourceVariable {
                        source_range: variable_source_range,
                        name: variable,
                    },
                    Rc::new(domain),
                    Rc::new(codomain),
                ),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a non-dependent pi type.
fn parse_non_dependent_pi<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, NonDependentPi, start);

    // Parse the domain.
    let (domain, next, _) = try_eval!(cache, cache_key, parse_small_term(cache, tokens, start));

    // Consume the arrow.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        ThinArrow,
        &format!("{}", token::Variant::ThinArrow.to_string().code_str()),
    );

    // Parse the codomain.
    let (codomain, next, confident_next) = parse_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(domain.source_range, codomain.source_range),
                group: false,
                variant: Variant::Pi(
                    SourceVariable {
                        source_range: empty_source_range(tokens, start),
                        name: PLACEHOLDER_VARIABLE,
                    },
                    Rc::new(domain),
                    Rc::new(codomain),
                ),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse an application.
fn parse_application<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Application, start);

    // Parse the applicand.
    let (applicand, next, _) = try_eval!(cache, cache_key, parse_atom(cache, tokens, start));

    // Parse the argument.
    let (argument, next, confident_next) =
        try_eval!(cache, cache_key, parse_small_term(cache, tokens, next));

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(applicand.source_range, argument.source_range),
                group: false,
                variant: Variant::Application(Rc::new(applicand), Rc::new(argument)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a let.
#[allow(clippy::cognitive_complexity)]
#[allow(clippy::too_many_lines)]
fn parse_let<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Let, start);

    // Consume the variable.
    let variable_source_range = token_source_range(tokens, start);
    let (variable, next) =
        consume_token1!(cache, cache_key, tokens, start, Identifier, "a variable");

    // Parse the annotation, if there is one.
    let (annotation, next, annotation_confident_next) = if next < tokens.len() {
        if let token::Variant::Colon = tokens[next].variant {
            // Consume the colon.
            let next = consume_token0!(
                cache,
                cache_key,
                tokens,
                next,
                Colon,
                &format!("{}", token::Variant::Colon.to_string().code_str()),
            );

            // Parse the annotation.
            let (annotation, next, confident_next) =
                try_eval!(cache, cache_key, parse_small_term(cache, tokens, next));

            // Package up the annotation in the right form.
            (Some(Rc::new(annotation)), next, confident_next)
        } else {
            // There is no annotation.
            (None, next, true)
        }
    } else {
        // There is no annotation because we're at the end of the token stream.
        (None, next, true)
    };

    // Create a vector for parse errors.
    let mut errors = vec![];

    // Consume the equals sign.
    let (equals_found, next) = if annotation.is_some() {
        // Since we have an annotation, we can be confident we're parsing a let (if it were a lambda
        // or a pi type, it would have been parsed already). Find the equals sign and proceed.
        expect_token0!(
            tokens,
            next,
            errors,
            Equals,
            &format!("{}", token::Variant::Equals.to_string().code_str()),
            annotation_confident_next,
        )
    } else {
        // Since we don't have an annotation, we aren't sure yet whether what we're parsing is
        // actually a let. So bail out if we don't find the equals sign.
        (
            true,
            consume_token0!(
                cache,
                cache_key,
                tokens,
                next,
                Equals,
                &format!("{}", token::Variant::Equals.to_string().code_str()),
            ),
        )
    };

    // Parse the definition. But to avoid reporting redundant errors, we skip trying to parse the
    // definition if we didn't find an equals sign.
    let (definition, next, definition_confident_next) = if equals_found {
        parse_term(cache, tokens, next)
    } else {
        (
            Term {
                source_range: empty_source_range(tokens, next),
                group: false,
                variant: Variant::ParseError,
                errors: vec![],
            },
            next,
            false,
        )
    };

    // Consume the terminator.
    let (terminator_type, next) = expect_token1!(
        tokens,
        next,
        errors,
        Terminator,
        &format!(
            "{} or a line break followed by an expression",
            token::Variant::Terminator(TerminatorType::Semicolon)
                .to_string()
                .code_str(),
        ),
        definition_confident_next,
    );

    // Parse the body. But to avoid reporting redundant errors, we skip trying to parse the body if
    // we didn't find a terminator.
    let (body, next, body_confident_next) = if terminator_type.is_some() {
        parse_term(cache, tokens, next)
    } else {
        (
            Term {
                source_range: empty_source_range(tokens, next),
                group: false,
                variant: Variant::ParseError,
                errors: vec![],
            },
            next,
            false,
        )
    };

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(variable_source_range, body.source_range),
                group: false,
                variant: Variant::Let(
                    SourceVariable {
                        source_range: variable_source_range,
                        name: variable,
                    },
                    annotation,
                    Rc::new(definition),
                    Rc::new(body),
                ),
                errors: errors,
            },
            next,
            body_confident_next,
        ),
    )
}

// Parse the type of integers.
fn parse_integer<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Integer, start);

    // Consume the keyword.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        Integer,
        &format!("{}", token::Variant::Integer.to_string().code_str()),
    );

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: token_source_range(tokens, start),
                group: false,
                variant: Variant::Integer,
                errors: vec![],
            },
            next,
            true,
        ),
    )
}

// Parse an integer literal.
fn parse_integer_literal<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, IntegerLiteral, start);

    // Consume the integer.
    let (integer, next) = consume_token1!(
        cache,
        cache_key,
        tokens,
        start,
        IntegerLiteral,
        "an integer literal",
    );

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: token_source_range(tokens, start),
                group: false,
                variant: Variant::IntegerLiteral(integer),
                errors: vec![],
            },
            next,
            true,
        ),
    )
}

// Parse a negation.
fn parse_negation<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Negation, start);

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        Minus,
        &format!("{}", token::Variant::Minus.to_string().code_str()),
    );

    // Parse the subterm.
    let (subterm, next, confident_next) = parse_large_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(token_source_range(tokens, start), subterm.source_range),
                group: false,
                variant: Variant::Negation(Rc::new(subterm)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a sum.
fn parse_sum<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Sum, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_large_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        Plus,
        &format!("{}", token::Variant::Plus.to_string().code_str()),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_huge_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::Sum(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a difference.
fn parse_difference<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Difference, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_large_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        Minus,
        &format!("{}", token::Variant::Minus.to_string().code_str()),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_huge_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::Difference(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a product.
fn parse_product<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Product, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_small_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        Asterisk,
        &format!("{}", token::Variant::Asterisk.to_string().code_str()),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_large_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::Product(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a quotient.
fn parse_quotient<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Quotient, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_small_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        Slash,
        &format!("{}", token::Variant::Slash.to_string().code_str()),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_large_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::Quotient(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a less than comparison.
fn parse_less_than<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, LessThan, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_huge_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        LessThan,
        &format!("{}", token::Variant::LessThan.to_string().code_str()),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_huge_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::LessThan(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a less than or equal to comparison.
fn parse_less_than_or_equal_to<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, LessThanOrEqualTo, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_huge_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        LessThanOrEqualTo,
        &format!(
            "{}",
            token::Variant::LessThanOrEqualTo.to_string().code_str(),
        ),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_huge_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::LessThanOrEqualTo(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse an equality comparison.
fn parse_equal_to<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, EqualTo, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_huge_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        DoubleEquals,
        &format!("{}", token::Variant::DoubleEquals.to_string().code_str()),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_huge_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::EqualTo(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a greater than comparison.
fn parse_greater_than<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, GreaterThan, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_huge_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        GreaterThan,
        &format!("{}", token::Variant::GreaterThan.to_string().code_str()),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_huge_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::GreaterThan(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse a greater than or equal to comparison.
fn parse_greater_than_or_equal_to<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, GreaterThanOrEqualTo, start);

    // Parse the left subterm.
    let (term1, next, _) = try_eval!(cache, cache_key, parse_huge_term(cache, tokens, start));

    // Consume the operator.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        next,
        GreaterThanOrEqualTo,
        &format!(
            "{}",
            token::Variant::GreaterThanOrEqualTo.to_string().code_str(),
        ),
    );

    // Parse the right subterm.
    let (term2, next, confident_next) = parse_huge_term(cache, tokens, next);

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(term1.source_range, term2.source_range),
                group: false,
                variant: Variant::GreaterThanOrEqualTo(Rc::new(term1), Rc::new(term2)),
                errors: vec![],
            },
            next,
            confident_next,
        ),
    )
}

// Parse the type of Booleans.
fn parse_boolean<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Boolean, start);

    // Consume the keyword.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        Boolean,
        &format!("{}", token::Variant::Boolean.to_string().code_str()),
    );

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: token_source_range(tokens, start),
                group: false,
                variant: Variant::Boolean,
                errors: vec![],
            },
            next,
            true,
        ),
    )
}

// Parse the logical true value.
fn parse_true<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, True, start);

    // Consume the keyword.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        True,
        &format!("{}", token::Variant::True.to_string().code_str()),
    );

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: token_source_range(tokens, start),
                group: false,
                variant: Variant::True,
                errors: vec![],
            },
            next,
            true,
        ),
    )
}

// Parse the logical false value.
fn parse_false<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, False, start);

    // Consume the keyword.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        False,
        &format!("{}", token::Variant::False.to_string().code_str()),
    );

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: token_source_range(tokens, start),
                group: false,
                variant: Variant::False,
                errors: vec![],
            },
            next,
            true,
        ),
    )
}

// Parse an if expression.
fn parse_if<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, If, start);

    // Consume the `if` keyword.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        If,
        &format!("{}", token::Variant::If.to_string().code_str()),
    );

    // Parse the condition.
    let (condition, next, condition_confident_next) = parse_term(cache, tokens, next);

    // Create a vector for parse errors.
    let mut errors = vec![];

    // Consume the `then` keyword.
    let (found_then, next) = expect_token0!(
        tokens,
        next,
        errors,
        Then,
        &format!("{}", token::Variant::Then.to_string().code_str()),
        condition_confident_next,
    );

    // Parse the then branch. But to avoid reporting redundant errors, we skip trying to parse the
    // then branch if we didn't find a `then` keyword.
    let (then_branch, next, then_branch_confident_next) = if found_then {
        parse_term(cache, tokens, next)
    } else {
        (
            Term {
                source_range: empty_source_range(tokens, next),
                group: false,
                variant: Variant::ParseError,
                errors: vec![],
            },
            next,
            false,
        )
    };

    // Consume the `else` keyword.
    let (found_else, next) = expect_token0!(
        tokens,
        next,
        errors,
        Else,
        &format!("{}", token::Variant::Else.to_string().code_str()),
        then_branch_confident_next,
    );

    // Parse the else branch. But to avoid reporting redundant errors, we skip trying to parse the
    // else branch if we didn't find an `else` keyword.
    let (else_branch, next, else_branch_confident_next) = if found_else {
        parse_term(cache, tokens, next)
    } else {
        (
            Term {
                source_range: empty_source_range(tokens, next),
                group: false,
                variant: Variant::ParseError,
                errors: vec![],
            },
            next,
            false,
        )
    };

    // Construct and return the term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                source_range: span(token_source_range(tokens, start), else_branch.source_range),
                group: false,
                variant: Variant::If(
                    Rc::new(condition),
                    Rc::new(then_branch),
                    Rc::new(else_branch),
                ),
                errors: errors,
            },
            next,
            else_branch_confident_next,
        ),
    )
}

// Parse a group.
#[allow(clippy::too_many_lines)]
fn parse_group<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Group, start);

    // Consume the left parenthesis.
    let next = consume_token0!(
        cache,
        cache_key,
        tokens,
        start,
        LeftParen,
        &format!("{}", token::Variant::LeftParen.to_string().code_str()),
    );

    // Parse the inner term.
    let (term, next, confident_next) = try_eval!(cache, cache_key, parse_term(cache, tokens, next));

    // Consume the right parenthesis. Here we use `expect_token0!` rather than `consume_token0!`
    // because we know we're parsing a group at this point. If it were a lambda or a pi type, it
    // would have been parsed already. We're going to construct a more informative error than what
    // `expect_token0!` gives us, so we create a phony `errors` vector just for this macro
    // invocation.
    let mut phony_errors = vec![];
    let (found, next) = expect_token0!(
        tokens,
        next,
        phony_errors,
        RightParen,
        &format!("{}", token::Variant::RightParen.to_string().code_str()),
        confident_next,
    );

    // Create a copy of the errors vector for reporting additional errors.
    let mut errors = term.errors.clone();

    // Check if we found the right parenthesis.
    if !found {
        // We didn't find it. Report an error.
        errors.push(Rc::new(move |source_path, source_contents| {
            // Compute the source range for the left parenthesis.
            let left_parenthesis_source_range = token_source_range(tokens, start);

            // Check if we're at the end of the file.
            if next == tokens.len() {
                throw(
                    "This parenthesis was never closed:",
                    source_path,
                    Some((source_contents, left_parenthesis_source_range.0)),
                )
            } else {
                let left_parenthesis_listing = listing(
                    source_contents,
                    (left_parenthesis_source_range.0).0,
                    (left_parenthesis_source_range.0).1,
                );

                let unexpected_token_source_range = token_source_range(tokens, next);

                let unexpected_token_listing = listing(
                    source_contents,
                    (unexpected_token_source_range.0).0,
                    (unexpected_token_source_range.0).1,
                );

                Error {
                    message: if tokens[next].variant
                        == token::Variant::Terminator(TerminatorType::LineBreak)
                    {
                        if let Some(path) = source_path {
                            format!(
                                "{} {} This parenthesis was never closed:\n\n{}\n\nIt was \
                                    expected to be closed at the end of this line:\n\n{}",
                                "[Error]".red().bold(),
                                format!("[{}]", path.to_string_lossy().code_str()).magenta(),
                                left_parenthesis_listing,
                                unexpected_token_listing,
                            )
                        } else {
                            format!(
                                "{} This parenthesis was never closed:\n\n{}\n\nIt was \
                                    expected to be closed at the end of this line:\n\n{}",
                                "[Error]".red().bold(),
                                left_parenthesis_listing,
                                unexpected_token_listing,
                            )
                        }
                    } else if let Some(path) = source_path {
                        format!(
                            "{} {} This parenthesis was never closed:\n\n{}\n\nIt was \
                                expected to be closed before {}:\n\n{}",
                            "[Error]".red().bold(),
                            format!("[{}]", path.to_string_lossy().code_str()).magenta(),
                            left_parenthesis_listing,
                            tokens[next].to_string().code_str(),
                            unexpected_token_listing,
                        )
                    } else {
                        format!(
                            "{} This parenthesis was never closed:\n\n{}\n\nIt was \
                                expected to be closed before {}:\n\n{}",
                            "[Error]".red().bold(),
                            left_parenthesis_listing,
                            tokens[next].to_string().code_str(),
                            unexpected_token_listing,
                        )
                    },
                    reason: None,
                }
            }
        }));
    }

    // If we made it this far, we successfully parsed the group. Return the inner term.
    cache_return!(
        cache,
        cache_key,
        (
            Term {
                // It's important for this source range to include the parentheses, because parent
                // nodes might use this source range for their own source ranges. We want to avoid
                // a situation in which a parent node's source range includes one of these
                // parentheses but not both.
                source_range: span(
                    token_source_range(tokens, start),
                    token_source_range(tokens, next - 1),
                ),
                group: true,
                variant: term.variant,
                errors: errors,
            },
            next,
            found,
        ),
    )
}

// Parse an applicand (the left part of an application).
fn parse_atom<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, Atom, start);

    // Try to parse the type of all types.
    try_return!(cache, cache_key, parse_type(cache, tokens, start));

    // Try to parse a variable.
    try_return!(cache, cache_key, parse_variable(cache, tokens, start));

    // Try to parse the type of integers.
    try_return!(cache, cache_key, parse_integer(cache, tokens, start));

    // Try to parse an integer literal.
    try_return!(
        cache,
        cache_key,
        parse_integer_literal(cache, tokens, start),
    );

    // Try to parse the type of Booleans.
    try_return!(cache, cache_key, parse_boolean(cache, tokens, start));

    // Try to parse the logical true value.
    try_return!(cache, cache_key, parse_true(cache, tokens, start));

    // Try to parse the logical false value.
    try_return!(cache, cache_key, parse_false(cache, tokens, start));

    // Try to parse a group.
    try_return!(cache, cache_key, parse_group(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// Parse a small term.
fn parse_small_term<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, SmallTerm, start);

    // Try to parse an application.
    try_return!(cache, cache_key, parse_application(cache, tokens, start));

    // Try to parse an atom.
    try_return!(cache, cache_key, parse_atom(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// Parse a medium term.
fn parse_medium_term<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, MediumTerm, start);

    // Try to parse a product.
    try_return!(cache, cache_key, parse_product(cache, tokens, start));

    // Try to parse a quotient.
    try_return!(cache, cache_key, parse_quotient(cache, tokens, start));

    // Try to parse a small term.
    try_return!(cache, cache_key, parse_small_term(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// Parse a large term.
fn parse_large_term<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, LargeTerm, start);

    // Try to parse a negation.
    try_return!(cache, cache_key, parse_negation(cache, tokens, start));

    // Try to parse a medium term.
    try_return!(cache, cache_key, parse_medium_term(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// Parse a huge term.
fn parse_huge_term<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, HugeTerm, start);

    // Try to parse a sum.
    try_return!(cache, cache_key, parse_sum(cache, tokens, start));

    // Try to parse a difference.
    try_return!(cache, cache_key, parse_difference(cache, tokens, start));

    // Try to parse a large term.
    try_return!(cache, cache_key, parse_large_term(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// Parse a giant term.
fn parse_giant_term<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, GiantTerm, start);

    // Try to parse a less than comparison.
    try_return!(cache, cache_key, parse_less_than(cache, tokens, start));

    // Try to parse a less than or equal to comparison.
    try_return!(
        cache,
        cache_key,
        parse_less_than_or_equal_to(cache, tokens, start),
    );

    // Try to parse an equality comparison.
    try_return!(cache, cache_key, parse_equal_to(cache, tokens, start));

    // Try to parse a greater than comparison.
    try_return!(cache, cache_key, parse_greater_than(cache, tokens, start));

    // Try to parse a greater than or equal to comparison.
    try_return!(
        cache,
        cache_key,
        parse_greater_than_or_equal_to(cache, tokens, start),
    );

    // Try to parse a huge term.
    try_return!(cache, cache_key, parse_huge_term(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// Parse a jumbo term.
fn parse_jumbo_term<'a>(
    cache: &mut Cache<'a>,
    tokens: &'a [Token<'a>],
    start: usize,
) -> (Term<'a>, usize, bool) {
    // Check the cache.
    let cache_key = cache_check!(cache, JumboTerm, start);

    // Try to parse a lambda.
    try_return!(cache, cache_key, parse_lambda(cache, tokens, start));

    // Try to parse an annotated lambda.
    try_return!(
        cache,
        cache_key,
        parse_annotated_lambda(cache, tokens, start),
    );

    // Try to parse a pi type.
    try_return!(cache, cache_key, parse_pi(cache, tokens, start));

    // Try to parse a non-dependent pi type.
    try_return!(
        cache,
        cache_key,
        parse_non_dependent_pi(cache, tokens, start),
    );

    // Try to parse an if expression.
    try_return!(cache, cache_key, parse_if(cache, tokens, start));

    // Try to parse a giant term.
    try_return!(cache, cache_key, parse_giant_term(cache, tokens, start));

    // Since none of the parses succeeded, return an error.
    cache_return!(
        cache,
        cache_key,
        (error_term(tokens, start, "an expression"), start, false),
    )
}

// This is the top-level parsing function. All the parsed terms are guaranteed to have a non-`None`
// `source_range`. The parser also guarantees that all variables are bound, except of course the
// ones in the initial context. Variable shadowing is not allowed.
pub fn parse<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    tokens: &'a [Token<'a>],
    context: &[&'a str],
) -> Result<term::Term<'a>, Vec<Error>> {
    // Construct a hash table to memoize parsing results.
    let mut cache = Cache::new();

    // Parse the term.
    let (term, next, _) = parse_term(&mut cache, tokens, 0);

    // Collect the parsing error factories.
    let mut error_factories = vec![];
    collect_error_factories(&mut error_factories, &term);

    // Check if the parse was successful but we didn't consume all the tokens.
    if error_factories.is_empty() && next != tokens.len() {
        // Complain about the first unparsed token.
        error_factories.push(error_factory(tokens, next, "the end of the file"));
    }

    // Fail if there were any errors [tag:error_check].
    if !error_factories.is_empty() {
        return Err(error_factories
            .into_iter()
            .map(|error_factory| error_factory(source_path, source_contents))
            .collect());
    }

    // Flip the associativity of applications with non-grouped arguments from right to left.
    let reassociated_term = reassociate_sums_and_differences(
        None,
        reassociate_products_and_quotients(None, reassociate_applications(None, Rc::new(term))),
    );

    // Construct a mutable context.
    let mut context: HashMap<&'a str, usize> = context
        .iter()
        .enumerate()
        .map(|(i, variable)| (*variable, i))
        .collect();

    // Construct a vector to hold any errors that might be detected below.
    let mut errors = vec![];

    // Resolve variables.
    let resolved_term = resolve_variables(
        source_path,
        source_contents,
        &*reassociated_term,
        context.len(),
        &mut context,
        &mut errors,
    );

    // Check that definitions will be evaluated before they are used.
    check_definitions(
        source_path,
        source_contents,
        &resolved_term,
        context.len(),
        &mut context,
        &mut errors,
    );

    // Return the term if there are no errors. Otherwise return the errors.
    if errors.is_empty() {
        Ok(resolved_term)
    } else {
        Err(errors)
    }
}

// This function finds all the error factories within a term.
fn collect_error_factories<'a>(error_factories: &mut Vec<ErrorFactory<'a>>, term: &Term<'a>) {
    match &term.variant {
        Variant::ParseError
        | Variant::Type
        | Variant::Variable(_)
        | Variant::Integer
        | Variant::IntegerLiteral(_)
        | Variant::Boolean
        | Variant::True
        | Variant::False => {}
        Variant::Lambda(_, domain, body) => {
            if let Some(domain) = domain {
                collect_error_factories(error_factories, domain);
            }

            collect_error_factories(error_factories, body);
        }
        Variant::Pi(_, domain, codomain) => {
            collect_error_factories(error_factories, domain);
            collect_error_factories(error_factories, codomain);
        }
        Variant::Application(applicand, argument) => {
            collect_error_factories(error_factories, applicand);
            collect_error_factories(error_factories, argument);
        }
        Variant::Let(_, annotation, definition, body) => {
            if let Some(annotation) = annotation {
                collect_error_factories(error_factories, annotation);
            }

            collect_error_factories(error_factories, definition);
            collect_error_factories(error_factories, body);
        }
        Variant::Negation(subterm) => {
            collect_error_factories(error_factories, subterm);
        }
        Variant::Sum(term1, term2)
        | Variant::Difference(term1, term2)
        | Variant::Product(term1, term2)
        | Variant::Quotient(term1, term2)
        | Variant::LessThan(term1, term2)
        | Variant::LessThanOrEqualTo(term1, term2)
        | Variant::EqualTo(term1, term2)
        | Variant::GreaterThan(term1, term2)
        | Variant::GreaterThanOrEqualTo(term1, term2) => {
            collect_error_factories(error_factories, term1);
            collect_error_factories(error_factories, term2);
        }
        Variant::If(condition, then_branch, else_branch) => {
            collect_error_factories(error_factories, condition);
            collect_error_factories(error_factories, then_branch);
            collect_error_factories(error_factories, else_branch);
        }
    };

    for error_factory in &term.errors {
        error_factories.push(error_factory.clone());
    }
}

// Flip the associativity of applications from right to left.
#[allow(clippy::too_many_lines)]
fn reassociate_applications<'a>(acc: Option<Rc<Term<'a>>>, term: Rc<Term<'a>>) -> Rc<Term<'a>> {
    // In every case except the application case, if we have a value for the accumulator, we want
    // to construct an application with the accumulator as the applicand and the reduced term as
    // the argument. In the application case, we build up the accumulator.
    let reduced = match &term.variant {
        Variant::ParseError => {
            // This should be unreachable due to [ref:error_check].
            panic!(
                "{} called on a missing term or parse error.",
                "reassociate_applications".code_str(),
            )
        }
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
                domain
                    .as_ref()
                    .map(|domain| reassociate_applications(None, domain.clone())),
                reassociate_applications(None, body.clone()),
            ),
            errors: vec![],
        }),
        Variant::Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Pi(
                *variable,
                reassociate_applications(None, domain.clone()),
                reassociate_applications(None, codomain.clone()),
            ),
            errors: vec![],
        }),
        Variant::Application(applicand, argument) => {
            return if argument.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: span(acc.source_range, argument.source_range),
                        group: true,
                        variant: Variant::Application(
                            reassociate_applications(Some(acc), applicand.clone()),
                            reassociate_applications(None, argument.clone()),
                        ),
                        errors: vec![],
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Application(
                            reassociate_applications(None, applicand.clone()),
                            reassociate_applications(None, argument.clone()),
                        ),
                        errors: vec![],
                    })
                }
            } else {
                reassociate_applications(
                    Some(if let Some(acc) = acc {
                        Rc::new(Term {
                            source_range: span(acc.source_range, applicand.source_range),
                            group: true,
                            variant: Variant::Application(
                                acc,
                                reassociate_applications(None, applicand.clone()),
                            ),
                            errors: vec![],
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
            errors: vec![],
        }),
        Variant::Negation(subterm) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Negation(reassociate_applications(None, subterm.clone())),
            errors: vec![],
        }),
        Variant::Sum(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Sum(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::Difference(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Difference(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::Product(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Product(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::Quotient(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Quotient(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::LessThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::LessThan(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::LessThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::LessThanOrEqualTo(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::EqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::EqualTo(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::GreaterThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::GreaterThan(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::GreaterThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::GreaterThanOrEqualTo(
                reassociate_applications(None, term1.clone()),
                reassociate_applications(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::If(
                reassociate_applications(None, condition.clone()),
                reassociate_applications(None, then_branch.clone()),
                reassociate_applications(None, else_branch.clone()),
            ),
            errors: vec![],
        }),
    };

    // We end up here as long as `term` isn't an application. If we have an accumulator, construct
    // an application as described above. Otherwise, just return the reduced term.
    if let Some(acc) = acc {
        Rc::new(Term {
            source_range: span(acc.source_range, reduced.source_range),
            group: true,
            variant: Variant::Application(acc, reduced),
            errors: vec![],
        })
    } else {
        reduced
    }
}

// When reassociating products and quotients to be left-associative, this enum is used to record how
// each term is used.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum ProductOrQuotient {
    Product,
    Quotient,
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
        Variant::ParseError => {
            // This should be unreachable due to [ref:error_check].
            panic!(
                "{} called on a missing term or parse error.",
                "reassociate_products_and_quotients".code_str(),
            )
        }
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
                domain
                    .as_ref()
                    .map(|domain| reassociate_products_and_quotients(None, domain.clone())),
                reassociate_products_and_quotients(None, body.clone()),
            ),
            errors: vec![],
        }),
        Variant::Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Pi(
                *variable,
                reassociate_products_and_quotients(None, domain.clone()),
                reassociate_products_and_quotients(None, codomain.clone()),
            ),
            errors: vec![],
        }),
        Variant::Application(applicand, argument) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Application(
                reassociate_products_and_quotients(None, applicand.clone()),
                reassociate_products_and_quotients(None, argument.clone()),
            ),
            errors: vec![],
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
            errors: vec![],
        }),
        Variant::Negation(subterm) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Negation(reassociate_products_and_quotients(None, subterm.clone())),
            errors: vec![],
        }),
        Variant::Sum(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Sum(
                reassociate_products_and_quotients(None, term1.clone()),
                reassociate_products_and_quotients(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::Difference(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Difference(
                reassociate_products_and_quotients(None, term1.clone()),
                reassociate_products_and_quotients(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::Product(term1, term2) => {
            return if term2.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: span(acc.0.source_range, term2.source_range),
                        group: true,
                        variant: Variant::Product(
                            reassociate_products_and_quotients(Some(acc), term1.clone()),
                            reassociate_products_and_quotients(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Product(
                            reassociate_products_and_quotients(None, term1.clone()),
                            reassociate_products_and_quotients(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                }
            } else {
                reassociate_products_and_quotients(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: span(acc.source_range, term1.source_range),
                                group: true,
                                variant: match operator {
                                    ProductOrQuotient::Product => Variant::Product(
                                        acc,
                                        reassociate_products_and_quotients(None, term1.clone()),
                                    ),
                                    ProductOrQuotient::Quotient => Variant::Quotient(
                                        acc,
                                        reassociate_products_and_quotients(None, term1.clone()),
                                    ),
                                },
                                errors: vec![],
                            })
                        } else {
                            reassociate_products_and_quotients(None, term1.clone())
                        },
                        ProductOrQuotient::Product,
                    )),
                    term2.clone(),
                )
            };
        }
        Variant::Quotient(term1, term2) => {
            return if term2.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: span(acc.0.source_range, term2.source_range),
                        group: true,
                        variant: Variant::Quotient(
                            reassociate_products_and_quotients(Some(acc), term1.clone()),
                            reassociate_products_and_quotients(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Quotient(
                            reassociate_products_and_quotients(None, term1.clone()),
                            reassociate_products_and_quotients(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                }
            } else {
                reassociate_products_and_quotients(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: span(acc.source_range, term1.source_range),
                                group: true,
                                variant: match operator {
                                    ProductOrQuotient::Product => Variant::Product(
                                        acc,
                                        reassociate_products_and_quotients(None, term1.clone()),
                                    ),
                                    ProductOrQuotient::Quotient => Variant::Quotient(
                                        acc,
                                        reassociate_products_and_quotients(None, term1.clone()),
                                    ),
                                },
                                errors: vec![],
                            })
                        } else {
                            reassociate_products_and_quotients(None, term1.clone())
                        },
                        ProductOrQuotient::Quotient,
                    )),
                    term2.clone(),
                )
            };
        }
        Variant::LessThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::LessThan(
                reassociate_products_and_quotients(None, term1.clone()),
                reassociate_products_and_quotients(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::LessThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::LessThanOrEqualTo(
                reassociate_products_and_quotients(None, term1.clone()),
                reassociate_products_and_quotients(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::EqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::EqualTo(
                reassociate_products_and_quotients(None, term1.clone()),
                reassociate_products_and_quotients(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::GreaterThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::GreaterThan(
                reassociate_products_and_quotients(None, term1.clone()),
                reassociate_products_and_quotients(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::GreaterThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::GreaterThanOrEqualTo(
                reassociate_products_and_quotients(None, term1.clone()),
                reassociate_products_and_quotients(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::If(
                reassociate_products_and_quotients(None, condition.clone()),
                reassociate_products_and_quotients(None, then_branch.clone()),
                reassociate_products_and_quotients(None, else_branch.clone()),
            ),
            errors: vec![],
        }),
    };

    // We end up here as long as `term` isn't a product or quotient. If we have an accumulator,
    // construct a product or quotient as described above. Otherwise, just return the reduced term.
    if let Some((acc, operator)) = acc {
        Rc::new(Term {
            source_range: span(acc.source_range, reduced.source_range),
            group: true,
            variant: match operator {
                ProductOrQuotient::Product => Variant::Product(acc, reduced),
                ProductOrQuotient::Quotient => Variant::Quotient(acc, reduced),
            },
            errors: vec![],
        })
    } else {
        reduced
    }
}

// When reassociating sums and differences to be left-associative, this enum is used to record how
// each term is used.
#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum SumOrDifference {
    Sum,
    Difference,
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
        Variant::ParseError => {
            // This should be unreachable due to [ref:error_check].
            panic!(
                "{} called on a missing term or parse error.",
                "reassociate_sums_and_differences".code_str(),
            )
        }
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
                domain
                    .as_ref()
                    .map(|domain| reassociate_sums_and_differences(None, domain.clone())),
                reassociate_sums_and_differences(None, body.clone()),
            ),
            errors: vec![],
        }),
        Variant::Pi(variable, domain, codomain) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Pi(
                *variable,
                reassociate_sums_and_differences(None, domain.clone()),
                reassociate_sums_and_differences(None, codomain.clone()),
            ),
            errors: vec![],
        }),
        Variant::Application(applicand, argument) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Application(
                reassociate_sums_and_differences(None, applicand.clone()),
                reassociate_sums_and_differences(None, argument.clone()),
            ),
            errors: vec![],
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
            errors: vec![],
        }),
        Variant::Negation(subterm) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Negation(reassociate_sums_and_differences(None, subterm.clone())),
            errors: vec![],
        }),
        Variant::Sum(term1, term2) => {
            return if term2.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: span(acc.0.source_range, term2.source_range),
                        group: true,
                        variant: Variant::Sum(
                            reassociate_sums_and_differences(Some(acc), term1.clone()),
                            reassociate_sums_and_differences(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Sum(
                            reassociate_sums_and_differences(None, term1.clone()),
                            reassociate_sums_and_differences(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                }
            } else {
                reassociate_sums_and_differences(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: span(acc.source_range, term1.source_range),
                                group: true,
                                variant: match operator {
                                    SumOrDifference::Sum => Variant::Sum(
                                        acc,
                                        reassociate_sums_and_differences(None, term1.clone()),
                                    ),
                                    SumOrDifference::Difference => Variant::Difference(
                                        acc,
                                        reassociate_sums_and_differences(None, term1.clone()),
                                    ),
                                },
                                errors: vec![],
                            })
                        } else {
                            reassociate_sums_and_differences(None, term1.clone())
                        },
                        SumOrDifference::Sum,
                    )),
                    term2.clone(),
                )
            };
        }
        Variant::Difference(term1, term2) => {
            return if term2.group {
                if let Some(acc) = acc {
                    Rc::new(Term {
                        source_range: span(acc.0.source_range, term2.source_range),
                        group: true,
                        variant: Variant::Difference(
                            reassociate_sums_and_differences(Some(acc), term1.clone()),
                            reassociate_sums_and_differences(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                } else {
                    Rc::new(Term {
                        source_range: term.source_range,
                        group: term.group,
                        variant: Variant::Difference(
                            reassociate_sums_and_differences(None, term1.clone()),
                            reassociate_sums_and_differences(None, term2.clone()),
                        ),
                        errors: vec![],
                    })
                }
            } else {
                reassociate_sums_and_differences(
                    Some((
                        if let Some((acc, operator)) = acc {
                            Rc::new(Term {
                                source_range: span(acc.source_range, term1.source_range),
                                group: true,
                                variant: match operator {
                                    SumOrDifference::Sum => Variant::Sum(
                                        acc,
                                        reassociate_sums_and_differences(None, term1.clone()),
                                    ),
                                    SumOrDifference::Difference => Variant::Difference(
                                        acc,
                                        reassociate_sums_and_differences(None, term1.clone()),
                                    ),
                                },
                                errors: vec![],
                            })
                        } else {
                            reassociate_sums_and_differences(None, term1.clone())
                        },
                        SumOrDifference::Difference,
                    )),
                    term2.clone(),
                )
            };
        }
        Variant::Product(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Product(
                reassociate_sums_and_differences(None, term1.clone()),
                reassociate_sums_and_differences(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::Quotient(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::Quotient(
                reassociate_sums_and_differences(None, term1.clone()),
                reassociate_sums_and_differences(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::LessThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::LessThan(
                reassociate_sums_and_differences(None, term1.clone()),
                reassociate_sums_and_differences(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::LessThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::LessThanOrEqualTo(
                reassociate_sums_and_differences(None, term1.clone()),
                reassociate_sums_and_differences(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::EqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::EqualTo(
                reassociate_sums_and_differences(None, term1.clone()),
                reassociate_sums_and_differences(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::GreaterThan(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::GreaterThan(
                reassociate_sums_and_differences(None, term1.clone()),
                reassociate_sums_and_differences(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::GreaterThanOrEqualTo(term1, term2) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::GreaterThanOrEqualTo(
                reassociate_sums_and_differences(None, term1.clone()),
                reassociate_sums_and_differences(None, term2.clone()),
            ),
            errors: vec![],
        }),
        Variant::If(condition, then_branch, else_branch) => Rc::new(Term {
            source_range: term.source_range,
            group: term.group,
            variant: Variant::If(
                reassociate_sums_and_differences(None, condition.clone()),
                reassociate_sums_and_differences(None, then_branch.clone()),
                reassociate_sums_and_differences(None, else_branch.clone()),
            ),
            errors: vec![],
        }),
    };

    // We end up here as long as `term` isn't a sum or difference. If we have an accumulator,
    // construct a sum or difference as described above. Otherwise, just return the reduced term.
    if let Some((acc, operator)) = acc {
        Rc::new(Term {
            source_range: span(acc.source_range, reduced.source_range),
            group: true,
            variant: match operator {
                SumOrDifference::Sum => Variant::Sum(acc, reduced),
                SumOrDifference::Difference => Variant::Difference(acc, reduced),
            },
            errors: vec![],
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
    errors: &mut Vec<Error>,
) -> term::Term<'a> {
    match &term.variant {
        Variant::ParseError => {
            // This should be unreachable due to [ref:error_check].
            panic!(
                "{} called on a missing term or parse error.",
                "resolve_variables".code_str(),
            )
        }
        Variant::Type => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Type,
            }
        }
        Variant::Variable(variable) => {
            // Make sure the variable is in scope and calculate its De Bruijn index.
            let index = if let Some(variable_depth) = context.get(variable.name) {
                depth - 1 - variable_depth
            } else {
                // The variable isn't in scope. Report the error.
                errors.push(throw(
                    &if variable.name == PLACEHOLDER_VARIABLE {
                        format!(
                            "{} is not a valid variable name. Rather, it\u{2019}s used to indicate \
                                that a variable will not be used and thus doesn\u{2019}t need a \
                                name.",
                            variable.name.code_str(),
                        )
                    } else {
                        format!("Variable {} not in scope.", variable.name.code_str())
                    },
                    source_path,
                    Some((source_contents, variable.source_range.0)),
                ));

                // Use a bogus De Bruijn index for now.
                0
            };

            // Construct the variable.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Variable(variable.name, index),
            }
        }
        Variant::Lambda(variable, domain, body) => {
            // Resolve variables in the domain if it exists.
            let resolved_domain = domain.as_ref().map(|domain| {
                resolve_variables(source_path, source_contents, domain, depth, context, errors)
            });

            // If the variable is `PLACEHOLDER_VARIABLE`, don't check for naming conflicts, and
            // don't add it to the context.
            if variable.name != PLACEHOLDER_VARIABLE {
                // Report an error if the variable is already in the context.
                if context.contains_key(variable.name) {
                    errors.push(throw(
                        &format!("Variable {} already exists.", variable.name.code_str()),
                        source_path,
                        Some((source_contents, variable.source_range.0)),
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
                source_range: Some(term.source_range.0),
                variant: term::Variant::Lambda(
                    variable.name,
                    resolved_domain.map(Rc::new),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*body,
                        depth + 1,
                        &mut *guard,
                        errors,
                    )),
                ),
            }
        }
        Variant::Pi(variable, domain, codomain) => {
            // Resolve variables in the domain.
            let resolved_domain = resolve_variables(
                source_path,
                source_contents,
                &*domain,
                depth,
                context,
                errors,
            );

            // If the variable is `PLACEHOLDER_VARIABLE`, don't check for naming conflicts, and
            // don't add it to the context.
            if variable.name != PLACEHOLDER_VARIABLE {
                // Report an error if the variable is already in the context.
                if context.contains_key(variable.name) {
                    errors.push(throw(
                        &format!("Variable {} already exists.", variable.name.code_str()),
                        source_path,
                        Some((source_contents, variable.source_range.0)),
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
                source_range: Some(term.source_range.0),
                variant: term::Variant::Pi(
                    variable.name,
                    Rc::new(resolved_domain),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*codomain,
                        depth + 1,
                        &mut *guard,
                        errors,
                    )),
                ),
            }
        }
        Variant::Application(applicand, argument) => {
            // Just resolve variables in the applicand and the argument.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Application(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*applicand,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*argument,
                        depth,
                        context,
                        errors,
                    )),
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

                    // Report an error if the variable is already in the context.
                    if borrowed_context.contains_key(inner_variable.name) {
                        errors.push(throw(
                            &format!(
                                "Variable {} already exists.",
                                inner_variable.name.code_str(),
                            ),
                            source_path,
                            Some((source_contents, inner_variable.source_range.0)),
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
                        errors,
                    ))),
                    None => None,
                };

                // Resolve variables in the definition.
                let resolved_definition = resolve_variables(
                    source_path,
                    source_contents,
                    &*inner_definition,
                    new_depth,
                    borrowed_context,
                    errors,
                );

                // Add the definition to the vector.
                resolved_definitions.push((
                    inner_variable.name,
                    resolved_annotation,
                    Rc::new(resolved_definition),
                ));
            }

            // Temporarily borrow from the scope guard.
            let mut guard = context_cell.borrow_mut();
            let (borrowed_context, _) = &mut (*guard);

            // Resolve definitions in the body and construct the let.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Let(
                    resolved_definitions,
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &innermost_body,
                        new_depth,
                        borrowed_context,
                        errors,
                    )),
                ),
            }
        }
        Variant::Integer => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Integer,
            }
        }
        Variant::IntegerLiteral(integer) => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::IntegerLiteral(integer.clone()),
            }
        }
        Variant::Negation(subterm) => {
            // Just resolve variables in the subterm.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Negation(Rc::new(resolve_variables(
                    source_path,
                    source_contents,
                    &*subterm,
                    depth,
                    context,
                    errors,
                ))),
            }
        }
        Variant::Sum(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Sum(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::Difference(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Difference(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::Product(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Product(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::Quotient(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Quotient(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::LessThan(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::LessThan(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::LessThanOrEqualTo(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::LessThanOrEqualTo(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::EqualTo(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::EqualTo(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::GreaterThan(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::GreaterThan(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::GreaterThanOrEqualTo(term1, term2) => {
            // Just resolve variables in the subterms.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::GreaterThanOrEqualTo(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term1,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*term2,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
        Variant::Boolean => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::Boolean,
            }
        }
        Variant::True => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::True,
            }
        }
        Variant::False => {
            // There are no variables to resolve here.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::False,
            }
        }
        Variant::If(condition, then_branch, else_branch) => {
            // Just resolve variables in the condition and branches.
            term::Term {
                source_range: Some(term.source_range.0),
                variant: term::Variant::If(
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*condition,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*then_branch,
                        depth,
                        context,
                        errors,
                    )),
                    Rc::new(resolve_variables(
                        source_path,
                        source_contents,
                        &*else_branch,
                        depth,
                        context,
                        errors,
                    )),
                ),
            }
        }
    }
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

// This function checks that definitions are evaluated before they are used.
#[allow(clippy::too_many_lines)]
fn check_definitions<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    term: &term::Term<'a>,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    errors: &mut Vec<Error>,
) {
    match &term.variant {
        term::Variant::Type
        | term::Variant::Variable(_, _)
        | term::Variant::Integer
        | term::Variant::IntegerLiteral(_)
        | term::Variant::Boolean
        | term::Variant::True
        | term::Variant::False => {}
        term::Variant::Lambda(_, domain, body) => {
            if let Some(domain) = domain {
                check_definitions(source_path, source_contents, domain, depth, context, errors);
            }

            check_definitions(
                source_path,
                source_contents,
                body,
                depth + 1,
                context,
                errors,
            );
        }
        term::Variant::Pi(_, domain, codomain) => {
            check_definitions(source_path, source_contents, domain, depth, context, errors);
            check_definitions(
                source_path,
                source_contents,
                codomain,
                depth + 1,
                context,
                errors,
            );
        }
        term::Variant::Application(applicand, argument) => {
            check_definitions(
                source_path,
                source_contents,
                applicand,
                depth,
                context,
                errors,
            );
            check_definitions(
                source_path,
                source_contents,
                argument,
                depth,
                context,
                errors,
            );
        }
        term::Variant::Let(definitions, body) => {
            let new_depth = depth + definitions.len();

            for i in 0..definitions.len() {
                if !is_value(&definitions[i].2) {
                    let mut visited = HashSet::new();

                    check_definition(
                        source_path,
                        source_contents,
                        definitions,
                        new_depth,
                        i,
                        i,
                        &mut visited,
                        errors,
                    );
                }
            }

            check_definitions(
                source_path,
                source_contents,
                body,
                new_depth,
                context,
                errors,
            );
        }
        term::Variant::Negation(subterm) => {
            check_definitions(
                source_path,
                source_contents,
                subterm,
                depth,
                context,
                errors,
            );
        }
        term::Variant::Sum(term1, term2)
        | term::Variant::Difference(term1, term2)
        | term::Variant::Product(term1, term2)
        | term::Variant::Quotient(term1, term2)
        | term::Variant::LessThan(term1, term2)
        | term::Variant::LessThanOrEqualTo(term1, term2)
        | term::Variant::EqualTo(term1, term2)
        | term::Variant::GreaterThan(term1, term2)
        | term::Variant::GreaterThanOrEqualTo(term1, term2) => {
            check_definitions(source_path, source_contents, term1, depth, context, errors);
            check_definitions(source_path, source_contents, term2, depth, context, errors);
        }
        term::Variant::If(condition, then_branch, else_branch) => {
            check_definitions(
                source_path,
                source_contents,
                condition,
                depth,
                context,
                errors,
            );
            check_definitions(
                source_path,
                source_contents,
                then_branch,
                depth,
                context,
                errors,
            );
            check_definitions(
                source_path,
                source_contents,
                else_branch,
                depth,
                context,
                errors,
            );
        }
    }
}

// This function checks that all the free variables in a definition will stand for values by the
// time the definition is evaluated.
#[allow(clippy::too_many_arguments)]
#[allow(clippy::type_complexity)]
fn check_definition<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    definitions: &[(&'a str, Option<Rc<term::Term<'a>>>, Rc<term::Term<'a>>)],
    definitions_depth: usize, // Assumes the definitions have already been added to the context
    start_index: usize,       // [0, definitions.len())
    current_index: usize,     // [0, definitions.len())
    visited: &mut HashSet<usize>,
    errors: &mut Vec<Error>,
) {
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
                    check_definition(
                        source_path,
                        source_contents,
                        definitions,
                        definitions_depth,
                        start_index,
                        definition_index,
                        visited,
                        errors,
                    );
                } else if definition_index >= start_index {
                    errors.push(throw(
                        &format!(
                            "The definition of {} references {} (directly or indirectly), \
                                    which will not be available in time during evaluation.",
                            definitions[start_index].0.code_str(),
                            definitions[definition_index].0.code_str(),
                        ),
                        source_path,
                        definitions[start_index]
                            .2
                            .source_range
                            .map(|source_range| (source_contents, source_range)),
                    ));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        assert_fails_vec,
        parser::parse,
        term::{
            Term,
            Variant::{
                Application, Boolean, Difference, EqualTo, False, GreaterThan,
                GreaterThanOrEqualTo, If, Integer, IntegerLiteral, Lambda, LessThan,
                LessThanOrEqualTo, Let, Negation, Pi, Product, Quotient, Sum, True, Type, Variable,
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

        assert_fails_vec!(
            parse(None, source, &tokens[..], &context[..]),
            "file is empty",
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

        assert_fails_vec!(
            parse(None, source, &tokens[..], &context[..]),
            "not in scope",
        );
    }

    #[test]
    fn parse_lambda() {
        let source = "x => x";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 6)),
                variant: Lambda(
                    "x",
                    None,
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("x", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_annotated_lambda() {
        let source = "(x : a) => x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 12)),
                variant: Lambda(
                    "x",
                    Some(Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("a", 0),
                    })),
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

        assert_fails_vec!(
            parse(None, source, &tokens[..], &context[..]),
            "already exists",
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
                    Some(Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: Variable("a", 0),
                    })),
                    Rc::new(Term {
                        source_range: Some((11, 23)),
                        variant: Lambda(
                            "_",
                            Some(Rc::new(Term {
                                source_range: Some((16, 17)),
                                variant: Variable("a", 1),
                            })),
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

        assert_fails_vec!(
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
                                            Some(Rc::new(Term {
                                                source_range: Some((10, 14)),
                                                variant: Type,
                                            })),
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
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_integer() {
        let source = "int";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 3)),
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
    fn parse_negation() {
        let source = "-2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 2)),
                variant: Negation(Rc::new(Term {
                    source_range: Some((1, 2)),
                    variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                })),
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
    fn parse_less_than() {
        let source = "1 < 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: LessThan(
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
    fn parse_less_than_or_equal_to() {
        let source = "1 <= 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 6)),
                variant: LessThanOrEqualTo(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_equal_to() {
        let source = "1 == 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 6)),
                variant: EqualTo(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_greater_than() {
        let source = "1 > 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 5)),
                variant: GreaterThan(
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
    fn parse_greater_than_or_equal_to() {
        let source = "1 >= 2";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 6)),
                variant: GreaterThanOrEqualTo(
                    Rc::new(Term {
                        source_range: Some((0, 1)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&1).unwrap()),
                    }),
                    Rc::new(Term {
                        source_range: Some((5, 6)),
                        variant: IntegerLiteral(ToBigInt::to_bigint(&2).unwrap()),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_boolean() {
        let source = "bool";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], &context[..]).unwrap(),
            Term {
                source_range: Some((0, 4)),
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
                    Some(Rc::new(Term {
                        source_range: Some((5, 9)),
                        variant: Type,
                    })),
                    Rc::new(Term {
                        source_range: Some((14, 58)),
                        variant: Lambda(
                            "b",
                            Some(Rc::new(Term {
                                source_range: Some((19, 23)),
                                variant: Type,
                            })),
                            Rc::new(Term {
                                source_range: Some((28, 58)),
                                variant: Lambda(
                                    "f",
                                    Some(Rc::new(Term {
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
                                    })),
                                    Rc::new(Term {
                                        source_range: Some((44, 58)),
                                        variant: Lambda(
                                            "x",
                                            Some(Rc::new(Term {
                                                source_range: Some((49, 50)),
                                                variant: Variable("a", 2),
                                            })),
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
