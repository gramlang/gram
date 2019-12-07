use crate::{
    ast::{self, Node},
    error::{throw, Error},
    format::CodeStr,
    token::{self, Token},
    type_checker::TYPE,
};
use scopeguard::defer;
use std::{borrow::Borrow, cell::RefCell, collections::HashMap, path::Path, rc::Rc};

// Gram uses a packrat parser, i.e., a recursive descent parser with memoization. This guarantees
// linear-time parsing. We want to parse into the following abstract syntax:
//
//   e = x | ( x : e ) -> e | ( x : e ) => e | e e | ( e )
//
// If we interpret these rules as a grammar, we face two problems:
//
//   1. The grammar is left-recursive, and left recursion is not supported by the packrat parsing
//      technique.
//   2. The grammar is ambiguous. For example, the associativity of application has not been
//      specified. Packrat parsers resolve ambiguities using the order of the alternatives in the
//      grammar, but we'd prefer to have an unambiguous grammar in the first place.
//
// To address the ambiguity issue (2), we could start by making application left-associative. This
// is the natural choice to make currying ergonomic. However, this reinforces the left-recursion
// problem (1). To resolve this, we employ a trick: we parse applications as right-associative, but
// then we reassociate them in a post-processing step to achieve the desired left-associativity
// [tag:reassociate-applications]. For example, we parse `f x y` as `f [x y]`, then transform it
// into `[f x] y`. We have to be careful not to reassociate applications in which the
// associativity has already been specified by explicit grouping. For example, we should not
// reassociate `f (x y)`. Thus, when parsing, we record whether a term was parsed as group
// [tag:group-flag].
//
// Now, let's resolve ambiguities. The first ambiguity is that `( x : e ) -> e e` can be parsed in
// two ways:
//
//   1. [( x : e ) -> e] e
//   1. ( x : e ) -> [e e]
//
// A similar ambiguity arises with lambdas. To resolve this, we can disallow pi types and lambdas
// as applicands (unless they are surrounded by parentheses, of course). This leads to:
//
//   e = x | ( x : e ) -> e | ( x : e ) => e | applicand e | ( e )
//   applicand = x | applicand e | ( e )
//
// Now we have to encode the associativity of application into the grammar. As it stands,
// `applicand x e` can be parsed as either of:
//
//   1. [applicand x] e
//   2. applicand [x e]
//
// As discussed above, we'll parse application as right-associative. We can do this by disallowing
// applicands from being applications themselves. Thus, the final grammar is:
//
//   e = x | ( x : e ) -> e | ( x : e ) => e | applicand e | ( e )
//   applicand = x | ( e )

// This represents a fresh variable name. It's never added to the context.
pub const PLACEHOLDER_VARIABLE: &str = "_";

// When memoizing a function, we'll use this enum to identify which function is being memoized.
#[derive(Debug, Eq, Hash, PartialEq)]
enum CacheType {
    Node,
    Variable,
    Pi,
    Lambda,
    Application,
    Applicand,
    Group,
}

// A cache key consists of a `CacheType` indicating which function is being memoized together
// with a position in the input stream.
type CacheKey = (CacheType, usize);

// An `ErrorFactory` is a function which takes a source path and contents and produces an `Error`.
// It's cheaper to generate a closure that produces the `Error` than to generate the actual
// `Error`, which may contain a long string message.
type ErrorFactory<'a, 'b> = Rc<dyn Fn(Option<&'a Path>, &'a str) -> Error + 'b>;

// An `ParseError` is a pair containing:
// 1. An `ErrorFactory`, if there is a more useful error message than the default one.
// 2. The position of the token that caused the error (or perhaps a higher position, if we're
//    confident we're in the right production rule). This position will be used to rank errors and
//    choose the "best" one.
type ParseError<'a, 'b> = (Option<ErrorFactory<'a, 'b>>, usize);

// The result of a cache lookup is a pair consisting of the node that was parsed and the position
// of the next unconsumed token, if the parse was successful. Otherwise, the result is `None`.
type CacheResult<'a, 'b> = Option<(Node<'a>, usize)>;

// A cache is a map from cache key to result.
type Cache<'a, 'b> = HashMap<CacheKey, CacheResult<'a, 'b>>;

// This macro should be called at the beginning of every parsing function to do a cache lookup and
// return early on cache hit. This macro also updates the `$error` such that it occurs at `$start`
// or later.
macro_rules! cache_check {
    ($cache:ident, $type:ident, $start:expr, $error:ident $(,)?) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;

        // Do the cache lookup.
        if let Some(result) = $cache.get(&(CacheType::$type, start)) {
            return (*result).clone();
        }

        // Update the error if necessary to record that we've at least seen the `$start` token.
        if start > $error.1 {
            *$error = (None, start);
        }
    }};
}

// This macro should be used to cache a value and return it.
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

// This macro should be used instead of the `x?` syntax to return early upon the failure of
// `$expr` and cache the result. If `$expr` succeeds, this macro evaluates to its value.
macro_rules! fail_fast {
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

// This macro is meant to be used as follows:
//
//   let mut first_match = None;
//   try_parse!(first_match, ...);
//   try_parse!(first_match, ...);
//   ...
//
// Then `first_match` will contain the first matching node, if any matched at all.
macro_rules! try_parse {
    ($first_match:ident, $value:expr $(,)?) => {{
        // Record the match if one hasn't already been found.
        if $first_match.is_none() {
            $first_match = $value;
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
        $error:ident $(,)?
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let tokens = $tokens;
        let next = $next;

        // Fail if there are no more tokens to parse.
        if next == tokens.len() {
            if next > $error.1 {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            format!(
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
                );
            }

            cache_return!($cache, $type, start, None)
        }

        // Check if the token was the expected one.
        if let token::Variant::$variant = tokens[next].variant {
            next + 1
        } else {
            if next > $error.1 {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            format!(
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
        $error:ident $(,)?
    ) => {{
        // Macros are call-by-name, but we want call-by-value (or at least call-by-need) to avoid
        // accidentally evaluating arguments multiple times. Here we force eager evaluation.
        let start = $start;
        let tokens = $tokens;
        let next = $next;

        // Fail if there are no more tokens to parse.
        if next == tokens.len() {
            if next > $error.1 {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            format!(
                                "Expected an identifier after {}.",
                                tokens[next - 1].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next - 1].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                );
            }

            cache_return!($cache, $type, start, None)
        }

        // Check if the token is actually an identifier.
        if let token::Variant::Identifier(identifier) = tokens[next].variant {
            (identifier, next + 1)
        } else {
            if next > $error.1 {
                *$error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            format!(
                                "Expected an identifier but encountered {}.",
                                tokens[next].to_string().code_str(),
                            ),
                            source_path,
                            source_contents,
                            tokens[next].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                );
            }

            cache_return!($cache, $type, start, None)
        }
    }};
}

// This is the top-level parsing function. All the parsed nodes are guaranteed to have a non-`None`
// `source_range`. The parser also guarantees that all variables are bound, except of course the
// ones in the initial context. Variable shadowing is not allowed.
pub fn parse<'a, T: Borrow<[Token<'a>]>, U: Borrow<[&'a str]>>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    tokens: T,
    context: U, // `TYPE` is implicitly at the front of the context.
) -> Result<Node<'a>, Error> {
    // Get references to the borrowed data.
    let tokens = tokens.borrow();
    let context = context.borrow();

    // Construct a hash table to memoize parsing results.
    let mut cache = Cache::new();

    // Construct a mutable context.
    let mut context: HashMap<&'a str, usize> = [TYPE]
        .iter()
        .chain(context.iter())
        .enumerate()
        .map(|(i, variable)| (*variable, i))
        .collect();

    // Construct a default error in case none of the tokens can be parsed.
    let mut error: ParseError = (None, 0);

    // Parse the node.
    let result = parse_node(
        &mut cache,
        tokens,
        0,
        context.len(),
        &mut context,
        &mut error,
    );

    // Check if we managed to parse something.
    let first_unparsed_token = if let Some((node, next)) = result {
        // We parsed something, but did we parse everything?
        if next == tokens.len() {
            // We parsed everything. Flip the associativity of applications with non-grouped
            // arguments from right to left and return the result [ref:reassociate-applications].
            return Ok((*reassociate_applications(None, Rc::new(node))).clone());
        } else {
            // We didn't parse all the tokens. Remember which one we stopped at.
            next
        }
    } else {
        // The parse failed. Remember which token caused the parse to fail.
        error.1
    };

    // If we made it this far, something went wrong. See if we have an error factory.
    if let (Some(factory), _) = error {
        // We have one. Use it to generate the error.
        Err(factory(source_path, source_contents))
    } else {
        // All we have is the position of the token that caused the parse to fail. That position
        // might be the end of the stream, which means we were in the middle of parsing something
        // and expected more tokens. Let's check if we're at the end of the stream.
        Err(if first_unparsed_token < tokens.len() {
            // There was some token that caused the parse to fail. Report it.
            throw(
                format!(
                    "Unexpected {}.",
                    tokens[first_unparsed_token].to_string().code_str()
                ),
                source_path,
                source_contents,
                tokens[first_unparsed_token].source_range,
            )
        } else {
            // We ran out of tokens during parsing. Report that.
            throw(
                "Unexpected end of file.",
                source_path,
                source_contents,
                tokens.last().map_or((0, 0), |token| token.source_range),
            )
        })
    }
}

// Flip the associativity of applications from right to left.
fn reassociate_applications<'a>(acc: Option<Rc<Node<'a>>>, node: Rc<Node<'a>>) -> Rc<Node<'a>> {
    // In every case except the application case, if we have a value for the accumulator, we want to
    // construct an application with the accumulator as the applicand and the reduced node as the
    // argument. In the application case, we build up the accumulator.
    let reduced = match &node.variant {
        ast::Variant::Lambda(variable, domain, body) => Rc::new(Node {
            source_range: node.source_range,
            group: node.group,
            variant: ast::Variant::Lambda(
                variable,
                reassociate_applications(None, domain.clone()),
                reassociate_applications(None, body.clone()),
            ),
        }),
        ast::Variant::Pi(variable, domain, codomain) => Rc::new(Node {
            source_range: node.source_range,
            group: node.group,
            variant: ast::Variant::Pi(
                variable,
                reassociate_applications(None, domain.clone()),
                reassociate_applications(None, codomain.clone()),
            ),
        }),
        ast::Variant::Variable(_, _) => node,
        ast::Variant::Application(applicand, argument) => {
            return if argument.group {
                if let Some(acc) = acc {
                    Rc::new(Node {
                        source_range: if let (Some((start, _)), Some((_, end))) =
                            (acc.source_range, argument.source_range)
                        {
                            Some((start, end))
                        } else {
                            None
                        },
                        group: true, // To ensure the resulting node is still parse-able when printed
                        variant: ast::Variant::Application(
                            reassociate_applications(Some(acc), applicand.clone()),
                            reassociate_applications(None, argument.clone()),
                        ),
                    })
                } else {
                    Rc::new(Node {
                        source_range: node.source_range,
                        group: node.group,
                        variant: ast::Variant::Application(
                            reassociate_applications(None, applicand.clone()),
                            reassociate_applications(None, argument.clone()),
                        ),
                    })
                }
            } else {
                reassociate_applications(
                    Some(if let Some(acc) = acc {
                        Rc::new(Node {
                            source_range: if let (Some((start, _)), Some((_, end))) =
                                (acc.source_range, applicand.source_range)
                            {
                                Some((start, end))
                            } else {
                                None
                            },
                            group: true, // To ensure the resulting node is still parse-able when printed
                            variant: ast::Variant::Application(acc, applicand.clone()),
                        })
                    } else {
                        applicand.clone()
                    }),
                    argument.clone(),
                )
            };
        }
    };

    // We end up here as long as `node` isn't an application. If we have an accumulator, construct
    // an application as described above. Otherwise, just return the reduced node.
    if let Some(acc) = acc {
        Rc::new(Node {
            source_range: if let (Some((start, _)), Some((_, end))) =
                (acc.source_range, reduced.source_range)
            {
                Some((start, end))
            } else {
                None
            },
            group: true, // To ensure the resulting node is still parse-able when printed
            variant: ast::Variant::Application(acc, reduced),
        })
    } else {
        reduced
    }
}

// Parse a node.
fn parse_node<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Node, start, error);

    // This will be set to the result of the first successful parse.
    let mut first_match = None;

    // Try to parse an application.
    try_parse!(
        first_match,
        parse_application(cache, tokens, start, depth, context, error)
    );

    // Try to parse a variable.
    try_parse!(
        first_match,
        parse_variable(cache, tokens, start, depth, context, error)
    );

    // Try to parse a lambda.
    try_parse!(
        first_match,
        parse_lambda(cache, tokens, start, depth, context, error)
    );

    // Try to parse a pi type.
    try_parse!(
        first_match,
        parse_pi(cache, tokens, start, depth, context, error)
    );

    // Try to parse a group.
    try_parse!(
        first_match,
        parse_group(cache, tokens, start, depth, context, error)
    );

    // Return the match, if one was found.
    cache_return!(cache, Node, start, first_match)
}

// Parse a variable.
fn parse_variable<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Variable, start, error);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Variable, start, tokens, start, error);

    // Look up the variable in the context and compute its De Bruijn index.
    let index = if let Some(variable_depth) = context.get(variable) {
        depth - variable_depth - 1
    } else {
        if next > error.1 {
            *error = (
                Some(Rc::new(move |source_path, source_contents| {
                    throw(
                        format!("Undefined variable {}.", variable.code_str()),
                        source_path,
                        source_contents,
                        tokens[next - 1].source_range,
                    )
                }) as ErrorFactory),
                next,
            );
        }

        cache_return!(cache, Variable, start, None);
    };

    // Construct and return the variable.
    cache_return!(
        cache,
        Variable,
        start,
        Some((
            Node {
                source_range: Some(tokens[start].source_range),
                group: false,
                variant: ast::Variant::Variable(variable, index),
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
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Lambda, start, error);

    // Consume the left parenthesis.
    let variable_pos = consume_token!(cache, Lambda, start, tokens, LeftParen, start, error);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Lambda, start, tokens, variable_pos, error);

    // Consume the colon.
    let next = consume_token!(cache, Lambda, start, tokens, Colon, next, error);

    // Parse the domain type.
    let (domain, next) = fail_fast!(
        cache,
        Lambda,
        start,
        parse_node(cache, tokens, next, depth, context, error),
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Lambda, start, tokens, RightParen, next, error);

    // Consume the arrow.
    let next = consume_token!(cache, Lambda, start, tokens, ThickArrow, next, error);

    // If the variable is `PLACEHOLDER_VARIABLE`, don't check for naming conflicts, and don't add
    // it to the context.
    if variable != PLACEHOLDER_VARIABLE {
        // Fail if the variable is already in the context.
        if context.contains_key(variable) {
            if next > error.1 {
                *error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            format!("Variable {} already exists.", variable.code_str()),
                            source_path,
                            source_contents,
                            tokens[variable_pos].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                );
            }

            cache_return!(cache, Lambda, start, None);
        }

        // Add the variable to the context.
        context.insert(variable, depth);
    }

    // Remove the variable from the context (if it was added) when the function returns.
    let context_cell = RefCell::new(context);
    defer! {{ context_cell.borrow_mut().remove(variable); }};

    // Parse the body.
    let (body, next) = {
        let mut guard = context_cell.borrow_mut();

        fail_fast!(
            cache,
            Lambda,
            start,
            parse_node(cache, tokens, next, depth + 1, &mut *guard, error),
        )
    };

    // Construct and return the lambda.
    cache_return!(
        cache,
        Lambda,
        start,
        Some((
            Node {
                source_range: if let ((start, _), Some((_, end))) =
                    (tokens[start].source_range, body.source_range)
                {
                    Some((start, end))
                } else {
                    None
                },
                group: false,
                variant: ast::Variant::Lambda(variable, Rc::new(domain), Rc::new(body)),
            },
            next
        )),
    )
}

// Parse a pi type.
fn parse_pi<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Pi, start, error);

    // Consume the left parenthesis.
    let variable_pos = consume_token!(cache, Pi, start, tokens, LeftParen, start, error);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Pi, start, tokens, variable_pos, error);

    // Consume the colon.
    let next = consume_token!(cache, Pi, start, tokens, Colon, next, error);

    // Parse the domain type.
    let (domain, next) = fail_fast!(
        cache,
        Pi,
        start,
        parse_node(cache, tokens, next, depth, context, error),
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Pi, start, tokens, RightParen, next, error);

    // Consume the arrow.
    let next = consume_token!(cache, Pi, start, tokens, ThinArrow, next, error);

    // If the variable is `PLACEHOLDER_VARIABLE`, don't check for naming conflicts, and don't add
    // it to the context.
    if variable != PLACEHOLDER_VARIABLE {
        // Fail if the variable is already in the context.
        if context.contains_key(variable) {
            if next > error.1 {
                *error = (
                    Some(Rc::new(move |source_path, source_contents| {
                        throw(
                            format!("Variable {} already exists.", variable.code_str()),
                            source_path,
                            source_contents,
                            tokens[variable_pos].source_range,
                        )
                    }) as ErrorFactory),
                    next,
                );
            }

            cache_return!(cache, Pi, start, None);
        }

        // Add the variable to the context.
        context.insert(variable, depth);
    }

    // Remove the variable from the context (if it was added) when the function returns.
    let context_cell = RefCell::new(context);
    defer! {{ context_cell.borrow_mut().remove(variable); }};

    // Parse the codomain.
    let (codomain, next) = {
        let mut guard = context_cell.borrow_mut();

        fail_fast!(
            cache,
            Pi,
            start,
            parse_node(cache, tokens, next, depth + 1, &mut *guard, error),
        )
    };

    // Construct and return the pi type.
    cache_return!(
        cache,
        Pi,
        start,
        Some((
            Node {
                source_range: if let ((start, _), Some((_, end))) =
                    (tokens[start].source_range, codomain.source_range)
                {
                    Some((start, end))
                } else {
                    None
                },
                group: false,
                variant: ast::Variant::Pi(variable, Rc::new(domain), Rc::new(codomain)),
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
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Application, start, error);

    // Parse the applicand.
    let (applicand, next) = fail_fast!(
        cache,
        Application,
        start,
        parse_applicand(cache, tokens, start, depth, context, error),
    );

    // Parse the argument.
    let (argument, next) = fail_fast!(
        cache,
        Application,
        start,
        parse_node(cache, tokens, next, depth, context, error),
    );

    // Construct and return the application.
    cache_return!(
        cache,
        Application,
        start,
        Some((
            Node {
                source_range: if let (Some((start, _)), Some((_, end))) =
                    (applicand.source_range, argument.source_range)
                {
                    Some((start, end))
                } else {
                    None
                },
                group: false,
                variant: ast::Variant::Application(Rc::new(applicand), Rc::new(argument)),
            },
            next,
        )),
    )
}

// Parse an applicand (the left part of an application).
fn parse_applicand<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Applicand, start, error);

    // This will be set to the result of the first successful parse.
    let mut first_match = None;

    // Try to parse a variable.
    try_parse!(
        first_match,
        parse_variable(cache, tokens, start, depth, context, error)
    );

    // Try to parse a group.
    try_parse!(
        first_match,
        parse_group(cache, tokens, start, depth, context, error)
    );

    // Return the match, if one was found.
    cache_return!(cache, Applicand, start, first_match)
}

// Parse a group.
fn parse_group<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    error: &mut ParseError<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Group, start, error);

    // Consume the left parenthesis.
    let next = consume_token!(cache, Group, start, tokens, LeftParen, start, error);

    // Parse the inner node.
    let (node, next) = fail_fast!(
        cache,
        Group,
        start,
        parse_node(cache, tokens, next, depth, context, error),
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Group, start, tokens, RightParen, next, error);

    // If we made it this far, we successfully parsed the group. Return the inner node.
    cache_return!(
        cache,
        Group,
        start,
        Some((
            Node {
                source_range: node.source_range,
                group: true,
                variant: node.variant,
            },
            next,
        ))
    )
}

#[cfg(test)]
mod tests {
    use crate::{
        assert_fails,
        ast::{
            Node,
            Variant::{Application, Lambda, Pi, Variable},
        },
        parser::parse,
        tokenizer::tokenize,
        type_checker::TYPE,
    };
    use std::rc::Rc;

    #[test]
    fn parse_empty() {
        let source = "";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_fails!(
            parse(None, source, &tokens[..], context),
            "Unexpected end of file",
        );
    }

    #[test]
    fn parse_variable() {
        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["x"];

        assert_eq!(
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 1)),
                group: false,
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
            parse(None, source, &tokens[..], context),
            "Undefined variable",
        );
    }

    #[test]
    fn parse_lambda() {
        let source = "(x : a) => x";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 12)),
                group: false,
                variant: Lambda(
                    "x",
                    Rc::new(Node {
                        source_range: Some((5, 6)),
                        group: false,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Node {
                        source_range: Some((11, 12)),
                        group: false,
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

        assert_fails!(parse(None, source, &tokens[..], context), "already exists");
    }

    #[test]
    fn parse_lambda_placeholder_variable() {
        let source = "(_ : a) => (_ : a) => a";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 23)),
                group: false,
                variant: Lambda(
                    "_",
                    Rc::new(Node {
                        source_range: Some((5, 6)),
                        group: false,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Node {
                        source_range: Some((11, 23)),
                        group: false,
                        variant: Lambda(
                            "_",
                            Rc::new(Node {
                                source_range: Some((16, 17)),
                                group: false,
                                variant: Variable("a", 1),
                            }),
                            Rc::new(Node {
                                source_range: Some((22, 23)),
                                group: false,
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
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 12)),
                group: false,
                variant: Pi(
                    "x",
                    Rc::new(Node {
                        source_range: Some((5, 6)),
                        group: false,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Node {
                        source_range: Some((11, 12)),
                        group: false,
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

        assert_fails!(parse(None, source, &tokens[..], context), "already exists",);
    }

    #[test]
    fn parse_pi_placeholder_variable() {
        let source = "(_ : a) -> (_ : a) -> a";
        let tokens = tokenize(None, source).unwrap();
        let context = ["a"];

        assert_eq!(
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 23)),
                group: false,
                variant: Pi(
                    "_",
                    Rc::new(Node {
                        source_range: Some((5, 6)),
                        group: false,
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Node {
                        source_range: Some((11, 23)),
                        group: false,
                        variant: Pi(
                            "_",
                            Rc::new(Node {
                                source_range: Some((16, 17)),
                                group: false,
                                variant: Variable("a", 1),
                            }),
                            Rc::new(Node {
                                source_range: Some((22, 23)),
                                group: false,
                                variant: Variable("a", 2),
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
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 3)),
                group: true,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((0, 1)),
                        group: false,
                        variant: Variable("f", 1),
                    }),
                    Rc::new(Node {
                        source_range: Some((2, 3)),
                        group: false,
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
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 5)),
                group: true,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((0, 3)),
                        group: true,
                        variant: Application(
                            Rc::new(Node {
                                source_range: Some((0, 1)),
                                group: false,
                                variant: Variable("f", 2),
                            }),
                            Rc::new(Node {
                                source_range: Some((2, 3)),
                                group: false,
                                variant: Variable("x", 1),
                            }),
                        ),
                    }),
                    Rc::new(Node {
                        source_range: Some((4, 5)),
                        group: false,
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
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 6)),
                group: false,
                variant: Application(
                    Rc::new(Node {
                        source_range: Some((0, 1)),
                        group: false,
                        variant: Variable("f", 2),
                    }),
                    Rc::new(Node {
                        source_range: Some((3, 6)),
                        group: true,
                        variant: Application(
                            Rc::new(Node {
                                source_range: Some((3, 4)),
                                group: false,
                                variant: Variable("x", 1),
                            }),
                            Rc::new(Node {
                                source_range: Some((5, 6)),
                                group: false,
                                variant: Variable("y", 0),
                            }),
                        ),
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
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((1, 2)),
                group: true,
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn parse_identity_function() {
        let source = "(a : type) => (b : type) => (f : (_ : a) -> b) => (x : a) => f x";
        let tokens = tokenize(None, source).unwrap();
        let context = [];

        assert_eq!(
            parse(None, source, &tokens[..], context).unwrap(),
            Node {
                source_range: Some((0, 64)),
                group: false,
                variant: Lambda(
                    "a",
                    Rc::new(Node {
                        source_range: Some((5, 9)),
                        group: false,
                        variant: Variable(TYPE, 0),
                    }),
                    Rc::new(Node {
                        source_range: Some((14, 64)),
                        group: false,
                        variant: Lambda(
                            "b",
                            Rc::new(Node {
                                source_range: Some((19, 23)),
                                group: false,
                                variant: Variable(TYPE, 1),
                            }),
                            Rc::new(Node {
                                source_range: Some((28, 64)),
                                group: false,
                                variant: Lambda(
                                    "f",
                                    Rc::new(Node {
                                        source_range: Some((33, 45)),
                                        group: false,
                                        variant: Pi(
                                            "_",
                                            Rc::new(Node {
                                                source_range: Some((38, 39)),
                                                group: false,
                                                variant: Variable("a", 1),
                                            }),
                                            Rc::new(Node {
                                                source_range: Some((44, 45)),
                                                group: false,
                                                variant: Variable("b", 1),
                                            }),
                                        ),
                                    }),
                                    Rc::new(Node {
                                        source_range: Some((50, 64)),
                                        group: false,
                                        variant: Lambda(
                                            "x",
                                            Rc::new(Node {
                                                source_range: Some((55, 56)),
                                                group: false,
                                                variant: Variable("a", 2),
                                            }),
                                            Rc::new(Node {
                                                source_range: Some((61, 64)),
                                                group: true,
                                                variant: Application(
                                                    Rc::new(Node {
                                                        source_range: Some((61, 62)),
                                                        group: false,
                                                        variant: Variable("f", 1),
                                                    }),
                                                    Rc::new(Node {
                                                        source_range: Some((63, 64)),
                                                        group: false,
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
