use crate::{
    ast::{self, Node},
    error::{throw, Error},
    format::CodeStr,
    token::{self, Token},
};
use scopeguard::defer;
use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    path::Path,
    rc::Rc,
};

// Gram uses a packrat parser, i.e., a recursive descent parser with memoization. This guarantees
// linear-time parsing. We want to parse into the following abstract syntax:
//
//   e = x | ( x : e ) -> e | ( x : e ) => e | e e | ( e )
//
// Interpreted as a grammer, this is ambiguous because `e1 e2 e2` could be parsed as either
// `e1 (e2 e3)` or `(e1 e2) e3`. So we need to change the grammar to make application left- or
// right-associative. We'd prefer left-associativity, but that would lead to a left-recursive
// grammar, which is not supported by the packrat parsing technique. So we employ a trick: we parse
// applications as right-associative, but then we re-associate them in a post-processing step to
// achieve the desired left-associativity [tag:reassociate-applications]. So the grammar we'll be
// parsing is:
//
//   e = x | ( x : e ) -> e | ( x : e ) => e | e-pi-lambda-app e | ( e )
//   e-pi-lambda-app = x | ( e )

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

// The cache key will consist of a `CacheType` indicating which function is being memoized together
// with a position in the input stream.
type CacheKey = (CacheType, usize);

// When a parsing function succeeds, it returns a node together with the position of the first
// unconsumed token.
type CacheResultSuccess<'a> = (Node<'a>, usize);

// An `ErrorFactory` is a function which takes a source path and contents and produces an `Error`.
// It's cheaper to generate and pass around a pointer to a closure than the actual `Error`, which
// may contain a long string message.
type ErrorFactory<'a, 'b> = Rc<dyn Fn(Option<&'a Path>, &'a str) -> Error + 'b>;

// When a parsing function fails, it returns a pair of two things:
// 1. An `ErrorFactory` (see above).
// 2. The position of the first token that caused the parsing to fail. This can be used to
//    determine which error is the most relevant when failing to parse multiple types of nodes.
type CacheResultError<'a, 'b> = (ErrorFactory<'a, 'b>, usize);

// The result of a cache lookup is a `Result` specialized to the success and error types above.
type CacheResult<'a, 'b> = Result<CacheResultSuccess<'a>, CacheResultError<'a, 'b>>;

// A cache is a hash map from cache key to result.
type Cache<'a, 'b> = HashMap<CacheKey, CacheResult<'a, 'b>>;

// This macro should be called at the beginning of every parsing function to do a cache lookup and
// return early on cache hit. It also returns early if there are no remaining tokens to parse.
macro_rules! cache_check {
    ($cache:expr, $type:ident, $start:expr, $tokens:expr, $default:expr $(,)?) => {{
        if let Some(result) = $cache.get(&(CacheType::$type, $start)) {
            return (*result).clone();
        }

        if $start == $tokens.len() {
            cache_return!($cache, $type, $start, $default.clone())
        }
    }};
}

// This macro should be used to cache a value and return it.
macro_rules! cache_return {
    ($cache:expr, $type:ident, $start:expr, $value:expr $(,)?) => {{
        let result = $value;
        $cache.insert((CacheType::$type, $start), result.clone());
        return result;
    }};
}

// This macro should be used instead of the `x?` syntax to return early upon the failure of
// `$expr`. It caches the error before returning it. If `$expr` succeeds, this macro evaluates to
// its value.
macro_rules! fail_fast {
    ($cache:expr, $type:ident, $start:expr, $expr:expr $(,)?) => {{
        let result = $expr;
        match result {
            Ok(value) => value,
            Err(error) => cache_return!($cache, $type, $start, Err(error)),
        }
    }};
}

// This macro is meant to be used as follows:
//
//   let mut candidate = default.clone();
//   try_parse!(candidate, ...);
//   try_parse!(candidate, ...);
//   ...
//
// Then `candidate` will contain the longest matching node, or the error corresponding to the parse
// attempt that matched the most tokens if none of the attempts were successful.
macro_rules! try_parse {
    ($candidate:ident, $expr:expr $(,)?) => {{
        match $expr {
            Ok((node, next)) => {
                if let Ok((_, candidate_next)) = $candidate {
                    if next >= candidate_next {
                        $candidate = Ok((node, next));
                    }
                } else {
                    $candidate = Ok((node, next));
                }
            }
            Err((error, next)) => {
                if let Err((_, candidate_next)) = $candidate {
                    if next >= candidate_next {
                        $candidate = Err((error, next));
                    }
                }
            }
        }
    }};
}

// This macro consumes a single token and evaluates to the position of the next token.
macro_rules! consume_token {
    ($cache:expr, $type:ident, $start:expr, $tokens:expr, $variant:ident, $next:expr $(,)?) => {{
        if $next == $tokens.len() {
            cache_return!(
                $cache,
                $type,
                $start,
                Err((
                    Rc::new(move |source_path, source_contents| throw(
                        format!(
                            "Expected {} after {}.",
                            token::Variant::$variant.to_string().code_str(),
                            $tokens[$next - 1].to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        $tokens[$next - 1].source_range,
                    )) as ErrorFactory<'a, 'b>,
                    $next,
                )),
            )
        }

        if let token::Variant::$variant = $tokens[$next].variant {
            $next + 1
        } else {
            cache_return!(
                $cache,
                $type,
                $start,
                Err((
                    Rc::new(move |source_path, source_contents| throw(
                        format!(
                            "Expected {} but encountered {}.",
                            token::Variant::$variant.to_string().code_str(),
                            $tokens[$next].to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        $tokens[$next].source_range,
                    )) as ErrorFactory<'a, 'b>,
                    $next,
                )),
            )
        }
    }};
}

// This macro consumes an identifier and evaluates to the identifier paired with the position of
// the next token.
macro_rules! consume_identifier {
    ($cache:expr, $type:ident, $start:expr, $tokens:expr, $next:expr $(,)?) => {{
        if $next == $tokens.len() {
            cache_return!(
                $cache,
                $type,
                $start,
                Err((
                    Rc::new(move |source_path, source_contents| throw(
                        format!(
                            "Expected an identifier after {}.",
                            $tokens[$next - 1].to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        $tokens[$next - 1].source_range,
                    )) as ErrorFactory<'a, 'b>,
                    $next,
                )),
            )
        }

        if let token::Variant::Identifier(identifier) = $tokens[$next].variant {
            (identifier, $next + 1)
        } else {
            cache_return!(
                $cache,
                $type,
                $start,
                Err((
                    Rc::new(move |source_path, source_contents| throw(
                        format!(
                            "Expected an identifier but encountered {}.",
                            $tokens[$next].to_string().code_str(),
                        ),
                        source_path,
                        source_contents,
                        $tokens[$next].source_range,
                    )) as ErrorFactory<'a, 'b>,
                    $next,
                )),
            )
        }
    }};
}

// This is the top-level parsing function.
pub fn parse<'a, T: Borrow<[Token<'a>]>, U: BorrowMut<HashMap<&'a str, usize>>>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
    tokens: T,
    mut context: U,
) -> Result<Node<'a>, Error> {
    // Get references to the borrowed data.
    let tokens = tokens.borrow();
    let context = context.borrow_mut();

    // Construct a hash table to memoize parsing results.
    let mut cache = Cache::new();

    // Parse the node.
    let (node, next) = parse_node(
        &mut cache,
        tokens,
        0,
        context.len(),
        context,
        &Err((
            Rc::new(move |source_path, source_contents| {
                throw("Nothing to parse.", source_path, source_contents, (0, 0))
            }) as ErrorFactory<'a, 'a>,
            0,
        )),
    )
    .map_err(|error| error.0(source_path, source_contents))?;

    // Make sure we parsed all the tokens.
    if next != tokens.len() {
        return Err(throw(
            format!("Unexpected {}.", tokens[next].to_string().code_str()),
            source_path,
            source_contents,
            tokens[next].source_range,
        ));
    }

    // Flip the associativity of applications from right to left [ref:reassociate-applications].
    let reassociated = (*reassociate_applications(None, Rc::new(node))).clone();

    // If we made it this far, nothing went wrong.
    Ok(reassociated)
}

// Flip the associativity of applications from right to left.
fn reassociate_applications<'a>(acc: Option<Rc<Node<'a>>>, node: Rc<Node<'a>>) -> Rc<Node<'a>> {
    // In every case except the application case, if we have a value for the accumulator, we want to
    // construct an application with the accumulator as the applicand and the reduced node as the
    // argument. In the application case, we build up the accumulator.
    let reduced = match &node.variant {
        ast::Variant::Lambda(variable, domain, body) => Rc::new(Node {
            source_range: node.source_range,
            variant: ast::Variant::Lambda(
                variable,
                reassociate_applications(None, domain.clone()),
                reassociate_applications(None, body.clone()),
            ),
        }),
        ast::Variant::Pi(variable, domain, codomain) => Rc::new(Node {
            source_range: node.source_range,
            variant: ast::Variant::Pi(
                variable,
                reassociate_applications(None, domain.clone()),
                reassociate_applications(None, codomain.clone()),
            ),
        }),
        ast::Variant::Variable(_, _) => node,
        ast::Variant::Application(applicand, argument) => {
            return reassociate_applications(
                Some(if let Some(acc) = acc {
                    Rc::new(Node {
                        source_range: (acc.source_range.0, applicand.source_range.1),
                        variant: ast::Variant::Application(acc, applicand.clone()),
                    })
                } else {
                    applicand.clone()
                }),
                argument.clone(),
            )
        }
    };

    // We end up here as long as `node` isn't an application. If we have an accumulator, construct
    // an application as described above. Otherwise, just return the reduced node.
    if let Some(acc) = acc {
        Rc::new(Node {
            source_range: (acc.source_range.0, reduced.source_range.1),
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
    default: &CacheResult<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Node, start, tokens, default);

    // This variable will contain the longest-matching node, or the longest-matching error if no
    // node was successfully parsed.
    let mut candidate = default.clone();

    // Try to parse an application.
    try_parse!(
        candidate,
        parse_application(cache, tokens, start, depth, context, default)
    );

    // Try to parse a variable.
    try_parse!(
        candidate,
        parse_variable(cache, tokens, start, depth, context, default)
    );

    // Try to parse a pi type.
    try_parse!(
        candidate,
        parse_pi(cache, tokens, start, depth, context, default)
    );

    // Try to parse a lambda.
    try_parse!(
        candidate,
        parse_lambda(cache, tokens, start, depth, context, default)
    );

    // Try to parse a group.
    try_parse!(
        candidate,
        parse_group(cache, tokens, start, depth, context, default)
    );

    // Return the candidate, which may be a success or an error.
    cache_return!(cache, Node, start, candidate)
}

// Parse a variable.
fn parse_variable<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    default: &CacheResult<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Variable, start, tokens, default);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Variable, start, tokens, start);

    // Look up the variable in the context.
    let de_bruijn_index = if let Some(variable_depth) = context.get(variable) {
        depth - variable_depth - 1
    } else {
        cache_return!(
            cache,
            Variable,
            start,
            Err((
                Rc::new(move |source_path, source_contents| throw(
                    format!("Undefined variable {}.", variable.code_str()),
                    source_path,
                    source_contents,
                    tokens[next - 1].source_range,
                )) as ErrorFactory<'a, 'b>,
                next,
            )),
        )
    };

    // Construct and return the variable.
    cache_return!(
        cache,
        Variable,
        start,
        Ok((
            Node {
                source_range: tokens[start].source_range,
                variant: ast::Variant::Variable(variable, de_bruijn_index),
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
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    default: &CacheResult<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Pi, start, tokens, default);

    // Consume the left parenthesis.
    let next = consume_token!(cache, Pi, start, tokens, LeftParen, start);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Pi, start, tokens, next);

    // Consume the colon.
    let next = consume_token!(cache, Pi, start, tokens, Colon, next);

    // Parse the domain type.
    let (domain, next) = fail_fast!(
        cache,
        Pi,
        start,
        parse_node(
            cache,
            tokens,
            next,
            depth,
            context,
            &Err((
                Rc::new(move |source_path, source_contents| throw(
                    format!(
                        "Missing type for variable {}.",
                        tokens[next - 2].to_string().code_str(),
                    ),
                    source_path,
                    source_contents,
                    tokens[next - 2].source_range,
                )) as ErrorFactory<'a, 'b>,
                next,
            )),
        ),
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Pi, start, tokens, RightParen, next);

    // Consume the arrow.
    let next = consume_token!(cache, Pi, start, tokens, ThinArrow, next);

    // Add the variable to the context.
    context.insert(variable, depth);
    let context_cell = RefCell::new(context);
    defer! {{ context_cell.borrow_mut().remove(variable); }};

    // Parse the codomain type.
    let (codomain, next) = {
        let mut guard = context_cell.borrow_mut();

        fail_fast!(
            cache,
            Pi,
            start,
            parse_node(
                cache,
                tokens,
                next,
                depth + 1,
                &mut *guard,
                &Err((
                    Rc::new(move |source_path, source_contents| throw(
                        "This pi type has no codomain.",
                        source_path,
                        source_contents,
                        (
                            tokens[start].source_range.0,
                            tokens[next - 1].source_range.1,
                        ),
                    )) as ErrorFactory<'a, 'b>,
                    next,
                )),
            ),
        )
    };

    // Construct and return the pi type.
    cache_return!(
        cache,
        Pi,
        start,
        Ok((
            Node {
                source_range: (tokens[start].source_range.0, codomain.source_range.1),
                variant: ast::Variant::Pi(variable, Rc::new(domain), Rc::new(codomain)),
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
    default: &CacheResult<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Lambda, start, tokens, default);

    // Consume the left parenthesis.
    let next = consume_token!(cache, Lambda, start, tokens, LeftParen, start);

    // Consume the variable.
    let (variable, next) = consume_identifier!(cache, Lambda, start, tokens, next);

    // Consume the colon.
    let next = consume_token!(cache, Lambda, start, tokens, Colon, next);

    // Parse the domain type.
    let (domain, next) = fail_fast!(
        cache,
        Lambda,
        start,
        parse_node(
            cache,
            tokens,
            next,
            depth,
            context,
            &Err((
                Rc::new(move |source_path, source_contents| throw(
                    format!(
                        "Expected type for variable {}.",
                        tokens[next - 2].to_string().code_str(),
                    ),
                    source_path,
                    source_contents,
                    tokens[next - 2].source_range,
                )) as ErrorFactory<'a, 'b>,
                next,
            )),
        ),
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Lambda, start, tokens, RightParen, next);

    // Consume the arrow.
    let next = consume_token!(cache, Lambda, start, tokens, ThickArrow, next);

    // Add the variable to the context.
    context.insert(variable, depth);
    let context_cell = RefCell::new(context);
    defer! {{ context_cell.borrow_mut().remove(variable); }};

    // Parse the body.
    let (body, next) = {
        let mut guard = context_cell.borrow_mut();

        fail_fast!(
            cache,
            Lambda,
            start,
            parse_node(
                cache,
                tokens,
                next,
                depth + 1,
                &mut *guard,
                &Err((
                    Rc::new(move |source_path, source_contents| throw(
                        "This lambda has no body.",
                        source_path,
                        source_contents,
                        (
                            tokens[start].source_range.0,
                            tokens[next - 1].source_range.1,
                        ),
                    )) as ErrorFactory<'a, 'b>,
                    next,
                )),
            ),
        )
    };

    // Construct and return the lambda.
    cache_return!(
        cache,
        Lambda,
        start,
        Ok((
            Node {
                source_range: (tokens[start].source_range.0, body.source_range.1),
                variant: ast::Variant::Lambda(variable, Rc::new(domain), Rc::new(body)),
            },
            next,
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
    default: &CacheResult<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Application, start, tokens, default);

    // Parse the applicand.
    let (applicand, next) = fail_fast!(
        cache,
        Application,
        start,
        parse_applicand(cache, tokens, start, depth, context, default),
    );

    // This value will be moved into a closure below. We prefer to move just this value rather than
    // the whole applicand.
    let applicand_source_range = applicand.source_range;

    // Parse the argument.
    let (argument, next) = fail_fast!(
        cache,
        Application,
        start,
        parse_node(
            cache,
            tokens,
            next,
            depth,
            context,
            &Err((
                Rc::new(move |source_path, source_contents| throw(
                    "Missing argument for this applicand.",
                    source_path,
                    source_contents,
                    applicand_source_range,
                )) as ErrorFactory<'a, 'b>,
                next,
            )),
        ),
    );

    // Construct and return the application.
    cache_return!(
        cache,
        Application,
        start,
        Ok((
            Node {
                source_range: (applicand.source_range.0, argument.source_range.1),
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
    default: &CacheResult<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Applicand, start, tokens, default);

    // This variable will contain the longest-matching node, or the longest-matching error if no
    // node was successfully parsed.
    let mut candidate = default.clone();

    // Try to parse a variable.
    try_parse!(
        candidate,
        parse_variable(cache, tokens, start, depth, context, default)
    );

    // Try to parse a group.
    try_parse!(
        candidate,
        parse_group(cache, tokens, start, depth, context, default)
    );

    // Return the candidate, which may be a success or an error.
    cache_return!(cache, Applicand, start, candidate)
}

// Parse a group.
fn parse_group<'a, 'b>(
    cache: &mut Cache<'a, 'b>,
    tokens: &'b [Token<'a>],
    start: usize,
    depth: usize,
    context: &mut HashMap<&'a str, usize>,
    default: &CacheResult<'a, 'b>,
) -> CacheResult<'a, 'b> {
    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Group, start, tokens, default);

    // Consume the left parenthesis.
    let next = consume_token!(cache, Group, start, tokens, LeftParen, start);

    // Parse the inner node.
    let (node, next) = fail_fast!(
        cache,
        Group,
        start,
        parse_node(
            cache,
            tokens,
            next,
            depth,
            context,
            &Err((
                Rc::new(move |source_path, source_contents| throw(
                    "Missing expression in the group that starts here.",
                    source_path,
                    source_contents,
                    tokens[next - 1].source_range,
                )) as ErrorFactory<'a, 'b>,
                next,
            )),
        ),
    );

    // Consume the right parenthesis.
    let next = consume_token!(cache, Group, start, tokens, RightParen, next);

    // If we made it this far, we successfully parsed the group. Return the inner node.
    cache_return!(cache, Group, start, Ok((node, next)))
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Node,
            Variant::{Application, Lambda, Pi, Variable},
        },
        parser::parse,
        tokenizer::tokenize,
    };
    use std::{collections::HashMap, rc::Rc};

    #[test]
    fn parse_empty() {
        let source = "";
        let tokens = tokenize(None, source).unwrap();
        let mut context = HashMap::<&str, usize>::new();

        assert!(parse(None, source, &tokens[..], &mut context).is_err());
    }

    #[test]
    fn parse_lambda() {
        let source = "(x : a) => x";
        let tokens = tokenize(None, source).unwrap();
        let mut context = HashMap::<&str, usize>::new();
        context.insert("a", 0);

        assert_eq!(
            parse(None, source, &tokens[..], &mut context).unwrap(),
            Node {
                source_range: (0, 12),
                variant: Lambda(
                    "x",
                    Rc::new(Node {
                        source_range: (5, 6),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Node {
                        source_range: (11, 12),
                        variant: Variable("x", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_pi() {
        let source = "(x : a) -> x";
        let tokens = tokenize(None, source).unwrap();
        let mut context = HashMap::<&str, usize>::new();
        context.insert("a", 0);

        assert_eq!(
            parse(None, source, &tokens[..], &mut context).unwrap(),
            Node {
                source_range: (0, 12),
                variant: Pi(
                    "x",
                    Rc::new(Node {
                        source_range: (5, 6),
                        variant: Variable("a", 0),
                    }),
                    Rc::new(Node {
                        source_range: (11, 12),
                        variant: Variable("x", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_variable() {
        let source = "x";
        let tokens = tokenize(None, source).unwrap();
        let mut context = HashMap::<&str, usize>::new();
        context.insert("x", 0);

        assert_eq!(
            parse(None, source, &tokens[..], &mut context).unwrap(),
            Node {
                source_range: (0, 1),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn parse_application() {
        let source = "f x";
        let tokens = tokenize(None, source).unwrap();
        let mut context = HashMap::<&str, usize>::new();
        context.insert("f", 0);
        context.insert("x", 1);

        assert_eq!(
            parse(None, source, &tokens[..], &mut context).unwrap(),
            Node {
                source_range: (0, 3),
                variant: Application(
                    Rc::new(Node {
                        source_range: (0, 1),
                        variant: Variable("f", 1),
                    }),
                    Rc::new(Node {
                        source_range: (2, 3),
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
        let mut context = HashMap::<&str, usize>::new();
        context.insert("f", 0);
        context.insert("x", 1);
        context.insert("y", 2);

        assert_eq!(
            parse(None, source, &tokens[..], &mut context).unwrap(),
            Node {
                source_range: (0, 5),
                variant: Application(
                    Rc::new(Node {
                        source_range: (0, 3),
                        variant: Application(
                            Rc::new(Node {
                                source_range: (0, 1),
                                variant: Variable("f", 2),
                            }),
                            Rc::new(Node {
                                source_range: (2, 3),
                                variant: Variable("x", 1),
                            }),
                        ),
                    }),
                    Rc::new(Node {
                        source_range: (4, 5),
                        variant: Variable("y", 0),
                    }),
                ),
            },
        );
    }

    #[test]
    fn parse_group() {
        let source = "(x)";
        let tokens = tokenize(None, source).unwrap();
        let mut context = HashMap::<&str, usize>::new();
        context.insert("x", 0);

        assert_eq!(
            parse(None, source, &tokens[..], &mut context).unwrap(),
            Node {
                source_range: (1, 2),
                variant: Variable("x", 0),
            },
        );
    }

    #[test]
    fn parse_identity_function() {
        let source = "(a : type) => (b : type) => (f : (_ : a) -> b) => (x : a) => f x";
        let tokens = tokenize(None, source).unwrap();
        let mut context = HashMap::<&str, usize>::new();
        context.insert("type", 0);

        assert_eq!(
            parse(None, source, &tokens[..], &mut context).unwrap(),
            Node {
                source_range: (0, 64),
                variant: Lambda(
                    "a",
                    Rc::new(Node {
                        source_range: (5, 9),
                        variant: Variable("type", 0),
                    }),
                    Rc::new(Node {
                        source_range: (14, 64),
                        variant: Lambda(
                            "b",
                            Rc::new(Node {
                                source_range: (19, 23),
                                variant: Variable("type", 1),
                            }),
                            Rc::new(Node {
                                source_range: (28, 64),
                                variant: Lambda(
                                    "f",
                                    Rc::new(Node {
                                        source_range: (33, 45),
                                        variant: Pi(
                                            "_",
                                            Rc::new(Node {
                                                source_range: (38, 39),
                                                variant: Variable("a", 1),
                                            }),
                                            Rc::new(Node {
                                                source_range: (44, 45),
                                                variant: Variable("b", 1),
                                            }),
                                        ),
                                    }),
                                    Rc::new(Node {
                                        source_range: (50, 64),
                                        variant: Lambda(
                                            "x",
                                            Rc::new(Node {
                                                source_range: (55, 56),
                                                variant: Variable("a", 2),
                                            }),
                                            Rc::new(Node {
                                                source_range: (61, 64),
                                                variant: Application(
                                                    Rc::new(Node {
                                                        source_range: (61, 62),
                                                        variant: Variable("f", 1),
                                                    }),
                                                    Rc::new(Node {
                                                        source_range: (63, 64),
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
