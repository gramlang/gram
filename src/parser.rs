use crate::{
    ast::{self, Node},
    error::{throw, throw_context, Error},
    format::CodeStr,
    token::{self, Token},
};
use std::{borrow::Borrow, collections::HashMap, path::Path, rc::Rc};

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

// This is an alias for the hash table type we'll be using for memoization. The keys are pairs of
// cache type and start position in the token stream. The values are the results returned from the
// functions.
type Cache<'a> = HashMap<(CacheType, usize), Result<(Node<'a>, usize), Error>>;

// This macro should be called at the beginning of every parsing function to do a cache lookup and
// return early on cache hit. It also fails fast if there are no remaining tokens to parse.
macro_rules! cache_check {
    ($cache_name:expr, $cache_type:ident, $start:expr, $tokens:expr, $source_path:expr) => {{
        if let Some(result) = $cache_name.get(&(CacheType::$cache_type, $start)) {
            return (*result).clone();
        }

        if $start == $tokens.len() {
            cache_return!(
                $cache_name,
                $cache_type,
                $start,
                throw("Nothing to parse.", $source_path)
            )
        }
    }};
}

// This macro should be used instead of the `x?` syntax to return early upon the failure of
// `$expr`. It caches the error before returning it. If `$expr` succeeds, this macro evaluates to
// its value.
macro_rules! fail_fast {
    ($cache_name:expr, $cache_type:ident, $start:expr, $expr:expr) => {{
        let result = $expr;
        match result {
            Ok(value) => value,
            Err(error) => cache_return!($cache_name, $cache_type, $start, Err(error)),
        }
    }};
}

// This macro returns early upon the success of `$expr`. It caches the value before returning it.
// If `$expr` fails, this macro evaluates to the error.
macro_rules! succeed_fast {
    ($cache_name:expr, $cache_type:ident, $start:expr, $expr:expr) => {{
        let result = $expr;
        match result {
            Ok(value) => cache_return!($cache_name, $cache_type, $start, Ok(value)),
            Err(error) => error,
        }
    }};
}

// This macro should be used to cache a value and return it.
macro_rules! cache_return {
    ($cache_name:expr, $cache_type:ident, $start:expr, $value:expr) => {{
        let result = $value;
        $cache_name.insert((CacheType::$cache_type, $start), result.clone());
        return result;
    }};
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
        ast::Variant::Variable(_) => node,
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

// This is the top-level parsing function.
pub fn parse<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
) -> Result<Node<'a>, Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Construct a hash table to memoize parsing results.
    let mut cache = Cache::<'a>::new();

    // Parse the node.
    let (node, next) = parse_node(&mut cache, &source_path, source_contents, tokens, 0)?;

    // Make sure we parsed all the tokens.
    if next < tokens.len() {
        throw_context(
            format!("Unexpected {}.", next.to_string().code_str()),
            source_path,
            source_contents,
            tokens[next].source_range,
        )?;
    }

    // Flip the associativity of applications from right to left [ref:reassociate-applications].
    let reassociated = (*reassociate_applications(None, Rc::new(node))).clone();

    // If we made it this far, nothing went wrong.
    Ok(reassociated)
}

// Parse a node.
fn parse_node<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    cache: &mut Cache<'a>,
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
    start: usize,
) -> Result<(Node<'a>, usize), Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Node, start, tokens, source_path);

    // Try to parse an application.
    let application_error = succeed_fast!(
        cache,
        Node,
        start,
        parse_application(cache, &source_path, source_contents, tokens, start)
    );

    // Try to parse a variable.
    let _variable_error = succeed_fast!(
        cache,
        Node,
        start,
        parse_variable(cache, &source_path, source_contents, tokens, start)
    );

    // Try to parse a pi type.
    let pi_error = succeed_fast!(
        cache,
        Node,
        start,
        parse_pi(cache, &source_path, source_contents, tokens, start)
    );

    // Try to parse a lambda.
    let _lambda_error = succeed_fast!(
        cache,
        Node,
        start,
        parse_lambda(cache, &source_path, source_contents, tokens, start)
    );

    // Try to parse a group.
    let group_error = succeed_fast!(
        cache,
        Node,
        start,
        parse_group(cache, &source_path, source_contents, tokens, start)
    );

    // If we made it this far, we encountered something unexpected.
    cache_return!(
        cache,
        Node,
        start,
        Err(if tokens[start].variant == token::Variant::LeftParen {
            if start + 2 < tokens.len() && tokens[start + 2].variant == token::Variant::Colon {
                pi_error
            } else {
                group_error
            }
        } else {
            application_error
        })
    )
}

// Parse a variable.
fn parse_variable<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    cache: &mut Cache<'a>,
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
    start: usize,
) -> Result<(Node<'a>, usize), Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Check the cache and make sure we have some tokens to parse
    // [tag:variable_start_token_exists].
    cache_check!(cache, Variable, start, tokens, source_path);

    // Check that the start token is an identifier [ref:variable_start_token_exists].
    let start_token = &tokens[start];
    cache_return!(
        cache,
        Variable,
        start,
        if let token::Variant::Identifier(s) = start_token.variant {
            Ok((
                Node {
                    source_range: start_token.source_range,
                    variant: ast::Variant::Variable(s),
                },
                start + 1,
            ))
        } else {
            throw_context(
                format!(
                    "Unexpected {} where a variable is expected.",
                    start_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                start_token.source_range,
            )
        }
    )
}

// Parse a pi type.
#[allow(clippy::too_many_lines)]
fn parse_pi<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    cache: &mut Cache<'a>,
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
    start: usize,
) -> Result<(Node<'a>, usize), Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Check the cache and make sure we have some tokens to parse
    // [tag:pi_start_token_exists].
    cache_check!(cache, Pi, start, tokens, source_path);

    // Check that the start token is a left parenthesis [ref:pi_start_token_exists].
    let start_token = &tokens[start];
    if start_token.variant != token::Variant::LeftParen {
        cache_return!(
            cache,
            Pi,
            start,
            throw_context(
                format!(
                    "Unexpected {} at beginning of pi type.",
                    start_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                start_token.source_range,
            )
        )
    }
    let next = start + 1;

    // Check that we're not at the end of the stream [tag:pi_variable_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Pi,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing variable after {} in pi type.",
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Parse the variable [ref:pi_variable_token_exists].
    let next_token = &tokens[next];
    let variable = if let token::Variant::Identifier(variable) = next_token.variant {
        variable
    } else {
        cache_return!(
            cache,
            Pi,
            start,
            throw_context(
                format!(
                    "Unexpected {} in pi type where a variable is expected.",
                    next_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                start_token.source_range,
            )
        )
    };
    let next = next + 1;

    // Check that we're not at the end of the stream [tag:pi_colon_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Pi,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing {} after {} in pi type.",
                    ":".code_str(),
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Check that the next token is a colon [ref:pi_colon_token_exists].
    let colon_token = &tokens[next];
    if colon_token.variant != token::Variant::Colon {
        cache_return!(
            cache,
            Pi,
            start,
            throw_context(
                format!(
                    "Missing {} before {} in pi type.",
                    ":".code_str(),
                    colon_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                colon_token.source_range,
            )
        )
    }
    let next = next + 1;

    // Parse the domain type.
    let (domain, next) = fail_fast!(
        cache,
        Pi,
        start,
        parse_node(cache, &source_path, source_contents, tokens, next)
    );

    // Check that we're not at the end of the stream [tag:pi_close_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Pi,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing {} after {}.",
                    ")".code_str(),
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Check that the next token is a right parenthesis [ref:pi_close_token_exists].
    let right_paren_token = &tokens[next];
    if right_paren_token.variant != token::Variant::RightParen {
        cache_return!(
            cache,
            Pi,
            start,
            throw_context(
                format!(
                    "Missing {} before {}.",
                    ")".code_str(),
                    right_paren_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                right_paren_token.source_range,
            )
        )
    }
    let next = next + 1;

    // Check that we're not at the end of the stream [tag:pi_arrow_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Pi,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing {} after {}.",
                    "->".code_str(),
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Check that the next token is a thin arrow [ref:pi_arrow_token_exists].
    let arrow_token = &tokens[next];
    if arrow_token.variant != token::Variant::ThinArrow {
        cache_return!(
            cache,
            Pi,
            start,
            throw_context(
                format!(
                    "Missing {} before {}.",
                    "->".code_str(),
                    arrow_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                arrow_token.source_range,
            )
        )
    }
    let next = next + 1;

    // Parse the codomain type.
    let (codomain, next) = fail_fast!(
        cache,
        Pi,
        start,
        parse_node(cache, &source_path, source_contents, tokens, next)
    );

    // Construct and return the pi type.
    cache_return!(
        cache,
        Pi,
        start,
        Ok((
            Node {
                source_range: (start_token.source_range.0, codomain.source_range.1),
                variant: ast::Variant::Pi(variable, Rc::new(domain), Rc::new(codomain)),
            },
            next,
        ))
    )
}

// Parse a lambda.
#[allow(clippy::too_many_lines)]
fn parse_lambda<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    cache: &mut Cache<'a>,
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
    start: usize,
) -> Result<(Node<'a>, usize), Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Check the cache and make sure we have some tokens to parse
    // [tag:lambda_start_token_exists].
    cache_check!(cache, Lambda, start, tokens, source_path);

    // Check that the start token is a left parenthesis [ref:lambda_start_token_exists].
    let start_token = &tokens[start];
    if start_token.variant != token::Variant::LeftParen {
        cache_return!(
            cache,
            Lambda,
            start,
            throw_context(
                format!(
                    "Unexpected {} at beginning of lambda.",
                    start_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                start_token.source_range,
            )
        )
    }
    let next = start + 1;

    // Check that we're not at the end of the stream [tag:lambda_variable_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Lambda,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing variable after {} in lambda.",
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Parse the variable [ref:lambda_variable_token_exists].
    let next_token = &tokens[next];
    let variable = if let token::Variant::Identifier(variable) = next_token.variant {
        variable
    } else {
        cache_return!(
            cache,
            Lambda,
            start,
            throw_context(
                format!(
                    "Unexpected {} in lambda where a variable is expected.",
                    next_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                start_token.source_range,
            )
        )
    };
    let next = next + 1;

    // Check that we're not at the end of the stream [tag:lambda_colon_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Lambda,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing {} after {} in lambda.",
                    ":".code_str(),
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Check that the next token is a colon [ref:lambda_colon_token_exists].
    let colon_token = &tokens[next];
    if colon_token.variant != token::Variant::Colon {
        cache_return!(
            cache,
            Lambda,
            start,
            throw_context(
                format!(
                    "Missing {} before {} in lambda.",
                    ":".code_str(),
                    colon_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                colon_token.source_range,
            )
        )
    }
    let next = next + 1;

    // Parse the domain type.
    let (domain, next) = fail_fast!(
        cache,
        Lambda,
        start,
        parse_node(cache, &source_path, source_contents, tokens, next)
    );

    // Check that we're not at the end of the stream [tag:lambda_close_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Lambda,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing {} after {}.",
                    ")".code_str(),
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Check that the next token is a right parenthesis [ref:lambda_close_token_exists].
    let right_paren_token = &tokens[next];
    if right_paren_token.variant != token::Variant::RightParen {
        cache_return!(
            cache,
            Lambda,
            start,
            throw_context(
                format!(
                    "Missing {} before {}.",
                    ")".code_str(),
                    right_paren_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                right_paren_token.source_range,
            )
        )
    }
    let next = next + 1;

    // Check that we're not at the end of the stream [tag:lambda_arrow_token_exists].
    if next == tokens.len() {
        cache_return!(
            cache,
            Lambda,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing {} after {}.",
                    "=>".code_str(),
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Check that the next token is a thin arrow [ref:lambda_arrow_token_exists].
    let arrow_token = &tokens[next];
    if arrow_token.variant != token::Variant::ThickArrow {
        cache_return!(
            cache,
            Lambda,
            start,
            throw_context(
                format!(
                    "Missing {} before {}.",
                    "=>".code_str(),
                    arrow_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                arrow_token.source_range,
            )
        )
    }
    let next = next + 1;

    // Parse the codomain type.
    let (codomain, next) = fail_fast!(
        cache,
        Lambda,
        start,
        parse_node(cache, &source_path, source_contents, tokens, next)
    );

    // Construct and return the lambda.
    cache_return!(
        cache,
        Pi,
        start,
        Ok((
            Node {
                source_range: (start_token.source_range.0, codomain.source_range.1),
                variant: ast::Variant::Lambda(variable, Rc::new(domain), Rc::new(codomain)),
            },
            next,
        ))
    )
}

// Parse an application.
fn parse_application<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    cache: &mut Cache<'a>,
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
    start: usize,
) -> Result<(Node<'a>, usize), Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Application, start, tokens, source_path);

    // Parse the applicand.
    let (applicand, next) = fail_fast!(
        cache,
        Application,
        start,
        parse_applicand(cache, &source_path, source_contents, tokens, start)
    );

    // Parse the argument.
    let (argument, next) = fail_fast!(
        cache,
        Application,
        start,
        parse_node(cache, &source_path, source_contents, tokens, next)
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
        ))
    )
}

// Parse an applicand (the left part of an application).
fn parse_applicand<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    cache: &mut Cache<'a>,
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
    start: usize,
) -> Result<(Node<'a>, usize), Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Check the cache and make sure we have some tokens to parse.
    cache_check!(cache, Applicand, start, tokens, source_path);

    // Try to parse a variable.
    let _variable_error = succeed_fast!(
        cache,
        Applicand,
        start,
        parse_variable(cache, &source_path, source_contents, tokens, start)
    );

    // Try to parse a group.
    let group_error = succeed_fast!(
        cache,
        Applicand,
        start,
        parse_group(cache, &source_path, source_contents, tokens, start)
    );

    // If we made it this far, we encountered something unexpected.
    cache_return!(cache, Applicand, start, Err(group_error))
}

// Parse a group.
fn parse_group<'a, T: Borrow<Path>, U: Borrow<str>, V: Borrow<[Token<'a>]>>(
    cache: &mut Cache<'a>,
    source_path: &Option<T>,
    source_contents: U,
    tokens: V,
    start: usize,
) -> Result<(Node<'a>, usize), Error> {
    // Get references to the borrowed data.
    let source_path = source_path.as_ref().map(|path| path.borrow());
    let source_contents = source_contents.borrow();
    let tokens = tokens.borrow();

    // Check the cache and make sure we have some tokens to parse [tag:group_start_token_exists].
    cache_check!(cache, Group, start, tokens, source_path);

    // Check that the start token is a left parenthesis [ref:group_start_token_exists].
    let start_token = &tokens[start];
    if start_token.variant != token::Variant::LeftParen {
        cache_return!(
            cache,
            Group,
            start,
            throw_context(
                format!(
                    "Unexpected {} at beginning of group.",
                    start_token.to_string().code_str()
                ),
                source_path,
                source_contents,
                start_token.source_range,
            )
        )
    }

    // Parse the inner node.
    let (node, next) = fail_fast!(
        cache,
        Group,
        start,
        parse_node(cache, &source_path, source_contents, tokens, start + 1)
    );

    // Check that we're not at the end of the stream.
    if next == tokens.len() {
        cache_return!(
            cache,
            Group,
            start,
            throw(
                // The `unwrap` is safe because we wouldn't have made it this far if `tokens` were
                // empty.
                format!(
                    "Missing {} after {}.",
                    ")".code_str(),
                    tokens.last().unwrap().to_string().code_str()
                ),
                source_path
            )
        )
    }

    // Check that the next token is a right parenthesis.
    if tokens[next].variant != token::Variant::RightParen {
        cache_return!(
            cache,
            Group,
            start,
            throw_context(
                format!(
                    "Missing {} before {}.",
                    ")".code_str(),
                    tokens[next].to_string().code_str()
                ),
                source_path,
                source_contents,
                tokens[next].source_range,
            )
        )
    }

    // If we made it this far, we successfully parsed the group. Return the inner node.
    cache_return!(cache, Group, start, Ok((node, next + 1)))
}
