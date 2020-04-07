use crate::{
    error::{throw, Error},
    format::CodeStr,
    token::{
        TerminatorType, Token, Variant, BOOLEAN_KEYWORD, ELSE_KEYWORD, FALSE_KEYWORD, IF_KEYWORD,
        INTEGER_KEYWORD, THEN_KEYWORD, TRUE_KEYWORD, TYPE_KEYWORD,
    },
};
use num_bigint::BigInt;
use std::path::Path;
use unicode_segmentation::GraphemeCursor;

// Tokenize the contents of a source file.
#[allow(clippy::cognitive_complexity)]
#[allow(clippy::too_many_lines)]
pub fn tokenize<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
) -> Result<Vec<Token<'a>>, Vec<Error>> {
    // We'll be building up this vector of tokens.
    let mut tokens = vec![];

    // Construct a vector to hold any errors that might be detected below.
    let mut errors = vec![];

    // We want to iterate one code point at a time, but we also want the byte indices so we can
    // capture slices.
    let mut iter = source_contents.char_indices().peekable();

    // Consume the input one code point at a time.
    while let Some((i, c)) = iter.next() {
        // Match on the first code point of the token.
        match c {
            // Match tokens corresponding to symbols.
            '*' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::Asterisk,
                });
            }
            ':' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::Colon,
                });
            }
            '(' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::LeftParen,
                });
            }
            '+' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::Plus,
                });
            }
            ')' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::RightParen,
                });
            }
            '/' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::Slash,
                });
            }
            ';' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::Terminator(TerminatorType::Semicolon),
                });
            }
            '\n' => {
                // [tag:line_break] [tag:tokens_nonempty]
                if !tokens.is_empty()
                    && match tokens.last().unwrap().variant /* [ref:tokens_nonempty] */ {
                        Variant::Asterisk
                        | Variant::Colon
                        | Variant::DoubleEquals
                        | Variant::Else
                        | Variant::Equals
                        | Variant::GreaterThan
                        | Variant::GreaterThanOrEqualTo
                        | Variant::If
                        | Variant::LeftParen
                        | Variant::LessThan
                        | Variant::LessThanOrEqualTo
                        | Variant::Minus
                        | Variant::Plus
                        | Variant::Slash
                        /* [tag:no_consecutive_line_break_terminators] */
                        | Variant::Terminator(TerminatorType::LineBreak)
                        | Variant::Then
                        | Variant::ThickArrow
                        | Variant::ThinArrow => false,
                        Variant::Boolean
                        | Variant::False
                        | Variant::Identifier(_)
                        | Variant::Integer
                        | Variant::IntegerLiteral(_)
                        | Variant::RightParen
                        | Variant::Terminator(TerminatorType::Semicolon)
                        | Variant::True
                        | Variant::Type => true,
                    }
                {
                    tokens.push(Token {
                        source_range: (i, i + 1),
                        variant: Variant::Terminator(TerminatorType::LineBreak),
                    });
                }
            }
            '-' => {
                if let Some(&(_, '>')) = iter.peek() {
                    iter.next();
                    tokens.push(Token {
                        source_range: (i, i + 2),
                        variant: Variant::ThinArrow,
                    });
                } else {
                    tokens.push(Token {
                        source_range: (i, i + 1),
                        variant: Variant::Minus,
                    });
                }
            }
            '<' => {
                if let Some(&(_, '=')) = iter.peek() {
                    iter.next();
                    tokens.push(Token {
                        source_range: (i, i + 2),
                        variant: Variant::LessThanOrEqualTo,
                    });
                } else {
                    tokens.push(Token {
                        source_range: (i, i + 1),
                        variant: Variant::LessThan,
                    });
                }
            }
            '=' => match iter.peek() {
                Some(&(_, '=')) => {
                    iter.next();
                    tokens.push(Token {
                        source_range: (i, i + 2),
                        variant: Variant::DoubleEquals,
                    });
                }
                Some(&(_, '>')) => {
                    iter.next();
                    tokens.push(Token {
                        source_range: (i, i + 2),
                        variant: Variant::ThickArrow,
                    });
                }
                _ => {
                    tokens.push(Token {
                        source_range: (i, i + 1),
                        variant: Variant::Equals,
                    });
                }
            },
            '>' => {
                if let Some(&(_, '=')) = iter.peek() {
                    iter.next();
                    tokens.push(Token {
                        source_range: (i, i + 2),
                        variant: Variant::GreaterThanOrEqualTo,
                    });
                } else {
                    tokens.push(Token {
                        source_range: (i, i + 1),
                        variant: Variant::GreaterThan,
                    });
                }
            }

            // If the first code point is alphabetic according to the Unicode derived property,
            // keep reading subsequent alphanumeric code points and underscores to build up an
            // identifier or keyword.
            _ if c.is_alphabetic() || c == '_' => {
                let mut end = source_contents.len();

                while let Some((j, d)) = iter.peek() {
                    if d.is_alphanumeric() || *d == '_' {
                        iter.next();
                    } else {
                        end = *j;
                        break;
                    }
                }

                if &source_contents[i..end] == BOOLEAN_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Boolean,
                    });
                } else if &source_contents[i..end] == ELSE_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Else,
                    });
                } else if &source_contents[i..end] == FALSE_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::False,
                    });
                } else if &source_contents[i..end] == IF_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::If,
                    });
                } else if &source_contents[i..end] == INTEGER_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Integer,
                    });
                } else if &source_contents[i..end] == THEN_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Then,
                    });
                } else if &source_contents[i..end] == TRUE_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::True,
                    });
                } else if &source_contents[i..end] == TYPE_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Type,
                    });
                } else {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Identifier(&source_contents[i..end]),
                    });
                }
            }

            // If the first code point is a digit, keep reading subsequent digits to build up an
            // integer literal.
            '0'..='9' => {
                let mut end = source_contents.len();

                while let Some((j, d)) = iter.peek() {
                    if ('0'..='9').contains(d) {
                        iter.next();
                    } else {
                        end = *j;
                        break;
                    }
                }

                tokens.push(Token {
                    source_range: (i, end),
                    variant: Variant::IntegerLiteral(
                        // This unwrap is safe due to the format integer literals.
                        BigInt::parse_bytes(source_contents[i..end].as_bytes(), 10).unwrap(),
                    ),
                });
            }

            // Skip whitespace. Note that line breaks are handled above [ref:line_break].
            _ if c.is_whitespace() => continue,

            // Skip comments. Don't skip the terminating line break, if it exists [ref:line_break].
            '#' => {
                while let Some((j, _)) = iter.next() {
                    if iter.peek() == Some(&(j + 1, '\n')) {
                        break;
                    }
                }
            }

            // If we made it this far, the input contains something unexpected.
            _ => {
                // We are going to attempt to compute the problematic grapheme cluster. Note that we
                // might already be in the middle of a grapheme cluster, in which case this logic
                // will compute the remainder of it. To start, we create a cursor that represents
                // the current position within the source.
                let mut cursor = GraphemeCursor::new(i, source_contents.len(), true);

                // Now we find the next grapheme cluster boundary. The first `unwrap` is
                // justified because the docs indicate the only two errors that can be returned are
                // `GraphemeIncomplete::PreContext` and `GraphemeIncomplete::NextChunk`, but the
                // docs also state that these two conditions are impossible since the chunk we
                // provide is the whole source string. The second `unwrap` is justified because we
                // only get `None` in the case where we're at the end of the string, and we know
                // we're not at the end of the string since otherwise we would have exited the loop
                // already.
                let end = cursor.next_boundary(source_contents, 0).unwrap().unwrap();

                // Now that we've computed the grapheme cluster, construct and report the error.
                errors.push(throw(
                    &format!("Unexpected symbol {}.", &source_contents[i..end].code_str()),
                    source_path,
                    Some((source_contents, (i, end))),
                ));
            }
        }
    }

    // If there are any errors at this point, return them.
    if !errors.is_empty() {
        return Err(errors);
    }

    // Remove trailing line break terminators and line break terminators before certain token types.
    let mut filtered_tokens = vec![];
    let mut tokens_iter = tokens.iter().peekable();
    while let Some(token) = tokens_iter.next() {
        if let Variant::Terminator(TerminatorType::LineBreak) = token.variant {
            if let Some(next_token) = tokens_iter.peek() {
                if match next_token.variant {
                    Variant::Asterisk
                    | Variant::Colon
                    | Variant::DoubleEquals
                    | Variant::Else
                    | Variant::Equals
                    | Variant::GreaterThan
                    | Variant::GreaterThanOrEqualTo
                    | Variant::LessThan
                    | Variant::LessThanOrEqualTo
                    | Variant::Minus
                    | Variant::Plus
                    | Variant::RightParen
                    | Variant::Slash
                    | Variant::Then
                    | Variant::ThickArrow
                    | Variant::ThinArrow => false,
                    Variant::Boolean
                    | Variant::False
                    | Variant::Identifier(_)
                    | Variant::If
                    | Variant::Integer
                    | Variant::IntegerLiteral(_)
                    | Variant::LeftParen
                    | Variant::Terminator(TerminatorType::Semicolon)
                    | Variant::True
                    | Variant::Type => true,
                    Variant::Terminator(TerminatorType::LineBreak) => {
                        // [ref:no_consecutive_line_break_terminators]
                        panic!("Two consecutive line break terminators were found.");
                    }
                } {
                    filtered_tokens.push(token.clone());
                }
            }
        } else {
            filtered_tokens.push(token.clone());
        }
    }

    // If we made it this far, we've successfully tokenized the input.
    Ok(filtered_tokens)
}

#[cfg(test)]
mod tests {
    use crate::{
        assert_fails, assert_same,
        token::{
            TerminatorType, Token, Variant, BOOLEAN_KEYWORD, ELSE_KEYWORD, FALSE_KEYWORD,
            IF_KEYWORD, INTEGER_KEYWORD, THEN_KEYWORD, TRUE_KEYWORD, TYPE_KEYWORD,
        },
        tokenizer::tokenize,
    };
    use num_bigint::ToBigInt;

    #[test]
    fn tokenize_empty() {
        assert_same!(tokenize(None, "").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_whitespace() {
        assert_same!(tokenize(None, " \t\n").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_comment() {
        assert_same!(tokenize(None, "# Hello, World!").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_asterisk() {
        assert_same!(
            tokenize(None, "*").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Asterisk,
            }],
        );
    }

    #[test]
    fn tokenize_boolean() {
        assert_same!(
            tokenize(None, BOOLEAN_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, BOOLEAN_KEYWORD.len()),
                variant: Variant::Boolean,
            }],
        );
    }

    #[test]
    fn tokenize_colon() {
        assert_same!(
            tokenize(None, ":").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Colon,
            }],
        );
    }

    #[test]
    fn tokenize_double_equals() {
        assert_same!(
            tokenize(None, "==").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::DoubleEquals,
            }],
        );
    }

    #[test]
    fn tokenize_else() {
        assert_same!(
            tokenize(None, ELSE_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, ELSE_KEYWORD.len()),
                variant: Variant::Else,
            }],
        );
    }

    #[test]
    fn tokenize_equals() {
        assert_same!(
            tokenize(None, "=").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Equals,
            }],
        );
    }

    #[test]
    fn tokenize_false() {
        assert_same!(
            tokenize(None, FALSE_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, FALSE_KEYWORD.len()),
                variant: Variant::False,
            }],
        );
    }

    #[test]
    fn tokenize_greater_than() {
        assert_same!(
            tokenize(None, ">").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::GreaterThan,
            }],
        );
    }

    #[test]
    fn tokenize_greater_than_or_equal() {
        assert_same!(
            tokenize(None, ">=").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::GreaterThanOrEqualTo,
            }],
        );
    }

    #[test]
    fn tokenize_identifier() {
        assert_same!(
            tokenize(None, "\u{5e78}\u{798f}").unwrap(),
            vec![Token {
                source_range: (0, 6),
                variant: Variant::Identifier("\u{5e78}\u{798f}"),
            }],
        );
    }

    #[test]
    fn tokenize_if() {
        assert_same!(
            tokenize(None, IF_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, IF_KEYWORD.len()),
                variant: Variant::If,
            }],
        );
    }

    #[test]
    fn tokenize_integer() {
        assert_same!(
            tokenize(None, INTEGER_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, INTEGER_KEYWORD.len()),
                variant: Variant::Integer,
            }],
        );
    }

    #[test]
    fn tokenize_integer_literal() {
        assert_same!(
            tokenize(None, "42").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::IntegerLiteral(ToBigInt::to_bigint(&42).unwrap()),
            }],
        );
    }

    #[test]
    fn tokenize_left_paren() {
        assert_same!(
            tokenize(None, "(").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::LeftParen,
            }],
        );
    }

    #[test]
    fn tokenize_less_than() {
        assert_same!(
            tokenize(None, "<").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::LessThan,
            }],
        );
    }

    #[test]
    fn tokenize_less_than_or_equal() {
        assert_same!(
            tokenize(None, "<=").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::LessThanOrEqualTo,
            }],
        );
    }

    #[test]
    fn tokenize_minus() {
        assert_same!(
            tokenize(None, "-").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Minus,
            }],
        );
    }

    #[test]
    fn tokenize_plus() {
        assert_same!(
            tokenize(None, "+").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Plus,
            }],
        );
    }

    #[test]
    fn tokenize_right_paren() {
        assert_same!(
            tokenize(None, ")").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::RightParen,
            }],
        );
    }

    #[test]
    fn tokenize_slash() {
        assert_same!(
            tokenize(None, "/").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Slash,
            }],
        );
    }

    #[test]
    fn tokenize_terminator_line_break() {
        assert_same!(
            tokenize(None, "\n\ntype\n\ntype\n\n").unwrap(),
            vec![
                Token {
                    source_range: (2, 6),
                    variant: Variant::Type,
                },
                Token {
                    source_range: (6, 7),
                    variant: Variant::Terminator(TerminatorType::LineBreak),
                },
                Token {
                    source_range: (8, 12),
                    variant: Variant::Type,
                },
            ],
        );
    }

    #[test]
    fn tokenize_terminator_semicolon() {
        assert_same!(
            tokenize(None, ";;type;;type;;").unwrap(),
            vec![
                Token {
                    source_range: (0, 1),
                    variant: Variant::Terminator(TerminatorType::Semicolon),
                },
                Token {
                    source_range: (1, 2),
                    variant: Variant::Terminator(TerminatorType::Semicolon),
                },
                Token {
                    source_range: (2, 6),
                    variant: Variant::Type,
                },
                Token {
                    source_range: (6, 7),
                    variant: Variant::Terminator(TerminatorType::Semicolon),
                },
                Token {
                    source_range: (7, 8),
                    variant: Variant::Terminator(TerminatorType::Semicolon),
                },
                Token {
                    source_range: (8, 12),
                    variant: Variant::Type,
                },
                Token {
                    source_range: (12, 13),
                    variant: Variant::Terminator(TerminatorType::Semicolon),
                },
                Token {
                    source_range: (13, 14),
                    variant: Variant::Terminator(TerminatorType::Semicolon),
                },
            ],
        );
    }

    #[test]
    fn tokenize_then() {
        assert_same!(
            tokenize(None, THEN_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, THEN_KEYWORD.len()),
                variant: Variant::Then,
            }],
        );
    }

    #[test]
    fn tokenize_thick_arrow() {
        assert_same!(
            tokenize(None, "=>").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::ThickArrow,
            }],
        );
    }

    #[test]
    fn tokenize_thin_arrow() {
        assert_same!(
            tokenize(None, "->").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::ThinArrow,
            }],
        );
    }

    #[test]
    fn tokenize_true() {
        assert_same!(
            tokenize(None, TRUE_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, TRUE_KEYWORD.len()),
                variant: Variant::True,
            }],
        );
    }

    #[test]
    fn tokenize_type() {
        assert_same!(
            tokenize(None, TYPE_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, TYPE_KEYWORD.len()),
                variant: Variant::Type,
            }],
        );
    }

    #[test]
    fn tokenize_operators() {
        assert_same!(
            tokenize(None, ":()=>->").unwrap(),
            vec![
                Token {
                    source_range: (0, 1),
                    variant: Variant::Colon,
                },
                Token {
                    source_range: (1, 2),
                    variant: Variant::LeftParen,
                },
                Token {
                    source_range: (2, 3),
                    variant: Variant::RightParen,
                },
                Token {
                    source_range: (3, 5),
                    variant: Variant::ThickArrow,
                },
                Token {
                    source_range: (5, 7),
                    variant: Variant::ThinArrow,
                },
            ],
        );
    }

    #[test]
    fn tokenize_operators_with_whitespace() {
        assert_same!(
            tokenize(None, " : ( ) => -> ").unwrap(),
            vec![
                Token {
                    source_range: (1, 2),
                    variant: Variant::Colon,
                },
                Token {
                    source_range: (3, 4),
                    variant: Variant::LeftParen,
                },
                Token {
                    source_range: (5, 6),
                    variant: Variant::RightParen,
                },
                Token {
                    source_range: (7, 9),
                    variant: Variant::ThickArrow,
                },
                Token {
                    source_range: (10, 12),
                    variant: Variant::ThinArrow,
                },
            ],
        );
    }

    #[test]
    fn tokenize_unexpected_code_point() {
        assert_fails!(tokenize(None, "$"), "Unexpected symbol");
    }
}
