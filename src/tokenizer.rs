use crate::{
    error::{throw, Error},
    format::CodeStr,
    token::{TerminatorType, Token, Variant, INTEGER_KEYWORD, TYPE_KEYWORD},
};
use num_bigint::BigInt;
use std::path::Path;

// Tokenize the contents of a source file.
#[allow(clippy::too_many_lines)]
pub fn tokenize<'a>(
    source_path: Option<&'a Path>,
    source_contents: &'a str,
) -> Result<Vec<Token<'a>>, Error> {
    // We'll be building up this vector of tokens.
    let mut tokens = vec![];

    // We want to iterate one code point at a time, but we also want the byte indices so we can
    // capture slices.
    let mut iter = source_contents.char_indices().peekable();

    // Consume the input one code point at a time.
    while let Some((i, c)) = iter.next() {
        // Match on the first code point of the token.
        match c {
            // Match tokens corresponding to symbols.
            ':' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::Colon,
                });
            }
            '=' if iter.peek() != Some(&(i + 1, '>')) => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::Equals,
                });
            }
            '(' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::LeftParen,
                });
            }
            ')' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::RightParen,
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
                        Variant::Colon
                        | Variant::Equals
                        | Variant::LeftParen
                        | Variant::Terminator(TerminatorType::LineBreak) /* [tag:no_consecutive_line_break_terminators] */
                        | Variant::ThickArrow
                        | Variant::ThinArrow => false,
                        Variant::RightParen
                        | Variant::Terminator(TerminatorType::Semicolon)
                        | Variant::Type
                        | Variant::Identifier(_)
                        | Variant::Integer
                        | Variant::IntegerLiteral(_) => true,
                    }
                {
                    tokens.push(Token {
                        source_range: (i, i + 1),
                        variant: Variant::Terminator(TerminatorType::LineBreak),
                    });
                }
            }
            '=' if iter.peek() == Some(&(i + 1, '>')) => {
                iter.next();
                tokens.push(Token {
                    source_range: (i, i + 2),
                    variant: Variant::ThickArrow,
                });
            }
            '-' if iter.peek() == Some(&(i + 1, '>')) => {
                iter.next();
                tokens.push(Token {
                    source_range: (i, i + 2),
                    variant: Variant::ThinArrow,
                });
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

                if &source_contents[i..end] == TYPE_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Type,
                    });
                } else if &source_contents[i..end] == INTEGER_KEYWORD {
                    tokens.push(Token {
                        source_range: (i, end),
                        variant: Variant::Integer,
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
                return Err(throw(
                    &format!(
                        "Unexpected symbol {}.",
                        &source_contents[i..i + c.len_utf8()].code_str(),
                    ),
                    source_path,
                    source_contents,
                    (i, i + c.len_utf8()),
                ));
            }
        }
    }

    // Remove trailing line break terminators and line break terminators before certain token types.
    let mut filtered_tokens = vec![];
    let mut tokens_iter = tokens.iter().peekable();
    while let Some(token) = tokens_iter.next() {
        if token.variant == Variant::Terminator(TerminatorType::LineBreak) {
            if let Some(next_token) = tokens_iter.peek() {
                if match next_token.variant {
                    Variant::Colon
                    | Variant::Equals
                    | Variant::ThickArrow
                    | Variant::ThinArrow
                    | Variant::RightParen => false,
                    Variant::LeftParen
                    | Variant::Terminator(TerminatorType::Semicolon)
                    | Variant::Type
                    | Variant::Identifier(_)
                    | Variant::Integer
                    | Variant::IntegerLiteral(_) => true,
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
        assert_fails,
        token::{TerminatorType, Token, Variant, INTEGER_KEYWORD, TYPE_KEYWORD},
        tokenizer::tokenize,
    };
    use num_bigint::ToBigInt;

    #[test]
    fn tokenize_empty() {
        assert_eq!(tokenize(None, "").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_whitespace() {
        assert_eq!(tokenize(None, " \t\n").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_comment() {
        assert_eq!(tokenize(None, "# Hello, World!").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_colon() {
        assert_eq!(
            tokenize(None, ":").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Colon,
            }],
        );
    }

    #[test]
    fn tokenize_equals() {
        assert_eq!(
            tokenize(None, "=").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Equals,
            }],
        );
    }

    #[test]
    fn tokenize_terminator_line_break() {
        assert_eq!(
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
        assert_eq!(
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
    fn tokenize_left_paren() {
        assert_eq!(
            tokenize(None, "(").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::LeftParen,
            }],
        );
    }

    #[test]
    fn tokenize_right_paren() {
        assert_eq!(
            tokenize(None, ")").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::RightParen,
            }],
        );
    }

    #[test]
    fn tokenize_thick_arrow() {
        assert_eq!(
            tokenize(None, "=>").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::ThickArrow,
            }],
        );
    }

    #[test]
    fn tokenize_thin_arrow() {
        assert_eq!(
            tokenize(None, "->").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::ThinArrow,
            }],
        );
    }

    #[test]
    fn tokenize_type() {
        assert_eq!(
            tokenize(None, TYPE_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, TYPE_KEYWORD.len()),
                variant: Variant::Type,
            }],
        );
    }

    #[test]
    fn tokenize_integer() {
        assert_eq!(
            tokenize(None, INTEGER_KEYWORD).unwrap(),
            vec![Token {
                source_range: (0, INTEGER_KEYWORD.len()),
                variant: Variant::Integer,
            }],
        );
    }

    #[test]
    fn tokenize_identifier() {
        assert_eq!(
            tokenize(None, "\u{5e78}\u{798f}").unwrap(),
            vec![Token {
                source_range: (0, 6),
                variant: Variant::Identifier("\u{5e78}\u{798f}"),
            }],
        );
    }

    #[test]
    fn tokenize_integer_literal() {
        assert_eq!(
            tokenize(None, "42").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::IntegerLiteral(ToBigInt::to_bigint(&42).unwrap()),
            }],
        );
    }

    #[test]
    fn tokenize_operators() {
        assert_eq!(
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
        assert_eq!(
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
