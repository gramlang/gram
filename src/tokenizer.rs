use crate::{
    error::{throw, Error},
    format::CodeStr,
    token::{Token, Variant},
};
use std::path::Path;

// Tokenize the contents of a source file.
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
            // Match tokens consisting of a single fixed code point.
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
            ')' => {
                tokens.push(Token {
                    source_range: (i, i + 1),
                    variant: Variant::RightParen,
                });
            }

            // For tokens consisting of two fixed code points, match on the first code point and
            // test the subsequent code point accordingly.
            '=' => {
                if iter.peek() == Some(&(i + 1, '>')) {
                    iter.next();
                    tokens.push(Token {
                        source_range: (i, i + 2),
                        variant: Variant::ThickArrow,
                    });
                } else {
                    throw(
                        format!(
                            "Unexpected symbol {}.",
                            &source_contents[i..i + c.len_utf8()].code_str()
                        ),
                        source_path,
                        source_contents,
                        (i, i + c.len_utf8()),
                    )?;
                }
            }
            '-' => {
                if iter.peek() == Some(&(i + 1, '>')) {
                    iter.next();
                    tokens.push(Token {
                        source_range: (i, i + 2),
                        variant: Variant::ThinArrow,
                    });
                } else {
                    throw(
                        format!(
                            "Unexpected symbol {}.",
                            &source_contents[i..i + c.len_utf8()].code_str()
                        ),
                        source_path,
                        source_contents,
                        (i, i + c.len_utf8()),
                    )?;
                }
            }

            // If the first code point is alphabetic according to the Unicode derived property,
            // keep reading subsequent alphanumeric code points to build up an identifier.
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

                tokens.push(Token {
                    source_range: (i, end),
                    variant: Variant::Identifier(&source_contents[i..end]),
                });
            }

            // Skip whitespace.
            _ if c.is_whitespace() => continue,

            // Skip comments.
            '#' => {
                for (_, d) in &mut iter {
                    if d == '\n' {
                        break;
                    }
                }
            }

            // If we made it this far, the input contains something unexpected.
            _ => {
                throw(
                    format!(
                        "Unexpected symbol {}.",
                        &source_contents[i..i + c.len_utf8()].code_str()
                    ),
                    source_path,
                    source_contents,
                    (i, i + c.len_utf8()),
                )?;
            }
        }
    }

    // If we made it this far, we've successfully tokenized the input.
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use crate::{
        token::{Token, Variant},
        tokenizer::tokenize,
    };

    #[test]
    fn tokenize_empty() {
        assert_eq!(tokenize(None, "").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_whitespace() {
        assert_eq!(tokenize(None, " \t\n").unwrap(), vec![]);
    }

    #[test]
    fn tokenize_colon() {
        assert_eq!(
            tokenize(None, ":").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::Colon,
            }]
        );
    }

    #[test]
    fn tokenize_left_paren() {
        assert_eq!(
            tokenize(None, "(").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::LeftParen,
            }]
        );
    }

    #[test]
    fn tokenize_right_paren() {
        assert_eq!(
            tokenize(None, ")").unwrap(),
            vec![Token {
                source_range: (0, 1),
                variant: Variant::RightParen,
            }]
        );
    }

    #[test]
    fn tokenize_thick_arrow() {
        assert_eq!(
            tokenize(None, "=>").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::ThickArrow,
            }]
        );
    }

    #[test]
    fn tokenize_thin_arrow() {
        assert_eq!(
            tokenize(None, "->").unwrap(),
            vec![Token {
                source_range: (0, 2),
                variant: Variant::ThinArrow,
            }]
        );
    }

    #[test]
    fn tokenize_identifier() {
        assert_eq!(
            tokenize(None, "\u{5e78}\u{798f}").unwrap(),
            vec![Token {
                source_range: (0, 6),
                variant: Variant::Identifier("\u{5e78}\u{798f}"),
            }]
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
            ]
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
            ]
        );
    }

    #[test]
    fn tokenize_unexpected_code_point() {
        assert!(tokenize(None, "$").is_err());
    }

    #[test]
    fn tokenize_partial_thick_arrow() {
        assert!(tokenize(None, "=").is_err());
    }

    #[test]
    fn tokenize_partial_thin_arrow() {
        assert!(tokenize(None, "-").is_err());
    }
}
