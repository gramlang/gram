use num_bigint::BigInt;
use std::fmt::{Display, Formatter, Result};

// This keyword is for the type of all types, including itself.
pub const TYPE_KEYWORD: &str = "type";

// This keyword is for the type of integers.
pub const INTEGER_KEYWORD: &str = "integer";

// The first step of compilation is to split the source into a stream of tokens. This struct
// represents a single token.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub source_range: (usize, usize), // [start, end)
    pub variant: Variant<'a>,
}

// We assign each token a "variant" describing what kind of token it is.
// [tag:tokens] [ref:bison_tokens]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variant<'a> {
    Colon,
    Equals,
    Identifier(&'a str),
    Integer,
    IntegerLiteral(BigInt),
    LeftParen,
    Minus,
    Plus,
    RightParen,
    Terminator(TerminatorType),
    ThickArrow,
    ThinArrow,
    Type,
}

// A terminator can be a line break or a semicolon. Note that not every line break is parsed as a
// terminator, however.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TerminatorType {
    LineBreak,
    Semicolon,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.variant)
    }
}

impl<'a> Display for Variant<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Colon => write!(f, ":"),
            Self::Equals => write!(f, "="),
            Self::Identifier(name) => write!(f, "{}", name),
            Self::Integer => write!(f, "{}", INTEGER_KEYWORD),
            Self::IntegerLiteral(integer) => write!(f, "{}", integer),
            Self::LeftParen => write!(f, "("),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::RightParen => write!(f, ")"),
            Self::Terminator(TerminatorType::LineBreak) => write!(f, "\\n"),
            Self::Terminator(TerminatorType::Semicolon) => write!(f, ";"),
            Self::ThickArrow => write!(f, "=>"),
            Self::ThinArrow => write!(f, "->"),
            Self::Type => write!(f, "{}", TYPE_KEYWORD),
        }
    }
}
