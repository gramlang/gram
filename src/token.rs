use std::fmt::{Display, Formatter, Result};

// This keyword is for the type of all types, including itself.
pub const TYPE_KEYWORD: &str = "type";

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
    LeftParen,
    RightParen,
    Terminator,
    ThickArrow,
    ThinArrow,
    Type,
    Identifier(&'a str),
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
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::Terminator => write!(f, ";"),
            Self::ThickArrow => write!(f, "=>"),
            Self::ThinArrow => write!(f, "->"),
            Self::Type => write!(f, "{}", TYPE_KEYWORD),
            Self::Identifier(name) => write!(f, "{}", name),
        }
    }
}
