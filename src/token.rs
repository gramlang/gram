use std::fmt::{Display, Formatter, Result};

// This keyword is for the type of all types, including itself.
pub const TYPE: &str = "type";

// The first step of compilation is to split the source into a stream of tokens. This struct
// represents a single token.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub source_range: (usize, usize), // [start, end)
    pub variant: Variant<'a>,
}

// We assign each token a "variant" describing what kind of token it is.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variant<'a> {
    Colon,
    LeftParen,
    RightParen,
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
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::ThickArrow => write!(f, "=>"),
            Self::ThinArrow => write!(f, "->"),
            Self::Type => write!(f, "{}", TYPE),
            Self::Identifier(name) => write!(f, "{}", name),
        }
    }
}
