use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub source_range: (usize, usize), // [start, end)
    pub variant: Variant<'a>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Variant<'a> {
    Colon,
    LeftParen,
    RightParen,
    ThickArrow,
    ThinArrow,
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
            Self::Identifier(name) => write!(f, "{}", name),
        }
    }
}
