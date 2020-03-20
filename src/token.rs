use num_bigint::BigInt;
use std::fmt::{Display, Formatter, Result};

// Keywords
pub const BOOLEAN_KEYWORD: &str = "bool";
pub const ELSE_KEYWORD: &str = "else";
pub const FALSE_KEYWORD: &str = "false";
pub const IF_KEYWORD: &str = "if";
pub const INTEGER_KEYWORD: &str = "int";
pub const THEN_KEYWORD: &str = "then";
pub const TRUE_KEYWORD: &str = "true";
pub const TYPE_KEYWORD: &str = "type";

// The first step of compilation is to split the source into a stream of tokens. This struct
// represents a single token.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub source_range: (usize, usize), // Inclusive on the left and exclusive on the right
    pub variant: Variant<'a>,
}

// We assign each token a "variant" describing what kind of token it is.
// [tag:tokens] [ref:bison_tokens]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Variant<'a> {
    Asterisk,
    Boolean,
    Colon,
    DoubleEquals,
    Else,
    Equals,
    False,
    GreaterThan,
    GreaterThanOrEqualTo,
    Identifier(&'a str),
    If,
    Integer,
    IntegerLiteral(BigInt),
    LeftParen,
    LessThan,
    LessThanOrEqualTo,
    Minus,
    Plus,
    RightParen,
    Slash,
    Terminator(TerminatorType),
    Then,
    ThickArrow,
    ThinArrow,
    True,
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
            Self::Asterisk => write!(f, "*"),
            Self::Boolean => write!(f, "{}", BOOLEAN_KEYWORD),
            Self::Colon => write!(f, ":"),
            Self::DoubleEquals => write!(f, "=="),
            Self::Else => write!(f, "{}", ELSE_KEYWORD),
            Self::Equals => write!(f, "="),
            Self::False => write!(f, "{}", FALSE_KEYWORD),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqualTo => write!(f, ">="),
            Self::Identifier(name) => write!(f, "{}", name),
            Self::If => write!(f, "{}", IF_KEYWORD),
            Self::Integer => write!(f, "{}", INTEGER_KEYWORD),
            Self::IntegerLiteral(integer) => write!(f, "{}", integer),
            Self::LeftParen => write!(f, "("),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqualTo => write!(f, "<="),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::RightParen => write!(f, ")"),
            Self::Slash => write!(f, "/"),
            Self::Terminator(TerminatorType::LineBreak) => write!(f, "\\n"),
            Self::Terminator(TerminatorType::Semicolon) => write!(f, ";"),
            Self::Then => write!(f, "{}", THEN_KEYWORD),
            Self::ThickArrow => write!(f, "=>"),
            Self::ThinArrow => write!(f, "->"),
            Self::True => write!(f, "{}", TRUE_KEYWORD),
            Self::Type => write!(f, "{}", TYPE_KEYWORD),
        }
    }
}
