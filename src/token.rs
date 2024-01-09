use {
    crate::error::SourceRange,
    num_bigint::BigInt,
    std::fmt::{Display, Formatter, Result},
};

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
#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub source_range: SourceRange,
    pub variant: Variant<'a>,
}

// We assign each token a "variant" describing what kind of token it is.
// [tag:tokens] [ref:bison_tokens]
#[derive(Clone, Debug)]
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
    LeftCurly,
    LeftParen,
    LessThan,
    LessThanOrEqualTo,
    Minus,
    Plus,
    RightCurly,
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
#[derive(Clone, Debug)]
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
            Self::Boolean => write!(f, "{BOOLEAN_KEYWORD}"),
            Self::Colon => write!(f, ":"),
            Self::DoubleEquals => write!(f, "=="),
            Self::Else => write!(f, "{ELSE_KEYWORD}"),
            Self::Equals => write!(f, "="),
            Self::False => write!(f, "{FALSE_KEYWORD}"),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqualTo => write!(f, ">="),
            Self::Identifier(name) => write!(f, "{name}"),
            Self::If => write!(f, "{IF_KEYWORD}"),
            Self::Integer => write!(f, "{INTEGER_KEYWORD}"),
            Self::IntegerLiteral(integer) => write!(f, "{integer}"),
            Self::LeftCurly => write!(f, "{{"),
            Self::LeftParen => write!(f, "("),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqualTo => write!(f, "<="),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::RightCurly => write!(f, "}}"),
            Self::RightParen => write!(f, ")"),
            Self::Slash => write!(f, "/"),
            Self::Terminator(TerminatorType::LineBreak) => write!(f, "\\n"),
            Self::Terminator(TerminatorType::Semicolon) => write!(f, ";"),
            Self::Then => write!(f, "{THEN_KEYWORD}"),
            Self::ThickArrow => write!(f, "=>"),
            Self::ThinArrow => write!(f, "->"),
            Self::True => write!(f, "{TRUE_KEYWORD}"),
            Self::Type => write!(f, "{TYPE_KEYWORD}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        crate::{
            error::SourceRange,
            token::{
                TerminatorType, Token, Variant, BOOLEAN_KEYWORD, ELSE_KEYWORD, FALSE_KEYWORD,
                IF_KEYWORD, INTEGER_KEYWORD, THEN_KEYWORD, TRUE_KEYWORD, TYPE_KEYWORD,
            },
        },
        num_bigint::ToBigInt,
    };

    #[test]
    fn token_display() {
        assert_eq!(
            format!(
                "{}",
                Token {
                    source_range: SourceRange { start: 0, end: 0 },
                    variant: Variant::Asterisk,
                },
            ),
            "*",
        );
    }

    #[test]
    fn variant_asterisk_display() {
        assert_eq!(format!("{}", Variant::Asterisk), "*");
    }

    #[test]
    fn variant_boolean_display() {
        assert_eq!(format!("{}", Variant::Boolean), BOOLEAN_KEYWORD);
    }

    #[test]
    fn variant_colon_display() {
        assert_eq!(format!("{}", Variant::Colon), ":");
    }

    #[test]
    fn variant_double_equals_display() {
        assert_eq!(format!("{}", Variant::DoubleEquals), "==");
    }

    #[test]
    fn variant_else_display() {
        assert_eq!(format!("{}", Variant::Else), ELSE_KEYWORD);
    }

    #[test]
    fn variant_equals_display() {
        assert_eq!(format!("{}", Variant::Equals), "=");
    }

    #[test]
    fn variant_false_display() {
        assert_eq!(format!("{}", Variant::False), FALSE_KEYWORD);
    }

    #[test]
    fn variant_greater_than_display() {
        assert_eq!(format!("{}", Variant::GreaterThan), ">");
    }

    #[test]
    fn variant_greater_than_or_equal_to_display() {
        assert_eq!(format!("{}", Variant::GreaterThanOrEqualTo), ">=");
    }

    #[test]
    fn variant_identifier_display() {
        assert_eq!(format!("{}", Variant::Identifier("foo")), "foo");
    }

    #[test]
    fn variant_if_display() {
        assert_eq!(format!("{}", Variant::If), IF_KEYWORD);
    }

    #[test]
    fn variant_integer_display() {
        assert_eq!(format!("{}", Variant::Integer), INTEGER_KEYWORD);
    }

    #[test]
    fn variant_integer_literal_display() {
        assert_eq!(
            format!(
                "{}",
                Variant::IntegerLiteral(ToBigInt::to_bigint(&42_i32).unwrap()),
            ),
            "42",
        );
    }

    #[test]
    fn variant_left_curly_display() {
        assert_eq!(format!("{}", Variant::LeftCurly), "{");
    }

    #[test]
    fn variant_left_paren_display() {
        assert_eq!(format!("{}", Variant::LeftParen), "(");
    }

    #[test]
    fn variant_less_than_display() {
        assert_eq!(format!("{}", Variant::LessThan), "<");
    }

    #[test]
    fn variant_less_than_or_equal_to_display() {
        assert_eq!(format!("{}", Variant::LessThanOrEqualTo), "<=");
    }

    #[test]
    fn variant_minus_display() {
        assert_eq!(format!("{}", Variant::Minus), "-");
    }

    #[test]
    fn variant_plus_display() {
        assert_eq!(format!("{}", Variant::Plus), "+");
    }

    #[test]
    fn variant_right_curly_display() {
        assert_eq!(format!("{}", Variant::RightCurly), "}");
    }

    #[test]
    fn variant_right_paren_display() {
        assert_eq!(format!("{}", Variant::RightParen), ")");
    }

    #[test]
    fn variant_slash_display() {
        assert_eq!(format!("{}", Variant::Slash), "/");
    }

    #[test]
    fn variant_terminator_line_break_display() {
        assert_eq!(
            format!("{}", Variant::Terminator(TerminatorType::LineBreak)),
            "\\n",
        );
    }

    #[test]
    fn variant_terminator_semicolon_display() {
        assert_eq!(
            format!("{}", Variant::Terminator(TerminatorType::Semicolon)),
            ";",
        );
    }

    #[test]
    fn variant_then_display() {
        assert_eq!(format!("{}", Variant::Then), THEN_KEYWORD);
    }

    #[test]
    fn variant_thick_arrow_display() {
        assert_eq!(format!("{}", Variant::ThickArrow), "=>");
    }

    #[test]
    fn variant_thin_arrow_display() {
        assert_eq!(format!("{}", Variant::ThinArrow), "->");
    }

    #[test]
    fn variant_true_display() {
        assert_eq!(format!("{}", Variant::True), TRUE_KEYWORD);
    }

    #[test]
    fn variant_type_display() {
        assert_eq!(format!("{}", Variant::Type), TYPE_KEYWORD);
    }
}
