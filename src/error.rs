use crate::format::CodeStr;
use colored::Colorize;
use pad::{Alignment, PadStr};
use std::{
    cmp::{max, min},
    error, fmt,
    path::Path,
    rc::Rc,
};

// This is the primary error type we'll be using everywhere.
#[derive(Clone, Debug)]
pub struct Error {
    pub message: String,
    pub reason: Option<Rc<dyn error::Error>>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(reason) = &self.reason {
            write!(f, "{} Reason: {}", self.message, reason)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl error::Error for Error {
    fn source<'a>(&'a self) -> Option<&(dyn error::Error + 'static)> {
        self.reason.as_ref().map(|e| &**e)
    }
}

// This function constructs an `Error` that occurs at a specific location in a source file.
pub fn throw(
    message: &str,
    source_path: Option<&Path>,
    source_contents: &str,

    // Inclusive on the left and exclusive on the right
    source_range: (usize, usize),
) -> Error {
    {
        // Remember the relevant lines and the position of the start of the next line.
        let mut lines = vec![];
        let mut pos = 0_usize;

        // Find the relevant lines.
        for (i, line) in source_contents.split('\n').enumerate() {
            // Record the start of the line before we advance the cursor.
            let line_start = pos;

            // Move the cursor to the start of the next line.
            pos += line.len() + 1;

            // If the start of the line is past the end of the range, we're done.
            if line_start >= source_range.1 {
                break;
            }

            // If the start of the next line is before the start of the range, we haven't reached
            // the lines of interest yet.
            if pos <= source_range.0 {
                continue;
            }

            // Highlight the relevant part of the line.
            let trimmed_line = line.trim_end();
            let (section_start, section_end) = if source_range.0 > line_start {
                (
                    min(source_range.0 - line_start, trimmed_line.len()),
                    min(source_range.1 - line_start, trimmed_line.len()),
                )
            } else {
                (0, min(source_range.1 - line_start, trimmed_line.len()))
            };
            let colored_line = format!(
                "{}{}{}",
                &trimmed_line[..section_start],
                &trimmed_line[section_start..section_end].red(),
                &trimmed_line[section_end..],
            );

            // Record the line number and the line contents. We trim the end of the line to
            // remove any carriage return that might have been present before the line feed (as
            // well as any other whitespace).
            lines.push(((i + 1).to_string(), colored_line));
        }

        // Compute the width of the string representation of the hugest relevant line number.
        let gutter_width = lines
            .iter()
            .fold(0_usize, |acc, (line_number, _)| max(acc, line_number.len()));

        // Render the code listing with line numbers.
        let listing = lines
            .iter()
            .map(|(line_number, line)| {
                format!(
                    "{} {}",
                    format!(
                        "{} |",
                        line_number.pad(gutter_width, ' ', Alignment::Right, false),
                    )
                    .blue()
                    .bold(),
                    line,
                )
            })
            .collect::<Vec<_>>()
            .join("\n");

        // Now we have everything we need to construct the error.
        Error {
            message: if let Some(path) = source_path {
                if listing.is_empty() {
                    format!(
                        "Error in {}: {}",
                        path.to_string_lossy().code_str(),
                        message,
                    )
                } else {
                    format!(
                        "Error in {}: {}\n\n{}",
                        path.to_string_lossy().code_str(),
                        message,
                        listing,
                    )
                }
            } else if listing.is_empty() {
                format!("Error: {}", message)
            } else {
                format!("Error: {}\n\n{}", message, listing)
            },
            reason: None,
        }
    }
}

// This function constructs an `Error` from a message and a reason. It's written in a curried style
// so it can be used in a higher-order fashion, e.g.,
// `foo.map_err(lift("Error doing foo."))`.
pub fn lift<T: Into<String>, U: error::Error + 'static>(message: T) -> impl FnOnce(U) -> Error {
    let message = message.into();
    move |error: U| Error {
        message,
        reason: Some(Rc::new(error)),
    }
}

// This macro is useful for writing tests that deal with errors.
#[macro_export]
macro_rules! assert_fails {
    ($expr:expr, $substr:expr $(,)?) => {{
        // Before we actually evaluate the expression, disable terminal colors.
        colored::control::set_override(false);

        // Check that `$expr` fails and that the failure contains `$substr`.
        if let Err(error) = $expr {
            assert!(error.to_string().contains($substr));
        } else {
            assert!(false);
        }
    }};
}

#[cfg(test)]
mod tests {
    use crate::error::{lift, throw, Error};
    use std::path::Path;

    #[test]
    fn throw_no_path_empty_range() {
        colored::control::set_override(false);

        let error = throw("An error occurred.", None, "", (0, 0));

        assert_eq!(error.message, "Error: An error occurred.".to_owned());
    }

    #[test]
    fn throw_with_path_empty_range() {
        colored::control::set_override(false);

        let error = throw("An error occurred.", Some(Path::new("foo.g")), "", (0, 0));

        assert_eq!(
            error.message,
            "Error in `foo.g`: An error occurred.".to_owned(),
        );
    }

    #[test]
    fn throw_no_path_single_line_full_range() {
        colored::control::set_override(false);

        let error = throw("An error occurred.", None, "foo", (0, 3));

        assert_eq!(
            error.message,
            "Error: An error occurred.\n\n1 | foo".to_owned(),
        );
    }

    #[test]
    fn throw_with_path_single_line_full_range() {
        colored::control::set_override(false);

        let error = throw(
            "An error occurred.",
            Some(Path::new("foo.g")),
            "foo",
            (0, 3),
        );

        assert_eq!(
            error.message,
            "Error in `foo.g`: An error occurred.\n\n1 | foo".to_owned(),
        );
    }

    #[test]
    fn throw_no_path_multiple_lines_full_range() {
        colored::control::set_override(false);

        let error = throw("An error occurred.", None, "foo\nbar\nbaz", (0, 11));

        assert_eq!(
            error.message,
            "Error: An error occurred.\n\n1 | foo\n2 | bar\n3 | baz".to_owned(),
        );
    }

    #[test]
    fn throw_no_path_multiple_lines_partial_range() {
        colored::control::set_override(false);

        let error = throw("An error occurred.", None, "foo\nbar\nbaz\nqux", (5, 11));

        assert_eq!(
            error.message,
            "Error: An error occurred.\n\n2 | bar\n3 | baz".to_owned(),
        );
    }

    #[test]
    fn throw_no_path_many_lines_partial_range() {
        colored::control::set_override(false);

        let error = throw(
            "An error occurred.",
            None,
            "foo\nbar\nbaz\nqux\nfoo\nbar\nbaz\nqux\nfoo\nbar\nbaz\nqux",
            (33, 42),
        );

        assert_eq!(
            error.message,
            "Error: An error occurred.\n\n 9 | foo\n10 | bar\n11 | baz".to_owned(),
        );
    }

    #[test]
    fn lift_simple() {
        colored::control::set_override(false);

        let error = lift("An error occurred.")(Error {
            message: "This is why.".to_owned(),
            reason: None,
        });

        assert_eq!(error.message, "An error occurred.".to_owned());

        assert_eq!(error.reason.unwrap().to_string(), "This is why.".to_owned());
    }
}
