//! Module for defining errors.

use crate::span::Span;
use std::borrow::Cow;
use std::error::Error as StdError;
use std::fmt;

/// Represents an error with an associated span and message.
#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub span: Span,
    pub message: Cow<'static, str>,
}

impl Error {
    /// Creates a new `Error` with the given message and span.
    pub fn new<S: Into<Cow<'static, str>>>(message: S, span: Span) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

impl fmt::Display for Error {
    /// Formats the error for display purposes.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {:?}", self.message, self.span)
    }
}

impl StdError for Error {
    /// Provides the underlying error message.
    fn description(&self) -> &str {
        &self.message
    }
}
