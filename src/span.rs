//! A module for handling spans and spanned data. This is useful for tasks like tracking positions in source code.
//!
//! The module provides the following structures:
//!
//! - [Span] : Represents a span with a start and end position.
//! - [Spanned] : Wraps data with a span.
//! - [Position] : Represents a specific position in a source.
//!

/// Represents a span with a start and end position.
#[derive(Clone, Debug)]
pub struct Span(pub usize, pub usize);

impl Span {
    /// Creates a new `Span` with the given start and end positions.
    pub fn new(start: usize, end: usize) -> Self {
        Self(start, end)
    }

    /// Creates a ghost `Span` with both start and end set to 0.
    pub fn ghost() -> Self {
        Self(0, 0)
    }

    /// Combines two spans into one, taking the minimum start and maximum end.
    pub fn mix(self, other: Self) -> Self {
        Self(
            std::cmp::min(self.0, other.0),
            std::cmp::max(self.1, other.1),
        )
    }
}

/// Wraps data with a span.
#[derive(Debug)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Creates a new `Spanned` instance with the given data and span.
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }
}

/// Represents a specific position in a source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    /// Creates a new `Position` with the given line and column.
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}
