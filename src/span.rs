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
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, index: usize) -> Self {
        Self {
            line,
            column,
            index,
        }
    }

    pub fn zeroed() -> Self {
        Self::new(1, 0, 0)
    }

    pub fn advance(&mut self, char: char) {
        if char == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
        self.index += char.len_utf8();
    }
}

/// Represents a span with a start and end position.
#[derive(Clone, Debug)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    /// Creates a new `Span` with the given start and end positions.
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    /// Creates a ghost `Span` with both start and end set to 0.
    pub fn empty() -> Self {
        Self::new(Position::zeroed(), Position::zeroed())
    }

    /// Combines two spans into one, taking the minimum start and maximum end.
    pub fn mix(self, other: Self) -> Self {
        Self::new(self.start, other.end)
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
