//! A module for handling spans and spanned data. This is useful for tasks like tracking positions in source code.
//!
//! The module provides the following structures:
//!
//! - [Span] : Represents a span with a start and end position.
//! - [Spanned] : Wraps data with a span.
//! - [Position] : Represents a specific position in a source.
//!

use core::fmt;

/// Represents a span with a start and end position.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Point {
    pub line: usize,
    pub column: usize,
}

impl Point {
    pub fn subtract(&self, other: &Point) -> Point {
        if self.line == other.line {
            Point {
                line: 0,
                column: self.column.saturating_sub(other.column),
            }
        } else {
            Point {
                line: self.line.saturating_sub(other.line),
                column: self.column,
            }
        }
    }

    pub fn offset(&self, doc: &str) -> usize {
        let mut offset = 0;
        for (line_num, line) in doc.lines().enumerate() {
            if line_num == self.line as usize {
                offset += self.column as usize;
                break;
            }
            offset += line.len() + 1;
        }
        offset
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.line.cmp(&other.line) {
            std::cmp::Ordering::Equal => self.column.cmp(&other.column).into(),
            other => other.into(),
        }
    }
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.line.cmp(&other.line) {
            std::cmp::Ordering::Equal => self.column.cmp(&other.column),
            other => other,
        }
    }
}

impl Point {
    pub fn new(line: usize, column: usize) -> Self {
        Self {
            line,
            column,
        }
    }

    pub fn zeroed() -> Self {
        Self::new(0, 0)
    }

    pub fn advance(&mut self, char: char) {
        if char == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }
}

/// Represents a span with a start and end position.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Point,
    pub end: Point,
}

impl Span {
    pub fn intersect(&self, span2: &Span) -> bool {
        !(self.end < span2.start || span2.end < self.start)
    }

    pub fn after(&self, span2: &Span) -> bool {
        self.start >= span2.end
    }

    /// Creates a new `Span` with the given start and end positions.
    pub fn new(start: Point, end: Point) -> Self {
        Self { start, end }
    }

    /// Creates a ghost `Span` with both start and end set to 0.
    pub fn empty() -> Self {
        Self::new(Point::zeroed(), Point::zeroed())
    }

    /// Combines two spans into one, taking the minimum start and maximum end.
    pub fn mix(self, other: Self) -> Self {
        Self::new(self.start, other.end)
    }
}

/// Wraps data with a span.
#[derive(Debug, Clone)]
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

pub type EditedSpan = Span;

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.line, self.column)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}
