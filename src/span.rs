//! A module for handling spans and spanned data. Useful for tasks like tracking positions in source
//! code.

use core::fmt;

/// Represents a position in a text document with a line and column number.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Point {
    pub line: usize,
    pub column: usize,
}

impl Point {
    /// Creates a new [Point] with the given line and column.
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    /// Creates a [Point] at the origin (line 0, column 0).
    pub fn zeroed() -> Self {
        Self::new(0, 0)
    }

    /// Advances the [Point] based on the provided character.
    /// Increments the line if the character is a newline, otherwise increments the column.
    pub fn advance(&mut self, character: char) {
        if character == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    /// Subtracts another [Point] from this one, returning the difference as a new [Point].
    /// If the points are on the same line, only the column difference is considered.
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

    /// Converts the [Point] into a byte offset within the provided document string.
    pub fn to_offset(&self, doc: &str) -> usize {
        let mut offset = 0;
        for (line_num, line) in doc.lines().enumerate() {
            if line_num == self.line {
                offset += self.column;
                break;
            }
            offset += line.len() + 1; // +1 for newline character
        }
        offset
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
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

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Represents a span with a start and end position, useful for describing a range within a text document.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Point,
    pub end: Point,
}

impl Span {
    /// Creates a new [Span] with the given start and end positions.
    pub fn new(start: Point, end: Point) -> Self {
        Self { start, end }
    }

    /// Creates an empty [Span] with both start and end at the origin.
    pub fn empty() -> Self {
        Self::new(Point::zeroed(), Point::zeroed())
    }

    /// Checks if this [Span] intersects with another [Span].
    pub fn overlap(&self, other: &Span) -> bool {
        !(self.end < other.start || other.end < self.start)
    }

    /// Checks if this [Span] intersects with another [Span].
    pub fn intersects(&self, other: &Span) -> bool {
        !(self.end <= other.start || other.end <= self.start)
    }

    /// Checks if this [Span] completely contains another [Span].
    pub fn contains(&self, other: &Span) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// Checks if this [Span] contains a specific [Point].
    pub fn contains_point(&self, point: &Point) -> bool {
        self.start <= *point && *point <= self.end
    }

    /// Checks if this [Span] starts after the end of another [Span].
    pub fn starts_after(&self, other: &Span) -> bool {
        self.start >= other.end
    }

    /// Combines two spans into one, taking the minimum start and maximum end.
    pub fn merge(self, other: Self) -> Self {
        Self::new(
            std::cmp::min(self.start, other.start),
            std::cmp::max(self.end, other.end),
        )
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

/// A generic type that wraps data along with a [Span], useful for attaching source information to parsed elements.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Creates a new [Span] instance with the given data and span.
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }
}