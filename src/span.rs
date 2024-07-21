#[derive(Debug)]
pub struct Span(usize, usize);

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self(start, end)
    }

    pub fn ghost() -> Self {
        Self(0, 0)
    }

    pub fn mix(self, o: Self) -> Self {
        Self(std::cmp::min(self.0, o.0), std::cmp::max(self.1, o.1))
    }
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }
}
