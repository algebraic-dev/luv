//! This module defines the [Hierarchy] structure which manages range-related data.

use crate::{
    prettytree::{PrettyPrint, Tree},
    span::Span,
};

#[derive(Debug, PartialEq, Eq)]
pub enum HierarchyError {
    RangeAlreadyExists,
    IntersectingRange,
    AdjacentRange,
    RangeNotFound,
}

/// Represents a site with an associated span and data.
pub struct Site<V> {
    span: Span,
    data: V,
}

impl<V> Site<V> {
    /// Creates a new `Site` with the given span and data.
    pub fn new(span: Span, data: V) -> Self {
        Site { span, data }
    }
}

/// Manages a collection of sites.
pub struct Hierarchy<V> {
    forest: Vec<Hierarchy<V>>,
    site: Site<V>,
}

impl<V: PrettyPrint> PrettyPrint for Hierarchy<V> {
    fn to_tree(&self) -> crate::prettytree::Tree {
        let mut children = Tree::label("child");

        for child in &self.forest {
            children = children.with(child.to_tree())
        }

        Tree::label(format!("scope {}", self.site.span))
            .with(self.site.data.to_tree())
            .with(children)
    }
}

impl<V> Hierarchy<V> {
    /// Creates a new `RangeMap` with the given span and data.
    pub fn new(span: Span, data: V) -> Self {
        Hierarchy {
            forest: Vec::new(),
            site: Site::new(span, data),
        }
    }

    /// Adds a new range to the `RangeMap` if it is contained and does not intersect.
    pub fn add_range(&mut self, span: &Span, data: V) -> Result<(), HierarchyError> {
        fn add<V>(
            map: &mut Hierarchy<V>,
            span: &Span,
            mut data: V,
        ) -> Result<Option<V>, HierarchyError> {
            if &map.site.span == span {
                Err(HierarchyError::RangeAlreadyExists)
            } else if map.site.span.contains(span) {
                for map in &mut map.forest {
                    let result = add(map, span, data)?;
                    if let Some(result) = result {
                        data = result;
                    } else {
                        return Ok(None);
                    }
                }

                map.forest.push(Hierarchy::new(span.clone(), data));
                map.site.span = map.site.span.clone().merge(span.clone());
                Ok(None)
            } else if map.site.span.intersects(span) {
                Err(HierarchyError::IntersectingRange)
            } else {
                Ok(Some(data))
            }
        }

        match add(self, span, data)? {
            Some(_) => Err(HierarchyError::AdjacentRange),
            None => Ok(()),
        }
    }

    /// Finds and returns a mutable reference to the data within a range for the given span.
    pub fn entry<'a>(&'a mut self, span: &Span) -> Result<&'a mut V, HierarchyError> {
        if &self.site.span == span {
            Ok(&mut self.site.data)
        } else if self.site.span.contains(span) {
            for map in &mut self.forest {
                let res = map.entry(span);
                if res.is_ok() {
                    return res;
                }
            }
            Ok(&mut self.site.data)
        } else if self.site.span.intersects(span) {
            Err(HierarchyError::IntersectingRange)
        } else {
            Err(HierarchyError::RangeNotFound)
        }
    }

    /// Accumulates and returns a vector of all [Site<V>] instances that intersect with the given span.
    pub fn accumulate<'a>(&'a self, span: &Span) -> Vec<&'a Site<V>> {
        fn acc<'a, V>(map: &'a Hierarchy<V>, span: &Span, data: &mut Vec<&'a Site<V>>) {
            if map.site.span.contains(span) {
                data.push(&map.site);

                for range in &map.forest {
                    acc(range, span, data)
                }
            }
        }

        let mut vec = Vec::new();
        acc(self, span, &mut vec);
        vec
    }
}

/// A builder for constructing a `RangeMap` with nested ranges.
#[derive(Default)]
pub struct HierarchyBuilder<T> {
    data: Vec<Hierarchy<T>>,
}

impl<T: Default> HierarchyBuilder<T> {
    /// Creates a new `RangeMapBuilder` with an initial `RangeMap` covering the given `span`.
    pub fn new(span: Span) -> Self {
        Self {
            data: vec![Hierarchy::new(span, Default::default())],
        }
    }

    /// Opens a new range within the current range, given the `span`.
    /// Panics if the new range intersects or is not contained within the current range.
    pub fn open(&mut self, span: Span) {
        if !self.data.is_empty() {
            let current_range = self.data.last_mut().expect("No current range found");

            if !current_range.site.span.contains(&span) {
                if current_range.site.span.intersects(&span) {
                    panic!(
                        "The new range {} intersects with the current range {}.",
                        span, current_range.site.span
                    );
                }
                panic!(
                    "The new range {} is not contained within the current range {}.",
                    span, current_range.site.span
                )
            }
        }

        self.data.push(Hierarchy::new(span, Default::default()));
    }

    /// Returns a mutable reference to the data of the innermost range.
    pub fn get(&mut self) -> &mut T {
        &mut self.data.last_mut().unwrap().site.data
    }

    /// Closes the most recently opened range and merges it into the parent range.
    /// Panics if there are no ranges to close.
    pub fn close(&mut self) {
        if self.data.len() > 1 {
            let last_range = self.data.pop().expect("No range to close");
            let parent_range = self.data.last_mut().expect("No parent range found");
            parent_range.forest.push(last_range);
        }
    }

    /// Completes the range building process and returns the final `RangeMap`.
    /// Panics if there are unclosed ranges remaining.
    pub fn finish(&mut self) -> Hierarchy<T> {
        assert_eq!(self.data.len(), 1, "Not all ranges were closed.");
        self.data.pop().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::span::Point;

    use super::*;
    fn point(x: usize, y: usize) -> Point {
        Point::new(x, y)
    }
    fn span(start_x: usize, start_y: usize, end_x: usize, end_y: usize) -> Span {
        Span::new(point(start_x, start_y), point(end_x, end_y))
    }

    #[test]
    fn test_add_range() {
        let mut map = Hierarchy::new(span(0, 0, 10, 10), "Data1");
        let span2 = span(2, 0, 5, 1);
        let span3 = span(5, 0, 7, 10);

        assert!(map.add_range(&span2, "Data2").is_ok());
        assert_eq!(
            map.add_range(&span3, "Data3"),
            Err(HierarchyError::IntersectingRange)
        );
    }

    #[test]
    fn test_entry() {
        let mut map = Hierarchy::new(span(0, 0, 10, 10), "Data1");
        map.add_range(&span(2, 3, 5, 10), "Data2").unwrap();

        let result = map.entry(&span(0, 0, 10, 10));
        assert!(result.is_ok());
        assert_eq!(*result.unwrap(), "Data1");
    }

    #[test]
    fn test_entry_not_found() {
        let mut map = Hierarchy::new(span(0, 0, 10, 10), "Data1");
        let entry = map.entry(&span(20, 0, 30, 0));
        assert_eq!(entry, Err(HierarchyError::RangeNotFound));
    }

    #[test]
    fn test_entry_intersecting_range() {
        let mut map = Hierarchy::new(span(0, 0, 10, 10), "Data1");
        map.add_range(&span(5, 0, 7, 10), "Data2").unwrap();

        assert_eq!(
            map.entry(&span(5, 0, 15, 10)),
            Err(HierarchyError::IntersectingRange)
        );
    }

    #[test]
    fn test_add_range_with_existing_range() {
        let mut map = Hierarchy::new(span(0, 0, 10, 10), "Data1");
        assert_eq!(
            map.add_range(&span(0, 0, 10, 10), "NewData"),
            Err(HierarchyError::RangeAlreadyExists)
        );
    }

    #[test]
    fn test_open_range() {
        let mut builder: HierarchyBuilder<&str> = HierarchyBuilder::new(span(0, 0, 10, 10));

        builder.open(span(1, 1, 5, 5));
        let data = builder.get();
        *data = "NestedSiteata";

        builder.close();
        let map = builder.finish();
        let data = map.forest.first().unwrap().site.data;
        assert_eq!(data, "NestedSiteata");
    }

    #[test]
    fn test_get_data() {
        let mut builder = HierarchyBuilder::new(span(0, 0, 10, 10));

        *builder.get() = "Siteata";

        let map = builder.finish();
        assert_eq!(map.site.data, "Siteata");
    }
}
