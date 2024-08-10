//! The storage is a module for storing stuff, creating ids for entities and retriving them.

//! The storage module provides functionality for storing entities, creating unique IDs for them, and retrieving them.

use std::marker::PhantomData;
use crate::id::{Id, TypeKind};

/// A storage system for managing entities of type `T` identified by `Id<I>`.
///
/// This generic storage allows entities to be stored, retrieved, and managed using unique IDs.
/// The `TypeKind` trait ensures that only certain types can be used as identifiers.
pub struct Storage<I: TypeKind, T> {
    data: Vec<Option<T>>,
    reuse: Vec<u64>,
    marker: PhantomData<I>,
    counter: u64,
}

impl<I: TypeKind, T> Storage<I, T> {
    /// Creates a new, empty storage.
    pub fn new() -> Self {
        Storage {
            data: Vec::new(),
            counter: 0,
            reuse: Vec::new(),
            marker: PhantomData,
        }
    }

    /// Adds an entity to the storage and returns a unique `Id` for it.
    pub fn add(&mut self, entity: T) -> Id<I> {
        let id_value = if let Some(reused_id) = self.reuse.pop() {
            self.data[reused_id as usize] = Some(entity);
            reused_id
        } else {
            self.data.push(Some(entity));
            self.counter = self.counter.wrapping_add(1);
            self.counter - 1
        };
        Id::new(id_value)
    }

    /// Retrieves a reference to an entity by its `Id`.
    pub fn get(&self, id: &Id<I>) -> Option<&T> {
        self.data.get(id.value() as usize)?.as_ref()
    }

    /// Retrieves a mutable reference to an entity by its `Id`.
    pub fn get_mut(&mut self, id: &Id<I>) -> Option<&mut T> {
        self.data.get_mut(id.value() as usize)?.as_mut()
    }

    /// Removes an entity by its `Id`.
    pub fn remove(&mut self, id: Id<I>) -> Option<T> {
        let index = id.value() as usize;
        if index < self.data.len() {
            let removed = self.data[index].take();
            self.reuse.push(index as u64);
            removed
        } else {
            None
        }
    }

    /// Returns the number of entities stored in the `Storage`.
    pub fn len(&self) -> usize {
        self.data.iter().filter(|e| e.is_some()).count()
    }

    /// Returns `true` if the storage is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Clears all entities from the storage.
    pub fn clear(&mut self) {
        self.data.clear();
        self.counter = 0;
        self.reuse.clear();
    }
}
