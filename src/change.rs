//! Information about changes in static and dynamic environments.

use std::collections::HashMap;
use std::hash::Hash;

/// Map for checking for changes.
pub struct ChangeMap<'a, K, V> {
    added: HashMap<K, &'a V>,
    removed: HashMap<K, &'a V>,
    changed: HashMap<K, (&'a V, &'a V)>,
}

impl<'a, K, V> Default for ChangeMap<'a, K, V> {
    fn default() -> Self {
        Self {
            added: Default::default(),
            removed: Default::default(),
            changed: Default::default(),
        }
    }
}

impl<'a, K: Hash + Eq + Clone, V> ChangeMap<'a, K, V> {
    /// Adds an item to the added map or updates the changed map if already removed.
    pub fn add(&mut self, key: K, v: &'a V) {
        if let Some(old) = self.removed.get(&key) {
            self.changed.insert(key.clone(), (old, v));
        }
        self.added.insert(key, v);
    }

    /// Removes an item from the added map or updates the changed map if already added.
    pub fn remove(&mut self, key: K, v: &'a V) {
        if let Some(new) = self.added.get(&key) {
            self.changed.insert(key.clone(), (v, new));
        }
        self.removed.insert(key, v);
    }

    /// Iterates through unchanged removed items.
    pub fn iter_unchanged_removed(&self) -> impl Iterator<Item = (&K, &&V)> {
        self.removed
            .iter()
            .filter(move |(k, _)| !self.changed.contains_key(k))
    }

    /// Iterates through unchanged added items.
    pub fn iter_unchanged_added(&self) -> impl Iterator<Item = (&K, &&V)> {
        self.added
            .iter()
            .filter(move |(k, _)| !self.changed.contains_key(k))
    }

    /// Iterates through changed items.
    pub fn iter_changed(&self) -> impl Iterator<Item = (&K, &(&V, &V))> {
        self.changed.iter()
    }
}

/// Map for checking for duplicated stuff
#[derive(Default)]
pub struct DuplicateMap<'a, K, V> {
    map: HashMap<K, Vec<&'a V>>,
}

impl<'a, K: Hash + Eq + Clone, V> DuplicateMap<'a, K, V> {
    /// Adds an item to the map. Tracks duplicates by adding to the list of values.
    pub fn add(&mut self, key: K, v: &'a V) {
        self.map.entry(key).or_default().push(v);
    }

    /// Checks if a key has duplicates.
    pub fn has_duplicates(&self, key: &K) -> bool {
        self.map.get(key).map_or(false, |values| values.len() > 1)
    }

    /// Returns an iterator over all keys with duplicates.
    pub fn iter_keys_with_duplicates(&self) -> impl Iterator<Item = &K> {
        self.map
            .iter()
            .filter_map(|(key, values)| if values.len() > 1 { Some(key) } else { None })
    }

    /// Returns an iterator over all values for a specific key.
    pub fn iter_values(&self, key: &K) -> impl Iterator<Item = &&V> {
        self.map.get(key).into_iter().flat_map(|v| v.iter())
    }
}
