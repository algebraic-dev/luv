//! Information about changes in static and dynamic environments.

use std::collections::HashMap;
use std::hash::Hash;

use im_rc::HashSet;

use crate::{
    r#abstract::Name,
    syntax::{Change, SyntaxNode},
};

/// Map for checking for changes.
pub struct ChangeMap<'a, K, V> {
    pub added: HashMap<K, Vec<&'a V>>,
    pub removed: HashMap<K, Vec<&'a V>>,
    pub changed: HashSet<K>,
}

impl<'a> ChangeMap<'a, Name, SyntaxNode> {
    pub fn of_changes(changes: Vec<Change<'a>>) -> Self {
        let mut change_map = ChangeMap::default();

        for change in changes {
            match change {
                Change::Added(node) => {
                    for node in node {
                        if let Some(info) = node.toplevel_info() {
                            change_map.add(info, node)
                        }
                    }
                }
                Change::Removed(node) => {
                    for node in node {
                        if let Some(info) = node.toplevel_info() {
                            change_map.remove(info, node)
                        }
                    }
                }
            }
        }

        change_map
    }
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

impl<'a, K: Hash + Eq + Clone, V: Clone> ChangeMap<'a, K, V> {
    pub fn add(&mut self, k: K, v: &'a V) {
        if self.removed.get(&k).is_some() {
            self.changed.insert(k.clone());
        }

        self.added.entry(k).or_default().push(v);
    }

    pub fn remove(&mut self, k: K, v: &'a V) {
        if self.added.get(&k).is_some() {
            self.changed.insert(k.clone());
        }

        self.removed.entry(k).or_default().push(v);
    }

    /// Iterates through unchanged removed items.
    pub fn iter_unchanged_removed(&self) -> impl Iterator<Item = (K, &V)> {
        self.removed
            .clone()
            .into_iter()
            .filter_map(move |(k, vec_v)| {
                // Check if key `k` is not in `self.changed`
                if !self.changed.contains(&k) {
                    // Iterate through the values in `vec_v` and yield each as part of a tuple with key `k`
                    Some(
                        vec_v
                            .into_iter()
                            .map(move |v| (k.clone(), v))
                            .collect::<Vec<_>>()
                            .into_iter(),
                    )
                } else {
                    // If key is changed, return None
                    None
                }
            })
            .flatten()
    }

    /// Iterates through unchanged added items.
    pub fn iter_unchanged_added(&self) -> impl Iterator<Item = (K, &V)> {
        self.added
            .clone()
            .into_iter()
            .filter_map(move |(k, vec_v)| {
                // Check if key `k` is not in `self.changed`
                if !self.changed.contains(&k) {
                    // Iterate through the values in `vec_v` and yield each as part of a tuple with key `k`
                    Some(
                        vec_v
                            .into_iter()
                            .map(move |v| (k.clone(), v))
                            .collect::<Vec<_>>()
                            .into_iter(),
                    )
                } else {
                    // If key is changed, return None
                    None
                }
            })
            .flatten()
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
