//! This structure stores all the relations between all types of data.

use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::graph::NodeIndex;
use petgraph::prelude::StableDiGraph;
use petgraph::stable_graph::Neighbors;
use petgraph::Direction;
use std::hash::Hash;

/// Stores relationships between different types of data using a directed graph.
#[derive(Debug)]
pub struct Relations<K> {
    pub graph: StableDiGraph<K, i32>,
    pub nodes: HashMap<K, NodeIndex>,
}

impl<K> Default for Relations<K> {
    fn default() -> Self {
        Self {
            graph: Default::default(),
            nodes: Default::default(),
        }
    }
}

impl<K: Hash + Eq + Clone> Relations<K> {
    /// Connects two nodes and checks for cycles. Returns true if the connection was added.
    pub fn connect<T: Into<K>, F: Into<K>>(&mut self, f: F, t: T) {
        let from = f.into();
        let to = t.into();

        let from = self.add_node(from);
        let to = self.add_node(to);

        self.graph.add_edge(from, to, 0);
    }

    /// Adds a node to the graph if it doesn't exist and returns its index.
    pub fn add_node(&mut self, node: K) -> NodeIndex {
        *self
            .nodes
            .entry(node.clone())
            .or_insert_with(|| self.graph.add_node(node))
    }

    /// Removes an edge between two nodes.
    pub fn remove_edge(&mut self, f: K, t: K) {
        let from_node = *self.nodes.get(&f).expect("From node does not exist");
        let to_node = *self.nodes.get(&t).expect("To node does not exist");

        if let Some(edge) = self.graph.find_edge(from_node, to_node) {
            self.graph.remove_edge(edge);
        }
    }

    /// Removes a node.
    pub fn remove_node(&mut self, start: K) -> Option<K> {
        let node = *self.nodes.get(&start).expect("node does not exist");
        self.graph.remove_node(node)
    }

    /// Removes a node and if the nodes that depend on it only depend on this node,
    /// they will also be removed recursively. Returns all the removed [AnId]s.
    pub fn remove_node_and_dependent(&mut self, start: K) -> Option<HashSet<K>> {
        let mut removed = HashSet::new();

        let start_index = match self.nodes.get(&start) {
            Some(&index) => index,
            None => return None,
        };

        let mut to_remove = VecDeque::new();
        to_remove.push_back(start_index);

        while let Some(node_index) = to_remove.pop_front() {
            let node_id = self.graph[node_index].clone();
            removed.insert(node_id);

            let successors: Vec<_> = self
                .graph
                .neighbors_directed(node_index, Direction::Incoming)
                .collect();

            for &successor in &successors {
                if self.depends_on(successor).count() == 1 {
                    to_remove.push_back(successor);
                }
            }

            self.graph.remove_node(node_index);
        }
        for node_id in removed.iter() {
            self.nodes.remove(node_id);
        }

        Some(removed)
    }

    /// Returns everything that got affected by changes in a node.
    pub fn affected(&mut self, start: &[K]) -> HashSet<K> {
        let mut affected = HashSet::new();
        let mut queue = VecDeque::new();

        // Add start nodes to the queue
        for id in start {
            if let Some(&index) = self.nodes.get(id) {
                queue.push_back(index);
            }
        }

        while let Some(node_index) = queue.pop_front() {
            if !affected.contains(&self.graph[node_index]) {
                affected.insert(self.graph[node_index].clone());

                let dependents = self.dependents(node_index).collect::<Vec<_>>();

                for neighbor in dependents {
                    if !affected.contains(&self.graph[neighbor]) {
                        queue.push_back(neighbor);
                    }
                }
            }
        }

        affected
    }

    // `Map<petgraph::stable_graph::Neighbors<'_, ()>, {closure@src/relation.rs:128:18: 128:21}>
    // the weight is AnId
    pub fn get_dependents(&self, successor: K) -> impl Iterator<Item = (K, NodeIndex)> + '_ {
        let successor = self.nodes.get(&successor).unwrap();
        self.graph
            .neighbors_directed(*successor, Direction::Incoming)
            .filter_map(move |neighbor| {
                let weight = self.graph.node_weight(neighbor)?;
                Some((weight.clone(), neighbor))
            })
    }

    pub fn get_dependencies(&self, successor: K) -> impl Iterator<Item = (K, NodeIndex)> + '_ {
        let successor = self.nodes.get(&successor).unwrap();
        self.graph
            .neighbors_directed(*successor, Direction::Outgoing)
            .filter_map(move |neighbor| {
                let weight = self.graph.node_weight(neighbor)?;
                Some((weight.clone(), neighbor))
            })
    }

    pub fn connected(&mut self, successor: K) -> Neighbors<i32, u32> {
        let successor = self.nodes.get(&successor).unwrap();
        self.graph
            .neighbors_undirected(*successor)
    }


    fn dependents(&mut self, successor: NodeIndex) -> Neighbors<i32, u32> {
        self.graph
            .neighbors_directed(successor, Direction::Incoming)
    }

    fn depends_on(&mut self, successor: NodeIndex) -> Neighbors<i32, u32> {
        self.graph
            .neighbors_directed(successor, Direction::Outgoing)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::{AnId, Id};

    #[test]
    fn test_remove_node_and_dependent() {
        let mut relations = Relations::default();

        let node_a = AnId::Definition(Id::new(1));
        let node_b = AnId::Definition(Id::new(2));
        let node_c = AnId::Definition(Id::new(3));
        let node_d = AnId::Module(Id::new(4));

        relations.connect(node_a, node_b);
        relations.connect(node_b, node_c);
        relations.connect(node_c, node_d);

        let removed = relations.affected(&[node_b]);

        println!("{:?}", removed);
    }
}
