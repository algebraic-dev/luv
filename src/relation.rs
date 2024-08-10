//! This structure stores all the relations between all types of data.

use std::collections::{HashMap, VecDeque};

use crate::id::AnId;

use im_rc::HashSet;
use petgraph::graph::NodeIndex;
use petgraph::prelude::StableDiGraph;
use petgraph::stable_graph::Neighbors;
use petgraph::Direction;

/// Stores relationships between different types of data using a directed graph.
#[derive(Default)]
pub struct Relations {
    graph: StableDiGraph<AnId, ()>,
    nodes: HashMap<AnId, NodeIndex>,
}

impl Relations {
    /// Connects two nodes and checks for cycles. Returns true if the connection was added.
    pub fn connect<T: Into<AnId>, F: Into<AnId>>(&mut self, f: F, typ: (), t: T) {
        let from: AnId = f.into();
        let to: AnId = t.into();

        let from = self.add_node(from);
        let to = self.add_node(to);

        self.graph.add_edge(from, to, typ);
    }

    /// Adds a node to the graph if it doesn't exist and returns its index.
    fn add_node(&mut self, node: AnId) -> NodeIndex {
        *self
            .nodes
            .entry(node)
            .or_insert_with(|| self.graph.add_node(node))
    }

    /// Removes an edge between two nodes.
    pub fn remove_edge(&mut self, f: AnId, t: AnId) {
        let from_node = *self.nodes.get(&f).expect("From node does not exist");
        let to_node = *self.nodes.get(&t).expect("To node does not exist");

        if let Some(edge) = self.graph.find_edge(from_node, to_node) {
            self.graph.remove_edge(edge);
        }
    }

    /// Removes a node and if the nodes that depend on it only depend on this node,
    /// they will also be removed recursively. Returns all the removed [AnId]s.
    pub fn remove_node_and_dependent(&mut self, start: AnId) -> Option<HashSet<AnId>> {
        let mut removed = HashSet::new();

        let start_index = match self.nodes.get(&start) {
            Some(&index) => index,
            None => return None,
        };

        let mut to_remove = VecDeque::new();
        to_remove.push_back(start_index);

        while let Some(node_index) = to_remove.pop_front() {
            let node_id = self.graph[node_index];
            removed.insert(node_id);

            let successors: Vec<_> = self
                .graph
                .neighbors_directed(node_index, Direction::Incoming)
                .collect();

            for &successor in &successors {
                println!("depends_on {}", self.depends_on(successor).count());
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
    pub fn affected(&mut self, start: &[AnId]) -> HashSet<AnId> {
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
                affected.insert(self.graph[node_index]);

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

    fn dependents(&mut self, successor: NodeIndex) -> Neighbors<(), u32> {
        self.graph
            .neighbors_directed(successor, Direction::Incoming)
    }

    fn depends_on(&mut self, successor: NodeIndex) -> Neighbors<(), u32> {
        self.graph
            .neighbors_directed(successor, Direction::Outgoing)
    }
}

#[cfg(test)]
mod tests {
    use crate::id::Id;
    use super::*;

    #[test]
    fn test_remove_node_and_dependent() {
        let mut relations = Relations::default();

        let node_a = AnId::Definition(Id::new(1));
        let node_b = AnId::Definition(Id::new(2));
        let node_c = AnId::Definition(Id::new(3));
        let node_d = AnId::Module(Id::new(4));

        relations.connect(node_a, (), node_b);
        relations.connect(node_b, (), node_c);
        relations.connect(node_c, (), node_d);

        let removed = relations.affected(&[node_b]);

        println!("{:?}", removed);
    }
}
