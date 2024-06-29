//! Nodes and values that form the internal representation graph.

use std::fmt;

use imctk_ids::{Id, Id32};

mod vtables;

pub mod collections;

pub mod builder;
pub mod generic;

pub mod fine;

/// Identifies an individual node within a collection of nodes.
#[derive(Id)]
#[repr(transparent)]
pub struct NodeId(Id32);

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n{}", self.id_index())
    }
}
