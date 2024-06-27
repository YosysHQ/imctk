//! Nodes of the internal representation graph.
//!
//! This module defines the [`Node`] trait for internal representation nodes and the [`Nodes`] type
//! holding a heterogeneous collection of such nodes.

mod node_trait;
mod nodes;

pub use node_trait::{DynNode, Node, NodeDyn, NodeDynAuto, NodeType};
pub use nodes::{NodeError, NodeId, Nodes};

pub mod value;

pub mod fine;
