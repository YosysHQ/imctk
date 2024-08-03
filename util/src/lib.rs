//! Incremental Model Checking Toolkit - Shared Utility Code
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(missing_docs)]

#[macro_use]
pub mod give_take;

pub mod partition_refinement;
pub mod topo_sorted_sccs;
pub mod unordered_pair;
pub mod vec_sink;
