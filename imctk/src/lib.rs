//! Incremental Model Checking Toolkit
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(missing_docs)]

mod wide_ptr;

pub mod topo_sorted_sccs;

pub mod extract;
pub mod import;
pub mod ir;
