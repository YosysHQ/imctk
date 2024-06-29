//! Incremental Model Checking Toolkit
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(missing_docs)]

mod wide_ptr;

#[macro_use]
pub mod give_take;

pub mod ir;

pub mod import;

pub mod unordered_pair;
