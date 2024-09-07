// TODO fully document and add #![warn(missing_docs)]
#![deny(unsafe_code)]

pub mod comb_sim;
pub mod seq_sim;

pub mod bit_matrix;

pub mod circuit_sat;

pub mod refinement;

pub mod unroll;

pub mod fragment;

mod time_step;

pub use time_step::TimeStep;
