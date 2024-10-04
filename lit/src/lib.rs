//! Numeric identifiers for variables and Boolean literals
mod lit;
mod pol;
mod var;

pub use lit::Lit;
pub use pol::{Negate, NegateInPlace, Pol};
pub use var::Var;
