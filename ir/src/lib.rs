//! Incremental Model Checking Toolkit - Internal representation
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(missing_docs)]
pub mod node;
pub mod var;

pub mod index;

pub mod env;

pub mod prelude {
    //! Re-exports of all commonly used internal representation traits and types.
    pub use super::{
        env::Env,
        node::{
            builder::{NodeBuilder, NodeBuilderDyn},
            generic::{
                DynNode, DynTerm, Node, NodeDyn, NodeDynAuto, NodeType, Term, TermDyn, TermDynAuto,
                TermType,
            },
            NodeId,
        },
        var::{Lit, Var},
    };
}

mod wide_ptr;
