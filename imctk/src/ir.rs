//! Internal representation
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
