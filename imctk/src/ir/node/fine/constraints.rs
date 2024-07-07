//! Fine grained constraints for representing invariants of combinational and sequential
//! bit-level circuits.
use crate::{
    ir::{
        node::{
            builder::NodeBuilder,
            generic::{Node, NodeDyn},
        },
        var::{Lit, Var},
    },
    unordered_pair::UnorderedPair,
};

/// A binary clause, requiring at least one of two inputs to be true at any time.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BinClause {
    /// The inputs for the binary clause constraint.
    pub inputs: UnorderedPair<Lit>,
}

impl Node for BinClause {
    const NAME: &'static str = "BinClause";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.inputs.map(|lit| lit.var()).into_iter()
    }

    fn apply_var_map(&mut self, var_map: impl FnMut(Var) -> Lit) {
        self.inputs.apply_var_map(var_map);
    }

    fn reduce(&mut self, builder: &mut impl NodeBuilder) -> bool {
        let [a, b] = *self.inputs;

        if a.var() == b.var() {
            if a == b {
                builder.equiv([a, Lit::TRUE]);
            }
            true
        } else if a.is_const() {
            if a == Lit::FALSE {
                builder.equiv([b, Lit::TRUE]);
            }
            true
        } else {
            false
        }
    }
}

impl NodeDyn for BinClause {
    fn representative_input_var(&self) -> Var {
        self.inputs[1].var()
    }

    fn max_var(&self) -> Var {
        self.inputs.max_element().var()
    }
}
