//! Fine grained terms for representing bit-level combinational and sequential circuits.
use imctk_ids::{Id, Id32};

use crate::{
    ir::{
        node::{
            builder::NodeBuilder,
            generic::{default_reduce_node, Term, TermDyn, TermNode},
        },
        var::{Lit, Pol, Var},
    },
    unordered_pair::UnorderedPair,
};

#[allow(unused_imports)] // rustdoc
use crate::ir::node::generic::Node;

use super::constraints::BinClause;

/// [`Term`] representing the Boolean 'and' of two values.
///
/// This is a combinational operation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct And {
    /// The operands for the 'and' given as input literals.
    pub inputs: UnorderedPair<Lit>,
}

/// [`Node`] representing the Boolean 'and' of two values.
pub type AndNode = TermNode<And>;

impl Term for And {
    type Output = Lit;

    const NAME: &'static str = "And";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.inputs.map(|lit| lit.var()).into_iter()
    }

    fn apply_var_map(&mut self, var_map: impl FnMut(Var) -> Lit) -> Pol {
        self.inputs.apply_var_map(var_map);
        Pol::Pos
    }

    fn reduce(&mut self, _builder: &mut impl NodeBuilder) -> Option<Self::Output> {
        let [a, b] = *self.inputs;

        if a.var() == b.var() {
            if a == b {
                Some(a)
            } else {
                Some(Lit::FALSE)
            }
        } else if a.is_const() {
            if a == Lit::FALSE {
                Some(Lit::FALSE)
            } else {
                Some(b)
            }
        } else {
            None
        }
    }

    fn reduce_node(&mut self, output: Lit, builder: &mut impl NodeBuilder) -> bool {
        if output.is_const() {
            if output == Lit::FALSE {
                builder.node(BinClause {
                    inputs: self.inputs.map(|input| !input).into(),
                });
                true
            } else {
                for input in *self.inputs {
                    builder.equiv([input, Lit::TRUE]);
                }
                true
            }
        } else {
            for i in 0..2 {
                let input = self.inputs[i];
                let other_input = self.inputs[i ^ 1];
                if input.var() == output.var() {
                    if input == output {
                        builder.node(BinClause {
                            inputs: [!output, other_input].into(),
                        });
                        return true;
                    } else {
                        builder.equiv([output, Lit::FALSE]);
                        builder.equiv([other_input, Lit::FALSE]);
                        return true;
                    }
                }
            }

            default_reduce_node(self, output, builder)
        }
    }
}

impl TermDyn for And {
    fn representative_input_var(&self) -> Var {
        self.inputs[1].var()
    }

    fn max_var(&self) -> Var {
        self.inputs.max_element().var()
    }
}

/// [`Term`] representing Boolean 'xor' ('exclusive or').
///
/// This is a combinational operation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Xor {
    /// The operands for the 'xor' given as input literals.
    pub inputs: UnorderedPair<Var>,
}

/// [`Node`] representing the Boolean 'xor' ('exclusive or').
pub type XorNode = TermNode<Xor>;

impl Term for Xor {
    type Output = Lit;
    const NAME: &'static str = "Xor";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.inputs.into_iter()
    }

    fn apply_var_map(
        &mut self,
        var_map: impl FnMut(Var) -> Lit,
    ) -> <Self::Output as crate::ir::var::VarOrLit>::Pol {
        self.inputs.apply_var_map_compose_pol(var_map)
    }

    fn reduce(&mut self, _builder: &mut impl NodeBuilder) -> Option<Self::Output> {
        let [a, b] = *self.inputs;

        if a == b {
            Some(Lit::FALSE)
        } else if a == Var::FALSE {
            Some(b.as_pos())
        } else {
            None
        }
    }

    // TODO node reductions
}

impl TermDyn for Xor {
    fn representative_input_var(&self) -> Var {
        self.inputs[1]
    }
}

/// [`Term`] representing a steady input or unconstrained steady value.
///
/// An important use case is unconstrained register initialization, where this provides the initial
/// value.
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct SteadyInput(Id32);

/// [`Node`] representing a steady input or a steady unconstrained constant value.
pub type SteadyInputNode = TermNode<SteadyInput>;

impl Term for SteadyInput {
    type Output = Lit;
    const NAME: &'static str = "Init";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [].into_iter()
    }

    fn apply_var_map(&mut self, _var_map: impl FnMut(Var) -> Lit) -> Pol {
        Pol::Pos
    }
}

impl TermDyn for SteadyInput {}

/// [`Term`] representing a time-varying input or unconstrained time-varying value.
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct Input(Id32);

/// [`Node`] representing a time-varying input or an unconstrained time-varying value.
pub type InputNode = TermNode<Input>;

impl Term for Input {
    type Output = Lit;
    const NAME: &'static str = "Input";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [].into_iter()
    }

    fn apply_var_map(&mut self, _var_map: impl FnMut(Var) -> Lit) -> Pol {
        Pol::Pos
    }
}

impl TermDyn for Input {}

/// [`Term`] representing a register that updates with each transition of the represented state transition system.
///
/// In the initial state it transparently passes through the `init` input, and after every
/// transition it will output the value the `next` input had before the transition.
///
/// When viewing time-varying signals as infinite streams this can also be seen as the "cons"
/// operation on infinite streams.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(C)]
pub struct Reg {
    /// Produced value in the initial state.
    pub init: Lit,
    /// Value to produce after the next transition.
    pub next: Var,
}

/// [`Node`] representing a register that updates with each transition of the represented state
/// transition system.
pub type RegNode = TermNode<Reg>;

impl Term for Reg {
    type Output = Lit;
    const NAME: &'static str = "Reg";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [self.init.var(), self.next].into_iter()
    }

    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [self.init.var()].into_iter()
    }

    fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) -> Pol {
        let mapped_next = var_map(self.next);
        let mapped_init = self.init.map_var_to_lit(var_map);

        *self = Reg {
            init: mapped_init ^ mapped_next.pol(),
            next: mapped_next.var(),
        };

        mapped_next.pol()
    }

    fn reduce(&mut self, _builder: &mut impl NodeBuilder) -> Option<Self::Output> {
        if self.next == Var::FALSE && self.init == Lit::FALSE {
            Some(Lit::FALSE)
        } else {
            None
        }
    }
}

impl TermDyn for Reg {
    fn representative_input_var(&self) -> Var {
        self.next
    }
}
