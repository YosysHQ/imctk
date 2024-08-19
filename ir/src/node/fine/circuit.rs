//! Fine grained terms for representing bit-level combinational and sequential circuits.
use imctk_ids::{Id, Id32};
use imctk_util::unordered_pair::UnorderedPair;

use crate::{
    node::{
        builder::NodeBuilder,
        generic::{default_reduce_node, SealedWrapper, Term, TermDyn, TermNode},
    },
    var::{Lit, Pol, Var},
};

#[allow(unused_imports)] // rustdoc
use crate::node::generic::Node;

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

    const STATIC_TYPE_INFO: SealedWrapper<usize> = SealedWrapper(1);

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.inputs.map(|lit| lit.var()).into_iter()
    }

    fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) -> Pol {
        self.inputs = self.inputs.map(|lit| lit.lookup(&mut var_map));
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
                    inputs: self.inputs.map(|input| !input),
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
        mut var_map: impl FnMut(Var) -> Lit,
    ) -> <Self::Output as crate::var::VarOrLit>::Pol {
        let mut composed_pol = Pol::Pos;
        self.inputs = self.inputs.map(|var| {
            let mapped = var_map(var);
            composed_pol ^= mapped.pol();
            mapped.var()
        });
        composed_pol
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

    fn reduce_node(&mut self, output: Lit, builder: &mut impl NodeBuilder) -> bool {
        if output.is_const() {
            let [a, b] = self.inputs.into_values();
            builder.equiv([a.as_lit(), b ^ output.pol()]);
            true
        } else {
            for i in 0..2 {
                let input = self.inputs[i];
                let other_input = self.inputs[i ^ 1];
                if input == output.var() {
                    builder.equiv([other_input.as_lit(), Var::FALSE ^ output.pol()]);
                    return true;
                }
            }

            default_reduce_node(self, output, builder)
        }
    }
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
    const NAME: &'static str = "SteadyInput";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [].into_iter()
    }

    fn apply_var_map(&mut self, _var_map: impl FnMut(Var) -> Lit) -> Pol {
        Pol::Pos
    }

    fn is_steady(&self, _input_steady: impl Fn(Var) -> bool) -> bool {
        true
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

    fn is_steady(&self, _input_steady: impl Fn(Var) -> bool) -> bool {
        false
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
        let mapped_init = self.init.lookup(var_map);

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

    fn reduce_node(&mut self, output: Self::Output, builder: &mut impl NodeBuilder) -> bool {
        if self.next.as_pos() == output {
            builder.node(TermNode {
                output: output ^ self.init.pol(),
                term: Init {
                    input: self.init.var(),
                },
            });
            true
        } else {
            default_reduce_node(self, output, builder)
        }
    }

    fn is_steady(&self, _input_steady: impl Fn(Var) -> bool) -> bool {
        false
    }
}

impl TermDyn for Reg {
    fn representative_input_var(&self) -> Var {
        self.next
    }
}

/// [`Term`] representing the initial value of any given variable.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Init {
    /// The variable this term represents the initial value of.
    pub input: Var,
}

/// [`Node`] representing the initial value of any given variable.
pub type InitNode = TermNode<Init>;

impl Term for Init {
    type Output = Lit;
    const NAME: &'static str = "Init";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [self.input].into_iter()
    }

    fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) -> Pol {
        let input_lit = var_map(self.input);
        self.input = input_lit.var();
        input_lit.pol()
    }

    fn is_steady(&self, _input_steady: impl Fn(Var) -> bool) -> bool {
        true
    }

    fn reduce(&mut self, _buf: &mut impl NodeBuilder) -> Option<Self::Output> {
        if self.input == Var::FALSE {
            Some(Lit::FALSE)
        } else {
            None
        }
    }
}

impl TermDyn for Init {}

/// Extension trait to more conveniently add [fine-grained circuit terms][`self`] to any
/// [`NodeBuilder`].
pub trait FineCircuitNodeBuilder: NodeBuilder {
    /// Adds an [`And`] term to the environment.
    fn and(&mut self, inputs: impl Into<UnorderedPair<Lit>>) -> Lit {
        self.term(And {
            inputs: inputs.into(),
        })
    }

    /// Adds an [`And`] term to the environment that computes the Boolean 'or' of two inputs using
    /// De Morgan's laws.
    fn or(&mut self, inputs: impl Into<UnorderedPair<Lit>>) -> Lit {
        !self.term(And {
            inputs: inputs.into().map(|lit| !lit),
        })
    }

    /// Adds a [`Xor`] term to the environment, automatically normalizing polarities as required.
    fn xor(&mut self, inputs: impl Into<UnorderedPair<Lit>>) -> Lit {
        let inputs = inputs.into();
        self.term(Xor {
            inputs: inputs.map(|lit| lit.var()),
        }) ^ inputs[0].pol()
            ^ inputs[1].pol()
    }

    /// Adds a [`Reg`] term to the environment, automatically normalizing polarities as required.
    fn reg(&mut self, init: Lit, next: Lit) -> Lit {
        self.term(Reg {
            init: init ^ next.pol(),
            next: next.var(),
        }) ^ next.pol()
    }

    /// Adds a [`Init`] term to the environment, automatically normalizing polarities as required.
    fn init(&mut self, input: Lit) -> Lit {
        self.term(Init { input: input.var() }) ^ input.pol()
    }
}

impl<T: NodeBuilder> FineCircuitNodeBuilder for T {}
