//! Import of AIGER into imctk's internal representation.

use std::io;

use flussab_aiger::aig::OrderedAig;
use imctk_ids::{id_vec::IdVec, Id};
use imctk_ir::{
    env::{Env, EnvWrapper},
    node::{
        builder::NodeBuilder,
        fine::circuit::{And, Input, Reg, SteadyInput},
        generic::TermNode,
    },
    var::{Lit, Var},
};

// TODO there might be a sufficiently ergonomic way to make this type safe when there is a stable
// way to name opaque impl Trait types

/// Type alias used to indicate which literals use the AIGER numbering as opposed to the target
/// numbering.
///
/// Note that this is a type alias and thus does not provide any type checking.
pub type AigerLit = Lit;

/// Type alias used to indicate which variables use the AIGER numbering as opposed to the target
/// numbering.
///
/// Note that this is a type alias and thus does not provide any type checking.
pub type AigerVar = Var;

/// Partial mapping of AIGER variables to existing literals.
#[derive(Default, Debug)]
pub struct ExistingAigerVarMap {
    /// Partial mapping of AIGER variables to existing literals.
    pub var_map: IdVec<AigerVar, Option<Lit>>,
    /// Partial mapping of AIGER latch indices (represented as [`SteadyInput`]) to literals used for
    /// register initialization in the resulting internal represenation.
    pub latch_init: IdVec<SteadyInput, Option<Lit>>,
}

impl ExistingAigerVarMap {
    /// Read the passed data and parse it as binary AIGER file, importing it to the given target
    /// environment.
    // TODO document how the existing mapping is used and the return values
    pub fn import_binary_aiger(
        self,
        env: &mut (impl NodeBuilder + EnvWrapper),
        binary_aiger: impl io::Read,
    ) -> Result<(IdVec<AigerVar, Lit>, OrderedAig<AigerLit>), flussab_aiger::ParseError> {
        let parser =
            flussab_aiger::binary::Parser::<AigerLit>::from_read(binary_aiger, Default::default())?;

        let parsed = parser.parse()?;
        Ok((self.import_ordered_aig(env, &parsed), parsed))
    }

    /// Import an [`OrderedAig`] (representing a parsed binary AIGER file) to the given target
    /// environment.
    // TODO document how the existing mapping is used and the return values
    pub fn import_ordered_aig(
        mut self,
        env: &mut (impl NodeBuilder + EnvWrapper),
        aig: &OrderedAig<AigerLit>,
    ) -> IdVec<AigerVar, Lit> {
        self.var_map
            .resize_with(aig.max_var_index + 1, Default::default);

        self.var_map[Var::FALSE].get_or_insert(Lit::FALSE);

        let mut var_pos = 1;

        for input_index in 0..aig.input_count {
            self.var_map[Var::from_index(var_pos)]
                .get_or_insert_with(|| env.term(Input::from_id_index(input_index)));
            var_pos += 1;
        }

        self.latch_init
            .resize_with(aig.latches.len(), Default::default);

        let first_aiger_latch_output_index = var_pos;

        for (latch_index, aiger_latch) in aig.latches.iter().enumerate() {
            let init = SteadyInput::from_id_index(latch_index);
            let init_lit =
                self.latch_init[init].get_or_insert_with(|| match aiger_latch.initialization {
                    Some(constant) => Lit::FALSE ^ constant,
                    None => env.term(init),
                });

            self.var_map[Var::from_index(var_pos)].get_or_insert_with(|| {
                let level_bound = env.env().var_defs().level_bound(init_lit.var()) + 1;
                let latch_output = env.env_mut().fresh_var_with_level_bound(level_bound);
                latch_output.as_lit()
            });
            var_pos += 1;
        }

        for aiger_and in aig.and_gates.iter() {
            let output_aiger_var = Var::from_index(var_pos);
            let inputs = aiger_and
                .inputs
                .map(|lit| lit.lookup(|var| self.var_map[var].unwrap()));

            let term = And {
                inputs: inputs.into(),
            };

            if let Some(lit) = self.var_map[output_aiger_var] {
                env.node(TermNode { output: lit, term });
            } else {
                self.var_map[output_aiger_var] = Some(env.term(term));
            }
            var_pos += 1;
        }

        for (latch_index, aiger_latch) in aig.latches.iter().enumerate() {
            let init = SteadyInput::from_id_index(latch_index);

            let mut init_lit = self.latch_init[init].unwrap();
            let next_lit = aiger_latch
                .next_state
                .lookup(|var| self.var_map[var].unwrap());

            let mut output_lit = self.var_map
                [AigerVar::from_index(first_aiger_latch_output_index + latch_index)]
            .unwrap();

            init_lit ^= next_lit.pol();
            output_lit ^= next_lit.pol();

            env.node(TermNode {
                output: output_lit,
                term: Reg {
                    init: init_lit,
                    next: next_lit.var(),
                },
            });
        }

        IdVec::from_vec(
            self.var_map
                .into_values()
                .into_iter()
                .map(|lit| lit.unwrap())
                .collect(),
        )
    }
}

/// Read the passed data and parse it as binary AIGER file, importing it to the given target
/// environment.
// TODO document the absence of an existing mapping and the return values
pub fn import_binary_aiger(
    env: &mut Env,
    binary_aiger: impl io::Read,
) -> Result<(IdVec<AigerVar, Lit>, OrderedAig<AigerLit>), flussab_aiger::ParseError> {
    ExistingAigerVarMap::default().import_binary_aiger(env, binary_aiger)
}

/// Import an [`OrderedAig`] (representing a parsed binary AIGER file) to the given target
/// environment.
// TODO document the absence of an existing mapping and the return values
pub fn import_ordered_aig(env: &mut Env, aig: &OrderedAig<AigerLit>) -> IdVec<AigerVar, Lit> {
    ExistingAigerVarMap::default().import_ordered_aig(env, aig)
}
