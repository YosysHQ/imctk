//! Rarity simulation based equivalence candiate class refinement.
use imctk_ids::id_vec::IdVec;
use imctk_inc_refine::RefinementKeys;
use imctk_ir::var::Var;
use imctk_util::hash::hash_value;

use crate::{
    bit_matrix::WordVec,
    seq_sim::{bit_sliced::BitSlicedSim, rarity::RaritySim},
};

use super::RefinementContext;

/// Rarity simulation based equivalence candiate class refinement.
pub struct RaritySimRefinement {
    sim: RaritySim,
    hashes: IdVec<Var, u64>,
    keys: RefinementKeys<u64>,
}

impl RaritySimRefinement {
    pub fn new(ref_ctx: &mut RefinementContext) -> Self {
        let RefinementContext {
            sim_model: model, ..
        } = ref_ctx;
        Self {
            sim: RaritySim::new(BitSlicedSim::new(model, 1 << 12), 20, 64),
            hashes: IdVec::from_vec(vec![0; model.init_steps.len()]),
            keys: Default::default(),
        }
    }

    pub fn run_round(&mut self, ref_ctx: &mut RefinementContext) {
        let RefinementContext {
            sim_model, refine, ..
        } = ref_ctx;
        self.sim.sim_round(sim_model, |sim| {
            for (var, hash) in self.hashes.iter_mut() {
                let state = sim.comb_state(var);

                let inv_mask = WordVec::splat(0u64.wrapping_sub(sim_model.zero_values[var] as u64));

                let mut hash_vec = WordVec::splat(*hash);

                for &word_vec in state {
                    hash_vec = (hash_vec * 0x2545f4914f6cdd1d) ^ word_vec ^ inv_mask;
                }
                *hash = hash_value(hash_vec.to_array());
            }
        });

        refine.refine_all(&mut self.keys, |var| {
            let sim_var = sim_model.sim_from_env[var].unwrap().var();
            self.hashes[sim_var]
        });
    }
}
