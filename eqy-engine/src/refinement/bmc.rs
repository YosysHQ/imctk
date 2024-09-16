//! Bounded model checking based equivalence candiate class refinement.
use std::mem::take;

use imctk_ir::{
    env::Env,
    prelude::NodeBuilderDyn,
    var::{Lit, Var},
};

use crate::{
    bit_matrix::WordVec,
    circuit_sat::CircuitSat,
    comb_sim::refine_sim::RefineSim,
    seq_sim::model::SimModel,
    time_step::TimeStep,
    unroll::{Unroll, UnrollMode},
};

use super::{driver::RefinementDriver, EnvVarRefinement, RefinementContext};

/// Bounded model checking based equivalence candiate class refinement.
pub struct BmcRefinement {
    bmc_sim: RefineSim,
    bmc: Unroll,
    bmc_env: Env,
    bmc_refine: EnvVarRefinement,
    bmc_sat: CircuitSat,
    bmc_cex_bit: usize,
}

impl Default for BmcRefinement {
    fn default() -> Self {
        Self {
            bmc_sim: RefineSim::default(),
            bmc: Unroll::new(UnrollMode::Bmc),
            bmc_env: Env::default(),
            bmc_refine: EnvVarRefinement::default(),
            bmc_sat: CircuitSat::default(),
            bmc_cex_bit: 0,
        }
    }
}

impl BmcRefinement {
    pub fn refine_for_time_step(&mut self, ref_ctx: &mut RefinementContext, time: TimeStep) {
        ref_ctx.sync_equivs();
        self.bmc_refine.sync_equivs(&self.bmc_env);

        let target_vars = Vec::from_iter(ref_ctx.refine.nonisolated_iter());

        // Unroll nonisolated vars to current timestep include the unrolled literals for tracking
        // candidate equivalences
        for &var in target_vars.iter() {
            let bmc_lit = self.bmc_lit_for_seq_var(ref_ctx.sim_model, ref_ctx.env, time, var);
            self.bmc_refine.insert_item(bmc_lit.var());
        }

        self.bmc_refine.sync_equivs(&self.bmc_env);

        // Refine unrolled equivalences with current simulation values
        self.bmc_refine.refine_all(&mut Default::default(), |var| {
            self.bmc_sim
                .get_pol_invariant_value(&self.bmc_env, var)
                .to_array()
        });

        // Refine sequential equivalences with unrolled equivalence classes
        ref_ctx.refine.refine_all(&mut Default::default(), |var| {
            self.bmc_class_for_seq_var(ref_ctx.sim_model, ref_ctx.env, time, var)
        });

        log::debug!("unrolled repr: {}", ref_ctx.refine.stats());

        let mut driver = <RefinementDriver<Var, u32, (Var, [u64; 4])>>::default();

        // Initialize refinement driver
        driver.reset(target_vars.iter().map(|&var| {
            let seq_class = ref_ctx.refine.root(var);
            let (bmc_lit, bmc_class) =
                self.bmc_lit_and_class_for_seq_var(ref_ctx.sim_model, ref_ctx.env, time, var);
            (var, !self.bmc_env.var_defs().level_bound(bmc_lit.var()), {
                // The driver will be used to refine via simulation values, but we initialize it
                // by using the known bmc classes as if they were simulation
                // values as that gives us the most precise known initial partition
                let mut refine_value = [0u64; 4];
                refine_value[0] = bmc_class.code() as u64;
                (seq_class, refine_value)
            })
        }));

        let mut should_flush_sim = false;

        let mut stats_seq_already_refiend = 0;
        let mut stats_bmc_arleady_equiv = 0;
        let mut stats_bmc_arleady_refined = 0;
        let mut stats_bmc_current_sim = 0;
        let mut stats_sat_equiv = 0;
        let mut stats_sat_cex = 0;

        // Refine partition by simulation and sat solving on the unrolled circuit
        while driver.refine(|seq_a, seq_b| {
            let seq_vars = [seq_a, seq_b];

            if !ref_ctx.refine.is_ancestor_of(seq_a, seq_b)
                && !ref_ctx.refine.is_ancestor_of(seq_b, seq_a)
            {
                // Already known inequivalent sequences from flushing or previous incremental use
                stats_seq_already_refiend += 1;
                return false;
            }

            let [bmc_a, bmc_b] = seq_vars
                .map(|var| self.bmc_lit_for_seq_var(ref_ctx.sim_model, ref_ctx.env, time, var));

            if bmc_a.var() == bmc_b.var() {
                // Known equivalent via the unrolled egraph
                stats_bmc_arleady_equiv += 1;
                return bmc_a == bmc_b;
            }

            if !self.bmc_refine.is_ancestor_of(bmc_a.var(), bmc_b.var())
                && !self.bmc_refine.is_ancestor_of(bmc_b.var(), bmc_a.var())
            {
                // Already known inequivalent at this time step from flushing or previous
                // incremental use
                stats_bmc_arleady_refined += 1;
                return false;
            }

            // Known distinct via the current simulation inputs
            if self.bmc_sim.get_value(&self.bmc_env, bmc_a)
                != self.bmc_sim.get_value(&self.bmc_env, bmc_b)
            {
                stats_bmc_current_sim += 1;
                return false;
            }

            // Equivalence currently unknown, query the sat solver for a counter example to
            // equivalence
            let query_result = self
                .bmc_sat
                .query_cubes(&mut self.bmc_env, [[bmc_a, !bmc_b], [!bmc_a, bmc_b]])
                .unwrap();

            if !query_result {
                // Solver proved equivalence in the current timestamp
                self.bmc_env.equiv([bmc_a, bmc_b]);
                self.bmc_refine.sync_equivs(&self.bmc_env);

                stats_sat_equiv += 1;
                true
            } else {
                // The solver found a counter example, add the required inputs to the simulation
                // inputs
                for &input_lit in self.bmc_sat.input_model() {
                    let mut value = self.bmc_sim.get_input(&self.bmc_env, input_lit);
                    value.as_array_mut()[self.bmc_cex_bit / 64] |= 1 << (self.bmc_cex_bit % 64);
                    self.bmc_sim.set_input(&self.bmc_env, input_lit, value);
                }
                // Sweep through the bits so the last 256 counter examples are always part of the
                // simulation
                self.bmc_cex_bit += 1;
                self.bmc_cex_bit %= WordVec::BITS as usize;

                if self.bmc_cex_bit == 0 {
                    // Every 256 counter examples we fully refine the bmc equivalence classes using
                    // the current simulation inputs, to ensure we don't overwrite inputs for a
                    // counter example that we haven't fully exploited yet
                    should_flush_sim = true;
                }

                debug_assert_ne!(
                    self.bmc_sim.get_value(&self.bmc_env, bmc_a),
                    self.bmc_sim.get_value(&self.bmc_env, bmc_b),
                    "{bmc_a} = {} vs {bmc_b} = {}, {:?}",
                    self.bmc_env.lit_repr(bmc_a),
                    self.bmc_env.lit_repr(bmc_b),
                    self.bmc_sat.input_model()
                );
                stats_sat_cex += 1;
                false
            }
        }) {
            if take(&mut should_flush_sim) {
                self.flush_sim(ref_ctx, time);
            }

            // Use simulation to refine the non-equivalent values of the last processed candidate
            // class
            driver.update(|var| {
                let unrolled = self.bmc_lit_for_seq_var(ref_ctx.sim_model, ref_ctx.env, time, var);

                (
                    ref_ctx.refine.root(var),
                    self.bmc_sim
                        .get_pol_invariant_value(&self.bmc_env, unrolled.var())
                        .into(),
                )
            });
        }

        // Finally refine using any counter examples not flushed so far
        self.flush_sim(ref_ctx, time);

        log::debug!(
            "stats: {:?}",
            [
                stats_seq_already_refiend,
                stats_bmc_arleady_equiv,
                stats_bmc_arleady_refined,
                stats_bmc_current_sim,
                stats_sat_equiv,
                stats_sat_cex
            ]
        );
    }

    fn bmc_lit_for_seq_var(
        &mut self,
        sim_model: &SimModel,
        env: &Env,
        step: TimeStep,
        seq_var: Var,
    ) -> Lit {
        // We use the sequential all zero input value to pick the polarity
        let zero_phase = sim_model.sim_from_env[seq_var]
            .unwrap()
            .lookup(|var| sim_model.zero_values[var]);

        let seq_lit = seq_var ^ zero_phase;

        let bmc_lit = self.bmc.unroll(env, &mut self.bmc_env, step, seq_lit);
        self.bmc_env.lit_repr(bmc_lit)
    }

    fn bmc_lit_and_class_for_seq_var(
        &mut self,
        sim_model: &SimModel,
        env: &Env,
        step: TimeStep,
        seq_var: Var,
    ) -> (Lit, Lit) {
        let bmc_lit = self.bmc_lit_for_seq_var(sim_model, env, step, seq_var);
        let bmc_root = self.bmc_refine.root(bmc_lit.var());
        (
            bmc_lit,
            bmc_root
                ^ self.bmc_sim.zero_phase(&self.bmc_env, bmc_lit)
                ^ self.bmc_sim.zero_phase(&self.bmc_env, bmc_root.as_lit()),
        )
    }

    fn bmc_class_for_seq_var(
        &mut self,
        sim_model: &SimModel,
        env: &Env,
        step: TimeStep,
        seq_var: Var,
    ) -> Lit {
        self.bmc_lit_and_class_for_seq_var(sim_model, env, step, seq_var)
            .1
    }

    fn flush_sim(&mut self, ref_env: &mut RefinementContext, time: TimeStep) {
        self.bmc_refine.refine_all(&mut Default::default(), |var| {
            self.bmc_sim
                .get_pol_invariant_value(&self.bmc_env, var)
                .to_array()
        });

        ref_env.refine.refine_all(&mut Default::default(), |var| {
            self.bmc_class_for_seq_var(ref_env.sim_model, ref_env.env, time, var)
        });

        log::debug!("flushed sim: {}", ref_env.refine.stats());
    }
}
