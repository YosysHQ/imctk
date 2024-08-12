use std::mem::take;

use imctk_ids::IdRange;
use imctk_inc_refine::{IncrementalRefinement, RefinementKeys};
use imctk_ir::{
    env::Env,
    node::fine::circuit::{AndNode, XorNode},
    prelude::NodeBuilderDyn,
    var::{Lit, Var},
};
use imctk_util::unordered_pair::UnorderedPair;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use sim::FraigSim;
use zwohash::HashSet;

use crate::{bit_matrix::WordVec, circuit_sat::CircuitSat};

pub mod sim;

pub struct Fraig {
    refine: IncrementalRefinement<Var>,
    refine_keys_word: RefinementKeys<u64>,
    refine_keys_vec: RefinementKeys<[u64; 4]>,
    failed: HashSet<UnorderedPair<Var>>,
    tmp_lits: Vec<Lit>,
    sim: FraigSim,
    sat: CircuitSat,
    fraiged_var_count: usize,
    var_count: usize,
    equiv_pos: usize,
    cex_counter: usize,
    used_bits: WordVec,
    last_bit: usize,
    rng: SmallRng,
}

impl Default for Fraig {
    fn default() -> Self {
        let mut new = Self {
            refine: Default::default(),
            refine_keys_word: Default::default(),
            refine_keys_vec: Default::default(),
            sim: Default::default(),
            sat: Default::default(),
            tmp_lits: Default::default(),
            failed: Default::default(),
            var_count: 0,
            fraiged_var_count: 0,

            equiv_pos: 0,
            cex_counter: 0,
            used_bits: WordVec::ZERO,
            last_bit: 0,
            rng: SmallRng::seed_from_u64(0),
        };
        new.refine.insert_item(Var::FALSE);
        new
    }
}

impl Fraig {
    pub fn fraig_lit(&mut self, env: &mut Env, mut lit: Lit) -> Lit {
        'outer: loop {
            self.refresh_env(env);
            lit = env.lit_repr(lit);
            let var = lit.var();
            if self.refine.is_leaf(var) {
                return lit;
            }

            let node = env.def_node(var).unwrap();

            fn rank_vec(word_vec: WordVec) -> u32 {
                let weight = <[u64; 4]>::from(word_vec)
                    .map(|word| word.count_ones())
                    .into_iter()
                    .sum::<u32>();
                weight.min(WordVec::BITS as u32 - weight)
            }

            if let Some(&and) = node.dyn_cast::<AndNode>() {
                let mut inputs = and.term.inputs.into_values().map(|lit| lit.var());

                inputs.sort_by_key(|&var| rank_vec(self.sim.get_pol_invariant_value(env, var)));
                for input_var in inputs {
                    if self.fraig_lit(env, input_var.as_lit()) != input_var.as_lit() {
                        continue 'outer;
                    }

                    if !self.refine.contains_item(var) {
                        continue 'outer;
                    }
                }
            } else if let Some(&xor) = node.dyn_cast::<XorNode>() {
                let mut inputs = xor.term.inputs.into_values();

                inputs.sort_by_key(|&var| rank_vec(self.sim.get_pol_invariant_value(env, var)));
                for input_var in inputs {
                    if self.fraig_lit(env, input_var.as_lit()) != input_var.as_lit() {
                        continue 'outer;
                    }

                    if !self.refine.contains_item(var) {
                        continue 'outer;
                    }
                }
            }

            if !self.refine.contains_item(var) {
                continue 'outer;
            }

            return self.fraig_lit_local(env, lit);
        }
    }

    pub fn fraig_all(&mut self, env: &mut Env) {
        for var in <IdRange<Var>>::from_index_range(self.fraiged_var_count..self.var_count) {
            self.fraig_lit(env, var.as_lit());
        }
        self.fraiged_var_count = self.var_count;
    }

    fn fraig_lit_local(&mut self, env: &mut Env, mut lit: Lit) -> Lit {
        let mut prerefine: Option<Var> = None;
        let mut had_equivs = false;
        let mut prerefine_all = false;
        'outer: loop {
            if had_equivs {
                had_equivs = false;
                env.rebuild_egraph();
            }
            self.refresh_env(env);
            lit = env.lit_repr(lit);
            let var = lit.var();

            if let Some(prerefine) = prerefine.take() {
                if self.refine.contains_item(prerefine) {
                    self.refine
                        .refine_subtree(&mut self.refine_keys_vec, prerefine, |var| {
                            self.sim.get_pol_invariant_value(env, var).into()
                        });
                }

                if prerefine_all {
                    self.sim.clear_all_hashes();
                    self.sim.update_hashes();
                    self.used_bits = WordVec::ZERO;

                    self.refine
                        .refine_all(&mut self.refine_keys_word, |var| self.sim.get_hash(var));
                    log::info!(
                        "{} (sim) [failed: {}]",
                        self.partition_stats(),
                        self.failed.len()
                    );
                }
            }

            if self.refine.is_leaf(var) {
                return lit;
            }

            let mut var_value = self.sim.get_pol_invariant_value(env, var);

            log::trace!("var value = {:016x}", var_value);

            let prefix_candidate = (var_value == WordVec::ZERO
                && self.refine.contains(var, Var::FALSE))
            .then_some(Var::FALSE);

            let mut bit = (self.last_bit + 1) % (WordVec::BITS as usize);

            let mut had_non_failed = false;

            for candidate in prefix_candidate
                .into_iter()
                .chain(self.refine.postorder_descendants_iter(var))
            {
                if self.failed.contains(&[var, candidate].into()) {
                    continue;
                }
                had_non_failed = true;
                let candidate_value = self.sim.get_pol_invariant_value(env, candidate);

                if candidate_value != var_value {
                    continue;
                }

                let candidate_lit = candidate
                    ^ (self.sim.zero_phase(env, lit)
                        ^ self.sim.zero_phase(env, candidate.as_lit()));

                if env.lit_repr(candidate_lit) == env.lit_repr(lit) {
                    continue;
                }

                // TODO configurable conflict limit
                let Some(result) = self.sat.check_equiv(env, [lit, candidate_lit], usize::MAX) else {
                    self.failed.insert([var, candidate].into());
                    continue;
                };

                match result {
                    Ok(()) => {
                        log::debug!("proved equivalence between {candidate} and {var}");

                        env.equiv([lit, candidate_lit]);

                        env.rebuild_egraph();
                        continue;
                    }
                    Err(inputs) => {
                        self.tmp_lits.clear();
                        self.tmp_lits.extend(inputs);
                    }
                }

                let mut failed_before = self.used_bits;
                let mut failed_twice = WordVec::ZERO;

                for &input_lit in self.tmp_lits.iter() {
                    let value = self.sim.get_input(env, input_lit);
                    failed_twice |= failed_before & !value;
                    failed_before |= !value;
                }

                self.cex_counter += 1;
                if self.cex_counter % (WordVec::BITS as usize) == 0 {
                    prerefine_all = true;
                }

                if failed_twice != !WordVec::ZERO {
                    while failed_twice.as_array_ref()[bit / 64] & (1 << (bit % 64)) != 0 {
                        bit += 1;
                        bit %= WordVec::BITS as usize;
                    }
                } else if self.used_bits != !WordVec::ZERO {
                    while self.used_bits.as_array_ref()[bit / 64] & (1 << (bit % 64)) != 0 {
                        bit += 1;
                        bit %= WordVec::BITS as usize;
                    }
                } else {
                    prerefine_all = true;
                    prerefine = Some(var);
                    continue 'outer;
                }

                self.used_bits.as_array_mut()[bit / 64] |= 1 << (bit % 64);

                for &input_lit in self.tmp_lits.iter() {
                    let mut value = self.sim.get_input(env, input_lit);
                    failed_twice |= failed_before & !value;
                    failed_before |= !value;
                    value.as_array_mut()[bit / 64] |= 1 << (bit % 64);
                    self.sim.set_input(env, input_lit, value);
                }

                self.last_bit = bit;
                var_value = self.sim.get_pol_invariant_value(env, var);
            }

            if !had_non_failed {
                return lit;
            }

            prerefine = Some(var);
        }
    }

    fn refresh_env(&mut self, env: &mut Env) {
        let var_count = env.var_defs().len();
        if var_count > self.var_count {
            self.sim.batch_sim();

            let sim_vars = self.sim.import_order().len();
            for var in <IdRange<Var>>::from_index_range(self.var_count..var_count) {
                if env.lit_repr(var.as_lit()) != var.as_lit() {
                    continue;
                }
                self.sim
                    .ensure_var(env, var, |_| self.rng.gen::<[u64; 4]>().into());
            }
            for lit in self.sim.import_order()[sim_vars..].iter() {
                let var = lit.var();
                if var.index() >= self.var_count && env.var_defs().var_repr(var) == var {
                    self.refine.insert_item(var);
                }
            }

            self.var_count = var_count;

            for w in 0..4 {
                self.refine.refine_all(&mut self.refine_keys_word, |var| {
                    self.sim.get_precomputed_pol_invariant_value_word(var, w)
                });
            }
        }

        // TODO figure out if we can be smart about the which repr to keep in self.refine
        if env.equiv_vars().len() > self.equiv_pos {
            for &var in env.equiv_vars()[self.equiv_pos..].iter() {
                self.refine.remove_item(var);
            }
            self.equiv_pos = env.equiv_vars().len();
        }
    }

    pub fn partition_stats(&mut self) -> impl std::fmt::Display + std::fmt::Debug {
        Self::partition_stats_refine(&mut self.refine)
    }

    fn partition_stats_refine(
        refine: &mut IncrementalRefinement<Var>,
    ) -> impl std::fmt::Display + std::fmt::Debug {
        let root = refine.root_count();
        let classes = refine.nonleaf_root_count();
        assert_eq!(classes, refine.nonleaf_root_count2());
        let unique = root - classes;
        let constants = refine.ancestral_sibling_count(Var::FALSE);
        let pending = refine.item_count() - root;

        imctk_util::fmt::fmt_closure(move |f| {
            write!(
                f,
                "uniq: {unique} class: {classes} cnst: {constants} pend: {pending}"
            )
        })
    }

    pub fn recycle_sim(&mut self, env: &Env) {
        let old_sim = take(&mut self.sim);

        for input_lit in old_sim.inputs() {
            self.sim
                .set_input(env, input_lit, old_sim.get_imported_input(input_lit));
        }

        for var in env.var_defs().repr_vars() {
            self.sim
                .ensure_var(env, var, |_| self.rng.gen::<[u64; 4]>().into());
        }
    }

    pub fn random_sim(&mut self, env: &mut Env, rounds: usize) {
        self.refresh_env(env);

        let inputs = Vec::from_iter(self.sim.inputs());

        self.sim.clear_all_hashes();

        for round in 0..rounds {
            for &lit in inputs.iter() {
                self.sim
                    .set_input(env, lit, self.rng.gen::<[u64; 4]>().into());
            }
            self.sim.update_hashes();

            if (rounds - round - 1) % 5 == 0 {
                self.refine
                    .refine_all(&mut self.refine_keys_word, |var| self.sim.get_hash(var));
                log::info!("{} (sim round {})", self.partition_stats(), round);
            }
        }
    }
}
