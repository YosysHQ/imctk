//! Combinational circuit simulation for equivalence class refinement
use imctk_ids::id_vec::IdVec;
use imctk_ir::{
    env::Env,
    node::fine::circuit::{AndNode, XorNode},
    var::{Lit, Var},
};
use rand::{rngs::SmallRng, Rng, SeedableRng};

use crate::bit_matrix::WordVec;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum NodeOp {
    ConstFalse,
    Input,
    And,
    Xor,
}

#[derive(Debug)]
struct SimNode {
    op: NodeOp,
    zero_phase: bool,
    inputs: [Lit; 2],
    timestamp: u32,
}

impl SimNode {
    pub fn is_input(&self) -> bool {
        self.op == NodeOp::Input
    }
}

pub struct RefineSim {
    sim_from_env: IdVec<Var, Option<Lit>>,
    env_from_sim: IdVec<Var, Lit>,

    sim_node: IdVec<Var, SimNode>,
    value: IdVec<Var, WordVec>,
    inputs: Vec<Var>,
    equiv_pos: usize,

    timestamp: u32,

    input_rng: SmallRng,

    dirty: bool,
    batch_dirty: bool,
}

impl Default for RefineSim {
    fn default() -> Self {
        Self {
            sim_from_env: IdVec::from_vec(vec![Some(Lit::FALSE)]),
            env_from_sim: IdVec::from_vec(vec![Lit::FALSE]),

            sim_node: IdVec::from_vec(vec![SimNode {
                op: NodeOp::ConstFalse,
                inputs: [Lit::FALSE; 2],
                zero_phase: false,
                timestamp: 0,
            }]),
            value: IdVec::from_vec(vec![WordVec::ZERO]),
            inputs: Default::default(),
            equiv_pos: 0,
            timestamp: 0,
            input_rng: SmallRng::seed_from_u64(0),
            dirty: false,
            batch_dirty: false,
        }
    }
}

impl RefineSim {
    fn process_equivs(&mut self, env: &Env) {
        for &env_var in env.equiv_vars()[self.equiv_pos..].iter() {
            let env_repr_lit = env.var_defs().lit_repr(env_var.as_lit());

            let Some(sim_lit) = self.sim_from_env.get(env_var).copied().flatten() else { continue };

            if let Some(sim_repr_lit) = self
                .sim_from_env
                .grow_for_key(env_repr_lit.var())
                .map(|lit| lit ^ env_repr_lit.pol())
            {
                let (sim_keep, sim_elim) = if sim_lit < sim_repr_lit {
                    (sim_lit, sim_repr_lit)
                } else {
                    (sim_repr_lit, sim_lit)
                };

                let elim_node = &mut self.sim_node[sim_elim.var()];
                elim_node.op = NodeOp::And;
                elim_node.inputs = [Lit::TRUE, sim_keep ^ sim_elim.pol()];

                self.sim_from_env[env_repr_lit.var()] = Some(sim_keep ^ env_repr_lit.pol());
                self.sim_from_env[env_var] = None;
                self.env_from_sim[sim_keep.var()] = env_repr_lit ^ sim_keep.pol();
                self.env_from_sim[sim_elim.var()] = env_repr_lit ^ sim_elim.pol();
            } else {
                self.sim_from_env[env_repr_lit.var()] = Some(sim_lit ^ env_repr_lit.pol());
                self.sim_from_env[env_var] = None;
                self.env_from_sim[sim_lit.var()] = env_repr_lit ^ sim_lit.pol();
            }
        }

        self.equiv_pos = env.equiv_vars().len();
    }

    pub fn ensure_var(&mut self, env: &Env, env_var: Var) {
        self.import_repr_var(env, env.var_defs().var_repr(env_var));
    }

    fn import_lit(&mut self, env: &Env, env_lit: Lit) -> Lit {
        env.var_defs()
            .lit_repr(env_lit)
            .lookup(|var| self.import_repr_var(env, var))
    }

    fn import_repr_var(&mut self, env: &Env, env_var: Var) -> Lit {
        if env.equiv_vars().len() != self.equiv_pos {
            self.process_equivs(env);
        }
        if let Some(found_lit) = self.sim_from_env.grow_for_key(env_var) {
            return *found_lit;
        }

        let node = env.def_node(env_var).unwrap();

        if let Some(and) = node.dyn_cast::<AndNode>() {
            let output_pol = and.output.pol();
            let env_inputs = and.term.inputs.into_values();
            let sim_inputs = env_inputs.map(|lit| self.import_lit(env, lit));

            let [a_zero, b_zero] =
                sim_inputs.map(|lit| lit.lookup(|var| self.sim_node[var].zero_phase));
            let [a_timestamp, b_timestamp] =
                sim_inputs.map(|lit| self.sim_node[lit.var()].timestamp);
            let [a, b] =
                sim_inputs.map(|lit| self.value[lit.var()] ^ WordVec::splat(0u64 ^ lit.pol()));

            let zero_phase = a_zero & b_zero;
            let value = a & b;

            let sim_output = self
                .sim_node
                .push(SimNode {
                    op: NodeOp::And,
                    zero_phase,
                    inputs: sim_inputs,
                    timestamp: a_timestamp.min(b_timestamp),
                })
                .0;
            self.value.push(value);

            self.env_from_sim.push(env_var ^ output_pol);
            self.sim_from_env[env_var] = Some(sim_output ^ output_pol);

            return sim_output ^ output_pol;
        } else if let Some(xor) = node.dyn_cast::<XorNode>() {
            let output_pol = xor.output.pol();
            let env_inputs = xor.term.inputs.into_values().map(|var| var.as_lit());
            let sim_inputs = env_inputs.map(|lit| self.import_lit(env, lit));

            let [a_zero, b_zero] =
                sim_inputs.map(|lit| lit.lookup(|var| self.sim_node[var].zero_phase));
            let [a_timestamp, b_timestamp] =
                sim_inputs.map(|lit| self.sim_node[lit.var()].timestamp);
            let [a, b] =
                sim_inputs.map(|lit| self.value[lit.var()] ^ WordVec::splat(0u64 ^ lit.pol()));

            let zero_phase = a_zero ^ b_zero;
            let value = a ^ b;

            let sim_output = self
                .sim_node
                .push(SimNode {
                    op: NodeOp::Xor,
                    zero_phase,
                    inputs: sim_inputs,
                    timestamp: a_timestamp.min(b_timestamp),
                })
                .0;
            self.value.push(value);

            self.env_from_sim.push(env_var ^ output_pol);
            self.sim_from_env[env_var] = Some(sim_output ^ output_pol);

            return sim_output ^ output_pol;
        }

        let output_pol = node.output_lit().unwrap().pol();
        let sim_output = self
            .sim_node
            .push(SimNode {
                op: NodeOp::Input,
                zero_phase: false,
                inputs: [Lit::FALSE; 2],
                timestamp: self.timestamp,
            })
            .0;

        self.value.push(self.input_rng.gen::<[u64; 4]>().into());

        self.env_from_sim.push(env_var ^ output_pol);
        self.sim_from_env[env_var] = Some(sim_output ^ output_pol);

        self.inputs.push(sim_output);

        sim_output ^ output_pol
    }

    fn reset_timestamps(&mut self) {
        for node in self.sim_node.values_mut() {
            node.timestamp = 0;
        }
        self.timestamp = 1;
    }

    fn mark_dirty(&mut self) {
        if !self.dirty {
            self.dirty = true;
            self.timestamp += 1;
            #[cfg(coverage_nightly)]
            let limit = 4;
            #[cfg(not(coverage_nightly))]
            let limit = u32::MAX;

            if self.timestamp == limit {
                self.reset_timestamps();
            }
        }
    }

    pub fn set_input(&mut self, env: &Env, lit: Lit, value: WordVec) {
        self.mark_dirty();
        let sim_lit = self.import_lit(env, lit);
        assert!(self.sim_node[sim_lit.var()].is_input());
        self.value[sim_lit.var()] = value ^ WordVec::splat(0u64 ^ sim_lit.pol());
    }

    pub fn get_input(&mut self, env: &Env, lit: Lit) -> WordVec {
        let sim_lit = self.import_lit(env, lit);
        assert!(self.sim_node[sim_lit.var()].is_input());
        self.value[sim_lit.var()] ^ WordVec::splat(0u64 ^ sim_lit.pol())
    }

    pub fn zero_phase(&mut self, env: &Env, lit: Lit) -> bool {
        let sim_lit = self.import_lit(env, lit);
        self.sim_node[sim_lit.var()].zero_phase ^ sim_lit.pol()
    }

    pub fn get_value(&mut self, env: &Env, lit: Lit) -> WordVec {
        if self.dirty {
            self.timestamp += 1;
            self.dirty = false;
            self.batch_dirty = true;
        }
        let sim_lit = self.import_lit(env, lit);
        self.compute_lit(sim_lit)
    }

    pub fn get_pol_invariant_value(&mut self, env: &Env, var: Var) -> WordVec {
        if self.dirty {
            self.timestamp += 1;
            self.dirty = false;
            self.batch_dirty = true;
        }
        let sim_lit = self.import_lit(env, var.as_lit());
        let sim_repr_lit = sim_lit.var() ^ self.sim_node[sim_lit.var()].zero_phase;
        self.compute_lit(sim_repr_lit)
    }

    fn compute_lit(&mut self, sim_lit: Lit) -> WordVec {
        let sim_var = sim_lit.var();
        let mut node = &mut self.sim_node[sim_var];
        if node.timestamp == self.timestamp {
            return self.value[sim_var] ^ WordVec::splat(0u64 ^ sim_lit.pol());
        }
        node.timestamp = self.timestamp;
        let [a, b] = node.inputs.map(|lit| self.compute_lit(lit));
        node = &mut self.sim_node[sim_var];
        let value;
        match node.op {
            NodeOp::ConstFalse | NodeOp::Input => value = self.value[sim_var],

            NodeOp::And => {
                value = a & b;
                self.value[sim_var] = value
            }
            NodeOp::Xor => {
                value = a ^ b;
                self.value[sim_var] = value
            }
        }

        value ^ WordVec::splat(0u64 ^ sim_lit.pol())
    }
}
