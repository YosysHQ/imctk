use std::time::Duration;

use imctk_ids::id_vec::IdVec;
use imctk_ir::{
    env::Env,
    node::fine::{
        circuit::{AndNode, XorNode},
        constraints::BinClause,
    },
    var::{Lit, Var},
};
use quanta::Instant;

pub struct CircuitSat {
    solver:
        Option<imctk_abc::sat::glucose2::Solver<'static, imctk_abc::sat::glucose2::CircuitProp>>,
    sat_from_env: IdVec<Var, Option<Lit>>,
    env_from_sat: IdVec<Var, Lit>,
    sat_inputs: IdVec<Var, bool>,
    extra_inputs: Vec<Lit>,
    query_count: usize,
    pub solve_time: Duration,
    pub sat_time: Duration,
    pub unsat_time: Duration,
}

impl Default for CircuitSat {
    fn default() -> Self {
        Self {
            solver: None,
            sat_from_env: IdVec::from_vec(vec![Some(Lit::FALSE)]),
            env_from_sat: IdVec::from_vec(vec![Lit::FALSE]),
            sat_inputs: IdVec::from_vec(vec![false]),
            extra_inputs: vec![],
            query_count: 0,
            solve_time: Default::default(),
            sat_time: Default::default(),
            unsat_time: Default::default(),
        }
    }
}

impl CircuitSat {
    pub fn solver(
        &mut self,
    ) -> &mut imctk_abc::sat::glucose2::Solver<'static, imctk_abc::sat::glucose2::CircuitProp> {
        self.solver.get_or_insert_with(Default::default)
    }

    fn add_to_solver(&mut self, env: &Env, lit: Lit) -> Lit {
        // TODO first process env equivs?
        let var = lit.var();
        let output_pol = lit.pol();

        let slot = self.sat_from_env.grow_for_key(var);
        if let Some(sat_var) = *slot {
            return sat_var ^ output_pol;
        }

        let sat_lit: Lit;

        let repr = env.var_defs().lit_repr(var.as_lit());
        if repr != var.as_lit() {
            sat_lit = self.add_to_solver(env, repr);

            self.sat_from_env[var] = Some(sat_lit);
            return sat_lit ^ output_pol;
        } else if let Some(node) = env.def_node(var) {
            if let Some(and) = node.dyn_cast::<AndNode>() {
                let inputs = and.term.inputs.into_values();

                let sat_inputs = inputs.map(|input_lit| self.add_to_solver(env, input_lit));

                let sat_var = self.env_from_sat.push(var ^ and.output.pol()).0;
                self.sat_inputs.push(false);
                let sat_lit = sat_var ^ and.output.pol();

                let solver = self.solver();
                solver.add_and(sat_var, sat_inputs.into());

                self.sat_from_env[var] = Some(sat_lit);

                // TODO make this configurable
                self.add_redundant_nodes_to_solver(env, var);
                return sat_lit ^ output_pol;
            } else if let Some(xor) = node.dyn_cast::<XorNode>() {
                let inputs = xor.term.inputs.into_values();

                let sat_inputs =
                    inputs.map(|input_var| self.add_to_solver(env, input_var.as_lit()));

                let sat_var = self.env_from_sat.push(var ^ xor.output.pol()).0;
                self.sat_inputs.push(false);
                let sat_lit = sat_var ^ xor.output.pol();

                let solver = self.solver();
                solver.add_xor(sat_var, sat_inputs.into());

                self.sat_from_env[var] = Some(sat_lit);

                // TODO make this configurable
                self.add_redundant_nodes_to_solver(env, var);
                return sat_lit ^ output_pol;
            }
        }

        let sat_lit = self.env_from_sat.push(var.as_lit()).0.as_lit();
        self.sat_inputs.push(true);
        self.sat_from_env[var] = Some(sat_lit);
        sat_lit ^ output_pol
    }

    fn add_redundant_nodes_to_solver(&mut self, env: &Env, var: Var) {
        'outer: for node_id in env
            .defs_index()
            .find_non_primary_defs_unordered(var)
            .chain(env.uses_index().find_uses_unordered(var))
        {
            let node = env.nodes().get_dyn(node_id).unwrap();
            if let Some(and) = node.dyn_cast::<AndNode>() {
                let inputs = and.term.inputs.into_values();
                if !matches!(self.sat_from_env.get(and.output.var()), Some(Some(_))) {
                    continue 'outer;
                }
                for input in inputs.into_iter() {
                    if !matches!(self.sat_from_env.get(input.var()), Some(Some(_))) {
                        continue 'outer;
                    }
                }

                let [a, b] = inputs.map(|input_lit| self.add_to_solver(env, input_lit));
                let y = and.output.lookup(|var| self.sat_from_env[var].unwrap());

                let solver = self.solver();
                solver.add_clause(&[!a, !b, y]);
                solver.add_clause(&[a, !y]);
                solver.add_clause(&[b, !y]);

                // TODO also add xors
            } else if let Some(bin_clause) = node.dyn_cast::<BinClause>() {
                let inputs = bin_clause.inputs.into_values();
                for input in inputs.into_iter() {
                    if !matches!(self.sat_from_env.get(input.var()), Some(Some(_))) {
                        continue 'outer;
                    }
                }
                let [a, b] = inputs.map(|input_lit| self.add_to_solver(env, input_lit));

                let solver = self.solver();
                solver.add_clause(&[a, b]);
            }
        }
    }

    pub fn check_equiv(
        &mut self,
        env: &Env,
        lits: [Lit; 2],
        conflict_limit: usize,
    ) -> Option<Result<(), impl Iterator<Item = Lit> + Clone + '_>> {
        self.query_count += 1;

        if self.query_count == 10_000 {
            self.sat_from_env.clear();
            self.env_from_sat.clear();
            self.sat_inputs.clear();
            self.sat_from_env.push(Some(Lit::FALSE));
            self.env_from_sat.push(Lit::FALSE);
            self.sat_inputs.push(false);
            self.solver = None;
            self.query_count = 0;
        }
        if self.query_count % 1000 == 0 {
            log::debug!(
                "sat: {:.2?} ({:.2?} / {:.2?}){}",
                self.solve_time,
                self.sat_time,
                self.unsat_time,
                if self.query_count == 0 {
                    " (recycled)"
                } else {
                    ""
                }
            );
        }
        let sat_lits = lits.map(|lit| self.add_to_solver(env, lit));

        self.solver();
        let solver = self.solver.as_mut().unwrap();

        let solve_start = Instant::now();

        solver.new_round();
        for &lit in sat_lits.iter() {
            solver.mark_cone(lit.var());
        }

        let [mut a, mut b] = sat_lits;

        assert_ne!(a.var(), b.var());

        let mut limit_left = conflict_limit.max(4);
        let mut limit = 4;
        limit_left -= limit;

        let is_sat = loop {
            solver.cumulative_conflict_limit(limit);
            self.extra_inputs.clear();
            self.extra_inputs.extend([a, !b]);
            if let Some(is_sat) = solver.solve_assuming(&[a, !b]) {
                if is_sat {
                    break true;
                }

                break loop {
                    solver.cumulative_conflict_limit(limit);
                    self.extra_inputs.clear();
                    self.extra_inputs.extend([!a, b]);
                    if let Some(is_sat) = solver.solve_assuming(&[!a, b]) {
                        break is_sat;
                    }
                    limit += limit;
                    limit = limit.min(limit_left);
                    limit_left -= limit;
                    if limit == 0 {
                        let time = solve_start.elapsed();

                        self.solve_time += time;
                        return None;
                    }
                };
            }
            limit += limit;
            limit = limit.min(limit_left);
            limit_left -= limit;
            if limit == 0 {
                let time = solve_start.elapsed();

                self.solve_time += time;
                return None;
            }

            (a, b) = (b, a);
        };

        if is_sat {
            // TODO check whether we can make abc's glucose2 report these as inputs so we don't have
            // to add them here

            // TODO also check that the input assumption is a failed literal before including it
            self.extra_inputs.retain(|lit| self.sat_inputs[lit.var()]);
        }

        let time = solve_start.elapsed();

        self.solve_time += time;
        if is_sat {
            self.sat_time += time;
        } else {
            self.unsat_time += time;
        }

        Some(if is_sat {
            Err(solver
                .input_model()
                .iter()
                .filter(|&&sat_lit| sat_lit != Lit::TRUE)
                .chain(self.extra_inputs.iter())
                .map(|&sat_lit| sat_lit.lookup(|var| self.env_from_sat[var])))
        } else {
            Ok(())
        })
    }
}
