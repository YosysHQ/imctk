//! Circuit aware SAT solving.
//!
//! This provides a circuit aware SAT solver that can be used to solve queries for combinational
//! circuits represented in an [environment][Env].
//!
//! This is a wrapper around abc's glucose2 circuit aware SAT solver as described by Zhang, Jiang
//! and Mishchenko[^1].
//!
//! [^1]: [Zhang, He-Teng, Jie-Hong R. Jiang, and Alan Mishchenko. "A circuit-based SAT solver for
//!     logic synthesis." 2021 IEEE/ACM International Conference On Computer Aided Design (ICCAD).
//!     IEEE, 2021.
//!     (PDF)](https://people.eecs.berkeley.edu/~alanmi/publications/2022/iwls22_sweep.pdf)

use std::{cell::RefCell, collections::VecDeque, mem::take, time::Duration};

use imctk_abc::sat::glucose2::ProofTracer;
use imctk_ids::id_vec::IdVec;
use imctk_ir::{
    env::Env,
    node::fine::circuit::{AndNode, XorNode},
    prelude::NodeBuilderDyn,
    var::{Lit, Var},
};
use quanta::Instant;
use zwohash::{HashMap, HashSet};

use crate::seq_sim::model::XaigStep;

type InnerSolver<'a> = imctk_abc::sat::glucose2::Solver<'a, imctk_abc::sat::glucose2::CircuitProp>;

/// Circuit aware SAT solving adaptor.
///
/// See the [module level documentation][self] for an overview.
pub struct CircuitSat {
    solver: Option<InnerSolver<'static>>,
    sat_from_env: IdVec<Var, Option<Lit>>,
    env_from_sat: IdVec<Var, Lit>,
    sat_gates: IdVec<Var, Option<XaigStep>>,
    redundant_gates: IdVec<u32, CircuitCutGate>,

    input_model: Vec<Lit>,
    inner_model: Vec<Lit>,
    sat_cube_buf: Vec<Lit>,
    in_cube: HashMap<Lit, Lit>,

    equiv_pos: usize,

    solver_query_count: usize,
    query_count: usize,

    tracer: Option<Box<RefCell<Tracer>>>,

    failed_assumptions: HashSet<Lit>,
    pending_cubes: VecDeque<[usize; 2]>,
    unsat_cubes: Vec<[usize; 2]>,
    sat_cube: Option<[usize; 2]>,
    cube_buf: Vec<Lit>,

    conflicts_left: Option<usize>,

    pub sat_time: Duration,
    pub unsat_time: Duration,
    pub unknown_time: Duration,

    pub sat_count: u64,
    pub unsat_count: u64,
    pub unknown_count: u64,
}

impl Default for CircuitSat {
    fn default() -> Self {
        Self {
            solver: None,
            sat_from_env: IdVec::from_vec(vec![Some(Lit::FALSE)]),
            env_from_sat: IdVec::from_vec(vec![Lit::FALSE]),
            sat_gates: IdVec::from_vec(vec![None]),
            redundant_gates: IdVec::default(),

            input_model: vec![],
            inner_model: vec![],
            sat_cube_buf: vec![],
            in_cube: Default::default(),

            equiv_pos: usize::MAX,

            solver_query_count: 0,
            query_count: 0,

            tracer: Some(Default::default()),

            failed_assumptions: Default::default(),
            pending_cubes: Default::default(),
            sat_cube: None,
            unsat_cubes: vec![],
            cube_buf: vec![],

            conflicts_left: None,

            sat_time: Default::default(),
            unsat_time: Default::default(),
            unknown_time: Default::default(),

            sat_count: 0,
            unsat_count: 0,
            unknown_count: 0,
        }
    }
}

impl CircuitSat {
    fn add_to_solver(&mut self, solver: &mut InnerSolver, env: &Env, lit: Lit) -> Lit {
        let var = lit.var();
        let output_pol = lit.pol();

        let slot = self.sat_from_env.grow_for_key(var);
        if let Some(sat_var) = *slot {
            return sat_var ^ output_pol;
        }

        let sat_lit: Lit;

        let repr = env.var_defs().lit_repr(var.as_lit());
        if repr != var.as_lit() {
            sat_lit = self.add_to_solver(solver, env, repr);

            self.sat_from_env[var] = Some(sat_lit);
            return sat_lit ^ output_pol;
        }

        assert!(self.sat_gates.len() < (1 << 30));

        if let Some(node) = env.def_node(var) {
            if let Some(and) = node.dyn_cast::<AndNode>() {
                let inputs = and.term.inputs;

                let sat_inputs = inputs.map(|input_lit| self.add_to_solver(solver, env, input_lit));

                let sat_var = self.env_from_sat.push(var ^ and.output.pol()).0;
                self.sat_gates.push(Some(XaigStep::and(sat_inputs)));
                let sat_lit = sat_var ^ and.output.pol();

                solver.add_and(sat_var, sat_inputs);

                self.sat_from_env[var] = Some(sat_lit);

                return sat_lit ^ output_pol;
            } else if let Some(xor) = node.dyn_cast::<XorNode>() {
                let inputs = xor.term.inputs;

                let sat_inputs =
                    inputs.map(|input_var| self.add_to_solver(solver, env, input_var.as_lit()));

                let sat_var = self.env_from_sat.push(var ^ xor.output.pol()).0;
                self.sat_gates.push(Some(XaigStep::xor(sat_inputs)));
                let sat_lit = sat_var ^ xor.output.pol();

                solver.add_xor(sat_var, sat_inputs);

                self.sat_from_env[var] = Some(sat_lit);

                return sat_lit ^ output_pol;
            }
        }

        let sat_lit = self.env_from_sat.push(var.as_lit()).0.as_lit();
        self.sat_gates.push(None);
        self.sat_from_env[var] = Some(sat_lit);
        sat_lit ^ output_pol
    }

    pub fn reset(&mut self) {
        self.sat_from_env.clear();
        self.env_from_sat.clear();
        self.sat_gates.clear();
        self.redundant_gates.clear();
        self.sat_from_env.push(Some(Lit::FALSE));
        self.env_from_sat.push(Lit::FALSE);
        self.sat_gates.push(None);
        self.solver_query_count = 0;
        self.equiv_pos = usize::MAX;

        if let Some(solver) = &mut self.solver {
            solver.reset();
        }

        self.tracer.as_mut().unwrap().get_mut().reset();
    }

    fn new_query(&mut self) {
        if self.solver_query_count == 20_000 {
            self.reset();
        }
        if self.query_count % 1000 == 0 {
            let solve_time = self.sat_time + self.unsat_time + self.unknown_time;
            let query_count = self.sat_count + self.unsat_count + self.unknown_count;
            log::debug!(
                "sat: {} {:.2?} (S {} {:.2?} U {} {:.2?} F {} {:.2?}){}",
                query_count,
                solve_time,
                self.sat_count,
                self.sat_time,
                self.unsat_count,
                self.unsat_time,
                self.unknown_count,
                self.unknown_time,
                if self.solver_query_count < 1000 {
                    " (recycled)"
                } else {
                    ""
                }
            );
        }

        self.solver_query_count += 1;
        self.query_count += 1;
    }

    pub fn cumulative_conflict_limit(&mut self, limit: Option<usize>) {
        self.conflicts_left = limit;
    }

    pub fn query_lit(&mut self, env: &mut Env, lit: Lit) -> Option<bool> {
        self.query_cube(env, [lit])
    }

    pub fn query_cube(
        &mut self,
        env: &mut Env,
        cube: impl IntoIterator<Item = Lit>,
    ) -> Option<bool> {
        self.query_cubes(env, [cube])
    }

    pub fn query_cubes(
        &mut self,
        env: &mut Env,
        cubes: impl IntoIterator<Item = impl IntoIterator<Item = Lit>>,
    ) -> Option<bool> {
        env.rebuild_egraph();
        let start_time = Instant::now();
        self.new_query();
        let tracer = take(&mut self.tracer).unwrap_or_default();

        {
            let mut tracer = tracer.borrow_mut();
            tracer.events.clear();
        }

        let mut solver = take(&mut self.solver).unwrap_or_default();

        let mut tracer_ref = &*tracer;
        solver = solver.with_proof_tracer(&mut tracer_ref);

        let result = self.query_inner(&mut solver, &tracer, env, cubes);

        let mut new_units = false;
        let mut event_pos = 0;
        {
            let tracer = tracer.borrow_mut();
            while let Some(&event) = tracer.events.get(event_pos) {
                event_pos += 1;
                if let Some(unit) = event {
                    env.equiv([unit.lookup(|var| self.env_from_sat[var]), Lit::TRUE]);
                    new_units = true;
                }
            }
        }

        if new_units {
            env.rebuild_egraph();
        }

        {
            let mut tracer = tracer.borrow_mut();
            tracer.events.clear();
        }

        self.solver = Some(solver.stop_proof_tracing());
        self.tracer = Some(tracer);

        let elapsed = start_time.elapsed();
        match result {
            Some(false) => {
                self.unsat_count += 1;
                self.unsat_time += elapsed;
            }
            Some(true) => {
                self.sat_count += 1;
                self.sat_time += elapsed;
            }
            None => {
                self.unknown_count += 1;
                self.unknown_time += elapsed;
                self.conflicts_left = None;
            }
        }

        result
    }

    fn refresh_env(&mut self, solver: &mut InnerSolver, tracer: &RefCell<Tracer>, env: &mut Env) {
        env.rebuild_egraph();
        self.equiv_pos = self.equiv_pos.min(env.equiv_vars().len());

        while let Some(&env_var) = env.equiv_vars().get(self.equiv_pos) {
            self.equiv_pos += 1;
            let env_repr_lit = env.lit_repr(env_var.as_lit());

            let Some(sat_lit) = self.sat_from_env.get(env_var).copied().flatten() else { continue };

            if let Some(sat_repr_lit) = self
                .sat_from_env
                .grow_for_key(env_repr_lit.var())
                .map(|lit| lit ^ env_repr_lit.pol())
            {
                let (sat_keep, sat_elim) = if sat_lit < sat_repr_lit {
                    (sat_lit, sat_repr_lit)
                } else {
                    (sat_repr_lit, sat_lit)
                };

                // TODO This still has issues with maintaining all solver internal invariants, so it
                // remains disabled for now
                let _ = solver;
                let _ = tracer;
                #[cfg(any())]
                {
                    let inputs = [Lit::TRUE, sat_keep ^ sat_elim.pol()].into();

                    {
                        let mut tracer = tracer.borrow_mut();
                        tracer
                            .gate_elims
                            .insert(sat_elim.var(), sat_keep ^ sat_elim.pol());
                    }

                    if sat_keep.is_const() {
                        solver.add_tagged_clause(
                            &[sat_elim ^ !sat_keep.pol()],
                            (1 << 31) | sat_elim.index() as u32,
                        )
                    } else {
                        solver.add_and(sat_elim.var(), inputs);
                    }
                }

                self.sat_from_env[env_repr_lit.var()] = Some(sat_keep ^ env_repr_lit.pol());
                self.sat_from_env[env_var] = None;
                self.env_from_sat[sat_keep.var()] = env_repr_lit ^ sat_keep.pol();
                self.env_from_sat[sat_elim.var()] = env_repr_lit ^ sat_elim.pol();
            } else {
                self.sat_from_env[env_repr_lit.var()] = Some(sat_lit ^ env_repr_lit.pol());
                self.sat_from_env[env_var] = None;
                self.env_from_sat[sat_lit.var()] = env_repr_lit ^ sat_lit.pol();
            }
        }
    }

    fn query_inner(
        &mut self,
        solver: &mut InnerSolver,
        tracer: &RefCell<Tracer>,
        env: &mut Env,
        cubes: impl IntoIterator<Item = impl IntoIterator<Item = Lit>>,
    ) -> Option<bool> {
        self.refresh_env(solver, tracer, env);

        self.pending_cubes.clear();
        self.sat_cube = None;
        self.cube_buf.clear();
        self.sat_cube_buf.clear();
        self.unsat_cubes.clear();

        'cubes: for cube in cubes {
            let start = self.cube_buf.len();
            self.in_cube.clear();
            for lit in cube {
                let repr_lit = env.lit_repr(lit);
                let sat_lit = self.add_to_solver(solver, env, repr_lit);

                if sat_lit.is_const() {
                    if sat_lit == Lit::FALSE {
                        self.cube_buf.truncate(start);
                        self.sat_cube_buf.truncate(start);

                        self.cube_buf.push(lit);
                        self.sat_cube_buf.push(sat_lit);

                        self.unsat_cubes.push([start, self.cube_buf.len()]);
                        continue 'cubes;
                    } else {
                        continue;
                    }
                }

                if let Some(&inv_lit) = self.in_cube.get(&!sat_lit) {
                    self.cube_buf.truncate(start);
                    self.sat_cube_buf.truncate(start);

                    self.cube_buf.push(inv_lit);
                    self.cube_buf.push(lit);

                    self.sat_cube_buf.push(!sat_lit);
                    self.sat_cube_buf.push(sat_lit);

                    self.unsat_cubes.push([start, self.cube_buf.len()]);
                    continue 'cubes;
                }

                self.in_cube.entry(sat_lit).or_insert_with(|| {
                    self.cube_buf.push(repr_lit);
                    self.sat_cube_buf.push(sat_lit);
                    lit
                });
            }
            let end = self.cube_buf.len();
            if start == end {
                self.sat_cube = Some([0; 2]);
                self.input_model.clear();
                self.inner_model.clear();
                return Some(true);
            }
            self.pending_cubes.push_back([start, end]);
        }

        solver.new_round();

        for &sat_lit in self.sat_cube_buf.iter() {
            solver.mark_cone(sat_lit.var());
        }

        if self.pending_cubes.is_empty() {
            return Some(false);
        }

        let mut round_limit = 4;

        let mut left_of_round = self.pending_cubes.len();

        solver.produce_inner_model(true);

        loop {
            let Some(current_cube) = self.pending_cubes.pop_front() else {
                break Some(false);
            };

            let conflict_limit = [
                (!self.pending_cubes.is_empty()).then_some(round_limit),
                self.conflicts_left,
            ]
            .into_iter()
            .flatten()
            .min();

            if let Some(conflict_limit) = conflict_limit {
                if conflict_limit == 0 {
                    return None;
                }
                solver.cumulative_conflict_limit(conflict_limit);
            } else {
                solver.clear_limits();
            }

            let [cube_start, cube_end] = current_cube;

            let start_conflicts = solver.conflicts();

            let round_result = solver.solve_assuming(&self.sat_cube_buf[cube_start..cube_end]);

            if let Some(conflicts_left) = &mut self.conflicts_left {
                let end_conflicts = solver.conflicts();
                *conflicts_left =
                    (*conflicts_left).saturating_sub((end_conflicts - start_conflicts) as usize);
            }

            match round_result {
                Some(true) => {
                    self.sat_cube = Some(current_cube);
                    self.input_model.clear();
                    self.inner_model.clear();
                    for &sat_lit in solver.input_model() {
                        let lit = env
                            .var_defs()
                            .lit_repr(sat_lit.lookup(|var| self.env_from_sat[var]));
                        if lit.is_const() {
                            assert!(lit == Lit::TRUE)
                        } else if self.sat_gates[sat_lit.var()].is_some() {
                            self.inner_model.push(lit)
                        } else {
                            self.input_model.push(lit)
                        }
                    }
                }
                Some(false) => {
                    self.failed_assumptions.clear();
                    self.failed_assumptions.extend(solver.failed_assumptions());

                    let mut write = current_cube[0];
                    for read in current_cube[0]..current_cube[1] {
                        if self.failed_assumptions.remove(&!self.sat_cube_buf[read]) {
                            self.sat_cube_buf[write] = self.sat_cube_buf[read];
                            self.cube_buf[write] = self.cube_buf[read];
                            write += 1;
                        }
                    }

                    self.unsat_cubes.push([current_cube[0], write]);
                }
                None => {
                    self.pending_cubes.push_back(current_cube);
                }
            }

            if round_result == Some(true) {
                return round_result;
            }

            left_of_round -= 1;
            if left_of_round == 0 {
                round_limit += round_limit / 2;
                left_of_round = self.pending_cubes.len();
            }
        }
    }

    pub fn input_model(&self) -> &[Lit] {
        assert!(self.sat_cube.is_some());
        &self.input_model
    }

    pub fn inner_model(&self) -> &[Lit] {
        assert!(self.sat_cube.is_some());
        &self.inner_model
    }

    pub fn sat_cube(&self) -> &[Lit] {
        let [cube_start, cube_end] = self.sat_cube.unwrap();
        &self.cube_buf[cube_start..cube_end]
    }

    pub fn unsat_cubes(&self) -> impl Iterator<Item = &[Lit]> {
        self.unsat_cubes
            .iter()
            .map(|&[start, end]| &self.cube_buf[start..end])
    }
}

#[derive(Clone, Debug, Default)]
pub struct CircuitCut {
    pub env_from_cut: IdVec<Var, Lit>,
    pub gates: Vec<CircuitCutGate>,
    pub equivs: Vec<[Lit; 2]>,
    pub units: Vec<Lit>,
    pub failed_cube: Vec<Lit>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct CircuitCutGate {
    pub output: Lit,
    pub gate: XaigStep,
}

impl CircuitCutGate {
    pub fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) {
        self.output = self.output.lookup(&mut var_map);

        let inputs = self.gate.inputs.map(|input| input.lookup(&mut var_map));
        if self.gate.is_and() {
            self.gate = XaigStep::and(inputs.into());
        } else {
            self.gate = XaigStep::xor(inputs.into());
        }
    }

    pub fn is_constraining(&self) -> bool {
        self.output.is_const()
            || self.gate.inputs[0].is_const()
            || self.gate.inputs[1].is_const()
            || self.output.var() == self.gate.inputs[0].var()
            || self.output.var() == self.gate.inputs[1].var()
            || self.gate.inputs[0].var() == self.gate.inputs[1].var()
    }
}

#[derive(Default)]
struct Tracer {
    events: VecDeque<Option<Lit>>,

    gate_elims: HashMap<Var, Lit>,
}

impl Tracer {
    pub fn reset(&mut self) {
        self.events.clear();
        self.gate_elims.clear();
    }
}

impl ProofTracer for Tracer {
    fn learnt_clause(&mut self, clause_lits: &[Lit], _tags: &[u32], _units: &[Lit]) -> u32 {
        if clause_lits.len() == 1 {
            self.events.push_back(Some(clause_lits[0]));
        }
        !0
    }

    fn learnt_unit(&mut self, unit: Lit, _tag: u32, _units: &[Lit]) {
        self.events.push_back(Some(unit));
    }

    fn conflict(&mut self, _conflict_lits: &[Lit], _tags: &[u32], _units: &[Lit]) {}
}
