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

use std::{
    cell::RefCell,
    collections::VecDeque,
    mem::{replace, take},
    ops::Range,
    time::Duration,
};

use imctk_abc::sat::glucose2::ProofTracer;
use imctk_ids::{id_vec::IdVec, indexed_id_vec::IndexedIdVec};
use imctk_ir::{
    env::Env,
    node::fine::{
        circuit::{AndNode, XorNode},
        constraints::BinClause,
    },
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

    depth_limit: Option<usize>,
    bfs_queue: IndexedIdVec<u32, Var>,

    input_model: Vec<Lit>,
    inner_model: Vec<Lit>,
    sat_cube_buf: Vec<Lit>,
    in_cube: HashSet<Lit>,

    equiv_pos: usize,

    solver_query_count: usize,
    query_count: usize,

    stop_on_events: bool,
    tracer: Option<Box<RefCell<Tracer>>>,
    tracer_analysis: TracerAnalysis,
    unsat_cut: CircuitCut,
    sat_from_cut: IndexedIdVec<Var, Var>,

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

            depth_limit: None,
            bfs_queue: Default::default(),

            input_model: vec![],
            inner_model: vec![],
            sat_cube_buf: vec![],
            in_cube: Default::default(),

            equiv_pos: usize::MAX,

            solver_query_count: 0,
            query_count: 0,

            stop_on_events: false,
            tracer: Some(Default::default()),
            tracer_analysis: Default::default(),
            unsat_cut: Default::default(),
            sat_from_cut: Default::default(),

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

                // TODO make this configurable
                self.add_redundant_overlapping_nodes_to_solver(solver, env, var);
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

                // TODO make this configurable
                self.add_redundant_overlapping_nodes_to_solver(solver, env, var);
                return sat_lit ^ output_pol;
            }
        }

        let sat_lit = self.env_from_sat.push(var.as_lit()).0.as_lit();
        self.sat_gates.push(None);
        self.sat_from_env[var] = Some(sat_lit);
        sat_lit ^ output_pol
    }

    fn add_redundant_overlapping_nodes_to_solver(
        &mut self,
        solver: &mut InnerSolver,
        env: &Env,
        var: Var,
    ) {
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

                let [a, b] = inputs.map(|input_lit| self.add_to_solver(solver, env, input_lit));
                let y = and.output.lookup(|var| self.sat_from_env[var].unwrap());

                let tag = self
                    .redundant_gates
                    .push(CircuitCutGate {
                        output: y,
                        gate: XaigStep::and([a, b].into()),
                    })
                    .0
                    ^ (!0 >> 1);

                assert_eq!(tag & (0b1 << 31), 0);

                solver.add_tagged_clause(&[!a, !b, y], tag);
                solver.add_tagged_clause(&[a, !y], tag);
                solver.add_tagged_clause(&[b, !y], tag);
            } else if let Some(xor) = node.dyn_cast::<XorNode>() {
                let inputs = xor.term.inputs.into_values();
                if !matches!(self.sat_from_env.get(xor.output.var()), Some(Some(_))) {
                    continue 'outer;
                }
                for input in inputs.into_iter() {
                    if !matches!(self.sat_from_env.get(input), Some(Some(_))) {
                        continue 'outer;
                    }
                }

                let [a, b] =
                    inputs.map(|input_var| self.add_to_solver(solver, env, input_var.as_lit()));
                let y = xor.output.lookup(|var| self.sat_from_env[var].unwrap());

                let tag = self
                    .redundant_gates
                    .push(CircuitCutGate {
                        output: y,
                        gate: XaigStep::xor([a, b].into()),
                    })
                    .0
                    ^ (!0 >> 1);

                assert_eq!(tag & (0b1 << 31), 0);

                solver.add_tagged_clause(&[!y, a, b], tag);
                solver.add_tagged_clause(&[y, !a, b], tag);
                solver.add_tagged_clause(&[y, a, !b], tag);
                solver.add_tagged_clause(&[!y, !a, !b], tag);
            } else if let Some(bin_clause) = node.dyn_cast::<BinClause>() {
                let inputs = bin_clause.inputs.into_values();
                for input in inputs.into_iter() {
                    if !matches!(self.sat_from_env.get(input.var()), Some(Some(_))) {
                        continue 'outer;
                    }
                }
                let [a, b] = inputs.map(|input_lit| self.add_to_solver(solver, env, input_lit));

                let tag = self
                    .redundant_gates
                    .push(CircuitCutGate {
                        output: Lit::FALSE,
                        gate: XaigStep::and([!a, !b].into()),
                    })
                    .0
                    ^ (!0 >> 1);

                assert_eq!(tag & (0b1 << 31), 0);

                solver.add_tagged_clause(&[a, b], tag);
            }
        }
    }

    pub fn reset(&mut self) {
        self.sat_from_env.clear();
        self.env_from_sat.clear();
        self.sat_gates.clear();
        self.redundant_gates.clear();
        self.sat_from_env.push(Some(Lit::FALSE));
        self.env_from_sat.push(Lit::FALSE);
        self.sat_gates.push(None);
        self.tracer_analysis = Default::default();
        self.solver_query_count = 0;
        self.equiv_pos = usize::MAX;

        if let Some(solver) = &mut self.solver {
            solver.reset();
        }

        self.tracer.as_mut().unwrap().get_mut().reset();
    }

    pub fn set_enable_proof_recording(&mut self, enable: bool) {
        if replace(
            &mut self.tracer.as_mut().unwrap().get_mut().full_recording,
            enable,
        ) != enable
        {
            self.reset();
        }
    }

    // TODO the stop on event approach has some issues, a callback API would be preferred
    pub fn set_stop_on_event(&mut self, stop: bool) {
        self.stop_on_events = stop;
    }

    pub fn set_depth_limit(&mut self, limit: Option<usize>) {
        self.depth_limit = limit;
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
            tracer.conflict_events.clear();
        }

        let mut solver = take(&mut self.solver).unwrap_or_default();

        let mut tracer_ref = &*tracer;
        solver = solver.with_proof_tracer(&mut tracer_ref);

        let result = self.query_inner(&mut solver, &tracer, env, cubes);

        {
            let mut tracer = tracer.borrow_mut();
            if !tracer.full_recording {
                tracer.events.clear();
            }
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
                        continue 'cubes;
                    } else {
                        continue;
                    }
                }

                if self.in_cube.contains(&!sat_lit) {
                    self.cube_buf.truncate(start);
                    self.sat_cube_buf.truncate(start);
                    continue 'cubes;
                }

                if self.in_cube.insert(sat_lit) {
                    self.cube_buf.push(repr_lit);
                    self.sat_cube_buf.push(sat_lit);
                }
            }
            let end = self.cube_buf.len();
            self.pending_cubes.push_back([start, end]);
        }

        solver.new_round();

        if let Some(depth_limit) = self.depth_limit {
            self.bfs_queue.clear();

            for &sat_lit in self.sat_cube_buf.iter() {
                self.bfs_queue.insert(sat_lit.var());
            }

            let mut pos = 0;
            let mut next_depth = self.bfs_queue.len();
            let mut depth = 0;
            while let Some(&var) = self.bfs_queue.get(pos as u32) {
                if pos == next_depth {
                    depth += 1;
                    if depth == depth_limit {
                        break;
                    }
                    next_depth = self.bfs_queue.len();
                }
                pos += 1;
                if let Some(equiv) = tracer.borrow_mut().gate_elims.get(&var) {
                    self.bfs_queue.insert(equiv.var());
                    self.bfs_queue.insert(Var::FALSE);
                }
                if let Some(gate) = self.sat_gates[var] {
                    for sat_lit in gate.inputs {
                        self.bfs_queue.insert(sat_lit.var());
                    }
                }
            }

            for &sat_lit in self.bfs_queue.values() {
                solver.mark_var(sat_lit)
            }
        } else {
            for &sat_lit in self.sat_cube_buf.iter() {
                solver.mark_cone(sat_lit.var());
            }
        }

        if self.pending_cubes.is_empty() {
            return Some(false);
        }

        let mut round_limit = 4;

        let mut left_of_round = self.pending_cubes.len();

        solver.produce_inner_model(true);

        let mut event_pos = 0;

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

            let mut new_units = false;
            {
                let tracer = tracer.borrow_mut();
                while let Some(&event) = tracer.events.get(event_pos) {
                    event_pos += 1;
                    if let Some(unit) = event {
                        if self.stop_on_events {
                            self.solver_query_count -= 1;
                            return None;
                        }
                        env.equiv([unit.lookup(|var| self.env_from_sat[var]), Lit::TRUE]);
                        new_units = true;
                    }
                }
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

            if new_units {
                self.refresh_env(solver, tracer, env);
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

    pub fn next_event(&mut self, env: &mut Env) -> Option<(bool, &CircuitCut)> {
        self.process_event(env, false)
    }

    pub fn last_event(&mut self, env: &mut Env) -> Option<(bool, &CircuitCut)> {
        self.process_event(env, true)
    }

    fn process_event(&mut self, env: &mut Env, last: bool) -> Option<(bool, &CircuitCut)> {
        let tracer = self.tracer.get_or_insert_with(Default::default);
        let mut tracer = tracer.borrow_mut();
        let tracer = &mut *tracer;

        let failed_unit;
        let conflict_reason;

        let (was_query, failed_cube, reason) = match if last {
            tracer.events.pop_back()
        } else {
            tracer.events.pop_front()
        }? {
            Some(unit) => {
                failed_unit = !unit;
                (
                    false,
                    std::slice::from_ref(&failed_unit),
                    tracer.unit_reasons.get(&unit).unwrap(),
                )
            }
            None => {
                conflict_reason = (if last {
                    tracer.conflict_events.pop_back()
                } else {
                    tracer.conflict_events.pop_front()
                })
                .unwrap();

                (
                    true,
                    conflict_reason.failed_assumptions(tracer),
                    &conflict_reason.reason,
                )
            }
        };

        self.tracer_analysis.analyze_conflict(tracer, reason);

        self.sat_from_cut.clear();
        self.sat_from_cut.insert(Var::FALSE);

        self.unsat_cut.env_from_cut.clear();
        self.unsat_cut.gates.clear();
        self.unsat_cut.equivs.clear();
        self.unsat_cut.units.clear();
        self.unsat_cut.failed_cube.clear();

        let mut var_map = |var| self.sat_from_cut.insert(var).0.as_lit();

        for &gate_output in self.tracer_analysis.gates_seen.values() {
            let mut gate = CircuitCutGate {
                output: gate_output.as_lit(),
                gate: self.sat_gates[gate_output].unwrap(),
            };
            gate.apply_var_map(&mut var_map);
            self.unsat_cut.gates.push(gate);
        }

        assert!(self.redundant_gates.len() + tracer.tag_reasons.len() <= (1 << 31));
        for &tag in self.tracer_analysis.unknown_tags.iter() {
            let tag = tag ^ (!0 >> 1);
            let mut gate = self.redundant_gates[tag];
            gate.apply_var_map(&mut var_map);
            self.unsat_cut.gates.push(gate);
        }

        for &equiv_var in self.tracer_analysis.equivs_seen.values() {
            let equiv_lit = equiv_var.as_lit().lookup(&mut var_map);
            let repr_lit = tracer
                .gate_elims
                .get(&equiv_var)
                .unwrap()
                .lookup(&mut var_map);

            self.unsat_cut.equivs.push([equiv_lit, repr_lit]);
        }

        for &unit in self.tracer_analysis.units_seen.values() {
            self.unsat_cut.units.push(unit.lookup(&mut var_map));
        }

        for &lit in failed_cube {
            self.unsat_cut.failed_cube.push(lit.lookup(&mut var_map));
        }

        for &var in self.sat_from_cut.values() {
            self.unsat_cut
                .env_from_cut
                .push(env.lit_repr(self.env_from_sat[var]));
        }

        Some((was_query, &self.unsat_cut))
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

#[derive(Debug)]
struct Reason {
    tags_range: Range<usize>,
    units_range: Range<usize>,
}

impl Reason {
    pub fn tags<'a>(&self, tracer: &'a Tracer) -> &'a [u32] {
        &tracer.tag_buf[self.tags_range.clone()]
    }
    pub fn units<'a>(&self, tracer: &'a Tracer) -> &'a [Lit] {
        &tracer.lit_buf[self.units_range.clone()]
    }
}

struct ConflictReason {
    failed_cube_range: Range<usize>,
    reason: Reason,
}

impl ConflictReason {
    pub fn failed_assumptions<'a>(&self, tracer: &'a Tracer) -> &'a [Lit] {
        &tracer.lit_buf[self.failed_cube_range.clone()]
    }
}

#[derive(Default)]
struct Tracer {
    full_recording: bool,

    tag_reasons: IdVec<u32, Reason>,
    unit_reasons: HashMap<Lit, Reason>,

    events: VecDeque<Option<Lit>>,
    conflict_events: VecDeque<ConflictReason>,

    gate_elims: HashMap<Var, Lit>,

    tag_buf: Vec<u32>,
    lit_buf: Vec<Lit>,
}

impl Tracer {
    pub fn reset(&mut self) {
        self.tag_reasons.clear();
        self.unit_reasons.clear();
        self.events.clear();
        self.conflict_events.clear();
        self.gate_elims.clear();
        self.tag_buf.clear();
        self.lit_buf.clear();
    }

    pub fn store_tags(&mut self, tags: &[u32]) -> Range<usize> {
        let start = self.tag_buf.len();

        self.tag_buf.extend(tags);

        for tag in self.tag_buf[start..].iter_mut() {
            if *tag & (1 << 31) != 0 {
                let index = *tag & !(1 << 31);
                if self
                    .gate_elims
                    .contains_key(&Var::from_index(index as usize))
                {
                    *tag |= 1 << 30;
                }
            }
        }
        start..self.tag_buf.len()
    }

    pub fn store_lits(&mut self, units: &[Lit]) -> Range<usize> {
        let start = self.lit_buf.len();
        self.lit_buf.extend_from_slice(units);
        start..self.lit_buf.len()
    }
}

impl ProofTracer for Tracer {
    fn learnt_clause(&mut self, clause_lits: &[Lit], tags: &[u32], units: &[Lit]) -> u32 {
        if !self.full_recording {
            if clause_lits.len() == 1 {
                self.events.push_back(Some(clause_lits[0]));
            }
            return !0;
        }

        assert!(!tags.contains(&!0), "{tags:08x?}");

        let tags_range = self.store_tags(tags);
        let units_range = self.store_lits(units);

        let tag = self
            .tag_reasons
            .push(Reason {
                tags_range,
                units_range,
            })
            .0;

        if clause_lits.len() == 1 {
            let tags_range = self.store_tags(&[tag]);
            let units_range = self.store_lits(&[]);

            self.events.push_back(Some(clause_lits[0]));

            self.unit_reasons.insert(
                clause_lits[0],
                Reason {
                    tags_range,
                    units_range,
                },
            );
        }
        tag
    }

    fn learnt_unit(&mut self, unit: Lit, tag: u32, units: &[Lit]) {
        self.events.push_back(Some(unit));
        if !self.full_recording {
            return;
        }
        assert_ne!(tag, !0, "{tag:08x?}");

        let tags_range = self.store_tags(&[tag]);
        let units_range = self.store_lits(units);

        self.unit_reasons.insert(
            unit,
            Reason {
                tags_range,
                units_range,
            },
        );
    }

    fn conflict(&mut self, conflict_lits: &[Lit], tags: &[u32], units: &[Lit]) {
        if !self.full_recording {
            return;
        }
        assert!(!tags.contains(&!0), "{tags:08x?}");

        let failed_cube_range = self.store_lits(conflict_lits);

        for lit in self.lit_buf[failed_cube_range.clone()].iter_mut() {
            *lit ^= true;
        }

        let tags_range = self.store_tags(tags);
        let units_range = self.store_lits(units);

        self.events.push_back(None);

        self.conflict_events.push_back(ConflictReason {
            failed_cube_range,
            reason: Reason {
                tags_range,
                units_range,
            },
        })
    }
}

#[derive(Default)]
struct TracerAnalysis {
    tags_seen: IndexedIdVec<u32, u32>,
    units_seen: IndexedIdVec<u32, Lit>,
    gates_seen: IndexedIdVec<u32, Var>,
    equivs_seen: IndexedIdVec<u32, Var>,

    cut_defs: IndexedIdVec<u32, Var>,
    cut_uses: IndexedIdVec<u32, Var>,
    cut_inputs: IndexedIdVec<u32, Var>,

    cut_gates: Vec<CircuitCutGate>,
    cut_constraints: Vec<CircuitCutGate>,

    unknown_tags: Vec<u32>,
}

impl TracerAnalysis {
    fn reset(&mut self) {
        self.tags_seen.clear();
        self.units_seen.clear();
        self.gates_seen.clear();
        self.equivs_seen.clear();
        self.cut_inputs.clear();
        self.cut_defs.clear();
        self.cut_uses.clear();
        self.cut_gates.clear();
        self.cut_constraints.clear();
        self.unknown_tags.clear();
    }

    pub fn analyze_conflict(&mut self, tracer: &Tracer, reason: &Reason) {
        self.reset();
        self.analyze_reason(tracer, reason);
    }

    fn analyze_reason(&mut self, tracer: &Tracer, reason: &Reason) {
        for &unit in reason.units(tracer).iter() {
            let (_, _, inserted) = self.units_seen.insert(unit);
            if inserted {
                // TODO should be a setting
                if let Some(unit_reason) = tracer.unit_reasons.get(&unit) {
                    self.analyze_reason(tracer, unit_reason);
                };
            }
        }
        for &tag in reason.tags(tracer).iter() {
            if tag == !0 {
                log::error!("tags: {:08x?}", reason.tags(tracer));
                log::error!("units: {:?}", reason.units(tracer));
                panic!("found untagged reason clause");
            } else if tag & (1 << 31) == 0 {
                let (_, _, inserted) = self.tags_seen.insert(tag);

                if inserted {
                    if let Some(reason) = tracer.tag_reasons.get(tag) {
                        self.analyze_reason(tracer, reason);
                    } else {
                        self.unknown_tags.push(tag);
                    }
                }
            } else if tag & (1 << 30) == 0 {
                self.gates_seen
                    .insert(Var::from_index((tag & !(1 << 31)) as usize));
            } else {
                self.equivs_seen
                    .insert(Var::from_index((tag & !(0b11 << 30)) as usize));
            }
        }
    }
}
