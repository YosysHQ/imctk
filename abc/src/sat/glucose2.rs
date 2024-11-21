//! Bindings to abc's glucuse2 SAT solver

use std::{
    cell::RefCell,
    collections::HashSet,
    ffi::c_int,
    marker::PhantomData,
    mem::{take, ManuallyDrop},
    os::raw::c_void,
};

use imctk_abc_sys as abc;
use imctk_ir::var::{Lit, Var};
use imctk_util::unordered_pair::UnorderedPair;
use sealed::{CircuitMode, SolverMode};

/// Wrapper for `std::slice::from_raw_parts` that allows a null pointer when the length is zero.
///
/// # Safety
/// For a non-zero length the caller is responsible to uphold the safety requirements of
/// `std::slice::from_raw_parts`
unsafe fn from_raw_parts_nullptr<'a, T>(ptr: *const T, len: usize) -> &'a [T] {
    if len == 0 {
        &[]
    } else {
        // SAFETY: we checked for a non-zero length and documented the requirements for this case
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

/// CNF only propagation and no justification
pub struct CnfOnly;

/// CNF only propagation and circuit based justification
pub struct CircuitJust;

/// CNF and circuit propagation with circuit based justification
pub struct CircuitProp;

mod sealed {
    use super::*;

    pub trait SolverMode {
        const JFTR: c_int;
    }

    pub trait CircuitMode: SolverMode {}
}

impl sealed::SolverMode for CnfOnly {
    const JFTR: c_int = 0;
}

impl sealed::SolverMode for CircuitJust {
    const JFTR: c_int = 1;
}
impl sealed::CircuitMode for CircuitJust {}

impl sealed::SolverMode for CircuitProp {
    const JFTR: c_int = 2;
}

impl sealed::CircuitMode for CircuitProp {}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum State {
    Setup,
    Sat,
    Unsat,
    Unknown,
}

/// An instance of abc's glucose2 SAT solver
pub struct Solver<'a, Mode: SolverMode> {
    ptr: *mut c_void,
    state: State,
    dupcheck: HashSet<Var>,
    _phantom: PhantomData<(Mode, &'a mut ())>,
}

impl<Mode: SolverMode> Default for Solver<'static, Mode> {
    fn default() -> Self {
        // SAFETY: safe raw FFI calls
        unsafe {
            let ptr = abc::imctk_abc_glucose2_init();
            abc::imctk_abc_glucose2_set_incremental_mode(ptr);
            abc::imctk_abc_glucose2_set_jftr(ptr, Mode::JFTR);

            assert_eq!(size_of::<c_int>(), size_of::<Lit>());
            let mut new = Self {
                ptr,
                state: State::Setup,
                dupcheck: Default::default(),
                _phantom: PhantomData,
            };
            new.add_tagged_clause(&[Lit::TRUE], 1 << 31);
            new
        }
    }
}

impl<Mode: SolverMode> Drop for Solver<'_, Mode> {
    fn drop(&mut self) {
        // SAFETY: all references into the solver available to safe rust code share our lifetime
        unsafe {
            abc::imctk_abc_glucose2_release(self.ptr);
        }
    }
}

impl<Mode: SolverMode> Solver<'_, Mode> {
    /// Reset the solver back to the initial state
    ///
    /// Equivalent to creating a new solver, but avoids newly allocating solver resources.
    pub fn reset(&mut self) {
        self.state = State::Setup;
        // SAFETY: safe raw FFI calls
        unsafe {
            abc::imctk_abc_glucose2_reset(self.ptr);
            abc::imctk_abc_glucose2_set_incremental_mode(self.ptr);
            abc::imctk_abc_glucose2_set_jftr(self.ptr, Mode::JFTR);
        }
        self.add_tagged_clause(&[Lit::TRUE], 1 << 31);
    }

    fn ensure_var(&mut self, var: Var) {
        self.state = State::Setup;
        assert!(var.index() < (c_int::MAX / 2) as usize);

        // SAFETY: safe raw FFI call
        let nvars = unsafe { abc::imctk_abc_glucose2_nvars(self.ptr) } as usize;

        for _ in nvars..var.index() + 1 {
            // SAFETY: safe raw FFI call
            unsafe { abc::imctk_abc_glucose2_new_var(self.ptr) };
        }
    }

    /// Adds a CNF clause to the solver.
    pub fn add_clause(&mut self, lits: &[Lit]) {
        self.state = State::Setup;
        if let Some(max_var) = lits.iter().map(|&lit| lit.var()).max() {
            self.ensure_var(max_var);
        }
        // SAFETY: we called ensure_var to ensure all vars are in bounds
        unsafe { self.add_clause_unchecked(lits) }
    }

    /// Adds a CNF clause to the solver without checking whether the variables are in bounds.
    ///
    /// # Safety
    /// The caller needs to ensure that all variables are in bounds. E.g. by calling
    /// `Self::ensure_var`.
    unsafe fn add_clause_unchecked(&mut self, lits: &[Lit]) {
        // SAFETY: safe FFI call, if variables are in bounds, which is a documented requirement
        unsafe {
            abc::imctk_abc_glucose2_add_clause(
                self.ptr,
                lits.as_ptr().cast(),
                lits.len().try_into().unwrap(),
            );
        }
    }

    /// Adds a CNF clause with a given proof tracing tag.
    pub fn add_tagged_clause(&mut self, lits: &[Lit], tag: u32) {
        self.state = State::Setup;
        // SAFETY: safe FFI call
        let saved_tag = unsafe { abc::imctk_abc_glucose2_proof_trace_default_tag(self.ptr) };
        // SAFETY: safe FFI call
        unsafe {
            abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, tag);
        }
        self.add_clause(lits);
        // SAFETY: safe FFI call
        unsafe {
            abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, saved_tag);
        }
    }

    /// When using circuit based justification, include inner circuit nodes in the produced model.
    pub fn produce_inner_model(&mut self, produce_inner: bool) {
        // SAFETY: safe FFI call
        unsafe { abc::imctk_abc_glucose2_produce_inner_model(self.ptr, produce_inner as i32) };
    }

    /// Adds an AND gate to the solver.
    pub fn add_and(&mut self, output: Var, inputs: UnorderedPair<Lit>) {
        self.state = State::Setup;
        let (y, [a, b]) = (output.as_lit(), inputs.into_values());
        assert_ne!(a, b);

        if Mode::JFTR == 2 {
            self.ensure_var(output.max(a.var().max(b.var())));
        } else {
            let gate_tag = (output.index() as u32) | (1 << 31);
            // SAFETY: safe FFI call
            let saved_tag = unsafe { abc::imctk_abc_glucose2_proof_trace_default_tag(self.ptr) };
            // SAFETY: safe FFI call
            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, gate_tag);
            }

            self.add_clause(&[y, !a, !b]);
            // SAFETY: the above add_clause ensures all used vars are in bounds
            unsafe {
                self.add_clause_unchecked(&[!y, a]);
                self.add_clause_unchecked(&[!y, b]);
            }

            // SAFETY: safe FFI call
            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, saved_tag);
            }
        }

        if Mode::JFTR != 0 {
            // SAFETY: either ensure_var or add_clause above will ensure vars are in bounds
            unsafe {
                abc::imctk_abc_glucose2_set_var_fanin_lit(
                    self.ptr,
                    y.index() as c_int,
                    a.code() as c_int,
                    b.code() as c_int,
                );
            }
        }
    }

    /// Adds a XOR gate to the solver.
    pub fn add_xor(&mut self, output: Var, inputs: UnorderedPair<Lit>) {
        self.state = State::Setup;
        let (y, [a, b]) = (output.as_lit(), inputs.into_values());
        assert_ne!(a, b);

        if Mode::JFTR == 2 {
            self.ensure_var(output.max(a.var().max(b.var())));
        } else {
            let gate_tag = (output.index() as u32) | (1 << 31);
            // SAFETY: safe FFI call
            let saved_tag = unsafe { abc::imctk_abc_glucose2_proof_trace_default_tag(self.ptr) };
            // SAFETY: safe FFI call
            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, gate_tag);
            }

            self.add_clause(&[!y, a, b]);
            // SAFETY: the above add_clause ensures all used vars are in bounds
            unsafe {
                self.add_clause_unchecked(&[y, !a, b]);
                self.add_clause_unchecked(&[y, a, !b]);
                self.add_clause_unchecked(&[!y, !a, !b]);
            }

            // SAFETY: safe FFI call
            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, saved_tag);
            }
        }

        if Mode::JFTR != 0 {
            // SAFETY: either ensure_var or add_clause above will ensure vars are in bounds
            unsafe {
                abc::imctk_abc_glucose2_set_var_fanin_lit(
                    self.ptr,
                    y.index() as c_int,
                    b.code() as c_int,
                    a.code() as c_int,
                );
            }
        }
    }

    /// Reset the propagation window for circuit based justification.
    pub fn new_round(&mut self) {
        self.state = State::Setup;
        if Mode::JFTR == 0 {
            return;
        }

        // SAFETY: safe FFI call
        unsafe { abc::imctk_abc_glucose2_start_new_round(self.ptr) }
    }

    /// Extend the circuit based justification propagation window by the input cone of a variable.
    ///
    /// The inputs are only traversed up to any node that is already part of the propagation window.
    /// Hence, when [`Self::mark_var`] was called before, this might not add the full transitive
    /// input cone to the propagation window.
    pub fn mark_cone(&mut self, var: Var) {
        self.state = State::Setup;
        if Mode::JFTR == 0 {
            return;
        }

        self.ensure_var(var);

        // SAFETY: we called ensure_var to make the var in bounds
        unsafe { abc::imctk_abc_glucose2_mark_cone(self.ptr, var.index() as c_int) }
    }

    /// Extend the circuit based justification propagation window by a single variable.
    pub fn mark_var(&mut self, var: Var) {
        self.state = State::Setup;
        if Mode::JFTR == 0 {
            return;
        }

        self.ensure_var(var);

        // SAFETY: we called ensure_var to make the var in bounds
        unsafe { abc::imctk_abc_glucose2_mark_var(self.ptr, var.index() as c_int) }
    }

    /// Find a solution justifying the given assumptions.
    ///
    /// Returns `Some(true)` when a solution was fond, `Some(false)` when no solution exists and
    /// `None` when reaching any configured limit.
    pub fn solve_assuming(&mut self, assumptions: &[Lit]) -> Option<bool> {
        // abc's glucose2 can crash when there are duplicate vars in the assumptions
        self.dupcheck.clear();
        for &lit in assumptions.iter() {
            assert!(
                self.dupcheck.insert(lit.var()),
                "duplicate variable in assumptions"
            );
        }
        if let Some(max_var) = assumptions.iter().map(|&lit| lit.var()).max() {
            self.ensure_var(max_var);
        }
        // SAFETY: we called ensure_var to make the used vars in bounds
        unsafe {
            match abc::imctk_abc_glucose2_solve_limited(
                self.ptr,
                assumptions.as_ptr().cast(),
                assumptions.len().try_into().unwrap(),
                0,
                0,
            ) {
                -1 => {
                    self.state = State::Unsat;
                    Some(false)
                }
                1 => {
                    self.state = State::Sat;
                    Some(true)
                }
                _ => {
                    self.state = State::Unknown;
                    None
                }
            }
        }
    }

    /// Returns a subset of assumptions that made the last solve unsatisfiable.
    ///
    /// Returns an emtpy slice when the last call was not unsatisfiable.
    pub fn failed_assumptions(&mut self) -> &[Lit] {
        // SAFETY: safe FFI calls, from_raw_parts_nullptr allows the null pointer returned when not
        // unsat
        unsafe {
            let nlits = abc::imctk_abc_glucose2_conflict_size(self.ptr);
            let lits = abc::imctk_abc_glucose2_conflict_lits(self.ptr);

            from_raw_parts_nullptr(lits.cast::<Lit>(), nlits as usize)
        }
    }

    /// Sets a conflict limit for future solves.
    ///
    /// This limit remains in place for multiple solves, with each solve reducing the
    /// limit by the number of conflicts that occured.
    pub fn cumulative_conflict_limit(&mut self, limit: usize) {
        self.state = State::Setup;
        // SAFETY: safe FFI call
        unsafe { abc::imctk_abc_glucose2_set_conf_budget(self.ptr, i64::try_from(limit).unwrap()) }
    }

    /// Clears all configured limits.
    pub fn clear_limits(&mut self) {
        self.state = State::Setup;
        // SAFETY: safe FFI call
        unsafe { abc::imctk_abc_glucose2_budget_off(self.ptr) }
    }

    /// Returns the input model justifying the assumptions when in circuit based justification mode.
    ///
    /// When the last solve wasn't satisfiable or when not in circuit based justification mode, this
    /// will return `None`.
    ///
    /// When `Self::produce_inner_model` was used to request an inner model in addition to the
    /// inputs, this will contain both the inputs and inner nodes needed to justify the assumptions.
    pub fn try_input_model(&mut self) -> Option<&[Lit]> {
        if Mode::JFTR == 0 || self.state != State::Sat {
            return None;
        }
        // SAFETY: safe when the last solve was satisfying, which we checked above
        unsafe {
            let cex = abc::imctk_abc_glucose2_get_cex(self.ptr);
            let len = *cex as usize;

            Some(std::slice::from_raw_parts(cex.add(1).cast::<Lit>(), len))
        }
    }

    /// Returns the number of conflicts that occured in total.
    pub fn conflicts(&mut self) -> u64 {
        // SAFETY: safe FFI call
        unsafe { abc::imctk_abc_glucose2_conflicts(self.ptr) as u64 }
    }

    /// Returns the value of the given literal.
    /// 
    /// Panics if the last solve wasn't satisfiable.
    pub fn value(&self, lit: Lit) -> Option<bool> {
        assert!(self.state == State::Sat);
        // SAFETY: safe FFI call
        let nvars = unsafe { abc::imctk_abc_glucose2_nvars(self.ptr) } as usize;
        if lit.index() >= nvars {
            return None;
        }
        // SAFETY: safe FFI call
        let result = unsafe { abc::imctk_abc_glucose2_model_value(self.ptr, lit.var().index() as i32) };
        match result {
            0 => None,
            1 => Some(true ^ lit.pol()),
            -1 => Some(false ^ lit.pol()),
            _ => unreachable!(),
        }
    }
}

/// Trait to receive proof-tracing callbacks while solving.
pub trait ProofTracer {
    /// Callback invoked when learning a clause.
    fn learnt_clause(&mut self, clause_lits: &[Lit], tags: &[u32], units: &[Lit]) -> u32;

    /// Callback invoked when learning a unit clause.
    fn learnt_unit(&mut self, unit: Lit, tag: u32, units: &[Lit]);

    /// Callback invoked when the assumptions are detected to be in conflict.
    fn conflict(&mut self, conflict_lits: &[Lit], tags: &[u32], units: &[Lit]);
}

impl<T: ProofTracer> ProofTracer for &'_ RefCell<T> {
    fn learnt_clause(&mut self, clause_lits: &[Lit], tags: &[u32], units: &[Lit]) -> u32 {
        self.borrow_mut().learnt_clause(clause_lits, tags, units)
    }

    fn learnt_unit(&mut self, unit: Lit, tag: u32, units: &[Lit]) {
        self.borrow_mut().learnt_unit(unit, tag, units)
    }

    fn conflict(&mut self, conflict_lits: &[Lit], tags: &[u32], units: &[Lit]) {
        self.borrow_mut().conflict(conflict_lits, tags, units)
    }
}

struct AbortOnUnwind;

impl Drop for AbortOnUnwind {
    fn drop(&mut self) {
        panic!();
    }
}

impl<Mode: SolverMode> Solver<'_, Mode> {
    /// Detach any currently attached proof tracing callback.
    pub fn stop_proof_tracing(mut self) -> Solver<'static, Mode> {
        // SAFETY: safe FFI call
        unsafe { abc::imctk_abc_glucose2_stop_proof_trace(self.ptr) }

        let new = Solver {
            ptr: self.ptr,
            state: self.state,
            dupcheck: take(&mut self.dupcheck),
            _phantom: PhantomData,
        };

        let _ = ManuallyDrop::new(self);

        new
    }

    /// Attach a proof tracing callback.
    pub fn with_proof_tracer<T: ProofTracer>(mut self, tracer: &mut T) -> Solver<'_, Mode> {
        unsafe extern "C" fn learnt_clause_wrapper<T: ProofTracer>(
            tracer: *mut c_void,
            lits: *const c_int,
            nlits: c_int,
            tags: *const u32,
            ntags: c_int,
            units: *const c_int,
            nunits: c_int,
        ) -> u32 {
            // SAFETY: from_raw_parts_nullptr handles the potential null pointers for empty slices.
            // Using AbortOnUnwind ensures no panic will cross the FFI boundary
            unsafe {
                let abort_on_unwind = AbortOnUnwind;
                let tracer = &mut *tracer.cast::<T>();
                let lits = from_raw_parts_nullptr(lits.cast::<Lit>(), nlits as usize);
                let tags = from_raw_parts_nullptr(tags, ntags as usize);
                let units = from_raw_parts_nullptr(units.cast::<Lit>(), nunits as usize);
                let tag = tracer.learnt_clause(lits, tags, units);
                let _ = ManuallyDrop::new(abort_on_unwind);
                tag
            }
        }
        unsafe extern "C" fn learnt_unit_wrapper<T: ProofTracer>(
            tracer: *mut c_void,
            lit: c_int,
            tag: u32,
            units: *const c_int,
            nunits: c_int,
        ) {
            // SAFETY: from_raw_parts_nullptr handles the potential null pointers for empty slices.
            // Using AbortOnUnwind ensures no panic will cross the FFI boundary
            unsafe {
                let abort_on_unwind = AbortOnUnwind;
                let tracer = &mut *tracer.cast::<T>();
                let lit = Lit::from_code(lit as usize);
                let units = from_raw_parts_nullptr(units.cast::<Lit>(), nunits as usize);
                tracer.learnt_unit(lit, tag, units);
                let _ = ManuallyDrop::new(abort_on_unwind);
            }
        }
        unsafe extern "C" fn conflict_wrapper<T: ProofTracer>(
            tracer: *mut c_void,
            lits: *const c_int,
            nlits: c_int,
            tags: *const u32,
            ntags: c_int,
            units: *const c_int,
            nunits: c_int,
        ) {
            // SAFETY: from_raw_parts_nullptr handles the potential null pointers for empty slices.
            // Using AbortOnUnwind ensures no panic will cross the FFI boundary
            unsafe {
                let abort_on_unwind = AbortOnUnwind;
                let tracer = &mut *tracer.cast::<T>();
                let lits = from_raw_parts_nullptr(lits.cast::<Lit>(), nlits as usize);
                let tags = from_raw_parts_nullptr(tags, ntags as usize);
                let units = from_raw_parts_nullptr(units.cast::<Lit>(), nunits as usize);
                tracer.conflict(lits, tags, units);
                let _ = ManuallyDrop::new(abort_on_unwind);
            }
        }

        // SAFETY: the returned solver has the callback's lifetime, ensuring the callback can't be
        // invoked past its own lifetime
        unsafe {
            abc::imctk_abc_glucose2_start_proof_trace(
                self.ptr,
                (tracer as *mut T).cast(),
                Some(learnt_clause_wrapper::<T>),
                Some(learnt_unit_wrapper::<T>),
                Some(conflict_wrapper::<T>),
            );
        }

        let new = Solver {
            ptr: self.ptr,
            state: self.state,
            dupcheck: take(&mut self.dupcheck),
            _phantom: PhantomData,
        };

        let _ = ManuallyDrop::new(self);

        new
    }
}

impl<Mode: CircuitMode> Solver<'_, Mode> {
    /// Returns the input model justifying the assumptions.
    ///
    /// This panics when the last solve wasn't satisfiable.
    pub fn input_model(&mut self) -> &[Lit] {
        assert_eq!(self.state, State::Sat);

        self.try_input_model().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use imctk_ids::id_vec::IdVec;

    use super::*;

    #[test]
    pub fn test_cnf() {
        let mut solver: Solver<CnfOnly> = Default::default();

        let mut vars: IdVec<Var, ()> = Default::default();
        vars.push(());

        let v1 = vars.push(()).0.as_lit();
        let v2 = vars.push(()).0.as_lit();
        let v3 = vars.push(()).0.as_lit();

        solver.add_clause(&[v1, v2, v3]);
        solver.add_clause(&[v1, v2, !v3]);
        solver.add_clause(&[v1, !v2]);

        assert_eq!(solver.solve_assuming(&[!v1]), Some(false));
        assert_eq!(solver.solve_assuming(&[v1]), Some(true));
        assert_eq!(solver.solve_assuming(&[]), Some(true));
    }

    pub fn test_gates<T: SolverMode>() {
        let mut solver: Solver<T> = Default::default();

        let mut vars: IdVec<Var, ()> = Default::default();
        vars.push(());

        let v0 = vars.push(()).0.as_lit();
        let v1 = vars.push(()).0.as_lit();
        let v2 = vars.push(()).0.as_lit();
        let and_01 = vars.push(()).0.as_lit();
        let and_12 = vars.push(()).0.as_lit();
        let and_01_2 = vars.push(()).0.as_lit();
        let and_0_12 = vars.push(()).0.as_lit();
        let xor_12 = vars.push(()).0.as_lit();
        let combined = vars.push(()).0.as_lit();
        let delta_1 = vars.push(()).0.as_lit();
        let delta_2 = vars.push(()).0.as_lit();

        solver.add_and(and_01.var(), [v0, v1].into());
        solver.add_and(and_12.var(), [v1, v2].into());
        solver.add_and(and_01_2.var(), [and_01, v2].into());
        solver.add_and(and_0_12.var(), [v0, and_12].into());

        solver.add_xor(xor_12.var(), [v1, v2].into());

        solver.add_and(combined.var(), [v0, !xor_12].into());

        solver.add_xor(delta_1.var(), [and_01_2, and_0_12].into());
        solver.add_xor(delta_2.var(), [and_01_2, combined].into());

        assert_eq!(solver.solve_assuming(&[delta_1]), Some(false));
        assert_eq!(solver.solve_assuming(&[!delta_1]), Some(true));

        assert_eq!(solver.solve_assuming(&[delta_2]), Some(true));

        if let Some(inputs) = solver.try_input_model() {
            assert!(inputs.contains(&v0));
        }

        assert_eq!(solver.solve_assuming(&[!delta_2]), Some(true));
    }

    #[test]
    pub fn test_gates_cnf() {
        test_gates::<CnfOnly>();
    }

    #[test]
    pub fn test_gates_circuit_just() {
        test_gates::<CircuitJust>();
    }

    #[test]
    pub fn test_gates_circuit_prop() {
        test_gates::<CircuitProp>();
    }
}
