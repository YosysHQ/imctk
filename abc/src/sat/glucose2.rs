use std::{cell::RefCell, ffi::c_int, marker::PhantomData, mem::ManuallyDrop, os::raw::c_void};

use imctk_abc_sys as abc;
use imctk_ir::var::{Lit, Var};
use imctk_util::unordered_pair::UnorderedPair;
use sealed::{CircuitMode, SolverMode};

unsafe fn from_raw_parts_nullptr<'a, T>(ptr: *const T, len: usize) -> &'a [T] {
    if len == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(ptr, len)
    }
}

pub struct CnfOnly;
pub struct CircuitJust;
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

pub struct Solver<'a, Mode: SolverMode> {
    ptr: *mut c_void,
    state: State,
    _phantom: PhantomData<(Mode, &'a mut ())>,
}

impl<Mode: SolverMode> Default for Solver<'static, Mode> {
    fn default() -> Self {
        unsafe {
            let ptr = abc::imctk_abc_glucose2_init();
            abc::imctk_abc_glucose2_set_incremental_mode(ptr);
            abc::imctk_abc_glucose2_set_jftr(ptr, Mode::JFTR);

            assert_eq!(size_of::<c_int>(), size_of::<Lit>());
            let mut new = Self {
                ptr,
                state: State::Setup,
                _phantom: PhantomData,
            };
            new.add_tagged_clause(&[Lit::TRUE], 1 << 31);
            new
        }
    }
}

impl<Mode: SolverMode> Drop for Solver<'_, Mode> {
    fn drop(&mut self) {
        unsafe {
            abc::imctk_abc_glucose2_release(self.ptr);
        }
    }
}

impl<Mode: SolverMode> Solver<'_, Mode> {
    pub fn reset(&mut self) {
        self.state = State::Setup;
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
        let nvars = unsafe { abc::imctk_abc_glucose2_nvars(self.ptr) } as usize;

        for _ in nvars..var.index() + 1 {
            unsafe { abc::imctk_abc_glucose2_new_var(self.ptr) };
        }
    }
    fn valid_var(&self, var: Var) -> bool {
        let nvars = unsafe { abc::imctk_abc_glucose2_nvars(self.ptr) } as usize;
        var.index() < nvars
    }

    pub fn add_clause(&mut self, lits: &[Lit]) {
        self.state = State::Setup;
        if let Some(max_var) = lits.iter().map(|&lit| lit.var()).max() {
            self.ensure_var(max_var);
        }
        unsafe { self.add_clause_unchecked(lits) }
    }

    unsafe fn add_clause_unchecked(&mut self, lits: &[Lit]) {
        unsafe {
            abc::imctk_abc_glucose2_add_clause(
                self.ptr,
                lits.as_ptr().cast(),
                lits.len().try_into().unwrap(),
            );
        }
    }

    pub fn add_tagged_clause(&mut self, lits: &[Lit], tag: u32) {
        self.state = State::Setup;
        let saved_tag = unsafe { abc::imctk_abc_glucose2_proof_trace_default_tag(self.ptr) };
        unsafe {
            abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, tag);
        }
        self.add_clause(lits);
        unsafe {
            abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, saved_tag);
        }
    }

    pub fn add_and(&mut self, output: Var, inputs: UnorderedPair<Lit>) {
        self.state = State::Setup;
        let (y, [a, b]) = (output.as_lit(), inputs.into_values());
        assert_ne!(a, b);

        if Mode::JFTR == 2 {
            self.ensure_var(output.max(a.var().max(b.var())));
        } else {
            let gate_tag = (output.index() as u32) | (1 << 31);
            let saved_tag = unsafe { abc::imctk_abc_glucose2_proof_trace_default_tag(self.ptr) };
            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, gate_tag);
            }

            self.add_clause(&[y, !a, !b]);
            unsafe {
                self.add_clause_unchecked(&[!y, a]);
                self.add_clause_unchecked(&[!y, b]);
            }
            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, saved_tag);
            }
        }

        if Mode::JFTR != 0 {
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

    pub fn produce_inner_model(&mut self, produce_inner: bool) {
        unsafe { abc::imctk_abc_glucose2_produce_inner_model(self.ptr, produce_inner as i32) };
    }

    pub fn add_xor(&mut self, output: Var, inputs: UnorderedPair<Lit>) {
        self.state = State::Setup;
        let (y, [a, b]) = (output.as_lit(), inputs.into_values());
        assert_ne!(a, b);

        if Mode::JFTR == 2 {
            self.ensure_var(output.max(a.var().max(b.var())));
        } else {
            let gate_tag = (output.index() as u32) | (1 << 31);
            let saved_tag = unsafe { abc::imctk_abc_glucose2_proof_trace_default_tag(self.ptr) };
            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, gate_tag);
            }

            self.add_clause(&[!y, a, b]);
            unsafe {
                self.add_clause_unchecked(&[y, !a, b]);
                self.add_clause_unchecked(&[y, a, !b]);
                self.add_clause_unchecked(&[!y, !a, !b]);
            }

            unsafe {
                abc::imctk_abc_glucose2_proof_trace_set_default_tag(self.ptr, saved_tag);
            }
        }

        if Mode::JFTR != 0 {
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

    pub fn new_round(&mut self) {
        self.state = State::Setup;
        if Mode::JFTR == 0 {
            return;
        }

        unsafe { abc::imctk_abc_glucose2_start_new_round(self.ptr) }
    }

    pub fn mark_cone(&mut self, var: Var) {
        self.state = State::Setup;
        if Mode::JFTR == 0 || !self.valid_var(var) {
            return;
        }

        unsafe { abc::imctk_abc_glucose2_mark_cone(self.ptr, var.index() as c_int) }
    }

    pub fn mark_var(&mut self, var: Var) {
        self.state = State::Setup;
        if Mode::JFTR == 0 || !self.valid_var(var) {
            return;
        }

        unsafe { abc::imctk_abc_glucose2_mark_var(self.ptr, var.index() as c_int) }
    }

    pub fn solve_assuming(&mut self, assumptions: &[Lit]) -> Option<bool> {
        // FIXME abc's glucose2 can crash when there are duplicate vars in the assumptions, either
        // fix that in glucose or work around it here.
        if let Some(max_var) = assumptions.iter().map(|&lit| lit.var()).max() {
            self.ensure_var(max_var);
        }
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

    pub fn failed_assumptions(&mut self) -> &[Lit] {
        unsafe {
            let nlits = abc::imctk_abc_glucose2_conflict_size(self.ptr);
            let lits = abc::imctk_abc_glucose2_conflict_lits(self.ptr);

            from_raw_parts_nullptr(lits.cast::<Lit>(), nlits as usize)
        }
    }

    pub fn cumulative_conflict_limit(&mut self, limit: usize) {
        self.state = State::Setup;
        unsafe { abc::imctk_abc_glucose2_set_conf_budget(self.ptr, i64::try_from(limit).unwrap()) }
    }

    pub fn clear_limits(&mut self) {
        self.state = State::Setup;
        unsafe { abc::imctk_abc_glucose2_budget_off(self.ptr) }
    }

    pub fn try_input_model(&mut self) -> Option<&[Lit]> {
        if Mode::JFTR == 0 || self.state != State::Sat {
            return None;
        }
        unsafe {
            let cex = abc::imctk_abc_glucose2_get_cex(self.ptr);
            let len = *cex as usize;

            Some(std::slice::from_raw_parts(cex.add(1).cast::<Lit>(), len))
        }
    }

    pub fn conflicts(&mut self) -> u64 {
        unsafe { abc::imctk_abc_glucose2_conflicts(self.ptr) as u64 }
    }
}

pub trait ProofTracer {
    fn learnt_clause(&mut self, clause_lits: &[Lit], tags: &[u32], units: &[Lit]) -> u32;
    fn learnt_unit(&mut self, unit: Lit, tag: u32, units: &[Lit]);
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

impl<T: ProofTracer> ProofTracer for &'_ mut T {
    fn learnt_clause(&mut self, clause_lits: &[Lit], tags: &[u32], units: &[Lit]) -> u32 {
        (*self).learnt_clause(clause_lits, tags, units)
    }

    fn learnt_unit(&mut self, unit: Lit, tag: u32, units: &[Lit]) {
        (*self).learnt_unit(unit, tag, units)
    }

    fn conflict(&mut self, conflict_lits: &[Lit], tags: &[u32], units: &[Lit]) {
        (*self).conflict(conflict_lits, tags, units)
    }
}
struct AbortOnUnwind;

impl Drop for AbortOnUnwind {
    fn drop(&mut self) {
        panic!();
    }
}

impl<Mode: SolverMode> Solver<'_, Mode> {
    pub fn stop_proof_tracing(self) -> Solver<'static, Mode> {
        unsafe { abc::imctk_abc_glucose2_stop_proof_trace(self.ptr) }

        let new = Solver {
            ptr: self.ptr,
            state: self.state,
            _phantom: PhantomData,
        };

        let _ = ManuallyDrop::new(self);

        new
    }
    pub fn with_proof_tracer<T: ProofTracer>(self, tracer: &mut T) -> Solver<'_, Mode> {
        unsafe extern "C" fn learnt_clause_wrapper<T: ProofTracer>(
            tracer: *mut c_void,
            lits: *const c_int,
            nlits: c_int,
            tags: *const u32,
            ntags: c_int,
            units: *const c_int,
            nunits: c_int,
        ) -> u32 {
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
            _phantom: PhantomData,
        };

        let _ = ManuallyDrop::new(self);

        new
    }
}

impl<Mode: CircuitMode> Solver<'_, Mode> {
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
