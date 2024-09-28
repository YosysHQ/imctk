//! Unrolling of a sequential circuits for BMC, k-induction or similar.

use imctk_ids::{id_vec::IdVec, indexed_id_vec::IndexedIdVec, Id, Id32};
use imctk_ir::{
    env::{Env, LitMultimap},
    node::fine::circuit::{InitNode, Input, InputNode, RegNode, SteadyInputNode},
    prelude::{NodeBuilder, Term, TermDyn},
    var::{Lit, Pol, Var},
};
use imctk_util::vec_sink::VecSink;

use crate::time_step::TimeStep;

/// [`Term`] representing an [`Input`] at a specific [`TimeStep`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct UnrolledInput {
    /// The input of the sequential circuit.
    pub input: Input,
    /// The time step in the unrolled circuit.
    pub step: TimeStep,
}

impl Term for UnrolledInput {
    type Output = Lit;

    const NAME: &'static str = "UnrolledInput";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [].into_iter()
    }

    fn apply_var_map(&mut self, _var_map: impl FnMut(Var) -> Lit) -> Pol {
        Pol::Pos
    }

    fn is_steady(&self, _input_steady: impl Fn(Var) -> bool) -> bool {
        true
    }
}

impl TermDyn for UnrolledInput {}

/// [`Term`] representing the unknown and unconstrained past value of a sequential signal for
/// induction.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct UnknownPast {
    /// The signal of the sequential circuit.
    ///
    /// Note that this is not an input to the term as it refers to a signal in a different
    /// environment.
    pub seq: Var,
}

impl Term for UnknownPast {
    type Output = Lit;

    const NAME: &'static str = "UnknownPast";

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [].into_iter()
    }

    fn apply_var_map(&mut self, _var_map: impl FnMut(Var) -> Lit) -> Pol {
        Pol::Pos
    }

    fn is_steady(&self, _input_steady: impl Fn(Var) -> bool) -> bool {
        true
    }
}

impl TermDyn for UnknownPast {}

// In Induction mode, the first step contains everything that's steady and the second step contains
// the unconstrained past.
/// Selects whether to unroll for BMC or k-induction.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnrollMode {
    /// Perform unrolling for a bounded model check (BMC).
    Bmc,
    /// Perform unrolling for a k-induction step.
    ///
    /// In this mode the [`TimeStep`] of index 0 is used for steady signals (as well as the initial
    /// state of non-steady signals used to derive steady signals). Index 1 is used for the unknown
    /// and unconstrained past and the steps starting with index 2 will be unrolled as in BMC mode.
    ///
    /// This is suitable for k-induction as well as for interval property checking.
    Induction,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct SeqFromCombEntry {
    step: TimeStep,
    lit: Lit,
}

impl std::ops::BitXorAssign<Pol> for SeqFromCombEntry {
    fn bitxor_assign(&mut self, rhs: Pol) {
        self.lit ^= rhs;
    }
}

impl std::ops::BitXor<Pol> for &'_ SeqFromCombEntry {
    type Output = SeqFromCombEntry;

    fn bitxor(self, rhs: Pol) -> SeqFromCombEntry {
        SeqFromCombEntry {
            step: self.step,
            lit: self.lit ^ rhs,
        }
    }
}

/// Unrolling of a sequential circuits and manages the mapping between the sequential and unrolled
/// circuit.
pub struct Unroll {
    seen_from_seq: IndexedIdVec<Id32, Var>,
    comb_from_seen: IdVec<TimeStep, IdVec<Id32, Option<Lit>>>,
    seq_from_comb: LitMultimap<SeqFromCombEntry>,

    mode: UnrollMode,

    var_stack: Vec<Var>,
}

impl Unroll {
    pub fn new(mode: UnrollMode) -> Self {
        let mut new = Self {
            seen_from_seq: Default::default(),
            comb_from_seen: Default::default(),
            seq_from_comb: Default::default(),

            mode,

            var_stack: vec![],
        };

        new.init();

        new
    }

    fn init(&mut self) {
        self.seen_from_seq.insert(Var::FALSE);
        self.seq_from_comb.insert_repr(
            Lit::FALSE,
            SeqFromCombEntry {
                step: TimeStep::FIRST,
                lit: Lit::FALSE,
            },
        );
    }

    pub fn unroll(&mut self, source: &Env, dest: &mut Env, step: TimeStep, seq_lit: Lit) -> Lit {
        if seq_lit.is_const() {
            seq_lit
        } else {
            let seq_lit = source.var_defs().lit_repr(seq_lit);

            self.unroll_repr_var(source, dest, step, seq_lit.var()) ^ seq_lit.pol()
        }
    }

    pub fn find_unrolled(&self, source: &Env, step: TimeStep, seq_lit: Lit) -> Option<Lit> {
        if seq_lit.is_const() {
            Some(seq_lit)
        } else {
            let seq_lit = source.var_defs().lit_repr(seq_lit);
            self.find_unrolled_repr_var(step, seq_lit.var())
                .map(|lit| lit ^ seq_lit.pol())
        }
    }

    pub fn find_seq_input(
        &mut self,
        source: &Env,
        dest: &Env,
        unroll_lit: Lit,
    ) -> Option<(TimeStep, Lit)> {
        self.seq_from_comb.merge_equivs(dest);

        for entry in self
            .seq_from_comb
            .lit_entries(dest.var_defs().lit_repr(unroll_lit))
        {
            let Some(node) = source.def_node(entry.lit.var()) else { continue };
            if node.dyn_cast::<InputNode>().is_some()
                || node.dyn_cast::<SteadyInputNode>().is_some()
            {
                return Some((entry.step, entry.lit));
            }
        }

        None
    }

    fn find_unrolled_repr_var(&self, step: TimeStep, seq_var: Var) -> Option<Lit> {
        let seen = self.seen_from_seq.get_key(&seq_var)?;
        let comb_from_seen = self.comb_from_seen.get(step)?;
        let slot = comb_from_seen.get(seen)?;
        *slot
    }

    fn unroll_repr_var(
        &mut self,
        source: &Env,
        dest: &mut Env,
        step: TimeStep,
        seq_var: Var,
    ) -> Lit {
        let seen = self.seen_from_seq.insert(seq_var).0;

        let comb_from_seen = self
            .comb_from_seen
            .grow_for_key_with(step, || IdVec::from_vec(vec![Some(Lit::FALSE)]));

        let slot = comb_from_seen.grow_for_key(seen);

        if let Some(comb_lit) = slot {
            return *comb_lit;
        }

        let unrolled_lit = self.unroll_repr_var_uncached(source, dest, step, seq_var);

        let comb_from_seen = self
            .comb_from_seen
            .grow_for_key_with(step, || IdVec::from_vec(vec![Some(Lit::FALSE)]));

        let slot = comb_from_seen.grow_for_key(seen);

        *slot = Some(unrolled_lit);

        unrolled_lit
    }

    fn unroll_repr_var_uncached(
        &mut self,
        source: &Env,
        dest: &mut Env,
        step: TimeStep,
        seq_var: Var,
    ) -> Lit {
        let unrolled_lit = if source.var_defs().is_steady(seq_var) && step != TimeStep::FIRST {
            self.unroll_repr_var(source, dest, TimeStep::FIRST, seq_var)
        } else {
            match (step.id_index(), self.mode) {
                (0, _) => self.unroll_first_bmc(source, dest, seq_var),
                (1, UnrollMode::Induction) => dest.term(UnknownPast { seq: seq_var }),
                _ => self.unroll_generic(source, dest, step, seq_var),
            }
        };

        self.seq_from_comb.insert(
            dest,
            unrolled_lit,
            SeqFromCombEntry {
                step,
                lit: seq_var.as_lit(),
            },
        );

        unrolled_lit
    }

    fn unroll_first_bmc(&mut self, source: &Env, dest: &mut Env, seq_var: Var) -> Lit {
        let node = source.def_node(seq_var).unwrap();
        let output_pol = node.output_lit().unwrap().pol();

        if let Some(reg) = node.dyn_cast::<RegNode>() {
            self.unroll(source, dest, TimeStep::FIRST, reg.term.init) ^ output_pol
        } else if let Some(init) = node.dyn_cast::<InitNode>() {
            self.unroll(source, dest, TimeStep::FIRST, init.term.input.as_lit()) ^ output_pol
        } else {
            self.unroll_generic(source, dest, TimeStep::FIRST, seq_var)
        }
    }

    fn unroll_generic(
        &mut self,
        source: &Env,
        dest: &mut Env,
        step: TimeStep,
        seq_var: Var,
    ) -> Lit {
        let node = source.def_node(seq_var).unwrap();
        let output_pol = node.output_lit().unwrap().pol();

        if let Some(reg) = node.dyn_cast::<RegNode>() {
            let prev_step = step.prev().unwrap();
            self.unroll(source, dest, prev_step, reg.term.next.as_lit()) ^ output_pol
        } else if let Some(init) = node.dyn_cast::<InitNode>() {
            self.unroll(source, dest, TimeStep::FIRST, init.term.input.as_lit()) ^ output_pol
        } else if let Some(&input) = node.dyn_cast::<InputNode>() {
            dest.term(UnrolledInput {
                input: input.term,
                step,
            }) ^ output_pol
        } else {
            let var_stack_len = self.var_stack.len();
            node.dyn_append_input_vars(VecSink::new(&mut self.var_stack));

            while self.var_stack.len() > var_stack_len {
                let var = self.var_stack.pop().unwrap();
                self.unroll(source, dest, step, var.as_lit());
            }

            node.dyn_term()
                .unwrap()
                .dyn_add_to_env_with_var_map(dest, &mut |var| {
                    self.find_unrolled(source, step, var.as_lit()).unwrap()
                })
                ^ output_pol
        }
    }
}
