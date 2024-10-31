//! Strategies for refining equivalence candiate classes.
use std::ops::{Deref, DerefMut};

use imctk_inc_refine::IncrementalRefinement;
use imctk_ir::{env::Env, var::Var};

use crate::seq_sim::model::SimModel;

pub mod driver;

pub mod bmc;
pub mod rarity_sim;

/// Wrapper for [`IncrementalRefinement<Var>`][IncrementalRefinement] to track known equivalences in
/// the environment.
#[derive(Default)]
pub struct EnvVarRefinement {
    refine: IncrementalRefinement<Var>,
    equiv_pos: usize,
}

impl Deref for EnvVarRefinement {
    type Target = IncrementalRefinement<Var>;

    fn deref(&self) -> &Self::Target {
        &self.refine
    }
}

impl DerefMut for EnvVarRefinement {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.refine
    }
}

impl EnvVarRefinement {
    pub fn sync_equivs(&mut self, env: &Env) {
        for &equiv in env.equiv_vars()[self.equiv_pos..].iter() {
            if !self.refine.contains_item(equiv) {
                continue;
            }
            let repr = env.var_defs().lit_repr(equiv.as_lit()).var();
            self.refine.insert_item(repr);
            self.refine.equiv([equiv, repr]);
            self.refine.remove_item(equiv);
        }
    }

    pub fn stats(&mut self) -> impl std::fmt::Display + std::fmt::Display {
        let root = self.refine.root_count();
        let classes = self.refine.nonleaf_root_count();
        let unique = root - classes;
        let constants = self.refine.ancestral_sibling_count(Var::FALSE);
        let pending = self.refine.item_count() - root;
        imctk_util::fmt::fmt_closure(move |f| {
            write!(
                f,
                "unique: {unique} classes: {classes} constant: {constants} pending: {pending}"
            )
        })
    }
}

/// Bundles an environment, equivalence candidates and a sequential simulation model.
pub struct RefinementContext<'a> {
    pub env: &'a mut Env,
    pub refine: &'a mut EnvVarRefinement,
    pub sim_model: &'a mut SimModel,
}

impl RefinementContext<'_> {
    pub fn sync_equivs(&mut self) {
        self.env.rebuild_egraph();
        self.refine.sync_equivs(self.env);
    }
}
