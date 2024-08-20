//! Equivalence aware multimaps with variables or literals as keys.
//!
//! These collection types are multimaps that will automatically merge entries for
//! [environemnt][Env] [variables][Var] or [literals][Lit] when they become known equivalent in the
//! environment.

// TODO should this be part of the ir crate instead?

use std::{
    hash::Hash,
    ops::{BitXor, BitXorAssign},
};

use imctk_ir::{
    env::Env,
    var::{Lit, Pol, Var},
};
use imctk_util::hash::hash_ref;
use table_seq::TableSeq;

/// Equivalence aware multimap with [`Lit`] keys.
pub struct LitMultimap<T> {
    equiv_pos: usize,
    entries: TableSeq<T>,
}

impl<T> Default for LitMultimap<T> {
    fn default() -> Self {
        Self {
            equiv_pos: Default::default(),
            entries: Default::default(),
        }
    }
}

impl<T> LitMultimap<T> {
    pub fn clear(&mut self) {
        self.equiv_pos = 0; // TODO it would be good if we can avoid that
        self.entries.resize(0);
    }
}

impl<T: Hash + Eq + BitXorAssign<Pol>> LitMultimap<T> {
    pub fn insert(&mut self, env: &Env, lit: Lit, value: T) -> bool {
        self.merge_equivs(env);
        let lit = env.var_defs().lit_repr(lit);

        self.insert_repr(lit, value)
    }

    pub fn insert_repr(&mut self, lit: Lit, mut value: T) -> bool {
        value ^= lit.pol();

        self.entries.grow_for_subtable(lit.index());

        self.entries
            .insert(
                lit.index(),
                hash_ref(&value),
                value,
                |found, inserted| found == inserted,
                hash_ref,
            )
            .1
            .is_none()
    }

    pub fn merge_equivs(&mut self, env: &Env) {
        for &equiv_var in env.equiv_vars()[self.equiv_pos..].iter() {
            if self.entries.len() <= equiv_var.index() {
                continue;
            }
            let repr_lit = env.var_defs().lit_repr(equiv_var.as_lit());
            let pol = repr_lit.pol();

            let equiv_entries = self.entries.take_subtable(equiv_var.index());

            self.entries.grow_for_subtable(repr_lit.index());

            for mut entry in equiv_entries {
                entry ^= pol;

                self.entries.insert(
                    repr_lit.index(),
                    hash_ref(&entry),
                    entry,
                    |found, inserted| found == inserted,
                    hash_ref,
                );
            }
        }
        self.equiv_pos = env.equiv_vars().len()
    }
}

impl<T> LitMultimap<T> {
    pub fn var_entries(&self, var: Var) -> impl Iterator<Item = &T> {
        (self.entries.len() > var.index())
            .then(|| self.entries.subtable_iter(var.index()))
            .into_iter()
            .flatten()
    }

    pub fn lit_entries<'a>(
        &'a self,
        lit: Lit,
    ) -> impl Iterator<Item = <&'a T as BitXor<Pol>>::Output> + '_
    where
        &'a T: BitXor<Pol>,
    {
        let pol = lit.pol();
        self.var_entries(lit.var()).map(move |entry| entry ^ pol)
    }
}

/// Equivalence aware multimap with [`Var`] keys.
#[derive(Default)]
pub struct VarMultimap<T> {
    equiv_pos: usize,
    entries: TableSeq<T>,
}

impl<T: Hash + Eq> VarMultimap<T> {
    pub fn insert(&mut self, env: &Env, var: Var, value: T) -> bool {
        self.merge_equivs(env);
        let var = env.var_defs().var_repr(var);

        self.entries.grow_for_subtable(var.index());

        self.entries
            .insert(
                var.index(),
                hash_ref(&value),
                value,
                |found, inserted| found == inserted,
                hash_ref,
            )
            .1
            .is_none()
    }

    pub fn merge_equivs(&mut self, env: &Env) {
        for &equiv_var in env.equiv_vars()[self.equiv_pos..].iter() {
            if self.entries.len() <= equiv_var.index() {
                continue;
            }
            let repr_var = env.var_defs().var_repr(equiv_var);

            let equiv_entries = self.entries.take_subtable(equiv_var.index());

            self.entries.grow_for_subtable(repr_var.index());

            for entry in equiv_entries {
                self.entries.insert(
                    repr_var.index(),
                    hash_ref(&entry),
                    entry,
                    |found, inserted| found == inserted,
                    hash_ref,
                );
            }
        }
        self.equiv_pos = env.equiv_vars().len()
    }
}

impl<T> VarMultimap<T> {
    pub fn var_entries(&self, var: Var) -> impl Iterator<Item = &T> {
        (self.entries.len() > var.index())
            .then(|| self.entries.subtable_iter(var.index()))
            .into_iter()
            .flatten()
    }
}
