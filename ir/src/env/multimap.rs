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

use crate::{
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
    /// Removes all entries from the collection.
    pub fn clear(&mut self) {
        self.equiv_pos = 0; // TODO it would be good if we can avoid that
        self.entries.resize(0);
    }
}

impl<T: Hash + Eq + BitXorAssign<Pol>> LitMultimap<T> {
    /// Inserts a value for a given literal key.
    ///
    /// Returns `false` when the value was already present for the given literal.
    pub fn insert(&mut self, env: &Env, lit: Lit, value: T) -> bool {
        self.merge_equivs(env);
        let lit = env.var_defs().lit_repr(lit);

        self.insert_repr(lit, value)
    }

    /// Inserts a value for a given literal key, assuming the literal is the representative for its
    /// equivalence class.
    ///
    /// Returns `false` when the value was already present for the given literal.
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

    /// Merges newly equivalent literals.
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
    /// Returns an iterator over the values associated with a given variable.
    pub fn var_entries(&self, var: Var) -> impl Iterator<Item = &T> {
        (self.entries.len() > var.index())
            .then(|| self.entries.subtable_iter(var.index()))
            .into_iter()
            .flatten()
    }

    /// Returns an iterator over the values associated with a given literal.
    pub fn lit_entries<'a>(
        &'a self,
        lit: Lit,
    ) -> impl Iterator<Item = <&'a T as BitXor<Pol>>::Output> + 'a
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
    /// Inserts a value for a given variable key.
    ///
    /// Returns `false` when the value was already present for the given variable.
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

    /// Merges newly equivalent (modulo negation) variables.
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
    /// Returns an iterator over the values associated with a given variable.
    pub fn var_entries(&self, var: Var) -> impl Iterator<Item = &T> {
        (self.entries.len() > var.index())
            .then(|| self.entries.subtable_iter(var.index()))
            .into_iter()
            .flatten()
    }
}

#[cfg(test)]
mod tests {
    use zwohash::HashSet;

    use crate::prelude::NodeBuilderDyn;

    use super::*;

    #[test]
    fn lit_multimap() {
        let mut env = Env::default();

        let mut a = env.fresh_var().as_lit();
        let mut b = env.fresh_var().as_lit();
        let mut c = env.fresh_var().as_lit();

        let mut map = <LitMultimap<Lit>>::default();

        let l = |index: usize| Var::from_index(index).as_lit();

        map.insert(&env, a, l(0));
        map.insert(&env, b, l(1));
        map.insert(&env, b, l(2));
        map.insert(&env, c, l(3));
        map.insert(&env, c, l(4));
        map.insert(&env, c, l(5));

        map.merge_equivs(&env);

        assert_eq!(
            HashSet::from_iter(map.lit_entries(a)),
            HashSet::from_iter([l(0)])
        );
        assert_eq!(
            HashSet::from_iter(map.lit_entries(b)),
            HashSet::from_iter([l(1), l(2)])
        );
        assert_eq!(
            HashSet::from_iter(map.lit_entries(c)),
            HashSet::from_iter([l(3), l(4), l(5)])
        );

        env.equiv([a, !c]);

        map.merge_equivs(&env);

        [a, b, c] = [a, b, c].map(|lit| env.lit_repr(lit));

        assert_eq!(
            HashSet::from_iter(map.lit_entries(a)),
            HashSet::from_iter([l(0), !l(3), !l(4), !l(5)])
        );
        assert_eq!(
            HashSet::from_iter(map.lit_entries(b)),
            HashSet::from_iter([l(1), l(2)])
        );
        assert_eq!(
            HashSet::from_iter(map.lit_entries(c)),
            HashSet::from_iter([!l(0), l(3), l(4), l(5)])
        );

        env.equiv([a, b]);

        map.merge_equivs(&env);

        [a, b, c] = [a, b, c].map(|lit| env.lit_repr(lit));

        assert_eq!(
            HashSet::from_iter(map.lit_entries(a)),
            HashSet::from_iter([l(0), l(1), l(2), !l(3), !l(4), !l(5)])
        );
        assert_eq!(
            HashSet::from_iter(map.lit_entries(b)),
            HashSet::from_iter([l(0), l(1), l(2), !l(3), !l(4), !l(5)])
        );
        assert_eq!(
            HashSet::from_iter(map.lit_entries(c)),
            HashSet::from_iter([!l(0), !l(1), !l(2), l(3), l(4), l(5)])
        );
    }

    #[test]
    fn var_multimap() {
        let mut env = Env::default();

        let mut a = env.fresh_var();
        let mut b = env.fresh_var();
        let mut c = env.fresh_var();

        let mut map = <VarMultimap<u32>>::default();

        map.insert(&env, a, 0);
        map.insert(&env, b, 1);
        map.insert(&env, b, 2);
        map.insert(&env, c, 3);
        map.insert(&env, c, 4);
        map.insert(&env, c, 5);

        map.merge_equivs(&env);

        assert_eq!(
            HashSet::from_iter(map.var_entries(a).copied()),
            HashSet::from_iter([0])
        );
        assert_eq!(
            HashSet::from_iter(map.var_entries(b).copied()),
            HashSet::from_iter([1, 2])
        );
        assert_eq!(
            HashSet::from_iter(map.var_entries(c).copied()),
            HashSet::from_iter([3, 4, 5])
        );

        env.equiv([a.as_lit(), c.as_lit()]);

        map.merge_equivs(&env);

        [a, b, c] = [a, b, c].map(|var| env.lit_repr(var.as_lit()).var());

        assert_eq!(
            HashSet::from_iter(map.var_entries(a).copied()),
            HashSet::from_iter([0, 3, 4, 5])
        );
        assert_eq!(
            HashSet::from_iter(map.var_entries(b).copied()),
            HashSet::from_iter([1, 2])
        );
        assert_eq!(
            HashSet::from_iter(map.var_entries(c).copied()),
            HashSet::from_iter([0, 3, 4, 5])
        );

        env.equiv([a.as_lit(), b.as_lit()]);

        map.merge_equivs(&env);

        [a, b, c] = [a, b, c].map(|var| env.lit_repr(var.as_lit()).var());

        assert_eq!(
            HashSet::from_iter(map.var_entries(a).copied()),
            HashSet::from_iter([0, 1, 2, 3, 4, 5])
        );
        assert_eq!(
            HashSet::from_iter(map.var_entries(b).copied()),
            HashSet::from_iter([0, 1, 2, 3, 4, 5])
        );
        assert_eq!(
            HashSet::from_iter(map.var_entries(c).copied()),
            HashSet::from_iter([0, 1, 2, 3, 4, 5])
        );
    }
}
