//! A `TrackedUnionFind` augments a [`UnionFind`] structure with change tracking.
use imctk_ids::{id_vec::IdVec, Id, IdRange};
use std::sync::Arc;

use crate::{
    change_tracking::{self, ChangeTracking, DrainChanges},
    Element, UnionFind,
};

#[cfg(test)]
#[path = "tests/test_tracked_union_find.rs"]
mod test_tracked_union_find;

pub use crate::change_tracking::{Generation, ObserverToken};

/// `Renumbering` represents a renumbering of all variables in `TrackedUnionFind`.
///
/// A renumbering stores a forward and a reverse mapping, and the old and new generation IDs.
///
/// The forward mapping maps each old variable to an optional new variable (variables may be deleted).
/// It is a requirement that equivalent old variables are either both mapped to the same new variable or both deleted.
///
/// The reverse mapping maps each new variable to its old representative.
/// The new set of variables is required to be contiguous, hence `reverse` is a total mapping.
pub struct Renumbering<Atom, Elem> {
    forward: IdVec<Atom, Option<Elem>>,
    reverse: IdVec<Atom, Elem>,
    old_generation: Generation,
    new_generation: Generation,
}

impl<Atom: Id, Elem: Id> std::fmt::Debug for Renumbering<Atom, Elem> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Renumbering")
            .field("forward", &self.forward)
            .field("reverse", &self.reverse)
            .field("old_generation", &self.old_generation)
            .field("new_generation", &self.new_generation)
            .finish()
    }
}

impl<Atom: Id, Elem: Id + Element<Atom = Atom>> Renumbering<Atom, Elem> {
    pub fn forward(&self) -> &IdVec<Atom, Option<Elem>> {
        &self.forward
    }
    pub fn reverse(&self) -> &IdVec<Atom, Elem> {
        &self.reverse
    }
    pub fn old_generation(&self) -> Generation {
        self.old_generation
    }
    pub fn new_generation(&self) -> Generation {
        self.new_generation
    }
    /// Returns the inverse of a reassignment of variables.
    pub fn get_reverse(
        forward: &IdVec<Atom, Option<Elem>>,
        union_find: &UnionFind<Atom, Elem>,
    ) -> IdVec<Atom, Elem> {
        let mut reverse: IdVec<Atom, Option<Elem>> = IdVec::default();
        for (old, &new_opt) in forward {
            if let Some(new) = new_opt {
                reverse
                    .grow_for_key(new.atom())
                    .replace(union_find.find(Elem::from_atom(old)).apply_pol_of(new));
            }
        }
        IdVec::from_vec(reverse.iter().map(|x| x.1.unwrap()).collect())
    }
    /// Returns `true` iff the arguments are inverses of each other.
    pub fn is_inverse(forward: &IdVec<Atom, Option<Elem>>, reverse: &IdVec<Atom, Elem>) -> bool {
        reverse.iter().all(|(new, &old)| {
            if let Some(&Some(e)) = forward.get(old.atom()) {
                Elem::from_atom(new) == e.apply_pol_of(old)
            } else {
                false
            }
        })
    }
    /// Creates a renumbering without checking whether the arguments are valid.
    pub fn new_unchecked(
        forward: IdVec<Atom, Option<Elem>>,
        reverse: IdVec<Atom, Elem>,
        old_generation: Generation,
        new_generation: Generation,
    ) -> Self {
        Renumbering {
            forward,
            reverse,
            old_generation,
            new_generation,
        }
    }
    /// Creates a new renumbering from the given forward and reverse assignment.
    pub fn new(
        forward: IdVec<Atom, Option<Elem>>,
        reverse: IdVec<Atom, Elem>,
        old_generation: Generation,
        new_generation: Generation,
    ) -> Self {
        debug_assert!(new_generation > old_generation);
        debug_assert!(Self::is_inverse(&forward, &reverse));
        Self::new_unchecked(forward, reverse, old_generation, new_generation)
    }
    /// Returns the new variable corresponding to the given old variable, if it exists.
    pub fn old_to_new(&self, old: Elem) -> Option<Elem> {
        self.forward
            .get(old.atom())
            .copied()
            .flatten()
            .map(|e| e.apply_pol_of(old))
    }
    /// Returns the old variable corresponding to the given new variable, if it exists.
    pub fn new_to_old(&self, new: Elem) -> Option<Elem> {
        self.reverse.get(new.atom()).map(|&e| e.apply_pol_of(new))
    }
    /// Returns `true` iff the given renumbering satisfies the constraint that equivalent variables are mapped identically.
    pub fn is_repr_reduction(&self, union_find: &UnionFind<Atom, Elem>) -> bool {
        union_find.lowest_unused_atom() <= self.forward.next_unused_key()
            && self.forward.iter().all(|(old, &new)| {
                let repr = union_find.find(Elem::from_atom(old));
                let repr_new = self.old_to_new(repr);
                repr_new == new
            })
    }
    pub fn compose(&self, other: &Renumbering<Atom, Elem>) -> Self {
        debug_assert!(self.new_generation == other.old_generation);
        let forward = IdVec::from_vec(
            self.forward
                .iter()
                .map(|(_, new)| new.and_then(|new| other.old_to_new(new)))
                .collect(),
        );
        let reverse = IdVec::from_vec(
            other
                .reverse
                .iter()
                .map(|(_, old)| self.new_to_old(*old).unwrap())
                .collect(),
        );
        Renumbering {
            forward,
            reverse,
            old_generation: self.old_generation,
            new_generation: other.new_generation,
        }
    }
}

/// `Change` represents a single change of a `TrackedUnionFind`
#[allow(missing_docs)] // dont want to document every subfield
#[derive(Clone)]
pub enum Change<Atom, Elem> {
    /// A `union` operation. The set with representative `merged_repr` is merged into the set with representative `new_repr`.
    Union { new_repr: Atom, merged_repr: Elem },
    /// A `make_repr` operation. `new_repr` is promoted to be the representative of its set, replacing `old_repr`.
    MakeRepr { new_repr: Atom, old_repr: Elem },
    /// A renumbering operation.
    Renumber(Arc<Renumbering<Atom, Elem>>),
    /// An allocation of variables up to `new_max`.
    AllocAtoms { new_max: Atom },
}

impl<Atom, Elem> change_tracking::Change for Change<Atom, Elem> {
    fn as_renumbering(&self) -> Option<(Generation, Generation)> {
        match self {
            Change::Renumber(renumbering) => {
                Some((renumbering.old_generation, renumbering.new_generation))
            }
            _ => None,
        }
    }
}

impl<Atom: Id, Elem: Id> std::fmt::Debug for Change<Atom, Elem> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Union {
                new_repr,
                merged_repr,
            } => f
                .debug_struct("Union")
                .field("new_repr", new_repr)
                .field("merged_repr", merged_repr)
                .finish(),
            Self::MakeRepr { new_repr, old_repr } => f
                .debug_struct("MakeRepr")
                .field("new_repr", new_repr)
                .field("old_repr", old_repr)
                .finish(),
            Self::Renumber(arg0) => f.debug_tuple("Renumber").field(arg0).finish(),
            Self::AllocAtoms { new_max } => f
                .debug_struct("AllocVars")
                .field("new_max", new_max)
                .finish(),
        }
    }
}

/// A `TrackedUnionFind` augments a [`UnionFind`] structure with change tracking.
pub struct TrackedUnionFind<Atom, Elem> {
    union_find: UnionFind<Atom, Elem>,
    tracking: ChangeTracking<Change<Atom, Elem>>,
}

impl<Atom: std::fmt::Debug + Id, Elem: std::fmt::Debug + Id + Element<Atom = Atom>> std::fmt::Debug
    for TrackedUnionFind<Atom, Elem>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TrackedUnionFind")
            .field("union_find", &self.union_find)
            .field("tracking", &self.tracking)
            .finish()
    }
}

impl<Atom, Elem> From<UnionFind<Atom, Elem>> for TrackedUnionFind<Atom, Elem> {
    fn from(union_find: UnionFind<Atom, Elem>) -> Self {
        Self {
            union_find,
            tracking: Default::default(),
        }
    }
}

impl<Atom, Elem> Default for TrackedUnionFind<Atom, Elem> {
    fn default() -> Self {
        UnionFind::default().into()
    }
}

impl<Atom, Elem> TrackedUnionFind<Atom, Elem> {
    /// Returns a shared reference to the contained `UnionFind`.
    pub fn get_union_find(&self) -> &UnionFind<Atom, Elem> {
        &self.union_find
    }
    /// Returns the contained `UnionFind`. All change tracking data is lost.
    pub fn into_union_find(self) -> UnionFind<Atom, Elem> {
        self.union_find
    }
}

impl<Atom: Id, Elem: Id + Element<Atom = Atom>> TrackedUnionFind<Atom, Elem> {
    pub fn len(&self) -> usize {
        self.union_find.len()
    }
    /// Returns an element's representative. See [`UnionFind::find`].
    pub fn find(&self, elem: Elem) -> Elem {
        self.union_find.find(elem)
    }
    /// Returns `true` if `atom` is a representative.
    pub fn is_repr(&self, atom: Atom) -> bool {
        self.union_find.find(Elem::from_atom(atom)) == Elem::from_atom(atom)
    }
    /// Declares two elements to be equivalent. See [`UnionFind::union_full`].
    pub fn union_full(&mut self, elems: [Elem; 2]) -> (bool, [Elem; 2]) {
        let (ok, roots) = self.union_find.union_full(elems);
        if ok {
            let new_repr = roots[0].atom();
            let merged_repr = roots[1].apply_pol_of(roots[0]);
            self.tracking.log(Change::Union {
                new_repr,
                merged_repr,
            });
        }
        (ok, roots)
    }
    /// Declares two elements to be equivalent. See [`UnionFind::union`].
    pub fn union(&mut self, lits: [Elem; 2]) -> bool {
        self.union_full(lits).0
    }
    /// Promotes an atom to be a representative. See [`UnionFind::make_repr`].
    pub fn make_repr(&mut self, new_repr: Atom) -> Elem {
        let old_repr = self.union_find.make_repr(new_repr);
        if old_repr.atom() != new_repr {
            self.tracking.log(Change::MakeRepr { new_repr, old_repr });
        }
        old_repr
    }
    pub fn fresh_atom(&mut self) -> Atom {
        let atom = self.union_find.fresh_atom();
        self.tracking.log(Change::AllocAtoms { new_max: atom });
        atom
    }
    pub fn fresh_atoms(&mut self, n: usize) -> IdRange<Atom> {
        let atoms = self.union_find.fresh_atoms(n);
        if let Some(last) = atoms.iter().next_back() {
            self.tracking.log(Change::AllocAtoms { new_max: last });
        }
        atoms
    }
    pub fn ensure_allocated(&mut self, atom: Atom) {
        if atom >= self.union_find.lowest_unused_atom() {
            self.union_find.ensure_allocated(atom);
            self.tracking.log(Change::AllocAtoms { new_max: atom });
        }
    }
    /// Renumbers all the variables in the `UnionFind`.
    /// The provided mapping **must** meet all the preconditions listed for [`Renumbering`].
    ///
    /// This resets the `UnionFind` to the trivial state (`find(a) == a` for all `a`) and increments the generation ID.
    ///
    /// This method will panic in debug mode if said preconditions are not met.
    pub fn renumber(
        &mut self,
        forward: IdVec<Atom, Option<Elem>>,
        reverse: IdVec<Atom, Elem>,
    ) -> Arc<Renumbering<Atom, Elem>> {
        let old_generation = self.tracking.generation();
        let new_generation = Generation(old_generation.0 + 1);
        let len = reverse.len();
        let renumbering = Arc::new(Renumbering::new(
            forward,
            reverse,
            old_generation,
            new_generation,
        ));
        debug_assert!(renumbering.is_repr_reduction(&self.union_find));
        self.tracking.log(Change::Renumber(renumbering.clone()));
        self.union_find = UnionFind::with_len(len);
        renumbering
    }
}

impl<Atom, Elem> TrackedUnionFind<Atom, Elem> {
    /// Constructs a new, empty `TrackedUnionFind`.
    pub fn new() -> Self {
        Self::default()
    }
    pub fn start_observing(&mut self) -> ObserverToken {
        self.tracking.start_observing()
    }
    pub fn clone_token(&mut self, token: &ObserverToken) -> ObserverToken {
        self.tracking.clone_token(token)
    }
    pub fn stop_observing(&mut self, token: ObserverToken) {
        self.tracking.stop_observing(token);
    }
    pub fn generation(&self) -> Generation {
        self.tracking.generation()
    }
    /// Calls the provided function `f` with the content of the token's private log and clears the log.
    ///
    /// Because the log is not necessarily contiguous in memory, `f` may be called multiple times.
    ///
    /// The slice argument to `f` is guaranteed to be non-empty.
    /// To allow looking up representatives `f` is also provided with a shared reference to the `UnionFind`.
    ///
    /// The method assumes that you will immediately process any `Renumbering` operations in the log
    /// and will update the token's generation field.
    ///
    /// Returns `true` iff `f` has been called at least once.
    pub fn drain_changes_with_fn(
        &self,
        token: &mut ObserverToken,
        mut f: impl FnMut(&[Change<Atom, Elem>], &UnionFind<Atom, Elem>),
    ) -> bool {
        self.tracking
            .drain_changes_with_fn(token, |ch| f(ch, &self.union_find))
    }
    /// Returns a draining iterator that returns and deletes entries from the token's private log.
    ///
    /// Dropping this iterator will clear any unread entries, call `stop` if this is undesirable.
    ///
    /// You must not leak the returned iterator. Otherwise log entries may be observed multiple times and appear duplicated.
    pub fn drain_changes<'a>(
        &self,
        token: &'a mut ObserverToken,
    ) -> DrainChanges<'_, 'a, Change<Atom, Elem>> {
        self.tracking.drain_changes(token)
    }
}
