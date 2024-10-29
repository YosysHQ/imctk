//! `UnionFind` efficiently tracks equivalences between variables.
use crate::Element;
use atomic::Atomic;
use imctk_ids::{id_vec::IdVec, Id, IdRange};
use std::sync::atomic::Ordering;

#[cfg(test)]
#[path = "tests/test_union_find.rs"]
mod test_union_find;

/// `UnionFind` efficiently tracks equivalences between variables.
///
/// Given "elements" of type `Elem`, this structure keeps track of any known equivalences between these elements.
/// Equivalences are assumed to be *transitive*, i.e. if `x = y` and `y = z`, then `x = z` is assumed to be true, and in fact,
/// automatically discovered by this structure.
///
/// Unlike a standard union find data structure, this version also keeps track of the *polarity* of elements.
/// For example, you might always have pairs of elements `+x` and `-x` that are exact opposites of each other.
/// If equivalences between `+x` and `-y` are then discovered, the structure also understands that `-x` and `+y` are similarly equivalent.
///
/// An element without its polarity is called an *atom*.
/// The [`Element<Atom>`](Element) trait is required on `Elem` so that this structure can relate elements, atoms and polarities.
///
/// For each set of elements that are equivalent up to a polarity change, this structure keeps track of a *representative*.
/// Each element starts out as its own representative, and if two elements are declared equivalent, the representative of one becomes the representative of both.
/// The method `find` returns the representative for any element.
///
/// To declare two elements as equivalent, use the `union` or `union_full` methods, see their documentation for details on their use.
///
/// NB: Since this structure stores atoms in an `IdVec`, the atoms used should ideally be a contiguous set starting at `Atom::MIN_ID`.
///
/// ## Example ##
/// ```
/// use imctk_lit::{Var, Lit};
/// use dsf::UnionFind;
///
/// let mut union_find: UnionFind<Var, Lit> = UnionFind::new();
/// let lit = |n| Var::from_index(n).as_lit();
///
/// assert_eq!(union_find.find(lit(4)), lit(4));
///
/// union_find.union([lit(3), lit(4)]);
/// assert_eq!(union_find.find(lit(4)), lit(3));
///
/// union_find.union([lit(1), !lit(2)]);
/// union_find.union([lit(2), lit(3)]);
/// assert_eq!(union_find.find(lit(1)), lit(1));
/// assert_eq!(union_find.find(lit(4)), !lit(1));
///
/// ```
pub struct UnionFind<Atom, Elem> {
    parent: IdVec<Atom, Atomic<Elem>>,
}

impl<Atom, Elem> Default for UnionFind<Atom, Elem> {
    fn default() -> Self {
        UnionFind {
            parent: Default::default(),
        }
    }
}

impl<Atom: Id, Elem: Id> Clone for UnionFind<Atom, Elem> {
    fn clone(&self) -> Self {
        let new_parent = self
            .parent
            .values()
            .iter()
            .map(|p| Atomic::new(p.load(Ordering::Relaxed)))
            .collect();
        Self {
            parent: IdVec::from_vec(new_parent),
        }
    }
}

impl<Atom, Elem> UnionFind<Atom, Elem> {
    /// Constructs an empty `UnionFind`.
    ///
    /// The returned struct obeys `find(a) == a` for all `a`.
    pub fn new() -> Self {
        UnionFind::default()
    }
}

impl<Atom: Id, Elem: Id + Element<Atom>> UnionFind<Atom, Elem> {
    /// Constructs an empty `UnionFind`, with room for `capacity` elements.
    pub fn with_capacity(capacity: usize) -> Self {
        UnionFind {
            parent: IdVec::from_vec(Vec::with_capacity(capacity)),
        }
    }
    /// Clears all equivalences, but retains any allocated memory.
    pub fn clear(&mut self) {
        self.parent.clear();
    }
    fn read_parent(&self, atom: Atom) -> Elem {
        if let Some(parent_cell) = self.parent.get(atom) {
            // This load is allowed to reorder with stores from `update_parent`, see there for details.
            parent_cell.load(Ordering::Relaxed)
        } else {
            Elem::from_atom(atom)
        }
    }
    // Important: Only semantically trivial changes are allowed using this method!!
    // Specifically, update_parent(atom, parent) should only be called if `parent` is already an ancestor of `atom`
    // Otherwise, concurrent calls to `read_parent` (which are explicitly allowed!) could return incorrect results.
    fn update_parent(&self, atom: Atom, parent: Elem) {
        if let Some(parent_cell) = self.parent.get(atom) {
            parent_cell.store(parent, Ordering::Relaxed);
        } else {
            // can only get here if the precondition or a data structure invariant is violated
            panic!("shouldn't happen: update_parent called with out of bounds argument");
        }
    }
    // Unlike `update_parent`, this is safe for arbitrary updates, since it requires &mut self.
    fn write_parent(&mut self, atom: Atom, parent: Elem) {
        if let Some(parent_cell) = self.parent.get(atom) {
            parent_cell.store(parent, Ordering::Relaxed);
        } else {
            debug_assert!(self.parent.next_unused_key() <= atom);
            while self.parent.next_unused_key() < atom {
                let next_elem = Elem::from_atom(self.parent.next_unused_key());
                self.parent.push(Atomic::new(next_elem));
            }
            self.parent.push(Atomic::new(parent));
        }
    }
    fn find_root(&self, mut elem: Elem) -> Elem {
        loop {
            // If we interleave with a call to `update_parent`, the parent may change
            // under our feet, but it's okay because in that case we get an ancestor instead,
            // which just skips some iterations of the loop!
            let parent = self.read_parent(elem.atom()).apply_pol_of(elem);
            if elem == parent {
                return elem;
            }
            debug_assert!(elem.atom() != parent.atom());
            elem = parent;
        }
    }
    // Worst-case `find` performance is linear. To keep amortised time complexity logarithmic,
    // we memoise the result of `find_root` by calling `update_parent` on every element
    // we traversed.
    fn update_root(&self, mut elem: Elem, root: Elem) {
        // Loop invariant: `root` is the representative of `elem`.
        loop {
            // Like in `find_root`, this may interleave with `update_root` calls, and we may skip some steps,
            // which is okay because the other thread will do the updates instead.
            let parent = self.read_parent(elem.atom()).apply_pol_of(elem);
            if parent == root {
                break;
            }
            // By the loop invariant, this just sets `elem`'s parent to its representative,
            // which satisfies the precondition for `update_parent`. Further if two threads end up
            // here simultaneously, they will both set to the same representative,
            // therefore the change is idempotent.
            self.update_parent(elem.atom(), root.apply_pol_of(elem));
            elem = parent;
        }
    }
    /// Returns the representative for an element. Elements are equivalent iff they have the same representative.
    ///
    /// Elements `a` and `b` are equivalent up to a polarity change iff they obey `find(a) = find(b) ^ p` for some polarity `p`.
    ///
    /// This operation is guaranteed to return `elem` itself for arguments `elem >= lowest_unused_atom()`.
    ///
    /// The amortised time complexity of this operation is **O**(log N).
    pub fn find(&self, elem: Elem) -> Elem {
        let root = self.find_root(elem);
        self.update_root(elem, root);
        root
    }
    /// Declares two elements to be equivalent. The new representative of both is the representative of the first element.
    ///
    /// If the elements are already equivalent or cannot be made equivalent (are equivalent up to a sign change),
    /// the operation returns `false` without making any changes. Otherwise it returns `true`.
    ///
    /// In both cases it also returns the original representatives of both arguments.
    ///
    /// The amortised time complexity of this operation is **O**(log N).
    pub fn union_full(&mut self, elems: [Elem; 2]) -> (bool, [Elem; 2]) {
        let [a, b] = elems;
        let ra = self.find(a);
        let rb = self.find(b);
        if ra.atom() == rb.atom() {
            (false, [ra, rb])
        } else {
            // The first write is only needed to ensure that the parent table actually contains `a`
            // and is a no-op otherwise.
            self.write_parent(ra.atom(), Elem::from_atom(ra.atom()));
            self.write_parent(rb.atom(), ra.apply_pol_of(rb));
            (true, [ra, rb])
        }
    }
    /// Declares two elements to be equivalent. The new representative of both is the representative of the first element.
    ///
    /// If the elements are already equivalent or cannot be made equivalent (are equivalent up to polarity),
    /// the operation returns `false` without making any changes. Otherwise it returns `true`.
    ///
    /// The amortised time complexity of this operation is **O**(log N).
    pub fn union(&mut self, elems: [Elem; 2]) -> bool {
        self.union_full(elems).0
    }
    /// Sets `atom` to be its own representative, and updates other representatives to preserve all existing equivalences.
    ///
    /// The amortised time complexity of this operation is **O**(log N).
    pub fn make_repr(&mut self, atom: Atom) -> Elem {
        let root = self.find(Elem::from_atom(atom));
        self.write_parent(atom, Elem::from_atom(atom));
        self.write_parent(root.atom(), Elem::from_atom(atom).apply_pol_of(root));
        root
    }
    /// Returns the lowest `Atom` value for which no equivalences are known.
    ///
    /// It is guaranteed that `find(a) == a` if `a >= lowest_unused_atom`, but the converse may not hold.
    pub fn lowest_unused_atom(&self) -> Atom {
        self.parent.next_unused_key()
    }
    /// Returns an iterator that yields all tracked atoms and their representatives.
    pub fn iter(&self) -> impl '_ + Iterator<Item = (Atom, Elem)> {
        IdRange::from(Atom::MIN_ID..self.lowest_unused_atom())
            .iter()
            .map(|atom| (atom, self.find(Elem::from_atom(atom))))
    }
}

impl<Atom: Id, Elem: Id + Element<Atom>> std::fmt::Debug for UnionFind<Atom, Elem> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // prints non-trivial sets of equivalent elements, always printing the representative first
        let mut sets = std::collections::HashMap::<Atom, Vec<Elem>>::new();
        for (atom, repr) in self.iter() {
            if Elem::from_atom(atom) != repr {
                sets.entry(repr.atom())
                    .or_insert_with(|| vec![Elem::from_atom(repr.atom())])
                    .push(Elem::from_atom(atom).apply_pol_of(repr));
            }
        }
        f.debug_set().entries(sets.values()).finish()
    }
}
