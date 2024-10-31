//! A `TrackedUnionFind` augments a [`UnionFind`] structure with change tracking.
use std::{cmp::Reverse, collections::VecDeque, iter::FusedIterator, mem::ManuallyDrop, sync::Arc};

use imctk_ids::{id_vec::IdVec, Id, Id64, IdAlloc};
use priority_queue::PriorityQueue;

use crate::{Element, UnionFind};

#[cfg(test)]
#[path = "tests/test_tracked_union_find.rs"]
mod test_tracked_union_find;

/// Globally unique ID for a `TrackedUnionFind`.
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct TrackedUnionFindId(Id64);
/// Generation number for a `TrackedUnionFind`
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct Generation(u64);
/// Observer ID for a `TrackedUnionFind` (not globally unique)
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct ObserverId(Id64);

/// An `ObserverToken` represents an observer of a `TrackedUnionFind`.
pub struct ObserverToken {
    tuf_id: TrackedUnionFindId,
    generation: Generation,
    observer_id: ObserverId,
}

impl ObserverToken {
    /// Returns the ID of the associated `TrackedUnionFind`.
    pub fn tuf_id(&self) -> TrackedUnionFindId {
        self.tuf_id
    }
    /// Returns the ID of the generation that this observer is on.
    pub fn generation(&self) -> Generation {
        self.generation
    }
    /// Returns the ID of the observer.
    ///
    /// Note that observer IDs are local to each `TrackedUnionFind`.
    pub fn observer_id(&self) -> ObserverId {
        self.observer_id
    }
    /// Returns `true` iff `self` and `other` belong to the same `TrackedUnionFind`.
    ///
    /// NB: This does **not** imply that variable IDs are compatible, for that you want `is_compatible`.
    pub fn is_same_tuf(&self, other: &ObserverToken) -> bool {
        self.tuf_id == other.tuf_id
    }
    /// Returns `true` iff `self` and `other` have compatible variable IDs.
    ///
    /// This is equivalent to whether they are on the same generation of the same `TrackedUnionFind`.
    pub fn is_compatible(&self, other: &ObserverToken) -> bool {
        self.tuf_id == other.tuf_id && self.generation == other.generation
    }
}

impl std::fmt::Debug for ObserverToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ObserverToken")
            .field("tuf_id", &self.tuf_id.0)
            .field("generation", &self.generation.0)
            .field("observer_id", &self.observer_id.0)
            .finish()
    }
}

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

impl<Atom: Id, Elem: Id + Element<Atom>> Renumbering<Atom, Elem> {
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
        }
    }
}

/// A `TrackedUnionFind` augments a [`UnionFind`] structure with change tracking.
pub struct TrackedUnionFind<Atom, Elem> {
    /// Globally unique ID.
    tuf_id: TrackedUnionFindId,
    union_find: UnionFind<Atom, Elem>,
    /// Log of changes with new changes appended to the end.
    /// Indices into this log are relative positions.
    /// The start of this log has relative position 0 and absolute position `log_start`.
    log: VecDeque<Change<Atom, Elem>>,
    observer_id_alloc: IdAlloc<ObserverId>,
    /// This stores absolute positions of each observers in the log,
    /// and also allows retrieving the minimum absolute position of any observer.
    ///
    /// `truncate_log` will reset that minimum position to be at relative position 0.
    observers: PriorityQueue<ObserverId, Reverse<u64>>,
    /// Offset between absolute and relative positions in the log.
    ///
    /// absolute position = relative position + `log_start`
    log_start: u64,
    /// Incremented on every renumbering.
    generation: Generation,
}

static TUF_ID_ALLOC: IdAlloc<TrackedUnionFindId> = IdAlloc::new();

impl<Atom, Elem> From<UnionFind<Atom, Elem>> for TrackedUnionFind<Atom, Elem> {
    fn from(union_find: UnionFind<Atom, Elem>) -> Self {
        Self {
            union_find,
            tuf_id: TUF_ID_ALLOC.alloc().unwrap(),
            log: Default::default(),
            observer_id_alloc: Default::default(),
            observers: Default::default(),
            log_start: 0,
            generation: Generation(0),
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

impl<Atom: Id, Elem: Id + Element<Atom>> TrackedUnionFind<Atom, Elem> {
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
        if ok && !self.observers.is_empty() {
            let new_repr = roots[0].atom();
            let merged_repr = roots[1].apply_pol_of(roots[0]);
            self.log.push_back(Change::Union {
                new_repr,
                merged_repr,
            })
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
        if old_repr.atom() != new_repr && !self.observers.is_empty() {
            self.log.push_back(Change::MakeRepr { new_repr, old_repr });
        }
        old_repr
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
        let old_generation = self.generation;
        let new_generation = Generation(old_generation.0 + 1);
        self.generation = new_generation;
        let renumbering = Arc::new(Renumbering::new(
            forward,
            reverse,
            old_generation,
            new_generation,
        ));
        debug_assert!(renumbering.is_repr_reduction(&self.union_find));
        if !self.observers.is_empty() {
            self.log.push_back(Change::Renumber(renumbering.clone()));
        }
        self.union_find = UnionFind::new();
        renumbering
    }
}

impl<Atom, Elem> TrackedUnionFind<Atom, Elem> {
    /// Constructs a new, empty `TrackedUnionFind`.
    pub fn new() -> Self {
        Self::default()
    }
    fn log_end(&self) -> u64 {
        self.log_start + self.log.len() as u64
    }
    /// Creates a new `ObserverToken` that can be used to track all changes since the call to this method.
    ///
    /// Conceptually, each observer has its own private log.
    /// Any changes that happen to the `UnionFind` will be recorded into the logs of all currently active observers.
    /// (In the actual implementation, only a single log is kept).
    ///
    /// After use, the `ObserverToken` must be disposed of with a call to `stop_observing`, otherwise
    /// the memory corresponding to old log entries cannot be reclaimed until the `TrackedUnionFind` is dropped.
    pub fn start_observing(&mut self) -> ObserverToken {
        let observer_id = self.observer_id_alloc.alloc().unwrap();
        self.observers.push(observer_id, Reverse(self.log_end()));
        ObserverToken {
            tuf_id: self.tuf_id,
            generation: self.generation,
            observer_id,
        }
    }
    /// Clones an `ObserverToken`, conceptually cloning the token's private log.
    pub fn clone_token(&mut self, token: &ObserverToken) -> ObserverToken {
        assert!(token.tuf_id == self.tuf_id);
        let new_observer_id = self.observer_id_alloc.alloc().unwrap();
        let pos = *self.observers.get_priority(&token.observer_id).unwrap();
        self.observers.push(new_observer_id, pos);
        ObserverToken {
            tuf_id: self.tuf_id,
            generation: token.generation,
            observer_id: new_observer_id,
        }
    }
    /// Deletes an `ObserverToken` and its associated state.
    ///
    /// You must call this to allow the `TrackedUnionFind` to allow memory to be reclaimed.
    pub fn stop_observing(&mut self, token: ObserverToken) {
        assert!(token.tuf_id == self.tuf_id);
        self.observers.remove(&token.observer_id);
        self.truncate_log();
    }
    fn truncate_log(&mut self) {
        if let Some((_, &Reverse(new_start))) = self.observers.peek() {
            if new_start > self.log_start {
                let delete = (new_start - self.log_start).try_into().unwrap();
                drop(self.log.drain(0..delete));
                self.log_start = new_start;
            }
        } else {
            self.log_start = self.log_end();
            self.log.clear();
        }
    }
    fn observer_rel_pos(&self, token: &ObserverToken) -> usize {
        assert!(token.tuf_id == self.tuf_id);
        let abs_pos = self.observers.get_priority(&token.observer_id).unwrap().0;
        debug_assert!(abs_pos >= self.log_start);
        (abs_pos - self.log_start).try_into().unwrap()
    }
    fn observer_inc_pos(&mut self, token: &ObserverToken, by: u64) {
        let (min, max) = (self.log_start, self.log_end());
        self.observers
            .change_priority_by(&token.observer_id, |pos| {
                let new = pos.0 + by;
                debug_assert!(new >= min && new <= max);
                *pos = Reverse(new);
            });
        self.truncate_log();
    }
    fn observer_set_rel_pos(&mut self, token: &ObserverToken, rel_pos: usize) {
        debug_assert!(rel_pos <= self.log.len());
        let abs_pos = self.log_start + rel_pos as u64;
        self.observers
            .change_priority(&token.observer_id, Reverse(abs_pos));
        self.truncate_log();
    }
    #[allow(clippy::type_complexity)]
    fn change_slices(
        &self,
        token: &ObserverToken,
    ) -> (
        &[Change<Atom, Elem>],
        &[Change<Atom, Elem>],
        &UnionFind<Atom, Elem>,
    ) {
        let rel_pos = self.observer_rel_pos(token);
        let (first, second) = self.log.as_slices();
        if rel_pos >= first.len() {
            (&second[rel_pos - first.len()..], &[], &self.union_find)
        } else {
            (&first[rel_pos..], second, &self.union_find)
        }
    }
    fn observer_has_seen(&self, token: &mut ObserverToken, changes: &[Change<Atom, Elem>]) {
        for change in changes {
            if let Change::Renumber(renumbering) = change {
                debug_assert!(token.generation == renumbering.old_generation);
                token.generation = renumbering.new_generation;
            }
        }
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
        &mut self,
        token: &mut ObserverToken,
        mut f: impl FnMut(&[Change<Atom, Elem>], &UnionFind<Atom, Elem>),
    ) -> bool {
        let (first, second, union_find) = self.change_slices(token);
        if !first.is_empty() {
            f(first, union_find);
            self.observer_has_seen(token, first);
            if !second.is_empty() {
                f(second, union_find);
                self.observer_has_seen(token, second);
            }
            let drained = first.len() + second.len();
            self.observer_inc_pos(token, drained as u64);
            true
        } else {
            false
        }
    }
    /// Returns a draining iterator that returns and deletes entries from the token's private log.
    ///
    /// Dropping this iterator will clear any unread entries, call `stop` if this is undesirable.
    ///
    /// You must not leak the returned iterator. Otherwise log entries may be observed multiple times and appear duplicated.
    pub fn drain_changes<'a>(
        &mut self,
        token: &'a mut ObserverToken,
    ) -> DrainChanges<'_, 'a, Atom, Elem> {
        assert!(token.tuf_id == self.tuf_id);
        let rel_pos = self.observer_rel_pos(token);
        DrainChanges {
            tuf: self,
            token,
            rel_pos,
        }
    }
}

/// A draining iterator.
///
/// Since this is a lending iterator, it does not implement the standard `Iterator` trait,
/// but its `map` and `cloned` methods will create a standard iterator.
pub struct DrainChanges<'a, 'b, Atom, Elem> {
    tuf: &'a mut TrackedUnionFind<Atom, Elem>,
    token: &'b mut ObserverToken,
    rel_pos: usize,
}

/// A draining iterator that has been mapped.
pub struct DrainChangesMap<'a, 'b, Atom, Elem, F> {
    inner: DrainChanges<'a, 'b, Atom, Elem>,
    f: F,
}

impl<'a, 'b, Atom, Elem> DrainChanges<'a, 'b, Atom, Elem> {
    /// Returns a reference to the first entry in the token's private log, without deleting it.
    ///
    /// Returns `None` if the log is empty.
    pub fn peek(&mut self) -> Option<&Change<Atom, Elem>> {
        self.tuf.log.get(self.rel_pos)
    }
    /// Returns a reference to the first entry in the token's private log. The entry will be deleted after its use.
    ///
    /// If this returns a `Renumbering`, it is assumed that you will process it and the token's generation number will be updated.
    ///
    /// Returns `None` if the log is empty. If `next` returned `None`, it will never return any more entries (the iterator is fused).
    ///
    /// As reflected by the lifetimes, the API only guarantees that the returned reference until the next call of any method of this iterator.
    /// (In practice, deletion is more lazy and happens on drop).
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<&Change<Atom, Elem>> {
        let ret = self.tuf.log.get(self.rel_pos);
        if let Some(change) = ret {
            self.tuf
                .observer_has_seen(self.token, std::slice::from_ref(change));
            self.rel_pos += 1;
        }
        ret
    }
    /// Drops the iterator but without deleting unread entries.
    pub fn stop(self) {
        self.tuf.observer_set_rel_pos(self.token, self.rel_pos);
        let _ = ManuallyDrop::new(self);
    }
    /// Returns `(n, Some(n))` where `n` is the number of unread entries.
    ///
    /// This method is designed to be compatible with the standard iterator method of the same name.
    pub fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.tuf.log.len() - self.rel_pos;
        (count, Some(count))
    }
    /// Creates a new iterator by lazily calling `f` on every change.
    #[must_use]
    pub fn map<B, F>(self, f: F) -> DrainChangesMap<'a, 'b, Atom, Elem, F>
    where
        F: FnMut(&Change<Atom, Elem>) -> B,
    {
        DrainChangesMap { inner: self, f }
    }
}

impl<'a, 'b, Atom: Clone, Elem: Clone> DrainChanges<'a, 'b, Atom, Elem> {
    /// Create a standard iterator by cloning every entry.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn cloned(
        self,
    ) -> DrainChangesMap<'a, 'b, Atom, Elem, fn(&Change<Atom, Elem>) -> Change<Atom, Elem>> {
        self.map(|x| x.clone())
    }
}

impl<Atom, Elem> Drop for DrainChanges<'_, '_, Atom, Elem> {
    fn drop(&mut self) {
        // mark any renumberings as seen
        while self.next().is_some() {}
        self.tuf
            .observer_set_rel_pos(self.token, self.tuf.log.len());
    }
}

impl<Atom, Elem, B, F> Iterator for DrainChangesMap<'_, '_, Atom, Elem, F>
where
    F: FnMut(&Change<Atom, Elem>) -> B,
{
    type Item = B;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(&mut self.f)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<Atom, Elem, B, F> ExactSizeIterator for DrainChangesMap<'_, '_, Atom, Elem, F> where
    F: FnMut(&Change<Atom, Elem>) -> B
{
}

impl<Atom, Elem, B, F> FusedIterator for DrainChangesMap<'_, '_, Atom, Elem, F> where
    F: FnMut(&Change<Atom, Elem>) -> B
{
}
