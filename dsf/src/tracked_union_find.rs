#![allow(missing_docs)]
#![allow(clippy::type_complexity)]
use std::{cmp::Reverse, collections::VecDeque, mem::ManuallyDrop, sync::Arc};

use atomic::Atomic;
use bytemuck::NoUninit;
use imctk_ids::{id_vec::IdVec, Id, Id64};
use priority_queue::PriorityQueue;

use crate::union_find::{Element, UnionFind};

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct TrackedUnionFindId(Id64);
// SAFETY: trust me bro
unsafe impl NoUninit for TrackedUnionFindId {}
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct Generation(u64);
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct ObserverId(Id64);
// SAFETY: trust me bro
unsafe impl NoUninit for ObserverId {}

#[derive(Debug)]
pub struct ObserverToken {
    tuf_id: TrackedUnionFindId,
    generation: Generation,
    observer_id: ObserverId,
}

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

impl<Atom: Id, Elem: Id + Element<Atom> + NoUninit> Renumbering<Atom, Elem> {
    pub fn get_reverse(forward: &IdVec<Atom, Option<Elem>>, union_find: &UnionFind<Atom, Elem>) -> IdVec<Atom, Elem> {
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
    pub fn is_inverse(forward: &IdVec<Atom, Option<Elem>>, reverse: &IdVec<Atom, Elem>) -> bool {
        reverse.iter().all(|(new, &old)| {
            if let Some(&Some(e)) = forward.get(old.atom()) {
                Elem::from_atom(new) == e.apply_pol_of(old)
            } else {
                false
            }
        })
    }
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
    pub fn old_to_new(&self, old: Elem) -> Option<Elem> {
        self.forward
            .get(old.atom())
            .copied()
            .flatten()
            .map(|e| e.apply_pol_of(old))
    }
    pub fn new_to_old(&self, new: Elem) -> Option<Elem> {
        self.reverse.get(new.atom()).map(|&e| e.apply_pol_of(new))
    }
    pub fn is_repr_reduction(&self, union_find: &UnionFind<Atom, Elem>) -> bool {
        union_find.lowest_unused_atom() <= self.forward.next_unused_key()
            && self.forward.iter().all(|(old, &new)| {
                let repr = union_find.find(Elem::from_atom(old));
                let repr_new = self.old_to_new(repr);
                repr_new == new
            })
    }
}

#[derive(Clone)]
pub enum Change<Atom, Elem> {
    Union { new_repr: Atom, merged_repr: Elem },
    MakeRepr { new_repr: Atom, old_repr: Elem },
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

pub struct TrackedUnionFind<Atom, Elem> {
    tuf_id: TrackedUnionFindId,
    union_find: UnionFind<Atom, Elem>,
    log: VecDeque<Change<Atom, Elem>>,
    observer_id_alloc: IdAlloc<ObserverId>,
    observers: PriorityQueue<ObserverId, Reverse<u64>>,
    log_start: u64,
    generation: Generation,
}

pub struct IdAlloc<T> {
    counter: Atomic<T>,
}

impl<T: Id + NoUninit> Default for IdAlloc<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Id + NoUninit> IdAlloc<T> {
    const fn new() -> Self {
        Self {
            counter: Atomic::new(T::MIN_ID),
        }
    }
    pub fn alloc_block(&self, n: usize) -> T {
        use atomic::Ordering::Relaxed;
        debug_assert!(n > 0);
        self.counter
            .fetch_update(Relaxed, Relaxed, |current_id| {
                current_id
                    .id_index()
                    .checked_add(n)
                    .and_then(T::try_from_id_index)
            })
            .expect("not enough IDs remaining")
    }
    pub fn alloc(&self) -> T {
        self.alloc_block(1)
    }
}

static TUF_ID_ALLOC: IdAlloc<TrackedUnionFindId> = IdAlloc::new();

impl<Atom, Elem> Default for TrackedUnionFind<Atom, Elem> {
    fn default() -> Self {
        Self {
            tuf_id: TUF_ID_ALLOC.alloc(),
            union_find: Default::default(),
            log: Default::default(),
            observer_id_alloc: Default::default(),
            observers: Default::default(),
            log_start: 0,
            generation: Generation(0),
        }
    }
}

impl<Atom: Id, Elem: Id + Element<Atom> + NoUninit> TrackedUnionFind<Atom, Elem> {
    pub fn find(&self, elem: Elem) -> Elem {
        self.union_find.find(elem)
    }
    pub fn union_full(&mut self, lits: [Elem; 2]) -> (bool, [Elem; 2]) {
        let (ok, roots) = self.union_find.union_full(lits);
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
    pub fn union(&mut self, lits: [Elem; 2]) -> bool {
        self.union_full(lits).0
    }
    pub fn make_repr(&mut self, new_repr: Atom) -> Elem {
        let old_repr = self.union_find.make_repr(new_repr);
        if old_repr.atom() != new_repr && !self.observers.is_empty() {
            self.log.push_back(Change::MakeRepr { new_repr, old_repr });
        }
        old_repr
    }
    pub fn renumber(&mut self, forward: IdVec<Atom, Option<Elem>>, reverse: IdVec<Atom, Elem>) {
        let old_generation = self.generation;
        let new_generation = Generation(old_generation.0 + 1);
        self.generation = new_generation;
        let renumbering = Renumbering::new(forward, reverse, old_generation, new_generation);
        debug_assert!(renumbering.is_repr_reduction(&self.union_find));
        if !self.observers.is_empty() {
            self.log.push_back(Change::Renumber(Arc::new(renumbering)));
        }
        self.union_find = UnionFind::new();
    }
}

impl<Atom, Elem> TrackedUnionFind<Atom, Elem> {
    pub fn new() -> Self {
        Self::default()
    }
    fn log_end(&self) -> u64 {
        self.log_start + self.log.len() as u64
    }
    pub fn start_observing(&mut self) -> ObserverToken {
        let observer_id = self.observer_id_alloc.alloc();
        self.observers.push(observer_id, Reverse(self.log_end()));
        ObserverToken {
            tuf_id: self.tuf_id,
            generation: self.generation,
            observer_id,
        }
    }
    pub fn clone_token(&mut self, token: &ObserverToken) -> ObserverToken {
        assert!(token.tuf_id == self.tuf_id);
        let new_observer_id = self.observer_id_alloc.alloc();
        let pos = *self.observers.get_priority(&token.observer_id).unwrap();
        self.observers.push(new_observer_id, pos);
        ObserverToken {
            tuf_id: self.tuf_id,
            generation: token.generation,
            observer_id: new_observer_id,
        }
    }
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
                println!("dropped {delete} entries");
                self.log_start = new_start;
            }
        } else {
            self.log_start = self.log_end();
            self.log.clear();
            println!("dropped all entries");
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

pub struct DrainChanges<'a, 'b, Atom, Elem> {
    tuf: &'a mut TrackedUnionFind<Atom, Elem>,
    token: &'b mut ObserverToken,
    rel_pos: usize,
}

pub struct DrainChangesMap<'a, 'b, Atom, Elem, F> {
    inner: DrainChanges<'a, 'b, Atom, Elem>,
    f: F,
}

impl<'a, 'b, Atom, Elem> DrainChanges<'a, 'b, Atom, Elem> {
    pub fn peek(&mut self) -> Option<&Change<Atom, Elem>> {
        self.tuf.log.get(self.rel_pos)
    }
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
    pub fn stop(self) {
        self.tuf.observer_set_rel_pos(self.token, self.rel_pos);
        let _ = ManuallyDrop::new(self);
    }
    pub fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.tuf.log.len() - self.rel_pos;
        (count, Some(count))
    }
    pub fn map<B, F>(self, f: F) -> DrainChangesMap<'a, 'b, Atom, Elem, F>
    where
        F: FnMut(&Change<Atom, Elem>) -> B,
    {
        DrainChangesMap { inner: self, f }
    }
}

impl<'a, 'b, Atom: Clone, Elem: Clone> DrainChanges<'a, 'b, Atom, Elem> {
    pub fn cloned(
        self,
    ) -> DrainChangesMap<'a, 'b, Atom, Elem, fn(&Change<Atom, Elem>) -> Change<Atom, Elem>> {
        self.map(|x| x.clone())
    }
}

impl<Atom, Elem> Drop for DrainChanges<'_, '_, Atom, Elem> {
    fn drop(&mut self) {
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

#[test]
fn test() {
    use imctk_lit::{Lit, Var};
    let l = |n| Var::from_index(n).as_lit();
    let mut tuf = TrackedUnionFind::<Var, Lit>::new();
    let mut token = tuf.start_observing();
    tuf.union([l(3), !l(4)]);
    tuf.union([l(8), l(7)]);
    let mut token2 = tuf.start_observing();
    tuf.union([l(4), l(5)]);
    for change in tuf.drain_changes(&mut token).cloned() {
        println!("{change:?}");
    }
    println!("---");
    tuf.union([!l(5), l(6)]);
    tuf.make_repr(l(4).var());
    let renumber: IdVec<Var, Option<Lit>> =
        IdVec::from_vec(vec![Some(l(0)), None, None, Some(l(1)), Some(!l(1)), Some(!l(1)), Some(l(1)), Some(l(2)), Some(l(2))]);
    let reverse = Renumbering::get_reverse(&renumber, &tuf.union_find);
    dbg!(&renumber, &reverse);
    tuf.renumber(renumber, reverse);
    tuf.union([l(0), l(1)]);
    let mut iter = tuf.drain_changes(&mut token);
    println!("{:?}", iter.next());
    iter.stop();
    println!("---");
    for change in tuf.drain_changes(&mut token2).cloned() {
        println!("{change:?}");
    }
}
