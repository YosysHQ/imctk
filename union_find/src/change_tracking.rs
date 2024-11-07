use imctk_ids::{Id, Id64, IdAlloc};
use priority_queue::PriorityQueue;
use std::{cmp::Reverse, collections::VecDeque, iter::FusedIterator, mem::ManuallyDrop};

/// Globally unique ID for a `ChangeTracking`.
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct ChangeTrackingId(Id64);
/// Generation number for a `ChangeTracking`
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct Generation(pub u64);
/// Observer ID for a `ChangeTracking` (not globally unique)
#[derive(Id, Debug)]
#[repr(transparent)]
pub struct ObserverId(Id64);

/// An `ObserverToken` represents an observer of a `ChangeTracking`.
pub struct ObserverToken {
    tracking_id: ChangeTrackingId,
    generation: Generation,
    observer_id: ObserverId,
}

impl ObserverToken {
    /// Returns the ID of the associated `ChangeTracking`.
    pub fn tracking_id(&self) -> ChangeTrackingId {
        self.tracking_id
    }
    /// Returns the ID of the generation that this observer is on.
    pub fn generation(&self) -> Generation {
        self.generation
    }
    /// Returns the ID of the observer.
    ///
    /// Note that observer IDs are local to each `ChangeTracking`.
    pub fn observer_id(&self) -> ObserverId {
        self.observer_id
    }
    /// Returns `true` iff `self` and `other` belong to the same `ChangeTracking`.
    ///
    /// NB: This does **not** imply that variable IDs are compatible, for that you want `is_compatible`.
    pub fn is_same_tracking(&self, other: &ObserverToken) -> bool {
        self.tracking_id == other.tracking_id
    }
    /// Returns `true` iff `self` and `other` have compatible variable IDs.
    ///
    /// This is equivalent to whether they are on the same generation of the same `ChangeTracking`.
    pub fn is_compatible(&self, other: &ObserverToken) -> bool {
        self.tracking_id == other.tracking_id && self.generation == other.generation
    }
}

impl std::fmt::Debug for ObserverToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ObserverToken")
            .field("tracking_id", &self.tracking_id.0)
            .field("generation", &self.generation.0)
            .field("observer_id", &self.observer_id.0)
            .finish()
    }
}

pub trait Change {
    fn as_renumbering(&self) -> Option<(Generation, Generation)>;
    fn is_renumbering(&self) -> bool {
        self.as_renumbering().is_some()
    }
}

pub struct ChangeTracking<Change> {
    /// Globally unique ID.
    tracking_id: ChangeTrackingId,
    /// Log of changes with new changes appended to the end.
    /// Indices into this log are relative positions.
    /// The start of this log has relative position 0 and absolute position `log_start`.
    log: VecDeque<Change>,
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

static TRACKING_ID_ALLOC: IdAlloc<ChangeTrackingId> = IdAlloc::new();

impl<C> Default for ChangeTracking<C> {
    fn default() -> Self {
        ChangeTracking {
            tracking_id: TRACKING_ID_ALLOC.alloc().unwrap(),
            log: VecDeque::new(),
            observer_id_alloc: IdAlloc::new(),
            observers: PriorityQueue::new(),
            log_start: 0,
            generation: Generation(0),
        }
    }
}

impl<C: Change> ChangeTracking<C> {
    pub fn log(&mut self, change: C) {
        if let Some((old_generation, new_generation)) = change.as_renumbering() {
            debug_assert!(self.generation == old_generation);
            debug_assert!(old_generation < new_generation);
            self.generation = new_generation;
        }
        if !self.observers.is_empty() {
            self.log.push_back(change);
        }
    }
    /// Constructs a new, empty `ChangeTracking`.
    pub fn new() -> Self {
        Self::default()
    }
    pub fn generation(&self) -> Generation {
        self.generation
    }
    fn log_end(&self) -> u64 {
        self.log_start + self.log.len() as u64
    }
    /// Creates a new `ObserverToken` that can be used to track all changes since the call to this method.
    ///
    /// Conceptually, each observer has its own private log.
    /// Any changes that are reported with `log` will be recorded into the logs of all currently active observers.
    /// (In the actual implementation, only a single log is kept).
    ///
    /// After use, the `ObserverToken` must be disposed of with a call to `stop_observing`, otherwise
    /// the memory corresponding to old log entries cannot be reclaimed until the `ChangeTracking` is dropped.
    pub fn start_observing(&mut self) -> ObserverToken {
        let observer_id = self.observer_id_alloc.alloc().unwrap();
        self.observers.push(observer_id, Reverse(self.log_end()));
        ObserverToken {
            tracking_id: self.tracking_id,
            generation: self.generation,
            observer_id,
        }
    }
    /// Clones an `ObserverToken`, conceptually cloning the token's private log.
    pub fn clone_token(&mut self, token: &ObserverToken) -> ObserverToken {
        assert!(token.tracking_id == self.tracking_id);
        let new_observer_id = self.observer_id_alloc.alloc().unwrap();
        let pos = *self.observers.get_priority(&token.observer_id).unwrap();
        self.observers.push(new_observer_id, pos);
        ObserverToken {
            tracking_id: self.tracking_id,
            generation: token.generation,
            observer_id: new_observer_id,
        }
    }
    /// Deletes an `ObserverToken` and its associated state.
    ///
    /// You must call this to allow the `ChangeTracking` to allow memory to be reclaimed.
    pub fn stop_observing(&mut self, token: ObserverToken) {
        assert!(token.tracking_id == self.tracking_id);
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
        assert!(token.tracking_id == self.tracking_id);
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
    fn change_slices(&self, token: &ObserverToken) -> (&[C], &[C]) {
        let rel_pos = self.observer_rel_pos(token);
        let (first, second) = self.log.as_slices();
        if rel_pos >= first.len() {
            (&second[rel_pos - first.len()..], &[])
        } else {
            (&first[rel_pos..], second)
        }
    }
    fn observer_has_seen(&self, token: &mut ObserverToken, changes: &[C]) {
        for change in changes {
            if let Some((old_generation, new_generation)) = change.as_renumbering() {
                debug_assert!(token.generation == old_generation);
                token.generation = new_generation;
            }
        }
    }
    /// Calls the provided function `f` with the content of the token's private log and clears the log.
    ///
    /// Because the log is not necessarily contiguous in memory, `f` may be called multiple times.
    ///
    /// The slice argument to `f` is guaranteed to be non-empty.
    ///
    /// The method assumes that you will immediately process any `Renumbering` operations in the log
    /// and will update the token's generation field.
    ///
    /// Returns `true` iff `f` has been called at least once.
    pub fn drain_changes_with_fn(
        &mut self,
        token: &mut ObserverToken,
        mut f: impl FnMut(&[C]),
    ) -> bool {
        let (first, second) = self.change_slices(token);
        if !first.is_empty() {
            f(first);
            self.observer_has_seen(token, first);
            if !second.is_empty() {
                f(second);
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
    pub fn drain_changes<'a>(&mut self, token: &'a mut ObserverToken) -> DrainChanges<'_, 'a, C> {
        assert!(token.tracking_id == self.tracking_id);
        let rel_pos = self.observer_rel_pos(token);
        DrainChanges {
            tracking: self,
            token,
            rel_pos,
        }
    }
}

/// A draining iterator.
///
/// Since this is a lending iterator, it does not implement the standard `Iterator` trait,
/// but its `map` and `cloned` methods will create a standard iterator.
pub struct DrainChanges<'a, 'b, C: Change> {
    tracking: &'a mut ChangeTracking<C>,
    token: &'b mut ObserverToken,
    rel_pos: usize,
}

/// A draining iterator that has been mapped.
pub struct DrainChangesMap<'a, 'b, C: Change, F> {
    inner: DrainChanges<'a, 'b, C>,
    f: F,
}

impl<'a, 'b, C: Change> DrainChanges<'a, 'b, C> {
    /// Returns a reference to the first entry in the token's private log, without deleting it.
    ///
    /// Returns `None` if the log is empty.
    pub fn peek(&mut self) -> Option<&C> {
        self.tracking.log.get(self.rel_pos)
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
    pub fn next(&mut self) -> Option<&C> {
        let ret = self.tracking.log.get(self.rel_pos);
        if let Some(change) = ret {
            self.tracking
                .observer_has_seen(self.token, std::slice::from_ref(change));
            self.rel_pos += 1;
        }
        ret
    }
    /// Drops the iterator but without deleting unread entries.
    pub fn stop(self) {
        self.tracking.observer_set_rel_pos(self.token, self.rel_pos);
        let _ = ManuallyDrop::new(self);
    }
    /// Returns `(n, Some(n))` where `n` is the number of unread entries.
    ///
    /// This method is designed to be compatible with the standard iterator method of the same name.
    pub fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.tracking.log.len() - self.rel_pos;
        (count, Some(count))
    }
    /// Creates a new iterator by lazily calling `f` on every change.
    #[must_use]
    pub fn map<B, F>(self, f: F) -> DrainChangesMap<'a, 'b, C, F>
    where
        F: FnMut(&C) -> B,
    {
        DrainChangesMap { inner: self, f }
    }
    /// Returns `true` if there are any unprocessed renumberings.
    pub fn any_renumberings(&self) -> bool {
        self.tracking.generation > self.token.generation
    }
}

impl<'a, 'b, C: Change + Clone> DrainChanges<'a, 'b, C> {
    /// Create a standard iterator by cloning every entry.
    #[must_use]
    pub fn cloned(self) -> DrainChangesMap<'a, 'b, C, fn(&C) -> C> {
        self.map(Clone::clone)
    }
}

impl<C: Change> Drop for DrainChanges<'_, '_, C> {
    fn drop(&mut self) {
        // mark any renumberings as seen
        while self.next().is_some() {}
        self.tracking
            .observer_set_rel_pos(self.token, self.tracking.log.len());
    }
}

impl<C: Change, B, F> Iterator for DrainChangesMap<'_, '_, C, F>
where
    F: FnMut(&C) -> B,
{
    type Item = B;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(&mut self.f)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<C: Change, B, F> ExactSizeIterator for DrainChangesMap<'_, '_, C, F> where F: FnMut(&C) -> B {}

impl<C: Change, B, F> FusedIterator for DrainChangesMap<'_, '_, C, F> where F: FnMut(&C) -> B {}
