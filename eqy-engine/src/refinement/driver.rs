//! Strategy for equivalence class refinement by scheduling pointwise equivalence queries
//! interleaved with splitting individual classes by computing class invariant values.
use imctk_ids::{id_vec::IdVec, Id};
use std::{cmp::Reverse, collections::BinaryHeap, hash::Hash};

pub struct RefinementDriver<T: Id, P, V> {
    items: IdVec<T::BaseId, T>,
    prios: IdVec<T::BaseId, P>,
    values: IdVec<T::BaseId, V>,
    repr: IdVec<T::BaseId, T::BaseId>,
    order: Vec<T::BaseId>,
    heap: BinaryHeap<(P, T::BaseId, T::BaseId)>,
    pending: Option<(usize, usize)>,
}

impl<T: Id, P: Ord + Copy, V: Ord + Hash + Copy> Default for RefinementDriver<T, P, V> {
    fn default() -> Self {
        Self {
            items: Default::default(),
            prios: Default::default(),
            values: Default::default(),
            repr: Default::default(),
            order: Default::default(),
            heap: Default::default(),
            pending: Default::default(),
        }
    }
}

impl<T: Id, P: Ord + Copy, V: Ord + Hash + Copy> RefinementDriver<T, P, V> {
    pub fn reset(&mut self, items: impl Iterator<Item = (T, P, V)>) {
        self.items.clear();
        self.prios.clear();
        self.values.clear();
        self.repr.clear();
        self.order.clear();
        self.heap.clear();
        self.pending = None;

        for (item, prio, value) in items {
            let id = self.items.push(item).0;
            self.prios.push(prio);
            self.values.push(value);
            self.order.push(id);
            self.repr.push(id);
        }

        self.subdivide(0, self.order.len());
    }

    pub fn refine(&mut self, mut eq: impl FnMut(T, T) -> bool) -> bool {
        assert!(self.pending.is_none(), "call update before calling refine");
        let Some((_, first, last)) = self.heap.pop() else { return false };
        let start = first.id_index();
        let end = last.id_index() + 1;

        assert!(end - start >= 2);

        self.order[start..end].sort_unstable_by_key(|&pos| Reverse(&self.prios[pos]));

        let mut equivs = start;

        let last = end - 1;
        for current in start..last {
            if eq(
                self.items[self.order[current]],
                self.items[self.order[current + 1]],
            ) {
                self.repr[self.order[current + 1]] = self.order[current];
                self.order.swap(current, current + 1);
                self.order.swap(current, equivs);
                equivs += 1;
            } else {
                self.order.swap(current, current + 1);
            }
        }

        self.repr[self.order[last]] = self.order[last];
        self.order.swap(last, equivs);
        equivs += 1;

        self.pending = Some((equivs, end));

        true
    }

    pub fn update(&mut self, mut value: impl FnMut(T) -> V) {
        let Some((start, end)) = self.pending.take() else { return };

        for i in start..end {
            self.values[self.order[i]] = value(self.items[self.order[i]]);
        }

        self.subdivide(start, end);

        let Some((_, first, last)) = self.heap.pop() else { return };
        let start = first.id_index();
        let end = last.id_index() + 1;

        for i in start..end {
            self.values[self.order[i]] = value(self.items[self.order[i]]);
        }

        self.subdivide(start, end);
    }

    fn subdivide(&mut self, start: usize, end: usize) {
        if start == end {
            return;
        }

        self.order[start..end].sort_unstable_by_key(|&pos| &self.values[pos]);

        self.subdivide_sorted(start, end)
    }

    fn subdivide_sorted(&mut self, start: usize, end: usize) {
        let len = end - start;

        if len <= 1 {
            assert_ne!(len, 0);
            let id = self.order[start];
            self.repr[id] = id;
            return;
        }

        let first_id = self.order[start];
        let last_id = self.order[end - 1];

        if self.values[first_id] == self.values[last_id] {
            let max_prio = Iterator::max((start..end).map(|i| self.prios[self.order[i]])).unwrap();

            self.heap.push((
                max_prio,
                Id::from_id_index(start),
                Id::from_id_index(end - 1),
            ));
            return;
        }

        let mut low = start;
        let mut span = end - start;

        let half = start + (end - start) / 2;

        while span > 1 {
            let step = span / 2;
            let mid = low + step;

            if self.values[self.order[mid]] == self.values[self.order[low]] {
                low += step;
                span -= step;
            } else if self.values[self.order[mid]] == self.values[self.order[low + span - 1]] {
                span = step;
            } else if mid < half {
                low += step;
                span -= step;
            } else {
                span = step;
            }
        }
        assert!(low >= start);
        assert!(low < end - 1);

        assert!(self.values[self.order[low]] != self.values[self.order[low + 1]]);

        self.subdivide_sorted(start, low + 1);
        self.subdivide_sorted(low + 1, end);
    }
}
