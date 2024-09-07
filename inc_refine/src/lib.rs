//! Incremental partition refinement.
//!
//! This provides an implementation of an incremental partition refinement data structure.
//! Incrementality is allowed in two different ways:
//!
//! 1. It is always possible to keep refining the partition, splitting classes into multiple
//!    classes. This is a usual requirement for partition refinement use cases.
//! 2. It is possible to add new elements of an unknown class to the partition. This means that the
//!    data structure doesn't represent a partition that happens to be incrementally updated, but
//!    represents what we will call an incremental partition.
//!
//! Such an incremental partition can be viewed as a forest of directed trees with the incremental
//! partition elements as vertices. Two elements are considered to be candidates for sharing a class
//! when either is an ancestor of the other.
//!
//! Any (sub-)tree can be refined using a class invariant function by labeling the subtree using the
//! class invariant values, duplicating the (sub-)tree for every observed value (all rooted at the
//! same parent) and then for each copy contracting all vertices that do not have the value
//! corresponding to that specific copy.
//!
//! When implementing this, the contracted copies can be constructed directly, avoiding any
//! intermediate blowup.
//!
//! The roots of the trees can serve as class representatives of the finest non-incremental
//! partition that is a coarsening of the incremental partition.
//!
//! A new element of an unknown class can be added by turning the forest into a single tree with the
//! new element as root.
//!
//! The data structure is implemented by maintaining:
//!
//! * A linked list representing an euler tour of the forest viewed as a tree with a virtual root
//!   not corresponding to an element, associating the unique incoming edge with each element. (An
//!   alternative perspective is that the forest is represented as an s-expr and we maintain a
//!   linked list of opening and closing parens.)
//!
//!   This requires fixing an order between siblings, which can be done arbitrarily.
//!
//! * An order maintenance data structure to quickly determine the relative order of two linked list
//!   elements. In this setting, this allows us to check whether one element is the parent of
//!   another.
//!
//! * Lazily updated shortcut links to find the first sibling among the children of the same parent
//!   and to find the root of any element.
//!
//! * Monotone linked list item timestamps that are updated whenever we move such an item into a new
//!   contracted "copy" and stored along the shortcut links so they can be invalidated when a target
//!   node moves. Note that the largest subclass during refinement doesn't have to move as we will
//!   implicitly obtain the contracted tree after having moved out all other elements.

// TODO fully document and add #![warn(missing_docs)]
#![deny(unsafe_code)]

use std::{
    hash::Hash,
    marker::PhantomData,
    mem::{swap, take},
    time::Duration,
};

use imctk_ids::{id_vec::IdVec, Id, Id32};
use zwohash::HashMap;

#[derive(Id, Debug)]
#[repr(transparent)]
struct NodeId(Id32);

impl Default for NodeId {
    fn default() -> Self {
        Self::MAX_ID
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(packed)]
struct Order(u64);

impl Default for Order {
    fn default() -> Self {
        Self(u64::MAX)
    }
}

#[derive(Clone, Debug)]
struct Node {
    prev: Option<NodeId>,
    next: Option<NodeId>,
    skip: NodeId,
    skip_timestamp: u32,
    order: Order,
}

impl Default for Node {
    fn default() -> Self {
        Self {
            prev: Default::default(),
            next: Default::default(),
            skip: NodeId::MIN_ID,
            skip_timestamp: 0,
            order: Default::default(),
        }
    }
}

impl NodeId {
    pub fn is_enter(self) -> bool {
        self.id_index() & 1 == 0
    }
    pub fn is_leave(self) -> bool {
        !self.is_enter()
    }
    pub fn other_end(self) -> Self {
        NodeId::from_id_index(self.id_index() ^ 1)
    }
}

#[derive(Clone, Copy, Debug)]
struct OrderNodeId {
    order: Order,
    node_id: NodeId,
}

impl Eq for OrderNodeId {}
impl PartialEq for OrderNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.order == other.order
    }
}

impl PartialOrd for OrderNodeId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OrderNodeId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.order.cmp(&other.order)
    }
}

#[derive(Clone, Copy)]
struct NodeRange {
    len: u32,
    first: NodeId,
    last: NodeId,
}

pub struct RefinementKeys<K> {
    by_key: HashMap<K, NodeRange>,
}

impl<K> Default for RefinementKeys<K> {
    fn default() -> Self {
        Self {
            by_key: Default::default(),
        }
    }
}

#[derive(Clone, Default)]
pub struct IncrementalRefinement<T: Id> {
    node: IdVec<NodeId, Node>,

    timestamp: IdVec<T, u32>,

    current_timestamp: u32,

    first: Option<NodeId>,
    last: Option<NodeId>,

    item_count: usize,
    tmp: Vec<OrderNodeId>,
    _phantom: PhantomData<T>,

    pub stats: [usize; 32],
    pub stats2: [Duration; 32],
}

impl<T: Id> IncrementalRefinement<T> {
    fn node_item(node_id: NodeId) -> T {
        T::from_id_index(node_id.id_index() >> 1)
    }
    fn enter_id(id: T) -> NodeId {
        NodeId::from_id_index(id.id_index() << 1)
    }
    fn leave_id(id: T) -> NodeId {
        NodeId::from_id_index((id.id_index() << 1) | 1)
    }

    fn bump_timestamp(&mut self) {
        self.current_timestamp += 2;
        if self.current_timestamp + 1 == u32::MAX {
            todo!("reset timestamps");
        }
    }

    fn get_skip(&self, node_id: NodeId, parent: bool) -> Option<NodeId> {
        let timestamp = self.node[node_id].skip_timestamp;
        if (timestamp & 1 != 0) != parent {
            None
        } else if self.timestamp[Self::node_item(self.node[node_id].skip)] == timestamp & !1 {
            Some(self.node[node_id].skip)
        } else {
            None
        }
    }

    fn set_skip(&mut self, node_id: NodeId, skip: NodeId, parent: bool) {
        let timestamp = self.timestamp[Self::node_item(skip)];
        self.node[node_id].skip = skip;
        self.node[node_id].skip_timestamp = timestamp | (parent as u32);
    }

    fn clear_skip(&mut self, node_id: NodeId) {
        self.node[node_id].skip_timestamp = 0;
    }

    pub fn insert_item(&mut self, id: T) -> bool {
        let enter = Self::enter_id(id);
        let leave = enter.other_end();
        let leave_node = self.node.grow_for_key(leave);
        if leave_node.prev.is_some() {
            return false;
        }

        self.bump_timestamp();
        *self.timestamp.grow_for_key_with(id, || u32::MAX) = self.current_timestamp;

        self.prepend_node(enter);
        self.clear_skip(enter);
        self.append_node(leave);
        self.clear_skip(leave);

        self.item_count += 1;

        true
    }

    pub fn remove_item(&mut self, id: T) -> bool {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { return false };
        if node.next.is_none() {
            return false;
        }
        self.timestamp[id] = u32::MAX;
        let leave = enter.other_end();

        self.unlink_node(enter);
        self.unlink_node(leave);

        self.item_count -= 1;
        true
    }

    pub fn contains_item(&self, id: T) -> bool {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { return false };
        node.next.is_some()
    }

    pub fn root(&mut self, id: T) -> T {
        let root = self.root_scan(id);
        self.root_update(id, root);
        root
    }

    fn root_scan(&mut self, mut id: T) -> T {
        loop {
            let first_sibling = self.first_sibling(id);

            let enter_first_sibling = Self::enter_id(first_sibling);
            let Some(parent) = self.node[enter_first_sibling].prev else {
                return id;
            };

            if let Some(candidate) = self.get_skip(enter_first_sibling, true) {
                if self.contains(Self::node_item(candidate), id) {
                    id = Self::node_item(candidate)
                } else {
                    id = Self::node_item(parent)
                }
            } else {
                id = Self::node_item(parent)
            }
        }
    }

    fn root_update(&mut self, mut id: T, root: T) {
        loop {
            let first_sibling = self.first_sibling(id);

            let enter_first_sibling = Self::enter_id(first_sibling);
            let Some(parent) = self.node[enter_first_sibling].prev else {
                return;
            };

            if let Some(candidate) = self.get_skip(enter_first_sibling, true) {
                if self.contains(Self::node_item(candidate), id) {
                    id = Self::node_item(candidate)
                } else {
                    id = Self::node_item(parent)
                }
            } else {
                id = Self::node_item(parent)
            }

            self.set_skip(enter_first_sibling, Self::leave_id(root), true);
        }
    }

    pub fn first_sibling(&mut self, id: T) -> T {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        if node.next.is_none() {
            panic!("item not present");
        }
        let value = self.first_sibling_scan(id);
        self.first_sibling_update(id, value);
        value
    }

    fn first_sibling_scan(&mut self, mut id: T) -> T {
        let mut counter = self.item_count * 2 + 4;
        loop {
            counter -= 1;
            if counter == 0 {
                panic!("unexpected loop in first_sibling_scan")
            }
            let enter = Self::enter_id(id);
            let prev_enter = if let Some(prev) = self.node[enter].prev {
                if prev.is_enter() {
                    return id;
                } else {
                    prev.other_end()
                }
            } else {
                return id;
            };

            let skip = if let Some(skip) = self.get_skip(enter, false) {
                if self.node[skip].order >= self.node[prev_enter].order {
                    prev_enter
                } else {
                    skip
                }
            } else {
                prev_enter
            };

            id = Self::node_item(skip);
        }
    }

    fn first_sibling_update(&mut self, mut id: T, value: T) {
        let mut counter = self.item_count * 2 + 4;
        loop {
            counter -= 1;
            if counter == 0 {
                panic!("unexpected loop in first_sibling_scan")
            }
            let enter = Self::enter_id(id);
            let prev_enter = if let Some(prev) = self.node[enter].prev {
                if prev.is_enter() {
                    return;
                } else {
                    prev.other_end()
                }
            } else {
                return;
            };

            let skip = if let Some(skip) = self.get_skip(enter, false) {
                if self.node[skip].order >= self.node[prev_enter].order {
                    prev_enter
                } else {
                    skip
                }
            } else {
                prev_enter
            };

            self.set_skip(enter, Self::enter_id(value), false);
            id = Self::node_item(skip);
        }
    }

    pub fn first_in_leaf_run(&mut self, id: T) -> T {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        if node.next.is_none() {
            panic!("item not present");
        }
        let value = self.first_in_leaf_run_scan(id);
        self.first_in_leaf_run_update(id, value);
        value
    }

    fn first_in_leaf_run_scan(&mut self, mut id: T) -> T {
        let mut counter = self.item_count * 2 + 4;
        loop {
            counter -= 1;
            if counter == 0 {
                panic!("unexpected loop in first_sibling_scan")
            }
            if !self.is_leaf(id) {
                return id;
            }

            let enter = Self::enter_id(id);
            let prev = self.node[enter].prev;

            let prev_enter = if let Some(prev) = prev {
                if prev.is_enter() || !self.is_leaf(Self::node_item(prev)) {
                    return id;
                } else {
                    prev.other_end()
                }
            } else {
                return id;
            };

            let leave = enter.other_end();

            let skip = if let Some(skip) = self.get_skip(leave, false) {
                if self.node[skip].order >= self.node[prev_enter].order {
                    prev_enter
                } else {
                    skip
                }
            } else {
                prev_enter
            };

            id = Self::node_item(skip);
        }
    }

    fn first_in_leaf_run_update(&mut self, mut id: T, value: T) {
        let mut counter = self.item_count * 2 + 4;
        loop {
            counter -= 1;
            if counter == 0 {
                panic!("unexpected loop in first_sibling_scan")
            }
            if !self.is_leaf(id) {
                return;
            }

            let enter = Self::enter_id(id);
            let prev = self.node[enter].prev;

            let prev_enter = if let Some(prev) = prev {
                if prev.is_enter() || !self.is_leaf(Self::node_item(prev)) {
                    return;
                } else {
                    prev.other_end()
                }
            } else {
                return;
            };

            let leave = enter.other_end();
            let skip = if let Some(skip) = self.get_skip(leave, false) {
                if self.node[skip].order >= self.node[prev_enter].order {
                    prev_enter
                } else {
                    skip
                }
            } else {
                prev_enter
            };

            self.set_skip(leave, Self::enter_id(value), false);
            id = Self::node_item(skip);
        }
    }

    pub fn parent(&mut self, id: T) -> Option<T> {
        let first_sibling = self.first_sibling(id);
        self.node[Self::enter_id(first_sibling)]
            .prev
            .map(Self::node_item)
    }

    pub fn descendant_count(&mut self, id: T) -> usize {
        let mut count = 0;

        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        let Some(mut iter) = node.next else {
            panic!("item not present");
        };

        let leave = enter.other_end();

        while iter != leave {
            count += 1;
            iter = self.node[iter].next.unwrap();
        }

        count / 2
    }

    pub fn child_count(&self, id: T) -> usize {
        let mut count = 0;

        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        let Some(mut iter) = node.next else {
            panic!("item not present");
        };

        let leave = enter.other_end();

        while iter != leave {
            debug_assert!(iter.is_enter());
            count += 1;
            iter = self.node[iter.other_end()].next.unwrap();
        }

        count
    }

    pub fn is_leaf(&self, id: T) -> bool {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        if node.next.is_none() {
            panic!("item not present");
        }
        node.next == Some(enter.other_end())
    }

    pub fn is_root(&mut self, id: T) -> bool {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        if node.next.is_none() {
            panic!("item not present");
        }
        let first_sibling = self.first_sibling(id);
        self.node[Self::enter_id(first_sibling)].prev.is_none()
    }

    pub fn is_isolated(&mut self, id: T) -> bool {
        self.is_leaf(id) && self.is_root(id)
    }

    pub fn ancestral_sibling_count(&mut self, id: T) -> usize {
        let mut count = 1; // For the root itself

        let mut root_iter = self.parent(id);
        let mut limit = 0;

        while let Some(current) = root_iter {
            limit += 1;
            if limit > self.item_count * 10 {
                panic!();
            }

            root_iter = self.parent(current);
            count += self.child_count(current);
        }

        count
    }

    pub fn root_count(&self) -> usize {
        let mut count = 0;
        let mut root_iter = self.first;

        while let Some(current) = root_iter {
            debug_assert!(current.is_enter());
            count += 1;
            root_iter = self.node[current.other_end()].next
        }

        count
    }

    pub fn contains(&self, outer: T, inner: T) -> bool {
        let enter_outer = Self::enter_id(outer);
        let Some(outer_node) = self.node.get(enter_outer) else {
            panic!("item {outer:?} not present");
        };
        if outer_node.next.is_none() {
            panic!("item {outer:?} not present");
        }
        let enter_inner = Self::enter_id(inner);
        let Some(inner_node) = self.node.get(enter_inner) else {
            panic!("item {inner:?} not present");
        };
        if inner_node.next.is_none() {
            panic!("item {inner:?} not present");
        }
        let leave_outer = enter_outer.other_end();

        outer_node.order < inner_node.order && inner_node.order < self.node[leave_outer].order
    }

    pub fn equiv(&mut self, items: [T; 2]) {
        let inner;
        let outer;
        if self.contains(items[0], items[1]) {
            [outer, inner] = items
        } else if self.contains(items[1], items[0]) {
            [inner, outer] = items
        } else {
            panic!("items {items:?} are non-overlapping");
        }

        let enter_outer = Self::enter_id(outer);
        let leave_outer = enter_outer.other_end();
        let enter_inner = Self::enter_id(inner);
        let leave_inner = enter_inner.other_end();

        self.unlink_node(enter_outer);
        self.unlink_node(leave_outer);

        self.clear_skip(enter_outer);
        self.insert_node_before(enter_outer, Some(enter_inner));
        self.clear_skip(leave_outer);
        self.insert_node_after(leave_outer, Some(leave_inner));

        self.bump_timestamp();
        self.timestamp[inner] = self.current_timestamp;
        self.timestamp[outer] = self.current_timestamp;
    }

    pub fn nonleaf_root_count(&mut self) -> usize {
        let mut count = 0;
        let mut root_rev_iter = self.last;

        while let Some(current) = root_rev_iter {
            if !current.is_leave() {
                println!("{:?}", self);
            }
            debug_assert!(
                current.is_leave(),
                "{current:?} {:?}",
                Self::node_item(current)
            );
            if self.is_leaf(Self::node_item(current)) {
                let first_leaf_in_run = self.first_in_leaf_run(Self::node_item(current));
                root_rev_iter = self.node[Self::enter_id(first_leaf_in_run)].prev;

                continue;
            }
            assert!(!self.is_leaf(Self::node_item(current)));
            count += 1;
            root_rev_iter = self.node[current.other_end()].prev;
        }

        count
    }

    pub fn nonleaf_root_iter(&mut self) -> impl Iterator<Item = T> + '_ {
        let mut root_rev_iter = self.last;

        std::iter::from_fn(move || loop {
            let current = root_rev_iter?;
            debug_assert!(current.is_leave());
            if self.is_leaf(Self::node_item(current)) {
                let first_leaf_in_run = self.first_in_leaf_run(Self::node_item(current));
                root_rev_iter = self.node[Self::enter_id(first_leaf_in_run)].prev;

                continue;
            }
            assert!(!self.is_leaf(Self::node_item(current)));

            root_rev_iter = self.node[current.other_end()].prev;
            return Some(Self::node_item(current));
        })
    }

    pub fn postorder_descendants_iter(&self, id: T) -> impl Iterator<Item = T> + '_ {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        let Some(mut iter) = node.next else {
            panic!("item not present");
        };
        let leave = enter.other_end();

        std::iter::from_fn(move || loop {
            if iter == leave {
                return None;
            } else {
                let current = iter;
                iter = self.node[current].next.unwrap();
                if current.is_leave() {
                    return Some(Self::node_item(current));
                }
            }
        })
    }

    pub fn child_iter(&self, id: T) -> impl Iterator<Item = T> + '_ {
        let enter = Self::enter_id(id);
        let Some(node) = self.node.get(enter) else { panic!("item not present") };
        let Some(mut iter) = node.next else {
            panic!("item not present");
        };
        let leave = enter.other_end();

        std::iter::from_fn(move || loop {
            if iter == leave {
                return None;
            } else {
                let current = iter;
                iter = self.node[current.other_end()].next.unwrap();
                if current.is_leave() {
                    return Some(Self::node_item(current));
                }
            }
        })
    }

    pub fn nonisolated_iter(&mut self) -> impl Iterator<Item = T> + '_ {
        self.nonisolated_with_root_iter().map(|(item, _root)| item)
    }

    pub fn nonisolated_with_root_iter(&mut self) -> impl Iterator<Item = (T, T)> + '_ {
        let mut root_rev_iter = self.last;

        let mut span: Option<(NodeId, NodeId)> = None;

        std::iter::from_fn(move || loop {
            if let Some((iter, leave)) = &mut span {
                if *iter == *leave {
                    span = None;
                } else {
                    *iter = self.node[*iter].next.unwrap();
                    let current = *iter;
                    if current.is_leave() {
                        return Some((Self::node_item(current), Self::node_item(*leave)));
                    } else {
                        continue;
                    }
                }
            }
            let current = root_rev_iter?;
            debug_assert!(current.is_leave());

            if self.is_leaf(Self::node_item(current)) {
                let first_leaf_in_run = self.first_in_leaf_run(Self::node_item(current));
                root_rev_iter = self.node[Self::enter_id(first_leaf_in_run)].prev;

                continue;
            }
            assert!(!self.is_leaf(Self::node_item(current)));

            root_rev_iter = self.node[current.other_end()].prev;

            if self.is_leaf(Self::node_item(current)) {
                continue;
            }
            let enter = current.other_end();
            span = Some((enter, current));
        })
    }

    pub fn item_count(&self) -> usize {
        self.item_count
    }

    pub fn refine_all<K>(&mut self, keys: &mut RefinementKeys<K>, mut key: impl FnMut(T) -> K)
    where
        K: Eq + Hash,
    {
        let mut root_rev_iter = self.last;

        while let Some(current) = root_rev_iter {
            debug_assert!(current.is_leave());

            if self.is_leaf(Self::node_item(current)) {
                let first_leaf_in_run = self.first_in_leaf_run(Self::node_item(current));
                root_rev_iter = self.node[Self::enter_id(first_leaf_in_run)].prev;

                continue;
            }
            assert!(!self.is_leaf(Self::node_item(current)));

            root_rev_iter = self.node[current.other_end()].prev;

            if self.is_leaf(Self::node_item(current)) {
                continue;
            }

            let root_leave = current;
            let root_enter = current.other_end();
            self.refine_inner(root_enter, root_leave, None, &mut keys.by_key, &mut key)
        }

        #[cfg(debug_assertions)]
        self.check_linked_list_nesting_and_order();
    }

    pub fn refine_subtree<K>(
        &mut self,
        keys: &mut RefinementKeys<K>,
        subtree: T,
        mut key: impl FnMut(T) -> K,
    ) where
        K: Eq + Hash,
    {
        let insert_before = self.parent(subtree).map(|parent| Self::leave_id(parent));

        let enter = Self::enter_id(subtree);
        let leave = enter.other_end();

        self.refine_inner(enter, leave, insert_before, &mut keys.by_key, &mut key);

        #[cfg(debug_assertions)]
        self.check_linked_list_nesting_and_order();
    }

    fn refine_inner<K>(
        &mut self,
        first: NodeId,
        last: NodeId,
        insert_before: Option<NodeId>,
        by_key: &mut HashMap<K, NodeRange>,
        key: &mut impl FnMut(T) -> K,
    ) where
        K: Eq + Hash,
    {
        by_key.clear();

        let mut current = first;

        let end = self.node[last].next;

        loop {
            let current_iter = self.node[current].next;

            self.unlink_node(current);

            let key = key(Self::node_item(current));
            match by_key.entry(key) {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    let class = entry.into_mut();

                    class.len += 1;
                    self.node[current].prev = Some(class.last);
                    self.node[class.last].next = Some(current);
                    class.last = current;
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(NodeRange {
                        len: 1,
                        first: current,
                        last: current,
                    });
                }
            }
            if current == last {
                break;
            }
            current = current_iter.unwrap();
        }

        let mut drain_by_key = by_key.drain().map(|(_, value)| value);
        let mut largest_class = drain_by_key.next().unwrap();

        let mut others: Option<NodeRange> = None;

        for mut current_class in drain_by_key {
            if current_class.len > largest_class.len {
                swap(&mut current_class, &mut largest_class);
            }

            if let Some(others) = &mut others {
                self.node[current_class.first].prev = Some(others.last);
                self.node[others.last].next = Some(current_class.first);
                others.last = current_class.last;
            } else {
                others = Some(current_class);
            }
        }

        if let Some(next_root_enter) = end {
            if let Some(prev_root_leave) =
                self.node[next_root_enter].prev.replace(largest_class.last)
            {
                self.node[prev_root_leave].next = Some(largest_class.first);
                self.node[largest_class.first].prev = Some(prev_root_leave);
            } else {
                self.first = Some(largest_class.first);
            }
            self.node[largest_class.last].next = Some(next_root_enter);
        } else {
            // there's no following root so we append the range
            if let Some(last) = self.last.replace(largest_class.last) {
                self.node[last].next = Some(largest_class.first);
                self.node[largest_class.first].prev = Some(last);
            } else {
                self.first = Some(largest_class.first);
            }
        }

        let mut refined_iter = others.map(|range| range.first);

        while let Some(refined) = refined_iter {
            self.node[refined].prev = None;
            refined_iter = self.node[refined].next.take();

            self.clear_skip(refined);
            self.insert_node_before(refined, insert_before);
        }
    }

    pub fn refine_items(&mut self, items: impl IntoIterator<Item = T>) {
        self.tmp.clear();
        let mut nodes_to_move = take(&mut self.tmp);

        for id in items {
            if !self.contains_item(id) {
                continue;
            }
            let enter = Self::enter_id(id);
            let leave = enter.other_end();
            nodes_to_move.push(OrderNodeId {
                order: self.node[enter].order,
                node_id: enter,
            });
            nodes_to_move.push(OrderNodeId {
                order: self.node[leave].order,
                node_id: leave,
            });
            self.unlink_node(enter);
            self.unlink_node(leave);
        }

        if nodes_to_move.is_empty() {
            return;
        }

        nodes_to_move.sort_unstable();

        for OrderNodeId { node_id, .. } in nodes_to_move.drain(..) {
            self.clear_skip(node_id);
            self.append_node(node_id);
        }

        self.tmp = nodes_to_move;
    }

    fn unlink_node(&mut self, node_id: NodeId) {
        let prev = self.node[node_id].prev.take();
        let next = self.node[node_id].next.take();

        if let Some(prev) = prev {
            debug_assert_eq!(self.node[prev].next, Some(node_id));
            self.node[prev].next = next;
        } else {
            self.first = next;
        }

        if let Some(next) = next {
            debug_assert_eq!(self.node[next].prev, Some(node_id));
            self.node[next].prev = prev;
        } else {
            self.last = prev;
        }

        self.bump_timestamp();
        self.timestamp[Self::node_item(node_id)] = self.current_timestamp;

        debug_assert_eq!(self.first.is_none(), self.last.is_none());
    }

    fn append_node(&mut self, node_id: NodeId) {
        debug_assert_eq!(self.node[node_id].prev, None);
        debug_assert_eq!(self.node[node_id].next, None);

        self.bump_timestamp();
        self.timestamp[Self::node_item(node_id)] = self.current_timestamp;

        if let Some(last) = self.last.replace(node_id) {
            debug_assert_eq!(self.node[last].next, None);
            self.node[node_id].prev = Some(last);
            self.node[last].next = Some(node_id);

            let low = self.node[last].order.0 + 1;
            let select = low + ((u64::MAX - low) >> 32);

            self.node[node_id].order = Order(select);
            if select == low {
                self.fixup_order(node_id);
            }
        } else {
            debug_assert_eq!(self.first, None);
            self.first = Some(node_id);
            self.last = Some(node_id);
            self.node[node_id].order = Order(u64::MAX / 2);
        }
    }

    fn prepend_node(&mut self, node_id: NodeId) {
        debug_assert_eq!(self.node[node_id].prev, None);
        debug_assert_eq!(self.node[node_id].next, None);

        self.bump_timestamp();
        self.timestamp[Self::node_item(node_id)] = self.current_timestamp;

        if let Some(first) = self.first.replace(node_id) {
            debug_assert_eq!(self.node[first].prev, None);
            self.node[node_id].next = Some(first);
            self.node[first].prev = Some(node_id);

            let high = self.node[first].order.0 - 1;
            let select = high - (high >> 32);

            self.node[node_id].order = Order(select);
            if select == high {
                self.fixup_order(node_id);
            }
        } else {
            debug_assert_eq!(self.last, None);
            self.first = Some(node_id);
            self.last = Some(node_id);
            self.node[node_id].order = Order(u64::MAX / 2);
        }
    }

    fn insert_node_after(&mut self, node_id: NodeId, target: Option<NodeId>) {
        let Some(target) = target else {
            self.prepend_node(node_id);
            return;
        };
        let Some(next) = self.node[target].next else {
            self.append_node(node_id);
            return;
        };

        self.insert_node_between(node_id, target, next);
    }

    fn insert_node_before(&mut self, node_id: NodeId, target: Option<NodeId>) {
        let Some(target) = target else {
            self.append_node(node_id);
            return;
        };
        let Some(prev) = self.node[target].prev else {
            self.prepend_node(node_id);
            return;
        };

        self.insert_node_between(node_id, prev, target);
    }

    fn insert_node_between(&mut self, node_id: NodeId, prev: NodeId, next: NodeId) {
        debug_assert_eq!(self.node[prev].next, Some(next));
        debug_assert_eq!(self.node[next].prev, Some(prev));

        self.bump_timestamp();
        self.timestamp[Self::node_item(node_id)] = self.current_timestamp;

        self.node[prev].next = Some(node_id);
        let low = self.node[prev].order.0 + 1;
        self.node[next].prev = Some(node_id);
        let high = self.node[next].order.0 - 1;

        debug_assert!(low <= high, "{low} {high}");

        let mid = low + ((high - low) >> 1);

        self.node[node_id].prev = Some(prev);
        self.node[node_id].next = Some(next);
        self.node[node_id].order.0 = mid;

        if low == mid || high == mid {
            self.fixup_order(node_id);
        }
    }

    fn fixup_order(&mut self, node_id: NodeId) {
        let mut next = self.node[node_id].next;
        let mut prev = self.node[node_id].prev;

        let mut next_order;
        let mut prev_order;

        let mut len = 2u64;
        loop {
            next_order = next.map(|next| self.node[next].order.0).unwrap_or(u64::MAX);
            prev_order = prev.map(|prev| self.node[prev].order.0).unwrap_or(u64::MIN);

            if (next_order - prev_order) > 2 * len.pow(2) {
                break;
            }

            assert!(next.is_some() || prev.is_some());

            if let Some(have_next) = next {
                next = self.node[have_next].next;
                len += 1;
            }

            if let Some(have_prev) = prev {
                prev = self.node[have_prev].prev;
                len += 1;
            }
        }

        let mut order = prev_order;
        let mut order_fract = 0;
        let total = next_order - prev_order;
        let step = total / len;
        let step_fract = total % len;

        assert!(step >= 2);

        let mut current_iter = if let Some(prev) = prev {
            self.node[prev].next
        } else {
            self.first
        };

        while let Some(current) = current_iter {
            if Some(current) == next {
                break;
            }
            current_iter = self.node[current].next;

            let last_order = order;
            order += step;
            order_fract += step_fract;
            let extra = order_fract >= len;
            order += extra as u64;
            order_fract -= len * (extra as u64);
            assert!(order >= last_order + 2);
            assert!(order <= next_order - 2);

            self.node[current].order.0 = order;
        }
    }

    fn check_linked_list_nesting_and_order(&self) {
        let mut node_iter = self.first;

        let mut prev = None;

        let mut stack = vec![];

        while let Some(current) = node_iter {
            assert_eq!(self.node[current].prev, prev);
            prev = node_iter;
            node_iter = self.node[current].next;

            if let Some(prev) = self.node[current].prev {
                assert!(
                    self.node[current]
                        .order
                        .0
                        .saturating_sub(self.node[prev].order.0)
                        >= 2
                );
            }
            if let Some(next) = self.node[current].next {
                assert!(
                    self.node[next]
                        .order
                        .0
                        .saturating_sub(self.node[current].order.0)
                        >= 2
                );
            }

            if current.is_enter() {
                stack.push(Self::node_item(current));
            } else {
                assert_eq!(stack.pop(), Some(Self::node_item(current)));
            }
        }

        assert!(stack.is_empty());

        assert_eq!(self.last, prev);
    }

    fn print_ordered_nodes(&self, target: &mut impl std::fmt::Write) -> std::fmt::Result {
        let mut node_iter = self.first;
        let mut level = 0;

        let fmt_node = |id: NodeId| {
            imctk_util::fmt::fmt_closure(move |target| {
                let item = Self::node_item(id);
                let dir = if id.is_enter() { "enter" } else { "leave" };
                write!(target, "{item:?}.{dir}")
            })
        };

        let mut prev = None;

        // To make sure the output is never misleading when debugging this implementation itself
        self.check_linked_list_nesting_and_order();

        while let Some(current) = node_iter {
            assert_eq!(self.node[current].prev, prev);
            prev = node_iter;
            node_iter = self.node[current].next;

            if let Some(prev) = self.node[current].prev {
                assert!(self.node[prev].order.0 + 1 < self.node[current].order.0);
            }
            if let Some(next) = self.node[current].next {
                assert!(self.node[next].order.0 - 1 > self.node[current].order.0);
            }

            if current.is_leave() {
                level -= 1;
            }
            const MAX_EXPLICIT_LEVEL: usize = 40;
            if level < MAX_EXPLICIT_LEVEL {
                for _ in 0..level {
                    target.write_char('|')?;
                }
            } else {
                let left = MAX_EXPLICIT_LEVEL - 3 - level.ilog10() as usize;

                for _ in 0..left {
                    target.write_char('|')?;
                }
                write!(target, " x{level} ")?;
            }
            target.write_char(if current.is_enter() { '.' } else { '\'' })?;
            writeln!(
                target,
                "- {} T={} skip={} @ {}",
                fmt_node(current),
                self.timestamp[Self::node_item(current)],
                fmt_node(self.node[current].skip),
                self.node[current].skip_timestamp,
            )?;
            if current.is_enter() {
                level += 1;
            }
        }

        Ok(())
    }
}

impl<T: Id> std::fmt::Debug for IncrementalRefinement<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print_ordered_nodes(f)
    }
}

#[cfg(test)]
mod tests {
    use zwohash::HashSet;

    use super::*;

    #[test]
    fn test_refine_items() {
        let mut refine: IncrementalRefinement<u32> = Default::default();

        for bits in 3..10u32 {
            for i in 0..(1 << bits) {
                refine.insert_item(i);
            }

            let mut hashes: HashSet<u32> = Default::default();

            for i in 0u32..1 << bits {
                let hash = i.wrapping_mul(0x2c9277b5) & !(!0 << bits);
                hashes.insert(hash);
            }

            assert_eq!(hashes.len(), 1 << bits);

            for b in (0..bits).rev() {
                assert!(
                    (0u32..1 << bits).any(|i| !(refine.is_leaf(i) && refine.parent(i).is_none()))
                );
                refine.refine_items((0u32..1 << bits).filter(|i| {
                    let hash = i.wrapping_mul(0x2c9277b5) & !(!0 << bits);
                    (hash >> b) & 1 != 0
                }));

                refine.check_linked_list_nesting_and_order();
            }

            for i in 0u32..1 << bits {
                assert!(refine.is_leaf(i) && refine.parent(i).is_none());
            }
        }
    }

    #[test]
    fn test_refine_all() {
        let mut refine: IncrementalRefinement<u32> = Default::default();

        for bits in 3..10u32 {
            for i in 0..(1 << bits) {
                refine.insert_item(i);
            }

            let mut hashes: HashSet<u32> = Default::default();

            for i in 0u32..1 << bits {
                let hash = i.wrapping_mul(0x2c9277b5) & !(!0 << bits);
                hashes.insert(hash);
            }

            assert_eq!(hashes.len(), 1 << bits);

            for b in (0..bits.div_ceil(2)).rev() {
                assert!(
                    (0u32..1 << bits).any(|i| !(refine.is_leaf(i) && refine.parent(i).is_none()))
                );

                refine.refine_all(&mut Default::default(), |i| {
                    let hash = i.wrapping_mul(0x2c9277b5) & !(!0 << bits);

                    (hash >> (b * 2)) & 0b11
                });

                refine.check_linked_list_nesting_and_order();
            }

            for i in 0u32..1 << bits {
                assert!(refine.is_leaf(i) && refine.parent(i).is_none());
            }
        }
    }

    #[test]
    fn test_nonisolated_with_root_iter() {
        let mut refine: IncrementalRefinement<u32> = Default::default();

        for i in 0..10 {
            refine.insert_item(i);
        }

        for p in [2, 3, 5] {
            refine.refine_all(&mut Default::default(), |i| i % p == 0);
        }

        // 0 1 2 3 4 5 6 7 8 9
        // x   x   x   x   x
        // x     x     x     x
        // x         x

        let nonisolated_with_root = Vec::from_iter(refine.nonisolated_with_root_iter());
        assert_eq!(
            nonisolated_with_root,
            [(3, 9), (9, 9), (2, 8), (4, 8), (8, 8), (1, 7), (7, 7)],
        );
    }
}
