//! Transparent [`slice`] and [`Vec`] wrappers with [`Id`] indexing.
use core::{
    fmt::{self, Debug},
    iter::FusedIterator,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use crate::{Id, IdRange};

/// Transparent [`[V]`][`slice`] wrapper, representing a collection that maps `K` keys to `V`
/// values.
///
/// It has entries `(k, v)` with `v` being the item at position [`k.id_index()`][Id::id_index] of
/// the wrapped slice. This means the keys always span a contiguous range of ids starting at at
/// [`K::MIN_ID`][Id::MIN_ID], having index `0`.
///
/// It comes with a guarantee that all entries have distinct and valid keys. Depending on the used
/// id type, this guarantee limits the maximum allowed length of the wrapped slice.
#[repr(transparent)]
pub struct IdSlice<K: Id, V> {
    _phantom: PhantomData<K>,
    // SAFETY: invariant `values.len() <= K::MAX_INDEX.saturating_add(1)``
    values: [V],
}

impl<K: Id, V> IdSlice<K, V> {
    /// Creates an `IdSlice` reference from a slice of values.
    ///
    /// # Panics
    ///
    /// Panics when `K` cannot index the full length of the slice.
    #[inline]
    pub fn from_slice(slice: &[V]) -> &Self {
        assert!(slice.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: explicit assert above
        unsafe { Self::from_slice_unchecked(slice) }
    }

    /// Creates an `IdSlice` reference from a slice of values without bounds checking.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that `K` can index the full length of the slice.
    #[inline(always)]
    pub unsafe fn from_slice_unchecked(slice: &[V]) -> &Self {
        debug_assert!(slice.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: transparent cast
        unsafe { &*(slice as *const [V] as *const Self) }
    }

    /// Creates a mutable `IdSlice` reference from a mutable slice of values.
    ///
    /// # Panics
    ///
    /// Panics when `K` cannot index the full length of the slice.
    #[inline]
    pub fn from_mut_slice(slice: &mut [V]) -> &mut Self {
        assert!(slice.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: explicit assert above
        unsafe { Self::from_mut_slice_unchecked(slice) }
    }

    /// Creates a mutable `IdSlice` reference from a mutable slice of values without bounds
    /// checking.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that `K` can index the full length of the slice.
    #[inline(always)]
    pub unsafe fn from_mut_slice_unchecked(slice: &mut [V]) -> &mut Self {
        debug_assert!(slice.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: transparent cast
        unsafe { &mut *(slice as *mut [V] as *mut Self) }
    }

    /// Returns the keys as a contiguous range of ids.
    #[inline(always)]
    pub fn keys(&self) -> IdRange<K> {
        // SAFETY: safe by struct level invariant
        unsafe { IdRange::from_index_range_unchecked(0..self.values.len()) }
    }

    /// Returns the values as a slice.
    ///
    /// This also provides access to the wrapped slice.
    #[inline(always)]
    pub fn values(&self) -> &[V] {
        &self.values
    }

    /// Returns the values as a mutable slice.
    ///
    /// This also provides mutable access to the wrapped slice.
    #[inline(always)]
    pub fn values_mut(&mut self) -> &mut [V] {
        &mut self.values
    }

    /// Returns an iterator over all entries using value references.
    ///
    /// Each entry is a `(K, &V)` pair.
    #[inline(always)]
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    /// Returns an iterator over all entries using mutable value references.
    ///
    /// Each entry is a `(K, &mut V)` pair.
    #[inline(always)]
    pub fn iter_mut(&mut self) -> <&mut Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    /// Returns the number of entries in the collection.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns `true` if there are no entries in the collection.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Returns a reference to the value associated with the given key without bounds checking.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that the key is valid for this mapping.
    #[inline(always)]
    pub unsafe fn get_unchecked(&self, key: K) -> &V {
        let index = key.id_index();
        debug_assert!(index < self.values.len());
        // SAFETY: caller requirements
        unsafe { self.values.get_unchecked(index) }
    }

    /// Returns a mutable reference to the value associated with the given key without bounds
    /// checking.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that the key is valid for this mapping.
    #[inline(always)]
    pub unsafe fn get_unchecked_mut(&mut self, key: K) -> &mut V {
        let index = key.id_index();
        debug_assert!(index < self.values.len());
        // SAFETY: caller requirements
        unsafe { self.values.get_unchecked_mut(index) }
    }

    /// Returns a reference to the value associated with the given key.
    ///
    /// Returns `None` when the key is out-of-bounds.
    #[inline(always)]
    pub fn get(&self, key: K) -> Option<&V> {
        self.values.get(key.id_index())
    }

    /// Returns a mutable reference to the value associated with the given key.
    ///
    /// Returns `None` when the key is out-of-bounds.
    #[inline(always)]
    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.values.get_mut(key.id_index())
    }

    /// Swaps the values associated with the two given keys.
    #[inline(always)]
    pub fn swap(&mut self, key_a: K, key_b: K) {
        self.values.swap(key_a.id_index(), key_b.id_index())
    }
}

impl<'a, K: Id, V> IntoIterator for &'a IdSlice<K, V> {
    type Item = (K, &'a V);

    type IntoIter =
        impl FusedIterator<Item = (K, &'a V)> + DoubleEndedIterator + ExactSizeIterator + 'a;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        let len = self.values.len();
        let values = &self.values;
        // SAFETY: safe by struct level invariant
        (0..len).map(move |i| unsafe { (K::from_id_index_unchecked(i), values.get_unchecked(i)) })
    }
}

impl<'a, K: Id, V> IntoIterator for &'a mut IdSlice<K, V> {
    type Item = (K, &'a mut V);

    type IntoIter =
        impl FusedIterator<Item = (K, &'a mut V)> + DoubleEndedIterator + ExactSizeIterator + 'a;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        let len = self.values.len();
        let ptr = self.values.as_mut_ptr();
        // SAFETY: safe by struct level invariant. Also note that we can't use a &mut slice as the
        // borrow checker doesn't know every yielded `i` is distinct
        (0..len).map(move |i| unsafe { (K::from_id_index_unchecked(i), &mut *ptr.add(i)) })
    }
}

impl<K: Id, V> Index<K> for IdSlice<K, V> {
    type Output = V;

    #[inline(always)]
    fn index(&self, index: K) -> &Self::Output {
        &self.values[index.id_index()]
    }
}

impl<K: Id, V> IndexMut<K> for IdSlice<K, V> {
    #[inline(always)]
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.values[index.id_index()]
    }
}

impl<K: Id, V> Index<IdRange<K>> for IdSlice<K, V> {
    type Output = [V];

    #[inline(always)]
    fn index(&self, index: IdRange<K>) -> &Self::Output {
        &self.values[index.indices()]
    }
}

impl<K: Id, V> IndexMut<IdRange<K>> for IdSlice<K, V> {
    #[inline(always)]
    fn index_mut(&mut self, index: IdRange<K>) -> &mut Self::Output {
        &mut self.values[index.indices()]
    }
}

impl<K: Id, U, V: PartialEq<U>> PartialEq<IdSlice<K, U>> for IdSlice<K, V> {
    #[inline(always)]
    fn eq(&self, other: &IdSlice<K, U>) -> bool {
        self.values == other.values
    }
}

impl<K: Id, V: Eq> Eq for IdSlice<K, V> {}

impl<K: Id, V: PartialOrd> PartialOrd for IdSlice<K, V> {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.values.partial_cmp(&other.values)
    }

    #[inline(always)]
    fn lt(&self, other: &Self) -> bool {
        self.values < other.values
    }

    #[inline(always)]
    fn le(&self, other: &Self) -> bool {
        self.values <= other.values
    }

    #[inline(always)]
    fn gt(&self, other: &Self) -> bool {
        self.values > other.values
    }

    #[inline(always)]
    fn ge(&self, other: &Self) -> bool {
        self.values >= other.values
    }
}

impl<K: Id, V: Ord> Ord for IdSlice<K, V> {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.values.cmp(&other.values)
    }
}

impl<K: Id, V: fmt::Debug> Debug for IdSlice<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

/// Transparent [`Vec`] wrapper, representing a collection that maps `K` keys to `V` values.
///
/// It has entries `(k, v)` with `v` being the item at position [`k.id_index()`][Id::id_index] of
/// the wrapped vector. This means the keys always span a contiguous range of ids starting at at
/// [`K::MIN_ID`][Id::MIN_ID], having index `0`.
///
/// It comes with a guarantee that all entries have distinct and valid keys. Depending on the used
/// id type, this guarantee limits the maximum allowed length of the wrapped vector.
#[repr(transparent)]
pub struct IdVec<K, V> {
    // SAFETY: invariant `values.len() <= K::MAX_ID_INDEX.saturating_add(1)``
    values: Vec<V>,
    _phantom: PhantomData<K>,
}

impl<K: Id, V: Clone> Clone for IdVec<K, V> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self {
            values: self.values.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<K: Id, V> Default for IdVec<K, V> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            values: Default::default(),
            _phantom: PhantomData,
        }
    }
}

impl<K: Id, V> Deref for IdVec<K, V> {
    type Target = IdSlice<K, V>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: safe by struct level invariant (same invariant as IdSlice)
        unsafe { IdSlice::from_slice_unchecked(&self.values) }
    }
}

impl<K: Id, V> DerefMut for IdVec<K, V> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: safe by struct level invariant (same invariant as IdSlice)
        unsafe { IdSlice::from_mut_slice_unchecked(&mut self.values) }
    }
}

impl<K: Id, V> IdVec<K, V> {
    /// Creates an `IdVec` from a vector of values.
    #[inline]
    pub fn from_vec(vec: Vec<V>) -> Self {
        assert!(vec.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: explicit assert above
        unsafe { Self::from_vec_unchecked(vec) }
    }

    /// Creates an `IdVec` from a vector of values without bounds checking.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that `K` can index the full length of the vector.
    #[inline(always)]
    pub unsafe fn from_vec_unchecked(vec: Vec<V>) -> Self {
        debug_assert!(vec.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: transparent cast
        Self {
            values: vec,
            _phantom: PhantomData,
        }
    }

    /// Creates an `IdVec` reference from a reference to a vector of values.
    ///
    /// # Panics
    ///
    /// Panics when `K` cannot index the full length of the vector.
    #[inline]
    pub fn from_vec_ref(vec: &Vec<V>) -> &Self {
        assert!(vec.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: explicit assert above
        unsafe { Self::from_vec_ref_unchecked(vec) }
    }

    /// Creates an `IdVec` reference from a reference to a vector of values without bounds checking.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that `K` can index the full length of the vector.
    #[inline(always)]
    pub unsafe fn from_vec_ref_unchecked(vec: &Vec<V>) -> &Self {
        debug_assert!(vec.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: transparent cast
        unsafe { &*(vec as *const Vec<V> as *const Self) }
    }

    /// Creates a mutable `IdVec` reference from a mutable reference to a vector of values.
    ///
    /// # Panics
    ///
    /// Panics when `K` cannot index the full length of the vector.
    #[inline]
    pub fn from_vec_mut_ref(vec: &mut Vec<V>) -> &mut Self {
        assert!(vec.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: explicit assert above
        unsafe { Self::from_vec_mut_ref_unchecked(vec) }
    }

    /// Creates a mutable `IdVec` reference from a mutable reference to a vector of values without
    /// bounds checking.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that `K` can index the full length of the vector.
    #[inline(always)]
    pub unsafe fn from_vec_mut_ref_unchecked(vec: &mut Vec<V>) -> &mut Self {
        debug_assert!(vec.len() <= K::MAX_ID_INDEX.saturating_add(1));
        // SAFETY: transparent cast
        unsafe { &mut *(vec as *mut Vec<V> as *mut Self) }
    }

    /// Returns a reference to the values.
    ///
    /// This also provides immutable access to the wrapped vector.
    #[inline(always)]
    pub fn values(&self) -> &Vec<V> {
        &self.values
    }

    /// Converts this collection into a vector of the contained values.
    ///
    /// This also provides owned access to the wrapped vector.
    #[inline(always)]
    pub fn into_values(self) -> Vec<V> {
        self.values
    }

    /// Returns a mutable reference to the values.
    ///
    /// This also provides mutable access to the wrapped vector.
    ///
    /// # Safety
    ///
    /// The caller has to ensure that the returned vector does not grow in excess of what `K` can
    /// index.
    #[inline(always)]
    pub unsafe fn values_mut_unchecked(&mut self) -> &mut Vec<V> {
        &mut self.values
    }

    /// Inserts a value as a new entry, using the id with the smallest available index as key.
    ///
    /// This returns the used key and a mutable reference to the just inserted value.
    #[inline(always)]
    pub fn push(&mut self, value: V) -> (K, &mut V) {
        let index = self.values.len();
        let key = K::from_id_index(index);
        self.values.push(value);

        // SAFETY: the `K::from_index` calls performs the requried checks for our invariant
        (key, unsafe { self.values.get_unchecked_mut(index) })
    }

    /// Removes and returns the entry with the id having the largest used index.
    #[inline(always)]
    pub fn pop(&mut self) -> Option<(K, V)> {
        let value = self.values.pop()?;
        let key = K::from_id_index(self.values.len());
        Some((key, value))
    }

    /// Returns the id with the smallest available index.
    ///
    /// This is the same key that would be used when calling [`push`][Self::push].
    #[inline(always)]
    pub fn next_unused_key(&self) -> K {
        K::from_id_index(self.len())
    }

    /// Appends default values until there is an entry with the given key.
    #[inline(always)]
    pub fn grow_for_key(&mut self, key: K) -> &mut V
    where
        V: Default,
    {
        self.grow_for_key_with(key, Default::default)
    }

    /// Appends values using the given closure until there is an entry with the given key.
    #[inline(always)]
    pub fn grow_for_key_with(&mut self, key: K, f: impl Fn() -> V) -> &mut V {
        if self.len() <= key.id_index() {
            self.values.resize_with(key.id_index() + 1, f)
        }
        &mut self[key]
    }

    /// Resizes the collection, creating new entries by cloning the given value.
    #[inline]
    pub fn resize(&mut self, len: usize, value: V)
    where
        V: Clone,
    {
        assert!(len <= K::MAX_ID_INDEX.saturating_add(1));
        self.values.resize(len, value)
    }

    /// Resizes the collection, creating new entries by calling the given closure.
    #[inline]
    pub fn resize_with(&mut self, len: usize, value: impl FnMut() -> V) {
        assert!(len <= K::MAX_ID_INDEX.saturating_add(1));
        self.values.resize_with(len, value)
    }

    /// Shrinks the collection, dropping all but the first `len` entries.
    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.values.truncate(len)
    }

    /// Inserts a vector of values using the smallest available ids as keys.
    #[inline]
    pub fn append(&mut self, entries: &mut Vec<V>) {
        let mut taken = std::mem::take(self).into_values();
        taken.append(entries);
        *self = Self::from_vec(taken)
    }

    /// Inserts values from an iterator using the smallest available ids as keys.
    #[inline]
    pub fn extend_values(&mut self, iter: impl IntoIterator<Item = V>) {
        // TODO document why we're not implementing the Extend trait
        self.modify_values(|values| values.extend(iter))
    }

    /// Removes all entries of the collection.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.values.clear();
    }

    /// Removes the entry with the given key, returning its value. If there is an entry with a
    /// larger key id, its key is changed to the key of the removed entry.
    #[inline(always)]
    pub fn swap_remove(&mut self, key: K) -> V {
        self.values.swap_remove(key.id_index())
    }

    /// Modifies the underlying [`Vec`] of values.
    #[inline(always)]
    pub fn modify_values<R>(&mut self, f: impl FnOnce(&mut Vec<V>) -> R) -> R {
        let guard = <LengthGuard<K, V>>::new(&mut self.values);
        f(guard.values)
    }

    /// Removes all entries, returning an iterator yielding the removed entries in order.
    pub fn drain_all_values(&mut self) -> std::vec::Drain<'_, V> {
        self.values.drain(..)
    }

    /// Retains only those entries where the given predicate holds for the value. Keys following the
    /// first removed entry are changed such that entry position and key id index remain the same
    /// for all entries.
    pub fn retain_values(&mut self, f: impl FnMut(&V) -> bool) {
        self.values.retain(f)
    }
}

struct LengthGuard<'a, K: Id, V> {
    _phantom: PhantomData<K>,
    values: &'a mut Vec<V>,
}

impl<'a, K: Id, V> LengthGuard<'a, K, V> {
    #[inline(always)]
    fn new(values: &'a mut Vec<V>) -> Self {
        Self {
            _phantom: PhantomData,
            values,
        }
    }
}

impl<'a, K: Id, V> Drop for LengthGuard<'a, K, V> {
    #[inline]
    fn drop(&mut self) {
        if self.values.len() > K::MAX_ID_INDEX.saturating_add(1) {
            self.values.clear();
            panic!("modified values vector length exceeds number of available ids");
        }
    }
}

impl<K: Id, V> IntoIterator for IdVec<K, V> {
    type Item = (K, V);

    type IntoIter = impl FusedIterator<Item = (K, V)> + DoubleEndedIterator + ExactSizeIterator;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter().enumerate().map(|(i, v)| {
            // SAFETY: safe by struct level invariant
            (unsafe { K::from_id_index_unchecked(i) }, v)
        })
    }
}

impl<'a, K: Id, V> IntoIterator for &'a IdVec<K, V> {
    type Item = (K, &'a V);

    type IntoIter = <&'a IdSlice<K, V> as IntoIterator>::IntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.deref().into_iter()
    }
}

impl<'a, K: Id, V> IntoIterator for &'a mut IdVec<K, V> {
    type Item = (K, &'a mut V);

    type IntoIter = <&'a mut IdSlice<K, V> as IntoIterator>::IntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.deref_mut().into_iter()
    }
}

impl<K: Id, V: fmt::Debug> Debug for IdVec<K, V> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn forwarded_instances() {
        for i in 0..16 {
            let mut arr_i: [bool; 4] = std::array::from_fn(|b| (i >> b) & 1 != 0);
            let id_slice_i = <IdSlice<u32, _>>::from_slice(&arr_i);

            for j in 0..16 {
                let arr_j: [bool; 4] = std::array::from_fn(|b| (j >> b) & 1 != 0);

                let id_slice_j = <IdSlice<u32, _>>::from_slice(&arr_j);

                assert_eq!(id_slice_i == id_slice_j, arr_i == arr_j);
                assert_eq!(id_slice_i <= id_slice_j, arr_i <= arr_j);
                assert_eq!(id_slice_i >= id_slice_j, arr_i >= arr_j);
                assert_eq!(id_slice_i > id_slice_j, arr_i > arr_j);
                assert_eq!(id_slice_i < id_slice_j, arr_i < arr_j);
                assert_eq!(id_slice_i.cmp(id_slice_j), arr_i.cmp(&arr_j));
                assert_eq!(
                    id_slice_i.partial_cmp(id_slice_j),
                    arr_i.partial_cmp(&arr_j)
                );
            }

            for u in 0..=4 {
                for v in u..=4 {
                    assert_eq!(id_slice_i[IdRange::from_index_range(u..v)], arr_i[u..v]);
                }
            }

            let before = arr_i;

            for u in 0..=4 {
                for v in u..=4 {
                    let id_slice_i = <IdSlice<u32, _>>::from_mut_slice(&mut arr_i);
                    id_slice_i[IdRange::from_index_range(u..v)].reverse();
                    arr_i[u..v].reverse();
                }
            }

            assert_eq!(arr_i, before);

            for u in 0..4 {
                for v in 0..4 {
                    let id_slice_i = <IdSlice<u32, _>>::from_mut_slice(&mut arr_i);
                    id_slice_i.swap(u as u32, v as u32);
                    arr_i.swap(u, v);
                }
            }
        }
    }
}
