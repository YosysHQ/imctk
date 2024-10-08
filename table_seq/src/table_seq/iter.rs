use hashbrown::HashTable;

/// Iterator yielding references to a subtable's entries.
pub struct SubtableIter<'a, T> {
    inner: SubtableIterInner<'a, T>,
}

impl<T> Default for SubtableIter<'_, T> {
    fn default() -> Self {
        Self {
            inner: SubtableIterInner::Small([].as_slice().iter()),
        }
    }
}

impl<'a, T> SubtableIter<'a, T> {
    pub(crate) fn single(value: &'a T) -> Self {
        Self {
            inner: SubtableIterInner::Small(std::slice::from_ref(value).iter()),
        }
    }

    pub(crate) fn pair(pair: &'a [T; 2]) -> Self {
        Self {
            inner: SubtableIterInner::Small(pair.as_slice().iter()),
        }
    }

    pub(crate) fn small(values: &'a [T]) -> Self {
        Self {
            inner: SubtableIterInner::Small(values.iter()),
        }
    }

    pub(crate) fn table(table: &'a HashTable<T>) -> Self {
        Self {
            inner: SubtableIterInner::Table(table.iter()),
        }
    }
}

enum SubtableIterInner<'a, T> {
    Small(std::slice::Iter<'a, T>),
    Table(hashbrown::hash_table::Iter<'a, T>),
}

impl<'a, T> Iterator for SubtableIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            SubtableIterInner::Small(iter) => iter.next(),
            SubtableIterInner::Table(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.inner {
            SubtableIterInner::Small(iter) => iter.size_hint(),
            SubtableIterInner::Table(iter) => iter.size_hint(),
        }
    }
}

impl<T> ExactSizeIterator for SubtableIter<'_, T> {
    fn len(&self) -> usize {
        match &self.inner {
            SubtableIterInner::Small(iter) => iter.len(),
            SubtableIterInner::Table(iter) => iter.len(),
        }
    }
}

/// Iterator yielding mutable references to a subtable's entries.
pub struct SubtableIterMut<'a, T> {
    inner: SubtableIterMutInner<'a, T>,
}

impl<T> Default for SubtableIterMut<'_, T> {
    fn default() -> Self {
        Self {
            inner: SubtableIterMutInner::Small(Default::default()),
        }
    }
}

impl<'a, T> SubtableIterMut<'a, T> {
    pub(crate) fn single(value: &'a mut T) -> Self {
        Self {
            inner: SubtableIterMutInner::Small(std::slice::from_mut(value).iter_mut()),
        }
    }

    pub(crate) fn pair(pair: &'a mut [T; 2]) -> Self {
        Self {
            inner: SubtableIterMutInner::Small(pair.as_mut_slice().iter_mut()),
        }
    }

    pub(crate) fn small(values: &'a mut [T]) -> Self {
        Self {
            inner: SubtableIterMutInner::Small(values.iter_mut()),
        }
    }

    pub(crate) fn table(table: &'a mut HashTable<T>) -> Self {
        Self {
            inner: SubtableIterMutInner::Table(table.iter_mut()),
        }
    }
}

enum SubtableIterMutInner<'a, T> {
    Small(std::slice::IterMut<'a, T>),
    Table(hashbrown::hash_table::IterMut<'a, T>),
}

impl<'a, T> Iterator for SubtableIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            SubtableIterMutInner::Small(iter) => iter.next(),
            SubtableIterMutInner::Table(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.inner {
            SubtableIterMutInner::Small(iter) => iter.size_hint(),
            SubtableIterMutInner::Table(iter) => iter.size_hint(),
        }
    }
}

impl<T> ExactSizeIterator for SubtableIterMut<'_, T> {
    fn len(&self) -> usize {
        match &self.inner {
            SubtableIterMutInner::Small(iter) => iter.len(),
            SubtableIterMutInner::Table(iter) => iter.len(),
        }
    }
}
