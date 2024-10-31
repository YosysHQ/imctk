//! Append only wrappers for mutable [`Vec`] references.

use std::ops::{Deref, DerefMut};

/// An append only wrappers for a mutable [`Vec`] reference.
///
/// This remembers the vector's length at creation and will not allow modification or deletion
/// of any element that was already present at creation.
///
/// The [`Deref`] and [`DerefMut`] implementations provide access to the slice of newly added
/// elements.
pub struct VecSink<'a, T> {
    fixed: usize,
    target: &'a mut Vec<T>,
}

impl<'a, T> VecSink<'a, T> {
    /// Creates an append only wrapper that ensures the currently present elements will be
    /// preserved.
    pub fn new(target: &'a mut Vec<T>) -> Self {
        Self {
            fixed: target.len(),
            target,
        }
    }

    /// Append a new element.
    ///
    /// This forwards to [`Vec::push`].
    #[inline(always)]
    pub fn push(&mut self, value: T) {
        self.target.push(value)
    }

    /// Append all elements drained from the given vector.
    ///
    /// This forwards to [`Vec::append`].
    #[inline(always)]
    pub fn append(&mut self, values: &mut Vec<T>) {
        self.target.append(values)
    }

    /// Implements [`Vec::truncate`] limited to the newly added elements.
    ///
    /// The given length referes to the number of newly added elements to keep, not to the number of
    /// elements in the underlying `Vec`.
    pub fn truncate(&mut self, len: usize) {
        self.target.truncate(self.fixed.saturating_add(len));
    }

    /// Implements [`Vec::dedup`] limited to the newly added elements.
    ///
    /// Note that this does not compare the first newly added element with its predecessor in the
    /// underlying `Vec`.
    pub fn dedup(&mut self)
    where
        T: Eq,
    {
        let mut write = 1;
        for read in 1..self.len() {
            if self[write - 1] == self[read] {
                continue;
            }
            self.swap(write, read);
            write += 1;
        }
        self.truncate(write);
    }

    /// Re-borrows the existing [`VecSink`].
    pub fn borrow_mut(&mut self) -> VecSink<'_, T> {
        VecSink {
            fixed: self.fixed,
            target: self.target,
        }
    }

    /// Re-borrows as a new [`VecSink`] hiding and protecting any elements already added.
    pub fn borrow_sink(&mut self) -> VecSink<'_, T> {
        VecSink {
            fixed: self.target.len(),
            target: self.target,
        }
    }
}

impl<T> Deref for VecSink<'_, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.target[self.fixed..]
    }
}

impl<T> DerefMut for VecSink<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.target[self.fixed..]
    }
}

impl<A> Extend<A> for VecSink<'_, A> {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        self.target.extend(iter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_usage() {
        let mut vec = vec![0, 1, 2];

        let mut other_vec = vec![5, 6, 7];

        let mut sink = VecSink::new(&mut vec);

        sink.push(3);
        sink.push(4);

        let slice: &[usize] = &sink;
        assert_eq!(slice, [3, 4]);
        sink.append(&mut other_vec);
        let slice: &mut [usize] = &mut sink;
        assert_eq!(slice, [3, 4, 5, 6, 7]);

        slice.reverse();
        slice.reverse();

        assert_eq!(vec, Vec::from_iter(0..8));
    }

    #[test]
    fn recursive() {
        pub fn build(depth: usize, mut target: VecSink<usize>) {
            assert!(target.is_empty());
            target.extend(0..depth);
            if depth > 0 {
                build(depth - 1, target.borrow_sink());
            }
        }

        let mut vec = vec![];

        build(5, VecSink::new(&mut vec));

        assert_eq!(vec.len(), 6 * 5 / 2);
    }
}
