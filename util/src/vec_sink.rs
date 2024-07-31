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

impl<'a, T> Deref for VecSink<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.target[self.fixed..]
    }
}

impl<'a, T> DerefMut for VecSink<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.target[self.fixed..]
    }
}

impl<'a, A> Extend<A> for VecSink<'a, A> {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        self.target.extend(iter)
    }
}
