//! Ownership-transferring mutable references.
// TODO more detail, separate crate?
use std::ops::{Deref, DerefMut};

pub use std::mem::ManuallyDrop;

/// Safely constructs an ownership-transferring [`Take`] reference.
#[macro_export]
macro_rules! give {
    ($name:ident $(: $T:ty)? = $value:expr) => {
        let mut give = $crate::give_take::ManuallyDrop::new($value);
        // SAFETY: give is scoped to the macro, preventing the user from accessing it after
        // the ownership is transferred
        let $name = unsafe { $crate::give_take::Take::new(&mut give $(as &mut $crate::give_take::ManuallyDrop<$T>)? ) };
    };
}

pub use crate::give;

/// An ownership-transferring mutable reference.
#[repr(transparent)]
pub struct Take<'a, T: ?Sized>(&'a mut ManuallyDrop<T>);

impl<'a, T> Take<'a, T> {
    /// Returns the referenced value, taking ownership and moving it out of the referenced
    /// storage.
    pub fn take(self) -> T {
        // SAFETY: the ManuallyDrop::new ensures that our Drop impl doesn't run, which makes it
        // safe to pass along ownership using ManuallyDrop::take
        unsafe { ManuallyDrop::take(ManuallyDrop::new(self).deref_mut().0) }
    }
}

impl<'a, T: ?Sized> Take<'a, T> {
    /// Takes a value by reference from a mutable [`ManuallyDrop`] reference.
    ///
    /// # Safety
    /// This takes ownership of the passed value and thus has the same safety requirements as
    /// [`ManuallyDrop::take`].
    pub unsafe fn new(value: &'a mut ManuallyDrop<T>) -> Self {
        Self(value)
    }

    /// Takes a value from a mutable raw pointer.
    ///
    /// # Safety
    /// This takes ownership of the target value, but keeps using the pointed to storage. Thus
    /// callers must ensure that the pointed to storage remains allocated and untouched until
    /// the [`Take`] value is consumed.
    pub unsafe fn from_raw_ptr(value: *mut T) -> Self {
        // SAFETY: ManuallDrop has a transparent layout and we documented the storage lifetime
        // requirements
        Self(unsafe { &mut *(value as *mut ManuallyDrop<T>) })
    }

    /// Obtains a raw pointer to the taken value, transferring ownership to the caller.
    pub fn into_raw_ptr(self) -> *mut T {
        ManuallyDrop::new(self).deref_mut().0 as *mut ManuallyDrop<T> as *mut T
    }
}

impl<'a, T: ?Sized> Deref for Take<'a, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T: ?Sized> DerefMut for Take<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<'a, T: ?Sized> Drop for Take<'a, T> {
    #[inline(always)]
    fn drop(&mut self) {
        // SAFETY: we have ownership of the value and either pass ownership elsewhere in which
        // case the corresponding methods ensure this isn't called, or will have to eventually
        // drop the value we're still owning:
        unsafe { ManuallyDrop::drop(self.0) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn give_and_take() {
        give!(some_string = "allocated".to_string());

        fn take_some_string(some: Take<String>) -> String {
            some.take()
        }

        assert_eq!(take_some_string(some_string), "allocated");
    }
}
