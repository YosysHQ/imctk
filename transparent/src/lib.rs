//! Safe casts for `repr(transparent)` structs.
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(missing_docs)]
#![warn(clippy::undocumented_unsafe_blocks)]

use std::{mem::ManuallyDrop, ptr::NonNull};

/// Derives a [`NewtypeCast`] instance for a `repr(transparent)` struct.
pub use imctk_derive::NewtypeCast;

/// Derives a [`SubtypeCast`] instance for a `repr(transparent)` struct.
pub use imctk_derive::SubtypeCast;

/// Provides read-only access to a `repr(transparent)` struct's data.
///
/// As this provides read-only access, this can be used for structs that are only safe to use with
/// some possible wrapped values, i.e. that behave somewhat like a subtype of the wrapped type.
///
/// # Safety
///
/// This may only be implemented if all values of `Self` can be safely transmuted into
/// [`Self::Repr`] values. Additionally it must be safe to transmute a `*mut Self` into a `*mut
/// Self::Repr`, i.e. they must have compatible pointer metadata.
///
/// For `repr(transparent)` structs, this trait can be safely derived.
pub unsafe trait SubtypeCast {
    /// Type of the `repr(transparent)` struct's only field.
    type Repr: ?Sized;

    /// Casts a value of a `repr(transparent)` wrapper into its inner type.
    #[inline(always)]
    fn into_repr(self) -> Self::Repr
    where
        Self: Sized,
        Self::Repr: Sized,
    {
        // SAFETY: transmutability is a documented trait-level requirement
        unsafe { core::mem::transmute_copy(&ManuallyDrop::new(self)) }
    }

    /// Casts a reference to a `repr(transparent)` wrapper into a reference to its inner type.
    #[inline(always)]
    fn as_repr(&self) -> &Self::Repr {
        // SAFETY: transmutability is a documented trait-level requirement
        unsafe { core::mem::transmute_copy::<&Self, &Self::Repr>(&self) }
    }

    /// Casts a mutable reference to a `repr(transparent)` wrapper into a mutable reference to
    /// its inner type without checking invariants.
    ///
    /// # Safety
    /// The caller must ensure that the subtype has fully documented invariants and that these are
    /// upheld by the provided value.
    #[inline(always)]
    unsafe fn from_repr_unchecked(repr: Self::Repr) -> Self
    where
        Self: Sized,
        Self::Repr: Sized,
    {
        // SAFETY: transmutability is a documented fn-level requirement
        unsafe { core::mem::transmute_copy(&ManuallyDrop::new(repr)) }
    }

    /// Casts a reference to an inner type into a reference of a `repr(transparent)` wrapper
    /// without checking invariants.
    ///
    /// # Safety
    /// The caller must ensure that the subtype has fully documented invariants and that these are
    /// upheld by the provided reference.
    #[inline(always)]
    unsafe fn from_repr_ref_unchecked(repr: &Self::Repr) -> &Self {
        // SAFETY: transmutability is a documented fn-level requirement
        unsafe { core::mem::transmute_copy::<&Self::Repr, &Self>(&repr) }
    }

    /// Casts a mutable reference to a `repr(transparent)` wrapper into a mutable reference to
    /// its inner type without checking invariants.
    ///
    /// # Safety
    /// The caller must ensure that the subtype has fully documented invariants and that these are
    /// upheld whenever the lifetime of the returned value ends. In particular this includes cases
    /// where the lifetime ends due to panic-unwinding.
    #[inline(always)]
    unsafe fn as_repr_mut_unchecked(&mut self) -> &mut Self::Repr {
        // SAFETY: transmutability is a documented fn-level requirement
        unsafe {
            &mut *core::mem::transmute_copy::<*mut Self, *mut Self::Repr>(&(self as *mut Self))
        }
    }

    /// Casts a mutable reference to an inner type into a mutable reference of a
    /// `repr(transparent)` wrapper without checking invariants.
    ///
    /// # Safety
    /// The caller must ensure that the subtype has fully documented invariants and that these are
    /// upheld by the provided reference.
    #[inline(always)]
    unsafe fn from_repr_mut_unchecked(repr: &mut Self::Repr) -> &mut Self {
        // SAFETY: transmutability is a documented fn-level requirement
        unsafe {
            &mut *core::mem::transmute_copy::<*mut Self::Repr, *mut Self>(
                &(repr as *mut Self::Repr),
            )
        }
    }

    #[doc(hidden)]
    const __STATIC_ASSERT_HELPER: () = ();
}

/// Provides mutable access to a `repr(transparent)` struct's data.
///
/// As this provides mutable access, it can only be used for structs that are safe to use with
/// arbitrary wrapped values. If this is not the case, only [`SubtypeCast`] can be implemented.
///
/// # Safety
///
/// This may only be implemented if all values of the implementing type can be safely transmuted
/// into [`<Self as SubtypeCast>::Repr`][`SubtypeCast::Repr`] values and if all `<Self as
/// SubtypeCast>::Repr` values can be safely transmuted back into the implementing type.
///
/// For `repr(transparent)` structs, this trait can be safely derived.
pub unsafe trait NewtypeCast: SubtypeCast {
    /// Casts a value of an inner type into a value of a `repr(transparent)` wrapper.
    #[inline(always)]
    fn from_repr(repr: Self::Repr) -> Self
    where
        Self: Sized,
        Self::Repr: Sized,
    {
        // SAFETY: transmutability is a documented trait-level requirement
        unsafe { Self::from_repr_unchecked(repr) }
    }

    /// Casts a reference to an inner type into a reference of a `repr(transparent)` wrapper.
    #[inline(always)]
    fn from_repr_ref(repr: &Self::Repr) -> &Self {
        // SAFETY: transmutability is a documented trait-level requirement
        unsafe { Self::from_repr_ref_unchecked(repr) }
    }

    /// Casts a mutable reference to a `repr(transparent)` wrapper into a mutable reference to
    /// its inner type.
    #[inline(always)]
    fn as_repr_mut(&mut self) -> &mut Self::Repr {
        // SAFETY: transmutability is a documented trait-level requirement
        unsafe { self.as_repr_mut_unchecked() }
    }

    /// Casts a mutable reference to an inner type into a mutable reference of a `repr(transparent)`
    /// wrapper.
    #[inline(always)]
    fn from_repr_mut(repr: &mut Self::Repr) -> &mut Self {
        // SAFETY: transmutability is a documented trait-level requirement
        unsafe { Self::from_repr_mut_unchecked(repr) }
    }
}

// SAFETY: We require compatible pointer metadata
unsafe impl<T: SubtypeCast + ?Sized> SubtypeCast for *const T {
    type Repr = *const T::Repr;
}
// SAFETY: We require compatible pointer metadata
unsafe impl<T: NewtypeCast + ?Sized> NewtypeCast for *const T {}

// SAFETY: We require compatible pointer metadata
unsafe impl<T: NewtypeCast + ?Sized> SubtypeCast for *mut T {
    type Repr = *mut T::Repr;
}

// SAFETY: We require compatible pointer metadata
unsafe impl<T: NewtypeCast + ?Sized> NewtypeCast for *mut T {}

// SAFETY: We require compatible pointer metadata
unsafe impl<T: NewtypeCast + ?Sized> SubtypeCast for NonNull<T> {
    type Repr = NonNull<T::Repr>;
}

// SAFETY: We require compatible pointer metadata
unsafe impl<T: NewtypeCast + ?Sized> NewtypeCast for NonNull<T> {}

// SAFETY: We require compatible pointer metadata, `Option<NonNull<T>>` has a defined repr.
unsafe impl<T: NewtypeCast + ?Sized> SubtypeCast for Option<NonNull<T>> {
    type Repr = Option<NonNull<T::Repr>>;
}

// SAFETY: We require compatible pointer metadata, `Option<NonNull<T>>` has a defined repr.
unsafe impl<T: NewtypeCast + ?Sized> NewtypeCast for Option<NonNull<T>> {}

// SAFETY: References are covariant wrt valid representations and we require compatible pointer
// metadata
unsafe impl<'a, T: SubtypeCast + ?Sized> SubtypeCast for &'a T {
    type Repr = &'a T::Repr;
}

// SAFETY: References are covariant wrt valid representations and we require compatible pointer
// metadata
unsafe impl<'a, T: NewtypeCast + ?Sized> NewtypeCast for &'a T {}

// SAFETY: Mutable references are invariant wrt valid representations and we require compatible
// pointer metadata
unsafe impl<'a, T: NewtypeCast + ?Sized> SubtypeCast for &'a mut T {
    type Repr = &'a mut T::Repr;
}

// SAFETY: Mutable references are invariant wrt valid representations and we require compatible
// pointer metadata
unsafe impl<'a, T: NewtypeCast + ?Sized> NewtypeCast for &'a mut T {}

// SAFETY: Slices are covariant wrt to valid item representations
unsafe impl<T: SubtypeCast> SubtypeCast for [T]
where
    T::Repr: Sized,
{
    type Repr = [T::Repr];
}

// SAFETY: Slices are covariant wrt to valid item representations
unsafe impl<T: NewtypeCast> NewtypeCast for [T] where T::Repr: Sized {}

// SAFETY: Arrays are covariant wrt to valid item representations
unsafe impl<T: SubtypeCast, const N: usize> SubtypeCast for [T; N]
where
    T::Repr: Sized,
{
    type Repr = [T::Repr];
}

// SAFETY: Arrays are covariant wrt to valid item representations
unsafe impl<T: NewtypeCast, const N: usize> NewtypeCast for [T; N] where T::Repr: Sized {}
