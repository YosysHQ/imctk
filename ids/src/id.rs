use core::{fmt::Debug, hash::Hash};

mod id_types;
mod primitive_impls;
mod u8_range_types;

/// Types that represent integer ids
///
/// A type of this trait represents an `usize` index value in the range `0..=Self::MAX_INDEX`, with
/// the specific representation used is up to the implementing type. Implementing types can have a
/// smaller size or alignment compared to `usize`, and can leave space for rustc's niche-value
/// optimization.
///
/// # Safety
/// This trait comes with several safety critical requirements for implementing types:
///
/// * The [`Clone`] implementation must return a [`Copy`].
/// * The [`PartialEq`], [`Eq`], [`PartialOrd`] and [`Ord`] implementation must behave as if this
///   type was a `struct` containing [`index: usize`][`Self::index()`] as only field and a
///   `#[derive(PartialEq, Eq, PartialOrd, Ord)]` attribute.
/// * Each valid index must have a unique representation , i.e. it must be possible to check two ids
///   for equality by comparing their raw bytes. This only applies to equality, and does not extend
///   to [`PartialOrd`] or [`Ord`].
/// * The [`Hash`] implementation must be deterministic and only depend on the represented index.
/// * The associated [`Self::Base`] type must be an [`Id`] with the same representation and index range (it can be `Self`).
/// * The associated [`Self::Generic`] type must be [`GenericId<{Self::MAX_INDEX}>`][`GenericId`].
/// * All implemented trait items must confirm to the their documented behavior.
///
/// Users of this trait may depend on implementing types following these requirements for upholding
/// their own safety invariants.
pub unsafe trait Id: Copy + Ord + Hash + Send + Sync + Debug {
    /// An [`Id`] type that has the same representation and index range as this type.
    ///
    /// This is provided to enable writing generic code that during monomorphization is only
    /// instantiated once per base type instead of once per id type.
    ///
    /// For types that are not newtype wrappers around an existing id type, this is usually
    /// [`Self`].
    type Base: Id<Base = Self::Base>;

    /// The [`GenericId`] type, parametrized to have the same index range as this type.
    ///
    /// This is used for type checked conversions between ids that do not have to share the same
    /// representation.
    type Generic: Id<Generic = Self::Generic>;

    /// Returns the id with a given index, panicking when the index is invalid.
    ///
    /// This panics if and only if `index > Self::MAX_INDEX`.
    #[inline(always)]
    #[track_caller]
    fn from_index(index: usize) -> Self {
        assert!(index <= Self::MAX_INDEX);
        // SAFETY: index asserted to be in range
        unsafe { Self::from_index_unchecked(index) }
    }

    /// Returns the index represented by this id.
    fn index(self) -> usize;

    /// The largest index representable by this id type.
    const MAX_INDEX: usize;
    /// The id with index zero.
    const MIN: Self;
    /// The id with the largest representable index.
    const MAX: Self;

    /// Returns the id with a given index, assuming a valid index.
    ///
    /// # Safety
    /// This is only safe to call when `index <= Self::MAX_INDEX`, which is not checked by this
    /// method.
    ///
    /// Implementations are encouraged to include a debug-only assertion for this requirement.
    unsafe fn from_index_unchecked(index: usize) -> Self;

    /// Returns the id with a given index, if it is valid.
    ///
    /// This returns `None` if and only if `index > Self::MAX_INDEX`.
    ///
    /// Never panics.
    #[inline(always)]
    fn try_from_index(index: usize) -> Option<Self> {
        if index <= Self::MAX_INDEX {
            // SAFETY: index checked to be in range
            Some(unsafe { Self::from_index_unchecked(index) })
        } else {
            None
        }
    }

    /// Returns a [`Self::Base`] id of the same index.
    ///
    /// This cannot fail and will never panic.
    #[inline(always)]
    fn into_base_id(self) -> Self::Base {
        // SAFETY: we require identical representations of Self and Self::Base
        unsafe { std::mem::transmute_copy(&self) }
    }

    /// Returns an id with the same index as a [`Self::Base`] id.
    ///
    /// This cannot fail and will never panic.
    #[inline(always)]
    fn from_base_id(base: Self::Base) -> Self {
        // SAFETY: we require identical representations of Self and Self::Base
        unsafe { std::mem::transmute_copy(&base) }
    }

    /// Returns a value of a compatible [`Id`] type having the same index.
    ///
    /// Two id types are considered compatible when the have the same [`Self::Base`] type.
    ///
    /// As compatible types can represent the same range of indices, this cannot fail and will never
    /// panic.
    #[inline(always)]
    fn cast_into_id<T: Id<Generic = Self::Generic>>(self) -> T {
        // SAFETY: safe due to the documented requirement on `Self::Generic`
        unsafe { T::from_index_unchecked(self.index()) }
    }

    /// Creates a value from a compatible [`Id`] type using the same index.
    ///
    /// Two id types are considered compatible when the have the same [`Self::Base`] type.
    ///
    /// As compatible types can represent the same range of indices, this cannot fail and will never
    /// panic.
    #[inline(always)]
    fn cast_from_id<T: Id<Generic = Self::Generic>>(from: T) -> Self {
        // SAFETY: safe due to the documented requirement on `Self::Generic`
        unsafe { Self::from_index_unchecked(from.index()) }
    }
}

/// A newtype wrapper around `usize` that limits the contained value to the `MAX_INDEX` generic
/// parameter.
///
/// Used to implement conversion between different [`Id`] types that have the same index range.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericId<const MAX_INDEX: usize>(usize);

impl<const MAX_INDEX: usize> Debug for GenericId<MAX_INDEX> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

/// SAFETY: The most direct implementation that upholds Id's safety requirements
unsafe impl<const MAX_INDEX: usize> Id for GenericId<MAX_INDEX> {
    type Base = Self;
    type Generic = Self;

    fn index(self) -> usize {
        self.0
    }

    const MAX_INDEX: usize = MAX_INDEX;

    const MIN: Self = Self(0);

    const MAX: Self = Self(MAX_INDEX);

    unsafe fn from_index_unchecked(index: usize) -> Self {
        debug_assert!(index <= Self::MAX_INDEX);
        Self(index)
    }
}

pub use id_types::{Id16, Id32, Id64, Id8, IdSize};
