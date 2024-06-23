use core::{fmt::Debug, hash::Hash};

mod id_types;
mod primitive_impls;
mod u8_range_types;

#[allow(unused_imports)] // docs

/// Used to construct constant values from an index.
///
/// Together with [`Id::FromConstIdIndex`], this works around the lack of support for `const fn` in
/// traits.
pub trait ConstIdFromIdIndex<const INDEX: usize> {
    /// This trait is usually implemented on the target [`Id`] type, in which case this can be `Self`.
    type Id: Id;

    /// The constant ID value having the index `INDEX`.
    const ID: Self::Id;
}

/// Types that represent integer ids
///
/// A type of this trait represents an `usize` index value in the range `0..=Self::MAX_ID_INDEX`,
/// with the specific representation used is up to the implementing type. Implementing types can
/// have a smaller size or alignment compared to `usize`, and can leave space for rustc's
/// niche-value optimization.
///
/// # Safety
/// This trait comes with several safety critical requirements for implementing types:
///
/// * The [`Clone`] implementation must return a [`Copy`].
/// * The [`PartialEq`], [`Eq`], [`PartialOrd`] and [`Ord`] implementation must behave as if this
///   type was a `struct` containing [`index: usize`][`Self::id_index()`] as only field and a
///   `#[derive(PartialEq, Eq, PartialOrd, Ord)]` attribute.
/// * Each valid index must have a unique representation , i.e. it must be possible to check two ids
///   for equality by comparing their raw bytes. This only applies to equality, and does not extend
///   to [`PartialOrd`] or [`Ord`].
/// * The [`Hash`] implementation must be deterministic and only depend on the represented index.
/// * The associated [`Self::BaseId`] type must be an [`Id`] with the same representation and index
///   range (it can be `Self`).
/// * The associated [`Self::GenericId`] type must be
///   [`GenericId<{Self::MAX_ID_INDEX}>`][`GenericId`].
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
    type BaseId: Id<BaseId = Self::BaseId>;

    /// The [`GenericId`] type, parametrized to have the same index range as this type.
    ///
    /// This is used for type checked conversions between ids that do not have to share the same
    /// representation.
    type GenericId: Id<GenericId = Self::GenericId>;

    /// Used to construct constant values from an index.
    ///
    /// This works around the lack of support for `const fn` in traits.
    type FromConstIdIndex<const INDEX: usize>: ConstIdFromIdIndex<INDEX, Id = Self>;

    /// Returns the id with a given index, panicking when the index is invalid.
    ///
    /// This panics if and only if `index > Self::MAX_ID_INDEX`.
    #[inline(always)]
    #[track_caller]
    fn from_id_index(index: usize) -> Self {
        assert!(index <= Self::MAX_ID_INDEX);
        // SAFETY: index asserted to be in range
        unsafe { Self::from_id_index_unchecked(index) }
    }

    /// Returns the index represented by this id.
    fn id_index(self) -> usize;

    /// The largest index representable by this id type.
    const MAX_ID_INDEX: usize;
    /// The id with index zero.
    const MIN_ID: Self;
    /// The id with the largest representable index.
    const MAX_ID: Self;

    /// Returns the id with a given index, assuming a valid index.
    ///
    /// # Safety
    /// This is only safe to call when `index <= Self::MAX_ID_INDEX`, which is not checked by this
    /// method.
    ///
    /// Implementations are encouraged to include a debug-only assertion for this requirement.
    unsafe fn from_id_index_unchecked(index: usize) -> Self;

    /// Returns the id with a given index, if it is valid.
    ///
    /// This returns `None` if and only if `index > Self::MAX_ID_INDEX`.
    ///
    /// Never panics.
    #[inline(always)]
    fn try_from_id_index(index: usize) -> Option<Self> {
        if index <= Self::MAX_ID_INDEX {
            // SAFETY: index checked to be in range
            Some(unsafe { Self::from_id_index_unchecked(index) })
        } else {
            None
        }
    }

    /// Returns a [`Self::BaseId`] id of the same index.
    ///
    /// This cannot fail and will never panic.
    #[inline(always)]
    fn into_base_id(self) -> Self::BaseId {
        // SAFETY: we require identical representations of Self and Self::BaseId
        unsafe { std::mem::transmute_copy(&self) }
    }

    /// Returns an id with the same index as a [`Self::BaseId`] id.
    ///
    /// This cannot fail and will never panic.
    #[inline(always)]
    fn from_base_id(base: Self::BaseId) -> Self {
        // SAFETY: we require identical representations of Self and Self::BaseId
        unsafe { std::mem::transmute_copy(&base) }
    }

    /// Returns a value of a compatible [`Id`] type having the same index.
    ///
    /// Two id types are considered compatible when the have the same [`Self::BaseId`] type.
    ///
    /// As compatible types can represent the same range of indices, this cannot fail and will never
    /// panic.
    #[inline(always)]
    fn cast_into_id<T: Id<GenericId = Self::GenericId>>(self) -> T {
        // SAFETY: safe due to the documented requirement on `Self::Generic`
        unsafe { T::from_id_index_unchecked(self.id_index()) }
    }

    /// Creates a value from a compatible [`Id`] type using the same index.
    ///
    /// Two id types are considered compatible when the have the same [`Self::BaseId`] type.
    ///
    /// As compatible types can represent the same range of indices, this cannot fail and will never
    /// panic.
    #[inline(always)]
    fn cast_from_id<T: Id<GenericId = Self::GenericId>>(from: T) -> Self {
        // SAFETY: safe due to the documented requirement on `Self::Generic`
        unsafe { Self::from_id_index_unchecked(from.id_index()) }
    }
}

/// A newtype wrapper around an `Id` type that limits the contained value to the `MAX_ID_INDEX`
/// generic parameter.
///
/// Used with `usize` as `Id` type to implement conversion between [`Id`] types that have different
/// representations but that share the same index range.
#[derive(imctk_derive::UnsafeInternalGenericId)]
#[repr(transparent)]
pub struct GenericId<const MAX_ID_INDEX: usize, Repr: Id = usize>(Repr);

impl<const MAX_ID_INDEX: usize, Repr: Id> Debug for GenericId<MAX_ID_INDEX, Repr> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl<const MAX_ID_INDEX: usize, Repr: Id, const INDEX: usize> ConstIdFromIdIndex<INDEX>
    for GenericId<MAX_ID_INDEX, Repr>
{
    type Id = Self;
    const ID: Self::Id = Self(<Repr::FromConstIdIndex<INDEX> as ConstIdFromIdIndex<INDEX>>::ID);
}

// SAFETY: The most direct implementation that upholds Id's safety requirements
unsafe impl<const MAX_ID_INDEX: usize, Repr: Id> Id for GenericId<MAX_ID_INDEX, Repr> {
    type BaseId = Self;
    type GenericId = GenericId<MAX_ID_INDEX, usize>;
    type FromConstIdIndex<const INDEX: usize> = Self;

    const MAX_ID_INDEX: usize = {
        assert!(MAX_ID_INDEX <= Repr::MAX_ID_INDEX);
        MAX_ID_INDEX
    };

    const MIN_ID: Self = Self(Repr::MIN_ID);
    const MAX_ID: Self =
        Self(<Repr::FromConstIdIndex<MAX_ID_INDEX> as ConstIdFromIdIndex<MAX_ID_INDEX>>::ID);

    #[inline(always)]
    unsafe fn from_id_index_unchecked(index: usize) -> Self {
        debug_assert!(index <= Self::MAX_ID_INDEX);
        // SAFETY: we checked that our MAX_ID_INDEX is not above Repr::MAX_ID_INDEX
        Self(unsafe { Repr::from_id_index_unchecked(index) })
    }

    #[inline(always)]
    fn id_index(self) -> usize {
        self.0.id_index()
    }
}

// SAFETY: we ensure that the newtype wrapped type is `Id` and thus `Send`
unsafe impl<const MAX_ID_INDEX: usize, Repr: Id> Send for GenericId<MAX_ID_INDEX, Repr> {}

// SAFETY: we ensure that the newtype wrapped type is `Id` and thus `Sync`
unsafe impl<const MAX_ID_INDEX: usize, Repr: Id> Sync for GenericId<MAX_ID_INDEX, Repr> {}

impl<const MAX_ID_INDEX: usize, Repr: Id> GenericId<MAX_ID_INDEX, Repr> {
    /// Returns a `Repr` id of the same index.
    pub fn into_generic_id_repr(self) -> Repr {
        self.0
    }
}

pub use id_types::{Id16, Id32, Id64, Id8, IdSize};
