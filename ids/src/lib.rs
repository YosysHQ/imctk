#![deny(unsafe_op_in_unsafe_fn)]
#![warn(missing_docs)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
//! Type checked and niche compatible integer ids.

#[cfg(doc)]
use core::hash::Hash;

mod id;

/// Derives an [`Id`] instance for a newtype wrapper around an existing [`Id`] type.
///
/// Deriving an [`Id`] instance requires the `#[repr(transparent)]` attribute on the target struct.
///
/// This automatically derives all of [`Id`]'s supertraits ([`Clone`], [`Copy`], [`PartialEq`],
/// [`Eq`], [`PartialOrd`], [`Ord`] , [`Hash`], [`Send`] and [`Sync`]) to ensure those traits are
/// implemented according to `Id`'s safety requirements.
pub use imctk_derive::Id;

pub use id::{GenericId, Id, Id16, Id32, Id64, Id8, IdSize};
