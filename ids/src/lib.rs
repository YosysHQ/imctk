//! Type checked and niche compatible integer ids.

#![feature(impl_trait_in_assoc_type)]
// We could avoid `impl_trait_in_assoc_type` at the cost of suboptimal iterator implementations and
// more boilerplate. Since it seems this is close to stabilization, let's not spend the effort on
// all that boilerplate right now.
//
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(missing_docs)]
#![warn(clippy::undocumented_unsafe_blocks)]

#[cfg(doc)]
use core::hash::Hash;

mod id;
pub mod id_alloc;
mod id_range;
pub mod id_vec;
pub mod indexed_id_vec;

pub mod id_set_seq;

/// Derives an [`Id`] instance for a newtype wrapper around an existing [`Id`] type.
///
/// Deriving an [`Id`] instance requires the `#[repr(transparent)]` attribute on the target struct.
///
/// This automatically derives all of [`Id`]'s supertraits ([`Clone`], [`Copy`], [`PartialEq`],
/// [`Eq`], [`PartialOrd`], [`Ord`] , [`Hash`], [`Send`] and [`Sync`]) to ensure those traits are
/// implemented according to `Id`'s safety requirements.
pub use imctk_derive::Id;

pub use id::{ConstIdFromIdIndex, GenericId, Id, Id16, Id32, Id64, Id8, IdSize};

pub use id_range::IdRange;

pub use id_alloc::IdAlloc;

// re-export this so that others can use it without depending on bytemuck explicitly
// in particular needed for #[derive(Id)]
pub use bytemuck::NoUninit;
