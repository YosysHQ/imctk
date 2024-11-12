//! This crate defines a structure [`UnionFind`] that allows tracking of equivalences between generic elements
//! and a structure [`TrackedUnionFind`] that provides the same functionality but augmented by change tracking.

#[doc(inline)]
pub use element::Element;
#[doc(inline)]
pub use tracked_union_find::TrackedUnionFind;
#[doc(inline)]
pub use union_find::UnionFind;

pub mod change_tracking;
pub mod element;
pub mod tracked_union_find;
pub mod union_find;
