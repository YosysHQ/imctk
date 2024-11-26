use std::{alloc::Layout, ptr};

use imctk_derive::define_ir_terms;
use imctk_ids::Id;
use imctk_lit::{Lit, Var};
use imctk_paged_storage::{
    index::{
        ContainedVars, IndexedCatalog, IndexedNode, IndexedNodeMut, IndexedNodeMutFamily,
        IndexedNodeRef, IndexedTermRef,
    },
    PagedStorageCatalog, PagedStorageItem, PagedStorageItemMut, PagedStorageItemRef,
};
use imctk_util::unordered_pair::UnorderedPair;
use strum_macros::EnumIter;

define_ir_terms! {
    root Bitlevel;
    var Var : Lit;

    term Input = #[opaque] InputId;
    term SteadyInput = #[opaque] SteadyInputId;
    term And(UnorderedPair<Lit>);

    term ConstFalse;

    #[normalize]
    {
        term Xor(UnorderedPair<Lit>);
        term Reg { #[guarding] next: Lit, init: Lit }
    }

    subset Aiger(And, Reg);
}

#[derive(Debug, Id)]
#[repr(transparent)]
pub struct InputId(pub u32);
#[derive(Debug, Id)]
#[repr(transparent)]
pub struct SteadyInputId(pub u32);

pub use And as AndTerm;
pub use Xor as XorTerm;

pub use BitlevelNode as Node;
pub use BitlevelNodeMut as NodeMut;
