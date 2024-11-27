use imctk_derive::define_ir_terms;
use imctk_ids::Id;
use imctk_lit::Var;
use imctk_paged_storage::{
    index::{
        ContainedVars, IndexedCatalog, IndexedNode, IndexedNodeMut, IndexedNodeMutFamily,
        IndexedNodeRef, IndexedTermRef,
    },
    PagedStorageCatalog, PagedStorageItem, PagedStorageItemMut, PagedStorageItemRef,
};
use imctk_util::unordered_pair::UnorderedPair;
use std::{alloc::Layout, ptr};
use imctk_union_find::Element;

define_ir_terms! {
    root Wordlevel;
    var Var : Var;

    term Input(#[opaque] InputId, #[opaque] Sort);
    term SteadyInput(#[opaque] SteadyInputId, #[opaque] Sort);
    term Const = #[opaque] Const;
    term Reg { #[guarding] next: Var, init: Var, #[opaque] sort: Sort };
    term And(UnorderedPair<Var>, #[opaque] Sort);
    term Or(UnorderedPair<Var>, #[opaque] Sort);
    term Xor(UnorderedPair<Var>, #[opaque] Sort);
    term Add(UnorderedPair<Var>, #[opaque] Sort);
    term Sub([Var; 2], #[opaque] Sort);
    term Mul(UnorderedPair<Var>, #[opaque] Sort);
    term Udiv([Var; 2], #[opaque] Sort);
    term Urem([Var; 2], #[opaque] Sort);
    term Sdiv([Var; 2], #[opaque] Sort);
    term Srem([Var; 2], #[opaque] Sort);
    term Smod([Var; 2], #[opaque] Sort);
    term UaddOverflow(UnorderedPair<Var>);
    term SaddOverflow(UnorderedPair<Var>);
    term UsubOverflow([Var; 2]);
    term SsubOverflow([Var; 2]);
    term UmulOverflow(UnorderedPair<Var>);
    term SmulOverflow(UnorderedPair<Var>);
    term SdivOverflow([Var; 2]);
    term Not(Var, #[opaque] Sort);
    term Eq(UnorderedPair<Var>);
    term Ugt([Var; 2]);
    term Uge([Var; 2]);
    term Sgt([Var; 2]);
    term Sge([Var; 2]);
    term Concat([Var; 2], #[opaque] Sort);
    term Ite { cond: Var, then: Var, els: Var, #[opaque] sort: Sort };
    term Zext(Var, #[opaque] Sort);
    term Sext(Var, #[opaque] Sort);
    term Slice(Var, #[opaque] u32, #[opaque] Sort);
    term Redxor(Var);
    term LogicalShiftLeft([Var; 2], #[opaque] Sort);
    term LogicalShiftRight([Var; 2], #[opaque] Sort);
    term ArithmeticShiftRight([Var; 2], #[opaque] Sort);
    term RotateLeft([Var; 2], #[opaque] Sort);
    term RotateRight([Var; 2], #[opaque] Sort);
}

#[derive(Debug, Id)]
#[repr(transparent)]
pub struct InputId(pub u32);
#[derive(Debug, Id)]
#[repr(transparent)]
pub struct SteadyInputId(pub u32);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Sort(pub u32);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Const {
    pub value: u64,
    pub sort: Sort,
}

impl ContainedVars<WordlevelCatalog> for [Var; 2] {
    fn contained_vars_into_extend(&self, sink: &mut impl Extend<<WordlevelCatalog as IndexedCatalog>::Var>) {
        sink.extend(self.iter().copied());
    }

    fn map_contained_vars(&self, fun: impl FnMut(<WordlevelCatalog as IndexedCatalog>::Lit) -> <WordlevelCatalog as IndexedCatalog>::Lit) -> Self {
        self.map(fun)
    }
}

pub trait HasSort {
    fn sort(&self) -> Sort;
}

macro_rules! sort {
    ( $name:ident ) => {
        impl HasSort for $name {
            fn sort(&self) -> Sort {
                self.1.clone()
            }
        }
    };
    ( $name:ident, $term:expr ) => {
        impl HasSort for $name {
            fn sort(&self) -> Sort {
                $term
            }
        }
    };
}

sort!(Input);
sort!(SteadyInput);
sort!(And);
sort!(Or);
sort!(Xor);
sort!(Add);
sort!(Sub);
sort!(Mul);
sort!(Udiv);
sort!(Urem);
sort!(Sdiv);
sort!(Srem);
sort!(Smod);
sort!(UaddOverflow, Sort(1));
sort!(SaddOverflow, Sort(1));
sort!(UsubOverflow, Sort(1));
sort!(SsubOverflow, Sort(1));
sort!(UmulOverflow, Sort(1));
sort!(SmulOverflow, Sort(1));
sort!(SdivOverflow, Sort(1));
sort!(Not);
sort!(Eq, Sort(1));
sort!(Ugt, Sort(1));
sort!(Uge, Sort(1));
sort!(Sgt, Sort(1));
sort!(Sge, Sort(1));
sort!(Concat);
sort!(Zext);
sort!(Sext);
sort!(Redxor, Sort(1));
sort!(LogicalShiftLeft);
sort!(LogicalShiftRight);
sort!(ArithmeticShiftRight);
sort!(RotateLeft);
sort!(RotateRight);

impl HasSort for Slice {
    fn sort(&self) -> Sort {
        self.2.clone()
    }
}

impl HasSort for Const {
    fn sort(&self) -> Sort {
        self.sort.clone()
    }
}

impl HasSort for Ite {
    fn sort(&self) -> Sort {
        self.sort.clone()
    }
}

impl HasSort for Reg {
    fn sort(&self) -> Sort {
        self.sort.clone()
    }
}

impl HasSort for WordlevelTerm {
    fn sort(&self) -> Sort {
        match self {
            WordlevelTerm::Add(term) => term.sort(),
            WordlevelTerm::And(term) => term.sort(),
            WordlevelTerm::ArithmeticShiftRight(term) => term.sort(),
            WordlevelTerm::Concat(term) => term.sort(),
            WordlevelTerm::Const(term) => term.sort(),
            WordlevelTerm::Eq(term) => term.sort(),
            WordlevelTerm::Input(term) => term.sort(),
            WordlevelTerm::Ite(term) => term.sort(),
            WordlevelTerm::LogicalShiftLeft(term) => term.sort(),
            WordlevelTerm::LogicalShiftRight(term) => term.sort(),
            WordlevelTerm::Mul(term) => term.sort(),
            WordlevelTerm::Not(term) => term.sort(),
            WordlevelTerm::Or(term) => term.sort(),
            WordlevelTerm::Redxor(term) => term.sort(),
            WordlevelTerm::Reg(term) => term.sort(),
            WordlevelTerm::RotateLeft(term) => term.sort(),
            WordlevelTerm::RotateRight(term) => term.sort(),
            WordlevelTerm::SaddOverflow(term) => term.sort(),
            WordlevelTerm::Sdiv(term) => term.sort(),
            WordlevelTerm::SdivOverflow(term) => term.sort(),
            WordlevelTerm::Sext(term) => term.sort(),
            WordlevelTerm::Sge(term) => term.sort(),
            WordlevelTerm::Sgt(term) => term.sort(),
            WordlevelTerm::Slice(term) => term.sort(),
            WordlevelTerm::Smod(term) => term.sort(),
            WordlevelTerm::SmulOverflow(term) => term.sort(),
            WordlevelTerm::Srem(term) => term.sort(),
            WordlevelTerm::SsubOverflow(term) => term.sort(),
            WordlevelTerm::SteadyInput(term) => term.sort(),
            WordlevelTerm::Sub(term) => term.sort(),
            WordlevelTerm::UaddOverflow(term) => term.sort(),
            WordlevelTerm::Udiv(term) => term.sort(),
            WordlevelTerm::Uge(term) => term.sort(),
            WordlevelTerm::Ugt(term) => term.sort(),
            WordlevelTerm::UmulOverflow(term) => term.sort(),
            WordlevelTerm::Urem(term) => term.sort(),
            WordlevelTerm::UsubOverflow(term) => term.sort(),
            WordlevelTerm::Xor(term) => term.sort(),
            WordlevelTerm::Zext(term) => term.sort(),
        }
    }
}