use std::{alloc::Layout, ptr};

use imctk_ids::Id;
use imctk_lit::{Lit, Var};
use imctk_paged_storage::{
    index::{IndexedCatalog, IndexedNode, IndexedNodeMut, IndexedTerm, NewLifetime},
    PagedStorageCatalog, PagedStorageItem, PagedStorageItemMut, PagedStorageItemRef,
};
use imctk_util::unordered_pair::UnorderedPair;

#[derive(Debug, Id)]
#[repr(transparent)]
pub struct InputId(pub u32);
#[derive(Debug, Id)]
#[repr(transparent)]
pub struct SteadyInputId(pub u32);

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct AndTerm(pub UnorderedPair<Lit>);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct XorTerm(pub UnorderedPair<Lit>);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Reg {
    pub next: Lit,
    pub init: Lit,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum BitlevelTerm {
    Input(InputId),
    SteadyInput(SteadyInputId),
    And(AndTerm),
    Xor(XorTerm),
    Reg(Reg),
}

pub enum BitlevelTermMut<'a> {
    Input(&'a mut InputId),
    SteadyInput(&'a mut SteadyInputId),
    And(&'a mut AndTerm),
    Xor(&'a mut XorTerm),
    Reg(&'a mut Reg),
}

#[derive(Clone, Debug)]
pub struct Node<Term> {
    pub output: Lit,
    pub term: Term,
}

pub struct NodeMut<'a, Term> {
    output: &'a mut Lit,
    term: Term,
}

impl<Term> Node<Term> {
    fn map_term<NewTerm>(self, f: impl FnOnce(Term) -> NewTerm) -> Node<NewTerm> {
        Node {
            output: self.output,
            term: f(self.term),
        }
    }
}

impl<'a, Term> NodeMut<'a, Term> {
    fn map_term<NewTerm>(self, f: impl FnOnce(Term) -> NewTerm) -> NodeMut<'a, NewTerm> {
        NodeMut {
            output: self.output,
            term: f(self.term),
        }
    }
}

impl<'a, Term> From<&'a mut Node<Term>> for NodeMut<'a, &'a mut Term> {
    fn from(value: &'a mut Node<Term>) -> Self {
        NodeMut {
            output: &mut value.output,
            term: &mut value.term,
        }
    }
}

impl<'a> From<&'a BitlevelTerm> for BitlevelTerm {
    fn from(value: &'a BitlevelTerm) -> Self {
        value.clone()
    }
}

#[derive(Default)]
pub struct BitlevelCatalog;

#[derive(Clone, Copy, PartialEq, Eq, Hash, EnumIter)]
pub enum BitlevelVariant {
    Input,
    SteadyInput,
    And,
    Xor,
    Reg,
}
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use BitlevelVariant as Variant;

unsafe impl PagedStorageCatalog for BitlevelCatalog {
    type Variant = BitlevelVariant;

    unsafe fn drop_item(&mut self, _variant: Self::Variant, _item: *mut u8) {}

    fn item_layout(&self, variant: Self::Variant) -> std::alloc::Layout {
        match variant {
            Variant::Input => Layout::new::<Node<InputId>>(),
            Variant::SteadyInput => Layout::new::<Node<SteadyInputId>>(),
            Variant::And => Layout::new::<Node<AndTerm>>(),
            Variant::Xor => Layout::new::<Node<XorTerm>>(),
            Variant::Reg => Layout::new::<Node<Reg>>(),
        }
    }
}

unsafe impl PagedStorageItem<BitlevelCatalog> for Node<BitlevelTerm> {
    fn storage_variant(&self, _catalog: &mut BitlevelCatalog) -> BitlevelVariant {
        match self.term {
            BitlevelTerm::Input(_) => Variant::Input,
            BitlevelTerm::SteadyInput(_) => Variant::SteadyInput,
            BitlevelTerm::And(_) => Variant::And,
            BitlevelTerm::Xor(_) => Variant::Xor,
            BitlevelTerm::Reg(_) => Variant::Reg,
        }
    }

    unsafe fn write_to_storage(self, ptr: *mut u8) {
        unsafe {
            match self.term {
                BitlevelTerm::Input(input_id) => ptr::write(
                    ptr as *mut Node<InputId>,
                    Node {
                        output: self.output,
                        term: input_id,
                    },
                ),
                BitlevelTerm::SteadyInput(steady_input_id) => ptr::write(
                    ptr as *mut Node<SteadyInputId>,
                    Node {
                        output: self.output,
                        term: steady_input_id,
                    },
                ),
                BitlevelTerm::And(and_term) => ptr::write(
                    ptr as *mut Node<AndTerm>,
                    Node {
                        output: self.output,
                        term: and_term,
                    },
                ),
                BitlevelTerm::Xor(xor_term) => ptr::write(
                    ptr as *mut Node<XorTerm>,
                    Node {
                        output: self.output,
                        term: xor_term,
                    },
                ),
                BitlevelTerm::Reg(reg) => ptr::write(
                    ptr as *mut Node<Reg>,
                    Node {
                        output: self.output,
                        term: reg,
                    },
                ),
            }
        }
    }

    unsafe fn read_from_storage(
        ptr: *const u8,
        _catalog: &BitlevelCatalog,
        variant: BitlevelVariant,
    ) -> Option<Self> {
        unsafe {
            Some(match variant {
                Variant::Input => {
                    ptr::read(ptr as *const Node<InputId>).map_term(BitlevelTerm::Input)
                }
                Variant::SteadyInput => {
                    ptr::read(ptr as *const Node<SteadyInputId>).map_term(BitlevelTerm::SteadyInput)
                }
                Variant::And => ptr::read(ptr as *const Node<AndTerm>).map_term(BitlevelTerm::And),
                Variant::Xor => ptr::read(ptr as *const Node<XorTerm>).map_term(BitlevelTerm::Xor),
                Variant::Reg => ptr::read(ptr as *const Node<Reg>).map_term(BitlevelTerm::Reg),
            })
        }
    }
}

unsafe impl PagedStorageItemRef<'_, BitlevelCatalog> for Node<BitlevelTerm> {
    unsafe fn ref_storage(
        ptr: *const u8,
        catalog: &BitlevelCatalog,
        variant: <BitlevelCatalog as PagedStorageCatalog>::Variant,
    ) -> Option<Self> {
        unsafe { Self::read_from_storage(ptr, catalog, variant) }
    }

    fn possible_storage_variants(
        _catalog: &BitlevelCatalog,
    ) -> impl Iterator<Item = <BitlevelCatalog as PagedStorageCatalog>::Variant> + '_ {
        Variant::iter()
    }
}

unsafe impl<'a> PagedStorageItemMut<'a, BitlevelCatalog> for NodeMut<'a, BitlevelTermMut<'a>> {
    unsafe fn mut_storage(
        ptr: *mut u8,
        _catalog: &BitlevelCatalog,
        variant: BitlevelVariant,
    ) -> Option<Self> {
        unsafe {
            Some(match variant {
                Variant::Input => NodeMut::map_term(
                    (&mut *(ptr as *mut Node<InputId>)).into(),
                    BitlevelTermMut::Input,
                ),
                Variant::SteadyInput => NodeMut::map_term(
                    (&mut *(ptr as *mut Node<SteadyInputId>)).into(),
                    BitlevelTermMut::SteadyInput,
                ),
                Variant::And => NodeMut::map_term(
                    (&mut *(ptr as *mut Node<AndTerm>)).into(),
                    BitlevelTermMut::And,
                ),
                Variant::Xor => NodeMut::map_term(
                    (&mut *(ptr as *mut Node<XorTerm>)).into(),
                    BitlevelTermMut::Xor,
                ),
                Variant::Reg => {
                    NodeMut::map_term((&mut *(ptr as *mut Node<Reg>)).into(), BitlevelTermMut::Reg)
                }
            })
        }
    }
}

impl IndexedTerm<BitlevelCatalog> for BitlevelTerm {
    fn use_vars(&self) -> impl Iterator<Item = Var> + '_ {
        match self {
            BitlevelTerm::Input(_) => vec![],
            BitlevelTerm::SteadyInput(_) => vec![],
            BitlevelTerm::And(and_term) => and_term.0.as_slice().into(),
            BitlevelTerm::Xor(xor_term) => xor_term.0.as_slice().into(),
            BitlevelTerm::Reg(reg) => vec![reg.next, reg.init],
        }
        .into_iter()
        .map(Lit::var)
    }
}

impl IndexedNode<BitlevelCatalog> for Node<BitlevelTerm> {
    fn def_var(&self) -> Option<Var> {
        Some(self.output.var())
    }

    fn term(&self) -> BitlevelTerm {
        self.term.clone()
    }
}

impl<'a> IndexedNodeMut<BitlevelCatalog> for NodeMut<'a, BitlevelTermMut<'a>> {
    fn rewrite(&mut self, mut fun: impl FnMut(Lit) -> Lit) {
        *self.output = fun(*self.output);
        match &mut self.term {
            BitlevelTermMut::Input(_) => {}
            BitlevelTermMut::SteadyInput(_) => {}
            BitlevelTermMut::And(and_term) => {
                and_term.0 = [fun(and_term.0[0]), fun(and_term.0[1])].into();
            }
            BitlevelTermMut::Xor(xor_term) => {
                xor_term.0 = [fun(xor_term.0[0]), fun(xor_term.0[1])].into();
            }
            BitlevelTermMut::Reg(reg) => {
                reg.init = fun(reg.init);
                reg.next = fun(reg.next);
            }
        }
    }
}

impl<'a> NewLifetime for NodeMut<'a, BitlevelTermMut<'a>> {
    type NewLifetime<'b> = NodeMut<'b, BitlevelTermMut<'b>>
    where 'a: 'b;
}

impl IndexedCatalog for BitlevelCatalog {
    type Var = Var;
    type Lit = Lit;
    type Ref<'a> = Node<BitlevelTerm>;
    type Mut<'a> = NodeMut<'a, BitlevelTermMut<'a>>;
    type Term = BitlevelTerm;
    type TermRef<'a> = BitlevelTerm;

    fn term_eq(a: &Self::TermRef<'_>, b: &Self::TermRef<'_>) -> bool {
        a == b
    }
}
