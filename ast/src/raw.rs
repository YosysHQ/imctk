use std::{
    cmp::Ordering,
    collections::HashSet,
    hash::{BuildHasher, BuildHasherDefault, Hash},
    sync::{Arc, RwLock},
};

use lazy_static::lazy_static;

use crate::ast::{
    AbstractionKind, Context, ContextItem, ContextItemRef, ContextRef, QuantifierKind,
    ShortCircuitOp, Symbol, SymbolRef, Term, TermRef,
};

pub struct RawAstHandle<T>(*const AstNode<T>);

pub trait RawAstData: Hash
where
    Self: Sized,
{
    type VariantRef<'a>
    where
        Self: 'a;
    type Owned: Clone + Hash + Eq + 'static;

    fn as_variant_ref(&self) -> Self::VariantRef<'_>;
    fn from_variant_ref(variant: Self::VariantRef<'_>) -> Self;
    fn global_unique_table() -> &'static RwLock<HashSet<Self::Owned>>;
    unsafe fn handle_to_owned(handle: RawAstHandle<Self>) -> Self::Owned;
    fn owned_to_handle(owned: &Self::Owned) -> RawAstHandle<Self>;
}

impl<T> Copy for RawAstHandle<T> {}

impl<T> Clone for RawAstHandle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> RawAstHandle<T> {
    pub fn new(variant: <T as RawAstData>::VariantRef<'_>) -> Self
    where
        T: RawAstData,
    {
        let variant = T::from_variant_ref(variant);
        let hash = BuildHasherDefault::<zwohash::ZwoHasher>::new().hash_one(&variant);

        Self(Arc::into_raw(Arc::new(AstNode { variant, hash })))
    }

    pub unsafe fn incref(self) {
        unsafe { Arc::increment_strong_count(self.0) }
    }

    pub unsafe fn decref(self) {
        unsafe { Arc::decrement_strong_count(self.0) }
    }

    pub fn unstable_id(self) -> usize {
        self.0 as usize
    }

    pub unsafe fn variant<'a>(self) -> <T as RawAstData>::VariantRef<'a>
    where
        T: RawAstData,
    {
        unsafe { (*self.0).variant.as_variant_ref() }
    }

    pub unsafe fn hash_value(self) -> u64 {
        unsafe { (*self.0).hash }
    }

    pub fn eq_ptr(self, other: Self) -> bool {
        self.0 == other.0
    }

    pub unsafe fn eq_value(self, other: Self) -> bool
    where
        T: PartialEq + Eq,
    {
        if self.eq_ptr(other) {
            return true;
        }
        if unsafe { self.hash_value() != other.hash_value() } {
            return false;
        }
        unsafe { (*self.0).variant == (*other.0).variant }
    }

    pub unsafe fn ord_value(self, other: Self) -> Ordering
    where
        T: PartialOrd + Ord,
    {
        if self.eq_ptr(other) {
            return Ordering::Equal;
        }
        unsafe { (*self.0).variant.cmp(&(*other.0).variant) }
    }

    pub unsafe fn canonicalize(self) -> T::Owned
    where
        T: RawAstData,
    {
        let unique_table = T::global_unique_table();
        let mut table = unique_table.write().unwrap();
        let owned = unsafe { T::handle_to_owned(self) };
        if let Some(found) = table.get(&owned) {
            found.clone()
        } else {
            table.insert(owned.clone());
            owned
        }
    }

    pub unsafe fn is_canonical(self) -> bool
    where
        T: RawAstData,
    {
        let unique_table = T::global_unique_table();
        let table = unique_table.read().unwrap();
        let owned = unsafe { T::handle_to_owned(self) };
        if let Some(found) = table.get(&owned) {
            self.eq_ptr(T::owned_to_handle(found))
        } else {
            false
        }
    }
}
unsafe impl<T: Sync> Send for RawAstHandle<T> {}
unsafe impl<T: Sync> Sync for RawAstHandle<T> {}

pub type RawSymbol = RawAstHandle<SymbolOwnedVariant>;
pub type RawTerm = RawAstHandle<TermOwnedVariant>;
pub type RawContextItem = RawAstHandle<ContextItemOwnedVariant>;
pub type RawContext = RawAstHandle<ContextOwnedVariant>;

pub struct AstNode<T> {
    variant: T,
    hash: u64,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SymbolOwnedVariant {
    Name(String),
    Id(u64),
    Scoped(Symbol, Symbol),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TermOwnedVariant {
    Var(Symbol),
    Const(Symbol),
    Apply(Symbol, Vec<Term>),
    Ite(Term, Term, Term),
    Quantifier(QuantifierKind, Symbol, Term),
    Abstraction(AbstractionKind, Symbol, Term, Term),
    Choice(Symbol, Term),
    ShortCircuit(ShortCircuitOp, Term, Term),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ContextItemOwnedVariant {
    Assumption([Term; 2]),
    ConstantDef(Symbol, Term),
    ValueClassDef(Symbol, Symbol, Term),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ContextOwnedVariant {
    Empty,
    Append(Context, ContextItem),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SymbolVariant<'a> {
    Name(&'a str),
    Id(u64),
    Scoped(SymbolRef<'a>, SymbolRef<'a>),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TermVariant<'a> {
    Var(SymbolRef<'a>),
    Const(SymbolRef<'a>),
    Apply(SymbolRef<'a>, &'a [TermRef<'a>]),
    Ite(TermRef<'a>, TermRef<'a>, TermRef<'a>),
    Quantifier(QuantifierKind, SymbolRef<'a>, TermRef<'a>),
    Abstraction(AbstractionKind, SymbolRef<'a>, TermRef<'a>, TermRef<'a>),
    Choice(SymbolRef<'a>, TermRef<'a>),
    ShortCircuit(ShortCircuitOp, TermRef<'a>, TermRef<'a>),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ContextItemVariant<'a> {
    Assumption([TermRef<'a>; 2]),
    ConstantDef(SymbolRef<'a>, TermRef<'a>),
    ValueClassDef(SymbolRef<'a>, SymbolRef<'a>, TermRef<'a>),
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ContextVariant<'a> {
    Empty,
    Append(ContextRef<'a>, ContextItemRef<'a>),
}

lazy_static! {
    static ref SYMBOL_TABLE: RwLock<HashSet<Symbol>> = RwLock::new(HashSet::new());
    static ref TERM_TABLE: RwLock<HashSet<Term>> = RwLock::new(HashSet::new());
    static ref CONTEXT_ITEM_TABLE: RwLock<HashSet<ContextItem>> = RwLock::new(HashSet::new());
    static ref CONTEXT_TABLE: RwLock<HashSet<Context>> = RwLock::new(HashSet::new());
}

impl RawAstData for SymbolOwnedVariant {
    type VariantRef<'a> = SymbolVariant<'a>;
    type Owned = Symbol;

    fn as_variant_ref(&self) -> SymbolVariant {
        match self {
            SymbolOwnedVariant::Name(name) => SymbolVariant::Name(name),
            SymbolOwnedVariant::Id(id) => SymbolVariant::Id(*id),
            SymbolOwnedVariant::Scoped(parent, child) => {
                SymbolVariant::Scoped(parent.as_ref(), child.as_ref())
            }
        }
    }

    fn from_variant_ref(variant: Self::VariantRef<'_>) -> Self {
        match variant {
            SymbolVariant::Name(name) => SymbolOwnedVariant::Name(name.into()),
            SymbolVariant::Id(id) => SymbolOwnedVariant::Id(id),
            SymbolVariant::Scoped(parent, child) => {
                SymbolOwnedVariant::Scoped(parent.into(), child.into())
            }
        }
    }

    fn global_unique_table() -> &'static RwLock<HashSet<Self::Owned>> {
        &*SYMBOL_TABLE
    }

    unsafe fn handle_to_owned(handle: RawAstHandle<Self>) -> Self::Owned {
        unsafe { handle.incref() };
        Symbol(handle)
    }

    fn owned_to_handle(owned: &Self::Owned) -> RawAstHandle<Self> {
        owned.0
    }
}

impl RawAstData for TermOwnedVariant {
    type VariantRef<'a> = TermVariant<'a>;
    type Owned = Term;

    fn as_variant_ref(&self) -> TermVariant {
        match self {
            TermOwnedVariant::Var(symbol) => TermVariant::Var(symbol.as_ref()),
            TermOwnedVariant::Const(symbol) => TermVariant::Const(symbol.as_ref()),
            TermOwnedVariant::Apply(symbol, args) => {
                TermVariant::Apply(symbol.as_ref(), unsafe { args.align_to().1 })
            }
            TermOwnedVariant::Ite(cond, then, els) => {
                TermVariant::Ite(cond.as_ref(), then.as_ref(), els.as_ref())
            }
            TermOwnedVariant::Quantifier(kind, symbol, body) => {
                TermVariant::Quantifier(*kind, symbol.as_ref(), body.as_ref())
            }
            TermOwnedVariant::Abstraction(kind, symbol, domain, body) => {
                TermVariant::Abstraction(*kind, symbol.as_ref(), domain.as_ref(), body.as_ref())
            }
            TermOwnedVariant::Choice(symbol, body) => {
                TermVariant::Choice(symbol.as_ref(), body.as_ref())
            }
            TermOwnedVariant::ShortCircuit(op, lhs, rhs) => {
                TermVariant::ShortCircuit(*op, lhs.as_ref(), rhs.as_ref())
            }
        }
    }

    fn from_variant_ref(variant: Self::VariantRef<'_>) -> Self {
        match variant {
            TermVariant::Var(symbol) => TermOwnedVariant::Var(symbol.into()),
            TermVariant::Const(symbol) => TermOwnedVariant::Const(symbol.into()),
            TermVariant::Apply(symbol, args) => TermOwnedVariant::Apply(
                symbol.into(),
                args.iter().map(|item| (*item).into()).collect(),
            ),
            TermVariant::Ite(cond, then, els) => {
                TermOwnedVariant::Ite(cond.into(), then.into(), els.into())
            }
            TermVariant::Quantifier(kind, symbol, body) => {
                TermOwnedVariant::Quantifier(kind, symbol.into(), body.into())
            }
            TermVariant::Abstraction(kind, symbol, domain, body) => {
                TermOwnedVariant::Abstraction(kind, symbol.into(), domain.into(), body.into())
            }
            TermVariant::Choice(symbol, body) => {
                TermOwnedVariant::Choice(symbol.into(), body.into())
            }
            TermVariant::ShortCircuit(op, lhs, rhs) => {
                TermOwnedVariant::ShortCircuit(op, lhs.into(), rhs.into())
            }
        }
    }

    fn global_unique_table() -> &'static RwLock<HashSet<Self::Owned>> {
        &*TERM_TABLE
    }

    unsafe fn handle_to_owned(handle: RawAstHandle<Self>) -> Self::Owned {
        unsafe { handle.incref() };
        Term(handle)
    }

    fn owned_to_handle(owned: &Self::Owned) -> RawAstHandle<Self> {
        owned.0
    }
}

impl RawAstData for ContextItemOwnedVariant {
    type VariantRef<'a> = ContextItemVariant<'a>;
    type Owned = ContextItem;

    fn as_variant_ref(&self) -> ContextItemVariant {
        match self {
            ContextItemOwnedVariant::Assumption([term_a, term_b]) => {
                ContextItemVariant::Assumption([term_a.as_ref(), term_b.as_ref()])
            }
            ContextItemOwnedVariant::ConstantDef(symbol, term) => {
                ContextItemVariant::ConstantDef(symbol.as_ref(), term.as_ref())
            }
            ContextItemOwnedVariant::ValueClassDef(symbol, value, body) => {
                ContextItemVariant::ValueClassDef(symbol.as_ref(), value.as_ref(), body.as_ref())
            }
        }
    }

    fn from_variant_ref(variant: Self::VariantRef<'_>) -> Self {
        match variant {
            ContextItemVariant::Assumption([term_a, term_b]) => {
                ContextItemOwnedVariant::Assumption([term_a.into(), term_b.into()])
            }
            ContextItemVariant::ConstantDef(symbol, term) => {
                ContextItemOwnedVariant::ConstantDef(symbol.into(), term.into())
            }
            ContextItemVariant::ValueClassDef(symbol, value, body) => {
                ContextItemOwnedVariant::ValueClassDef(symbol.into(), value.into(), body.into())
            }
        }
    }

    fn global_unique_table() -> &'static RwLock<HashSet<Self::Owned>> {
        &*CONTEXT_ITEM_TABLE
    }

    unsafe fn handle_to_owned(handle: RawAstHandle<Self>) -> Self::Owned {
        unsafe { handle.incref() };
        ContextItem(handle)
    }

    fn owned_to_handle(owned: &Self::Owned) -> RawAstHandle<Self> {
        owned.0
    }
}

impl RawAstData for ContextOwnedVariant {
    type VariantRef<'a> = ContextVariant<'a>;
    type Owned = Context;

    fn as_variant_ref(&self) -> ContextVariant {
        match self {
            ContextOwnedVariant::Empty => ContextVariant::Empty,
            ContextOwnedVariant::Append(context, item) => {
                ContextVariant::Append(context.as_ref(), item.as_ref())
            }
        }
    }

    fn from_variant_ref(variant: Self::VariantRef<'_>) -> Self {
        match variant {
            ContextVariant::Empty => ContextOwnedVariant::Empty,
            ContextVariant::Append(context, item) => {
                ContextOwnedVariant::Append(context.into(), item.into())
            }
        }
    }

    fn global_unique_table() -> &'static RwLock<HashSet<Self::Owned>> {
        &*CONTEXT_TABLE
    }

    unsafe fn handle_to_owned(handle: RawAstHandle<Self>) -> Self::Owned {
        unsafe { handle.incref() };
        Context(handle)
    }

    fn owned_to_handle(owned: &Self::Owned) -> RawAstHandle<Self> {
        owned.0
    }
}
