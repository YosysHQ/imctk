#[path = "raw.rs"]
mod raw;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum QuantifierKind {
    Forall,
    Exists,
    Unique,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ShortCircuitOp {
    And,
    Or,
    Implies,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum AbstractionKind {
    Set,
    Function,
}

use std::{hash::{Hash, Hasher}, marker::PhantomData, ops::Deref};

use fmt::FmtBuf;
use raw::{RawContext, RawContextItem, RawSymbol, RawTerm};

pub use raw::{ContextItemVariant, ContextVariant, SymbolVariant, TermVariant};

macro_rules! variant_impl {
    ($name:ty) => {
        impl Hash for $name {
            fn hash<H: Hasher>(&self, state: &mut H) {
                unsafe { self.0.hash_value().hash(state) }
            }
        }
        
        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                unsafe { self.0.eq_value(other.0) }
            }
        }
        
        impl Eq for $name {
        }
        
        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        
        impl Ord for $name {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                unsafe { self.0.ord_value(other.0) }
            }
        }        
    };
}

#[repr(transparent)]
pub struct Symbol(raw::RawSymbol);

impl Clone for Symbol {
    fn clone(&self) -> Self {
        unsafe { self.0.incref() };
        Self(self.0)
    }
}

impl Drop for Symbol {
    fn drop(&mut self) {
        unsafe { self.0.decref() };
    }
}

variant_impl!(Symbol);

pub struct Term(raw::RawTerm);

impl Clone for Term {
    fn clone(&self) -> Self {
        unsafe { self.0.incref() };
        Self(self.0)
    }
}
impl Drop for Term {
    fn drop(&mut self) {
        unsafe { self.0.decref() };
    }
}

variant_impl!(Term);

pub struct ContextItem(raw::RawContextItem);

impl Clone for ContextItem {
    fn clone(&self) -> Self {
        unsafe { self.0.incref() };
        Self(self.0)
    }
}
impl Drop for ContextItem {
    fn drop(&mut self) {
        unsafe { self.0.decref() };
    }
}

variant_impl!(ContextItem);

pub struct Context(raw::RawContext);

impl Clone for Context {
    fn clone(&self) -> Self {
        unsafe { self.0.incref() };
        Self(self.0)
    }
}
impl Drop for Context {
    fn drop(&mut self) {
        unsafe { self.0.decref() };
    }
}

variant_impl!(Context);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct SymbolRef<'a>(raw::RawSymbol, PhantomData<&'a Symbol>);

impl Deref for SymbolRef<'_> {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&Self, &Self::Target>(self) }
    }
}

variant_impl!(SymbolRef<'_>);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct TermRef<'a>(raw::RawTerm, PhantomData<&'a Term>);

impl Deref for TermRef<'_> {
    type Target = Term;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&Self, &Self::Target>(self) }
    }
}

variant_impl!(TermRef<'_>);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ContextItemRef<'a>(raw::RawContextItem, PhantomData<&'a ContextItem>);

impl Deref for ContextItemRef<'_> {
    type Target = ContextItem;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&Self, &Self::Target>(self) }
    }
}

variant_impl!(ContextItemRef<'_>);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ContextRef<'a>(raw::RawContext, PhantomData<&'a Context>);

impl Deref for ContextRef<'_> {
    type Target = Context;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&Self, &Self::Target>(self) }
    }
}


variant_impl!(ContextRef<'_>);

impl Symbol {
    pub fn as_ref(&self) -> SymbolRef {
        SymbolRef(self.0, PhantomData)
    }
    pub fn variant(&self) -> SymbolVariant {
        unsafe { self.0.variant() }
    }
    pub fn unstable_id(&self) -> usize {
        self.0.unstable_id()
    }
}

impl Term {
    pub fn as_ref(&self) -> TermRef {
        TermRef(self.0, PhantomData)
    }
    pub fn variant(&self) -> TermVariant {
        unsafe { self.0.variant() }
    }
    pub fn unstable_id(&self) -> usize {
        self.0.unstable_id()
    }
}

impl ContextItem {
    pub fn as_ref(&self) -> ContextItemRef {
        ContextItemRef(self.0, PhantomData)
    }
    pub fn variant(&self) -> ContextItemVariant {
        unsafe { self.0.variant() }
    }
    pub fn unstable_id(&self) -> usize {
        self.0.unstable_id()
    }
}

impl Context {
    pub fn as_ref(&self) -> ContextRef {
        ContextRef(self.0, PhantomData)
    }
    pub fn variant(&self) -> ContextVariant {
        unsafe { self.0.variant() }
    }
    pub fn unstable_id(&self) -> usize {
        self.0.unstable_id()
    }
}

impl From<SymbolRef<'_>> for Symbol {
    fn from(value: SymbolRef) -> Self {
        (*value).clone()
    }
}
impl From<TermRef<'_>> for Term {
    fn from(value: TermRef) -> Self {
        (*value).clone()
    }
}
impl From<ContextItemRef<'_>> for ContextItem {
    fn from(value: ContextItemRef) -> Self {
        (*value).clone()
    }
}
impl From<ContextRef<'_>> for Context {
    fn from(value: ContextRef) -> Self {
        (*value).clone()
    }
}
impl From<&'_ Symbol> for Symbol {
    fn from(value: &Symbol) -> Self {
        (*value).clone()
    }
}
impl From<&'_ Term> for Term {
    fn from(value: &Term) -> Self {
        (*value).clone()
    }
}
impl From<&'_ ContextItem> for ContextItem {
    fn from(value: &ContextItem) -> Self {
        (*value).clone()
    }
}
impl From<&'_ Context> for Context {
    fn from(value: &Context) -> Self {
        (*value).clone()
    }
}

impl From<SymbolVariant<'_>> for Symbol {
    fn from(variant: SymbolVariant<'_>) -> Self {
        Symbol(RawSymbol::new(variant))
    }
}

impl From<TermVariant<'_>> for Term {
    fn from(variant: TermVariant<'_>) -> Self {
        Term(RawTerm::new(variant))
    }
}

impl From<ContextItemVariant<'_>> for ContextItem {
    fn from(variant: ContextItemVariant<'_>) -> Self {
        ContextItem(RawContextItem::new(variant))
    }
}

impl From<ContextVariant<'_>> for Context {
    fn from(variant: ContextVariant<'_>) -> Self {
        Context(RawContext::new(variant))
    }
}

impl Symbol {
    fn add_to_fmt_buf(&self, buf: &mut FmtBuf) {
        buf.with_id(self.unstable_id(), |buf| match self.variant() {
            SymbolVariant::Name(name) => buf.write_str(name),
            SymbolVariant::Id(id) => buf.write_fmt(format_args!("{id}")),
            SymbolVariant::Scoped(parent, child) => {
                buf.write_str("(");
                parent.add_to_fmt_buf(buf);
                buf.write_str(").");
                child.add_to_fmt_buf(buf);
            }
        })
    }

    fn add_var_to_fmt_buf(&self, buf: &mut FmtBuf) {
        match self.variant() {
            SymbolVariant::Name(name) => buf.write_fmt(format_args!("?{name}")),
            SymbolVariant::Id(id) => buf.write_fmt(format_args!("?{id}")),
            SymbolVariant::Scoped(_, _) => {
                buf.write_str("VAR[");
                self.add_to_fmt_buf(buf);
                buf.write_str("]");
            }
        }
    }

    fn add_const_to_fmt_buf(&self, buf: &mut FmtBuf) {
        match self.variant() {
            SymbolVariant::Name(name) => buf.write_str(name),
            SymbolVariant::Id(id) => buf.write_fmt(format_args!("{id}")),
            SymbolVariant::Scoped(_, _) => {
                buf.write_str("CONST[");
                self.add_to_fmt_buf(buf);
                buf.write_str("]");
            }
        }
    }
}

impl Term {
    fn add_to_fmt_buf(&self, buf: &mut FmtBuf) {
        buf.with_id(self.unstable_id(), |buf| match self.variant() {
            TermVariant::Var(symbol) => {
                symbol.add_var_to_fmt_buf(buf);
            }
            TermVariant::Const(symbol) => {
                symbol.add_const_to_fmt_buf(buf);
            }
            TermVariant::Apply(name, args) => {
                name.add_const_to_fmt_buf(buf);
                buf.write_str("(");

                let mut sep = None;

                for arg in args {
                    if let Some(sep) = sep.replace(", ") {
                        buf.write_str(sep);
                    }
                    arg.add_to_fmt_buf(buf);
                }

                buf.write_str(")");
            }
            TermVariant::Ite(cond, then, els) => {
                buf.write_str("(");
                buf.write_str("if ");
                cond.add_to_fmt_buf(buf);
                buf.write_str(" then ");
                then.add_to_fmt_buf(buf);
                buf.write_str(" else ");
                els.add_to_fmt_buf(buf);
                buf.write_str(")");
            }
            TermVariant::Quantifier(kind, symbol, body) => {
                buf.write_str("(");
                buf.write_str(match kind {
                    QuantifierKind::Forall => "forall ",
                    QuantifierKind::Exists => "exists ",
                    QuantifierKind::Unique => "unique ",
                });
                symbol.add_var_to_fmt_buf(buf);
                buf.write_str(" => ");

                body.add_to_fmt_buf(buf);
                buf.write_str(")");
            }
            TermVariant::Abstraction(kind, symbol, domain, body) => {
                buf.write_str("(");
                buf.write_str(match kind {
                    AbstractionKind::Set => "set ",
                    AbstractionKind::Function => "function ",
                });
                symbol.add_var_to_fmt_buf(buf);
                buf.write_str(" : ");
                domain.add_to_fmt_buf(buf);
                buf.write_str(" => ");
                body.add_to_fmt_buf(buf);
                buf.write_str(")");
            }
            TermVariant::Choice(symbol, body) => {
                buf.write_str("(");
                buf.write_str("choose ");
                symbol.add_var_to_fmt_buf(buf);
                buf.write_str(" => ");
                body.add_to_fmt_buf(buf);
                buf.write_str(")");
            }
            TermVariant::ShortCircuit(op, lhs, rhs) => {
                buf.write_str("(");
                lhs.add_to_fmt_buf(buf);
                buf.write_str(match op {
                    ShortCircuitOp::And => " and ",
                    ShortCircuitOp::Or => " or ",
                    ShortCircuitOp::Implies => " implies ",
                });
                rhs.add_to_fmt_buf(buf);
                buf.write_str(")");
            }
        })
    }
}

impl ContextItem {
    fn add_to_fmt_buf(&self, buf: &mut FmtBuf) {
        match self.variant() {
            ContextItemVariant::Assumption([a, b]) => {
                a.add_to_fmt_buf(buf);
                if !match b.variant() {
                    TermVariant::Const(b) => {
                        matches!(b.variant(), SymbolVariant::Name("true"))
                    }
                    _ => false,
                } {
                    buf.write_str(" == ");
                    b.add_to_fmt_buf(buf);
                }
            }
            ContextItemVariant::ConstantDef(symbol, term) => {
                symbol.add_const_to_fmt_buf(buf);
                buf.write_str(" := ");
                term.add_to_fmt_buf(buf);
            }
            ContextItemVariant::ValueClassDef(symbol, value, body) => {
                symbol.add_const_to_fmt_buf(buf);
                buf.write_str("[");
                value.add_var_to_fmt_buf(buf);
                buf.write_str("] := ");
                body.add_to_fmt_buf(buf);
            }
        }
    }
}

impl Context {
    fn add_to_fmt_buf(&self, buf: &mut FmtBuf) {
        match self.variant() {
            ContextVariant::Empty => (),
            ContextVariant::Append(prefix, added) => {
                if !matches!(prefix.variant(), ContextVariant::Empty) {
                    prefix.add_to_fmt_buf(buf);
                    buf.write_str(", ");
                }
                added.add_to_fmt_buf(buf);
            }
        }
    }
}

pub struct WfTerm(pub Context, pub Term);

impl WfTerm {
    fn add_to_fmt_buf(&self, buf: &mut FmtBuf) {
        self.0.add_to_fmt_buf(buf);
        buf.write_str(" |- ");
        self.1.add_to_fmt_buf(buf);
    }
}

impl core::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = FmtBuf::default();
        self.add_to_fmt_buf(&mut buf);
        core::fmt::Display::fmt(&buf, f)
    }
}

impl core::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = FmtBuf::default();
        self.add_to_fmt_buf(&mut buf);
        core::fmt::Display::fmt(&buf, f)
    }
}

impl core::fmt::Display for ContextItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = FmtBuf::default();
        self.add_to_fmt_buf(&mut buf);
        core::fmt::Display::fmt(&buf, f)
    }
}
impl core::fmt::Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = FmtBuf::default();
        self.add_to_fmt_buf(&mut buf);
        core::fmt::Display::fmt(&buf, f)
    }
}
impl core::fmt::Display for WfTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = FmtBuf::default();
        self.add_to_fmt_buf(&mut buf);
        core::fmt::Display::fmt(&buf, f)
    }
}

impl core::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl core::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl core::fmt::Debug for ContextItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl core::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl core::fmt::Debug for WfTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        Symbol::from(SymbolVariant::Name(value))
    }
}

impl Term {
    pub fn constant(symbol: impl Into<Symbol>) -> Self {
        Term::from(TermVariant::Const(symbol.into().as_ref()))
    }

    pub fn var(symbol: impl Into<Symbol>) -> Self {
        Term::from(TermVariant::Var(symbol.into().as_ref()))
    }

    pub fn apply(symbol: impl Into<Symbol>, args: impl ApplyArgs) -> Self {
        args.with_args(|args| Term::from(TermVariant::Apply(symbol.into().as_ref(), args)))
    }
}

pub trait ApplyArgs {
    fn with_args<T>(self, f: impl FnOnce(&[TermRef]) -> T) -> T;
}

impl<A0: Into<Term>> ApplyArgs for (A0,) {
    fn with_args<T>(self, f: impl FnOnce(&[TermRef]) -> T) -> T {
        f(&[A0::into(self.0).as_ref()])
    }
}
impl<A0: Into<Term>, A1: Into<Term>> ApplyArgs for (A0, A1) {
    fn with_args<T>(self, f: impl FnOnce(&[TermRef]) -> T) -> T {
        f(&[A0::into(self.0).as_ref(), A1::into(self.1).as_ref()])
    }
}

impl ContextItem {
    pub fn assume_eq(a: impl Into<Term>, b: impl Into<Term>) -> Self {
        ContextItem::from(ContextItemVariant::Assumption([
            a.into().as_ref(),
            b.into().as_ref(),
        ]))
    }

    pub fn assume(a: impl Into<Term>) -> Self {
        Self::assume_eq(a, Term::constant("true"))
    }
}

impl From<TermRef<'_>> for ContextItem {
    fn from(value: TermRef) -> Self {
        ContextItem::assume(value)
    }
}
impl From<&'_ Term> for ContextItem {
    fn from(value: &Term) -> Self {
        ContextItem::assume(value)
    }
}
impl From<Term> for ContextItem {
    fn from(value: Term) -> Self {
        ContextItem::assume(value)
    }
}

impl Context {
    pub fn empty() -> Self {
        Context::from(ContextVariant::Empty)
    }

    pub fn append(ctx: impl Into<Context>, item: impl Into<ContextItem>) -> Self {
        Context::from(ContextVariant::Append(
            ctx.into().as_ref(),
            item.into().as_ref(),
        ))
    }

    pub fn push(&mut self, item: impl Into<ContextItem>) {
        *self = Self::append(self.as_ref(), item);
    }
}

mod fmt {
    use std::{collections::HashMap, fmt::Write};

    #[derive(Clone, Copy)]
    pub enum Chunk {
        Str(usize, usize),
        RefSlot,
        RefDef(usize),
        RefUse(usize),
    }

    #[derive(Default)]
    pub struct FmtBuf {
        str_buf: String,
        chunks: Vec<Chunk>,
        width: usize,
        ref_defs: HashMap<usize, usize>,
        ref_id: usize,
    }

    impl FmtBuf {
        pub fn write_str(&mut self, s: &str) {
            let start = self.str_buf.len();
            self.str_buf.push_str(s);
            let end = self.str_buf.len();

            self.width += s.len();

            match self.chunks.last_mut() {
                Some(Chunk::Str(_last_start, last_end)) if *last_end == start => {
                    *last_end = end;
                }
                _ => self.chunks.push(Chunk::Str(start, end)),
            }
        }
        pub fn write_fmt(&mut self, s: core::fmt::Arguments) {
            let start = self.str_buf.len();
            self.str_buf.write_fmt(s).unwrap();
            let end = self.str_buf.len();

            self.width += end - start;

            match self.chunks.last_mut() {
                Some(Chunk::Str(_last_start, last_end)) if *last_end == start => {
                    *last_end = end;
                }
                _ => self.chunks.push(Chunk::Str(start, end)),
            }
        }

        pub fn with_id(&mut self, id: usize, inner: impl FnOnce(&mut Self)) {
            if let Some(def_pos) = self.ref_defs.get(&id).copied() {
                let found_id = match &mut self.chunks[def_pos] {
                    chunk @ Chunk::RefSlot => {
                        let found_ref = self.ref_id;
                        self.ref_id += 1;
                        *chunk = Chunk::RefDef(found_ref);
                        found_ref
                    }
                    &mut Chunk::RefDef(id) => id,
                    _ => {
                        unreachable!()
                    }
                };

                self.chunks.push(Chunk::RefUse(found_id));

                return;
            }
            let placeholder_pos = self.chunks.len();
            self.chunks.push(Chunk::RefSlot);

            let start_width = self.width;

            inner(self);

            let inner_width = self.width - start_width;

            if inner_width >= 10 {
                let replaced = self.ref_defs.insert(id, placeholder_pos);
                assert!(replaced.is_none());
            }
        }
    }

    impl core::fmt::Display for FmtBuf {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for chunk in self.chunks.iter() {
                match *chunk {
                    Chunk::Str(start, end) => f.write_str(&self.str_buf[start..end])?,
                    Chunk::RefSlot => (),
                    Chunk::RefDef(id) => write!(f, "#{id} := ")?,
                    Chunk::RefUse(id) => write!(f, "#{id}")?,
                }
            }
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_some() {
        let bool_set = Term::constant("bool");

        let a = Term::var("a");
        let b = Term::var("b");

        let mut ctx = Context::empty();
        ctx.push(Term::apply("member", (&a, &bool_set)));
        ctx.push(ContextItem::assume_eq(&a, &b));

        let not_b = Term::apply("not", (b.as_ref(),));
        let mut term = Term::apply("and", (a.as_ref(), not_b.as_ref()));

        for _ in 0..3 {
            term = Term::apply("and", (term.as_ref(), term.as_ref()));
        }

        let wf_term = WfTerm(ctx, term);

        println!("{wf_term:?}");
    }
}