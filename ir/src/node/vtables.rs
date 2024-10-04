use std::{
    fmt::{self, Debug},
    hash::Hash,
    marker::PhantomData,
    mem::{ManuallyDrop, MaybeUninit},
    ops::Deref,
};

use crate::{var::Lit, wide_ptr::WidePtr};

use super::{
    collections::nodes::Nodes,
    generic::{
        DynNode, DynTerm, Node, NodeDyn, SealedWrapper, Term, TermDyn, TermNode, TermWrapper,
    },
    NodeId,
};

pub(crate) struct NodeTypeVTable {
    static_type_info: usize,
    type_info: fn() -> (std::any::TypeId, &'static str),
    name: &'static str,
    slot_size: usize,
    slot_align: usize,
    needs_drop: bool,
    drop_in_place: unsafe fn(*mut u8),
    dyn_vtable: *const u8,
    insert_raw_dyn: unsafe fn(&mut Nodes, *mut u8) -> (NodeId, *mut u8),
    wrapped_term_type: Option<fn() -> TermType>,
}

impl PartialEq for NodeTypeVTable {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        if self.static_type_info == other.static_type_info {
            if self.static_type_info == 0 {
                (self.type_info)().0 == (other.type_info)().0
            } else {
                true
            }
        } else {
            false
        }
    }
}

impl Eq for NodeTypeVTable {}

impl Hash for NodeTypeVTable {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.static_type_info != 0 {
            self.static_type_info.hash(state)
        } else {
            (self.type_info)().0.hash(state)
        }
    }
}

impl Debug for NodeTypeVTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ({}) {:p}",
            self.name,
            (self.type_info)().1,
            self.type_info
        )
    }
}

impl NodeTypeVTable {
    #[inline(never)]
    fn type_info<T: Node>() -> (std::any::TypeId, &'static str) {
        (std::any::TypeId::of::<T>(), std::any::type_name::<T>())
    }

    pub const fn of<T: Node>() -> Self {
        Self {
            static_type_info: match T::STATIC_TYPE_INFO {
                SealedWrapper(inner) => inner,
            },
            type_info: Self::type_info::<T>,
            name: T::NAME,
            slot_size: std::mem::size_of::<super::collections::nodes::ChunkSlot<T>>(),
            slot_align: std::mem::align_of::<super::collections::nodes::ChunkSlot<T>>(),
            needs_drop: std::mem::needs_drop::<T>(),
            // SAFETY: field is an `unsafe fn`
            drop_in_place: unsafe { |ptr: *mut u8| ptr.cast::<T>().drop_in_place() },
            // SAFETY: see [`WidePtr`] documentation
            dyn_vtable: unsafe {
                std::mem::transmute::<*const DynNode, WidePtr>(std::ptr::null::<T>()).vtable
            },
            // SAFETY: field is an `unsafe fn`
            insert_raw_dyn: unsafe {
                |nodes: &mut Nodes, ptr: *mut u8| {
                    let node = ptr.cast::<T>();
                    let (node_id, ptr) = nodes.insert_raw(node.read());
                    (node_id, ptr as *mut u8)
                }
            },
            wrapped_term_type: {
                if let Some(SealedWrapper(wrapper)) = T::TERM_TYPE_FOR_TERM_WRAPPER {
                    Some(wrapper)
                } else {
                    None
                }
            },
        }
    }
}

pub(crate) struct TermTypeVTable {
    static_type_info: usize,
    type_info: fn() -> (std::any::TypeId, &'static str),
    name: &'static str,
    build_term_node: for<'a> unsafe fn(Lit, *mut u8, &'a mut dyn FnMut(*mut u8)),
    dyn_vtable: *const u8,
    term_node_type: NodeType,
    wrapper_node_type: NodeType,
}

impl PartialEq for TermTypeVTable {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        if self.static_type_info == other.static_type_info {
            if self.static_type_info == 0 {
                (self.type_info)().0 == (other.type_info)().0
            } else {
                true
            }
        } else {
            false
        }
    }
}

impl Eq for TermTypeVTable {}

impl Hash for TermTypeVTable {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.static_type_info != 0 {
            self.static_type_info.hash(state)
        } else {
            (self.type_info)().0.hash(state)
        }
    }
}

impl Debug for TermTypeVTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, (self.type_info)().1)
    }
}

impl TermTypeVTable {
    #[inline(never)]
    fn type_info<T: Term>() -> (std::any::TypeId, &'static str) {
        (std::any::TypeId::of::<T>(), std::any::type_name::<T>())
    }

    pub const fn of<T: Term>() -> Self {
        TermTypeVTable {
            static_type_info: match T::STATIC_TYPE_INFO {
                SealedWrapper(value) => value,
            },
            type_info: Self::type_info::<T>,
            name: T::NAME,
            // SAFETY: field is an `unsafe fn`, this obtains ownership of term_ptr's target and
            // passes ownership on to the callback. It's documented that the storage for the
            // callback argument has a lifetime limited to the callback.
            build_term_node: unsafe {
                |output: Lit, term_ptr: *mut u8, callback: &mut dyn FnMut(*mut u8)| {
                    let term_ptr = term_ptr as *mut T;
                    let mut term_node = ManuallyDrop::new(TermNode {
                        output: output.try_into().unwrap(),
                        term: term_ptr.read(),
                    });
                    callback(&mut term_node as *mut ManuallyDrop<TermNode<T>> as *mut u8);
                }
            },
            term_node_type: NodeType::of::<TermNode<T>>(),
            wrapper_node_type: NodeType::of::<TermWrapper<T>>(),
            // SAFETY: see [`WidePtr`] documentation
            dyn_vtable: unsafe {
                std::mem::transmute::<*const DynTerm, WidePtr>(std::ptr::null::<T>()).vtable
            },
        }
    }
}

/// Transparent wrapper for a custom vtable pointer ([`NodeType`]) for a [`Node`] type `T`.
///
/// If `T` is known statically, this avoids the indirection through the vtable.
pub(super) trait GenericNodeType {
    /// If the node type is statically known this is the same as the node type, otherwise it is
    /// `DynNode` aka `dyn NodeDyn`.
    type RefTarget: NodeDyn + ?Sized;

    fn has_type<T: Node>(&self) -> bool;

    /// Returns `std::mem::size_of::<ChunkSlot<T>>()`.
    fn slot_size(&self) -> usize;
    /// Returns `std::mem::align_of::<ChunkSlot<T>>()`.
    fn slot_align(&self) -> usize;

    /// Cast `ptr` to a `*mut T` type and calls `drop_in_place`
    unsafe fn drop_in_place(&self, ptr: *mut u8);

    /// Cast `ptr` to a `*mut T` if `T` is statically known, or to a `*mut DynNode` otherwise.
    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget;

    /// Inserts the node pointed to by `node` into `nodes`.
    ///
    /// # Safety
    /// The `node` ptr must have the node type represented by `self` and it must be valid to move
    /// the node out of the pointed to storage. The returned pointer is the new location of the node
    /// within `nodes`.
    unsafe fn insert_raw_dyn(&self, nodes: &mut Nodes, node: *mut u8) -> (NodeId, *mut u8);

    fn wrapped_term_type(&self) -> Option<TermType>;
}

/// The [`GenericNodeType`] implementation for a statically known node type.
#[repr(transparent)]
pub(crate) struct KnownNodeType<T>(NodeType, PhantomData<T>);

impl<T: Node> KnownNodeType<T> {
    pub const fn new() -> Self {
        KnownNodeType(NodeType::of::<T>(), PhantomData)
    }
}

impl<T> Deref for KnownNodeType<T> {
    type Target = NodeTypeVTable;

    fn deref(&self) -> &Self::Target {
        self.0 .0
    }
}

/// The [`GenericNodeType`] implementation that dispatches through the [`NodeType`] vtable.
#[repr(transparent)]
pub(super) struct DynNodeType(pub NodeType);

impl Deref for DynNodeType {
    type Target = NodeTypeVTable;

    fn deref(&self) -> &Self::Target {
        self.0 .0
    }
}

impl GenericNodeType for DynNodeType {
    type RefTarget = DynNode;

    fn has_type<T: Node>(&self) -> bool {
        self.0 == NodeType::of::<T>()
    }

    fn slot_size(&self) -> usize {
        self.slot_size
    }

    fn slot_align(&self) -> usize {
        self.slot_align
    }

    unsafe fn drop_in_place(&self, ptr: *mut u8) {
        if self.needs_drop {
            // SAFETY: forwarding with same safety requirements
            unsafe { (self.drop_in_place)(ptr) }
        }
    }

    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget {
        // SAFETY: see documentation for `WidePtr`
        unsafe {
            std::mem::transmute(WidePtr {
                ptr,
                vtable: self.dyn_vtable,
            })
        }
    }

    unsafe fn insert_raw_dyn(&self, nodes: &mut Nodes, node: *mut u8) -> (NodeId, *mut u8) {
        // SAFETY: forwarding with same safety requirements
        unsafe { (self.insert_raw_dyn)(nodes, node) }
    }

    fn wrapped_term_type(&self) -> Option<TermType> {
        let get_term_type = self.wrapped_term_type?;
        Some(get_term_type())
    }
}

impl<T: Node> GenericNodeType for KnownNodeType<T> {
    type RefTarget = T;

    fn has_type<U: Node>(&self) -> bool {
        NodeType::of::<T>() == NodeType::of::<U>()
    }

    fn slot_size(&self) -> usize {
        std::mem::size_of::<T>()
    }

    fn slot_align(&self) -> usize {
        std::mem::align_of::<T>()
    }

    unsafe fn drop_in_place(&self, ptr: *mut u8) {
        // SAFETY: forwarding with same safety requirements
        unsafe { ptr.cast::<T>().drop_in_place() }
    }

    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget {
        ptr as *mut T
    }

    unsafe fn insert_raw_dyn(&self, nodes: &mut Nodes, node: *mut u8) -> (NodeId, *mut u8) {
        // SAFETY: we
        let (node_id, ptr) = unsafe { nodes.insert_raw((node as *mut T).read()) };
        (node_id, ptr as *mut u8)
    }

    fn wrapped_term_type(&self) -> Option<TermType> {
        let SealedWrapper(get_term_type) = T::TERM_TYPE_FOR_TERM_WRAPPER?;
        Some(get_term_type())
    }
}

pub(super) trait GenericTermType {
    type RefTarget: TermDyn + ?Sized;
    type GenericTermNodeType: GenericNodeType;
    type GenericTermWrapperType: GenericNodeType;

    /// Build a `TermNode` out of an output literal and a `Term`, consuming the `Term`.
    ///
    /// # Safety
    /// The `Term` pointer must point to a `Term` having the correct type for `self`. The `Term`
    /// is moved out of the storage pointed to by `Term`. The closure is called with the
    /// constructed `TermNode` passed as only argument and obtains ownership of that `TermNode`.
    /// The storage for the constructed `TermNode` is only valid for the duration of the callback.
    unsafe fn build_term_node<R>(
        &self,
        output: Lit,
        term: *mut u8,
        f: impl FnOnce(*mut u8) -> R,
    ) -> R;

    fn term_node(&self) -> Self::GenericTermNodeType;
    fn term_wrapper(&self) -> Self::GenericTermWrapperType;

    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget;
}

/// The [`GenericTermType`] implementation that dispatches through the [`TermType`] vtable.
#[repr(transparent)]
pub(super) struct DynTermType(pub TermType);

impl Deref for DynTermType {
    type Target = TermTypeVTable;

    fn deref(&self) -> &Self::Target {
        self.0 .0
    }
}

impl GenericTermType for DynTermType {
    type RefTarget = DynTerm;
    type GenericTermNodeType = DynNodeType;
    type GenericTermWrapperType = DynNodeType;

    unsafe fn build_term_node<R>(
        &self,
        output: Lit,
        term: *mut u8,
        f: impl FnOnce(*mut u8) -> R,
    ) -> R {
        let mut f = ManuallyDrop::new(f);
        let mut res = MaybeUninit::uninit();

        // SAFETY: the vtable method follows our documented safety requirements
        unsafe {
            (self.build_term_node)(output, term, &mut |node: *mut u8| {
                res.write(ManuallyDrop::take(&mut f)(node));
            });
            res.assume_init_read()
        }
    }

    fn term_node(&self) -> Self::GenericTermNodeType {
        DynNodeType(self.term_node_type)
    }

    fn term_wrapper(&self) -> Self::GenericTermWrapperType {
        DynNodeType(self.wrapper_node_type)
    }

    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget {
        // SAFETY: see documentation for `WidePtr`
        unsafe {
            std::mem::transmute(WidePtr {
                ptr,
                vtable: self.dyn_vtable,
            })
        }
    }
}

struct NodeTypeVTables<T>(PhantomData<T>);

impl<T: Node> NodeTypeVTables<T> {
    const VTABLE: NodeTypeVTable = NodeTypeVTable::of::<T>();
    const NODE_TYPE: NodeType = NodeType(&Self::VTABLE);
}

/// Dynamic type id for a [`Node`] implementation.
///
/// This can be seen as a [`std::any::TypeId`] that is statically known to refer to some type
/// implementing the [`Node`] trait.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeType(&'static NodeTypeVTable);

impl NodeType {
    /// Returns the unique `NodeType` for a given [`Node`] implementation `T`.
    pub const fn of<T: Node>() -> Self {
        NodeTypeVTables::<T>::NODE_TYPE
    }
}

impl Debug for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.0, f)
    }
}

impl NodeType {
    /// Returns the [`Node::NAME`] of the given node type.
    pub const fn name(&self) -> &'static str {
        self.0.name
    }
}

struct TermTypeVTables<T>(PhantomData<T>);

impl<T: Term> TermTypeVTables<T> {
    const VTABLE: TermTypeVTable = TermTypeVTable::of::<T>();
    const TERM_TYPE: TermType = TermType(&Self::VTABLE);
}

/// Dynamic type id for a [`Term`] implementation.
///
/// This can be seen as a [`std::any::TypeId`] that is statically known to refer to some type
/// implementing the [`Term`] trait.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TermType(&'static TermTypeVTable);

impl TermType {
    /// Returns the unique `TermType` for a given [`Term`] implementation `T`.
    pub const fn of<T: Term>() -> Self {
        <TermTypeVTables<T>>::TERM_TYPE
    }
}

impl Debug for TermType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.0, f)
    }
}

impl TermType {
    /// Returns the [`Node::NAME`] of the given term type.
    pub const fn name(&self) -> &'static str {
        self.0.name
    }
}
