use std::{
    fmt::{self, Debug},
    hash::Hash,
    marker::PhantomData,
    mem::{ManuallyDrop, MaybeUninit},
    ops::Deref,
};

use crate::{
    ir::var::{Lit, VarOrLit},
    wide_ptr::WidePtr,
};

use super::{
    collections::nodes::Nodes,
    generic::{
        DynNode, DynValue, Node, NodeDyn, SealedWrapper, Value, ValueDyn, ValueNode, ValueWrapper,
    },
    NodeId,
};

pub(crate) struct NodeTypeVTable {
    type_info: fn() -> (std::any::TypeId, &'static str),
    name: &'static str,
    slot_size: usize,
    slot_align: usize,
    needs_drop: bool,
    drop_in_place: unsafe fn(*mut u8),
    dyn_vtable: *const u8,
    insert_raw_dyn: unsafe fn(&mut Nodes, *mut u8) -> (NodeId, *mut u8),
    wrapped_value_type: Option<fn() -> ValueType>,
}

impl PartialEq for NodeTypeVTable {
    fn eq(&self, other: &Self) -> bool {
        self.type_info == other.type_info
    }
}

impl Eq for NodeTypeVTable {}

impl Hash for NodeTypeVTable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.type_info as usize).hash(state);
    }
}

impl Debug for NodeTypeVTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, (self.type_info)().1)
    }
}

impl NodeTypeVTable {
    #[inline(never)]
    fn type_info<T: Node>() -> (std::any::TypeId, &'static str) {
        (std::any::TypeId::of::<T>(), std::any::type_name::<T>())
    }

    pub const fn of<T: Node>() -> Self {
        Self {
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
            wrapped_value_type: {
                if let Some(SealedWrapper(wrapper)) = T::VALUE_TYPE_FOR_VALUE_WRAPPER {
                    Some(wrapper)
                } else {
                    None
                }
            },
        }
    }
}

pub(crate) struct ValueTypeVTable {
    type_info: fn() -> (std::any::TypeId, &'static str),
    name: &'static str,
    build_value_node: for<'a> unsafe fn(Lit, *mut u8, &'a mut dyn FnMut(*mut u8)),
    dyn_vtable: *const u8,
    value_node_type: NodeType,
    wrapper_node_type: NodeType,
}

impl PartialEq for ValueTypeVTable {
    fn eq(&self, other: &Self) -> bool {
        self.type_info == other.type_info
    }
}

impl Eq for ValueTypeVTable {}

impl Hash for ValueTypeVTable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.type_info as usize).hash(state);
    }
}

impl Debug for ValueTypeVTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, (self.type_info)().1)
    }
}

impl ValueTypeVTable {
    #[inline(never)]
    fn type_info<T: Value>() -> (std::any::TypeId, &'static str) {
        (std::any::TypeId::of::<T>(), std::any::type_name::<T>())
    }

    pub const fn of<T: Value>() -> Self {
        ValueTypeVTable {
            type_info: Self::type_info::<T>,
            name: T::NAME,
            // SAFETY: field is an `unsafe fn`, this obtains ownership of value_ptr's target and
            // passes ownership on to the callback. It's documented that the storage for the
            // callback argument has a lifetime limited to the callback.
            build_value_node: unsafe {
                |output: Lit, value_ptr: *mut u8, callback: &mut dyn FnMut(*mut u8)| {
                    let value_ptr = value_ptr as *mut T;
                    let mut value_node = ManuallyDrop::new(ValueNode {
                        output: VarOrLit::build_var_or_lit(output, |lit| lit.var(), |lit| lit),
                        value: value_ptr.read(),
                    });
                    callback(&mut value_node as *mut ManuallyDrop<ValueNode<T>> as *mut u8);
                }
            },
            value_node_type: NodeType::of::<ValueNode<T>>(),
            wrapper_node_type: NodeType::of::<ValueWrapper<T>>(),
            // SAFETY: see [`WidePtr`] documentation
            dyn_vtable: unsafe {
                std::mem::transmute::<*const DynValue, WidePtr>(std::ptr::null::<T>()).vtable
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

    fn node_type(&self) -> NodeType;

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

    fn wrapped_value_type(&self) -> Option<ValueType>;
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

    fn node_type(&self) -> NodeType {
        self.0
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

    fn wrapped_value_type(&self) -> Option<ValueType> {
        let get_value_type = self.wrapped_value_type?;
        Some(get_value_type())
    }
}

impl<T: Node> GenericNodeType for KnownNodeType<T> {
    type RefTarget = T;

    fn has_type<U: Node>(&self) -> bool {
        NodeType::of::<T>() == NodeType::of::<U>()
    }

    fn node_type(&self) -> NodeType {
        NodeType::of::<T>()
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

    fn wrapped_value_type(&self) -> Option<ValueType> {
        let SealedWrapper(get_value_type) = T::VALUE_TYPE_FOR_VALUE_WRAPPER?;
        Some(get_value_type())
    }
}

pub(super) trait GenericValueType {
    type RefTarget: ValueDyn + ?Sized;
    type GenericValueNodeType: GenericNodeType;
    type GenericValueWrapperType: GenericNodeType;

    /// Build a `ValueNode` out of an output literal and a `Value`, consuming the `Value`.
    ///
    /// # Safety
    /// The `value` pointer must point to a `Value` having the correct type for `self`. The `Value`
    /// is moved out of the storage pointed to by `value`. The closure is called with the
    /// constructed `ValueNode` passed as only argument and obtains ownership of that `ValueNode`.
    /// The storage for the constructed `ValueNode` is only valid for the duration of the callback.
    unsafe fn build_value_node<R>(
        &self,
        output: Lit,
        value: *mut u8,
        f: impl FnOnce(*mut u8) -> R,
    ) -> R;

    fn value_node(&self) -> Self::GenericValueNodeType;
    fn value_wrapper(&self) -> Self::GenericValueWrapperType;

    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget;
}

/// The [`GenericNodeType`] implementation for a statically known node type.
#[repr(transparent)]
pub(crate) struct KnownValueType<T>(ValueType, PhantomData<T>);

impl<T: Value> KnownValueType<T> {
    pub const fn new() -> Self {
        KnownValueType(ValueType::of::<T>(), PhantomData)
    }
}

impl<T> Deref for KnownValueType<T> {
    type Target = ValueTypeVTable;

    fn deref(&self) -> &Self::Target {
        self.0 .0
    }
}

/// The [`GenericValueType`] implementation that dispatches through the [`ValueType`] vtable.
#[repr(transparent)]
pub(super) struct DynValueType(pub ValueType);

impl Deref for DynValueType {
    type Target = ValueTypeVTable;

    fn deref(&self) -> &Self::Target {
        self.0 .0
    }
}

impl GenericValueType for DynValueType {
    type RefTarget = DynValue;
    type GenericValueNodeType = DynNodeType;
    type GenericValueWrapperType = DynNodeType;

    unsafe fn build_value_node<R>(
        &self,
        output: Lit,
        value: *mut u8,
        f: impl FnOnce(*mut u8) -> R,
    ) -> R {
        let mut f = ManuallyDrop::new(f);
        let mut res = MaybeUninit::uninit();

        // SAFETY: the vtable method follows our documented safety requirements
        unsafe {
            (self.build_value_node)(output, value, &mut |node: *mut u8| {
                res.write(ManuallyDrop::take(&mut f)(node));
            });
            res.assume_init_read()
        }
    }

    fn value_node(&self) -> Self::GenericValueNodeType {
        DynNodeType(self.value_node_type)
    }

    fn value_wrapper(&self) -> Self::GenericValueWrapperType {
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

impl<T: Value> GenericValueType for KnownValueType<T> {
    type RefTarget = T;
    type GenericValueNodeType = KnownNodeType<ValueNode<T>>;
    type GenericValueWrapperType = KnownNodeType<ValueWrapper<T>>;

    unsafe fn build_value_node<R>(
        &self,
        output: Lit,
        value: *mut u8,
        f: impl FnOnce(*mut u8) -> R,
    ) -> R {
        let value = value as *mut T;

        let node = ValueNode {
            output: <T::Output as VarOrLit>::build_var_or_lit(output, |lit| lit.var(), |lit| lit),
            // SAFETY: it's documented that we take ownership of the pointed to value
            value: unsafe { value.read() },
        };

        give!(node = node);

        f(node.into_raw_ptr() as *mut u8)
    }

    fn value_node(&self) -> Self::GenericValueNodeType {
        KnownNodeType::new()
    }

    fn value_wrapper(&self) -> Self::GenericValueWrapperType {
        KnownNodeType::new()
    }

    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget {
        ptr as *mut T
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
    /// Returns the [`Node::NAME`] value of the given node type.
    pub const fn name(&self) -> &'static str {
        self.0.name
    }
}

struct ValueTypeVTables<T>(PhantomData<T>);

impl<T: Value> ValueTypeVTables<T> {
    const VTABLE: ValueTypeVTable = ValueTypeVTable::of::<T>();
    const VALUE_TYPE: ValueType = ValueType(&Self::VTABLE);
}

/// Dynamic type id for a [`Value`] implementation.
///
/// This can be seen as a [`std::any::TypeId`] that is statically known to refer to some type
/// implementing the [`Value`] trait.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueType(&'static ValueTypeVTable);

impl ValueType {
    /// Returns the unique `ValueType` for a given [`Value`] implementation `T`.
    pub const fn of<T: Value>() -> Self {
        <ValueTypeVTables<T>>::VALUE_TYPE
    }
}

impl Debug for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.0, f)
    }
}

impl ValueType {
    /// Returns the [`Node::NAME`] value of the given node type.
    pub const fn name(&self) -> &'static str {
        self.0.name
    }
}
