use std::{
    fmt::{self, Debug},
    hash::{BuildHasher, BuildHasherDefault, Hash},
    marker::PhantomData,
    ops::Deref,
};

use zwohash::ZwoHasher;

use crate::ir::var::{Lit, Var};

/// Allows using a type for internal represenation nodes.
///
/// Everything that is object-safe is part of the [`NodeDyn`] supertrait.
pub trait Node: NodeDyn + Debug + Eq + Hash + Clone + 'static {
    /// A short name identifying the node type.
    const NAME: &'static str;

    /// Returns an iterator over all input variables of the node.
    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [].into_iter()
    }

    /// Returns an iterator over all input variables that should be taken into consideration when
    /// maintaining acyclicity.
    ///
    /// The default implementation forwards to [`input_var_iter`][Self::input_var_iter].
    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.input_var_iter()
    }

    /// Returns whether two nodes can be treated as equivalent.
    ///
    /// In particular for [`ValueNode`][crate::ir::node::value::ValueNode] nodes, the output variable
    /// or literal is ignored as it is fully defined in terms of the input variables.
    ///
    /// This defaults to forwarding to [`Eq`], which is correct for constraint nodes.
    fn def_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Rewrites all variables in the value using a given mapping.
    fn apply_var_map(&mut self, var_map: impl FnMut(Var) -> Lit) {
        let _ = var_map;

        // When a node contains variables, this default implementation isn't correct
        debug_assert!(self.input_var_iter().next().is_none());
        debug_assert!(self.output_var().is_none());
    }
}

/// Object-safe supertrait for [`Node`].
pub trait NodeDyn: NodeDynAuto + Debug {
    /// Returns a hash value of the defining part of a node.
    ///
    /// In particular for [`ValueNode`][crate::ir::node::value::ValueNode] nodes, the output
    /// variable or literal is ignored as it is fully defined in terms of the input variables.
    ///
    /// This defaults to forwarding to [`Hash`] using a [`ZwoHasher`].
    fn def_hash(&self) -> u64 {
        self.zzz_hidden_default_def_hash()
    }

    /// Returns a representative input variable.
    ///
    /// This is used like a hash value that is restricted to the contained variables or
    /// `Var::FALSE` and enables certain internal optimizations.
    fn representative_input_var(&self) -> Var {
        self.zzz_hidden_default_representative_input_var()
    }

    /// Returns the output variable defined by this node or `None` if this node does not have an
    /// output.
    ///
    /// If the output is a literal, the literal's variable is still considered an output variable in
    /// the context of this method.
    fn output_var(&self) -> Option<Var> {
        None
    }

    /// Returns the output literal defined by this node or `None` if this node does not have an
    /// output.
    ///
    /// If the output is a variable, this should return a positive polarity literal for that
    /// variable, which is what the default implementation does.
    fn output_lit(&self) -> Option<Lit> {
        self.output_var().map(|var| var.as_pos())
    }

    /// Returns the variable with the largest id among all variables referenced by this node.
    fn max_var(&self) -> Var {
        self.zzz_hidden_default_max_var()
    }
}

/// Automatically implemented object-safe supertrait for [`Node`].
///
/// This contains object-safe methods that will be automatically implemented via a blanket
/// implementation using the provided [`Node`] and [`NodeDyn`] items.
pub trait NodeDynAuto: Debug {
    /// Returns the dynamic [`NodeType`] corresponding to the concrete [`Node`] implementation
    /// for trait object.
    fn node_type(&self) -> NodeType;

    /// Object safe wrapper of [`Node::input_var_iter`].
    fn dyn_foreach_input_var(&self, f: &mut dyn FnMut(Var) -> bool);

    /// Object safe wrapper of [`Node::def_eq`].
    fn dyn_def_eq(&self, other: &DynNode) -> bool;

    /// Object safe wrapper of [`Node::apply_var_map`].
    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit);

    #[doc(hidden)]
    fn zzz_hidden_default_representative_input_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_max_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_def_hash(&self) -> u64;
}

impl<T: Node> NodeDynAuto for T {
    fn node_type(&self) -> NodeType {
        NodeType::of::<T>()
    }

    fn dyn_foreach_input_var(&self, f: &mut dyn FnMut(Var) -> bool) {
        for var in self.input_var_iter() {
            if !f(var) {
                break;
            }
        }
    }

    fn dyn_def_eq(&self, other: &DynNode) -> bool {
        let Some(other) = other.dyn_cast::<Self>() else {
            return false;
        };

        self.def_eq(other)
    }

    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit) {
        self.apply_var_map(var_repr)
    }

    #[inline(always)]
    fn zzz_hidden_default_representative_input_var(&self) -> Var {
        self.input_var_iter().next().unwrap_or(Var::FALSE)
    }

    #[inline(always)]
    fn zzz_hidden_default_max_var(&self) -> Var {
        self.input_var_iter()
            .max()
            .unwrap_or(Var::FALSE)
            .max(self.output_var().unwrap_or(Var::FALSE))
    }

    #[inline(always)]
    fn zzz_hidden_default_def_hash(&self) -> u64 {
        <BuildHasherDefault<ZwoHasher>>::default().hash_one(self)
    }
}

/// A trait object for a [`Node`] type.
///
/// Since [`Node`] is not object safe, we cannot use `dyn Node`. Instead have to use a separate
/// [`NodeDyn`] trait and provide this type alias to avoid the need for `dyn NodeDyn`.
pub type DynNode = dyn NodeDyn;

impl DynNode {
    /// Performs a checked dyamic cast of a [`DynNode`] to a given [`Node`] type.
    pub fn dyn_cast<T: Node>(&self) -> Option<&T> {
        if self.node_type() != NodeType::of::<T>() {
            None
        } else {
            // SAFETY: we checked the `node_type` which works like `TypeId`.
            Some(unsafe { &*(self as *const DynNode as *const T) })
        }
    }
}

/// The [`GenericNodeType`] implementation for a statically known node type.
#[repr(transparent)]
pub(super) struct KnownNodeType<T>(NodeType, PhantomData<T>);

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
pub(super) struct DynNodeType(NodeType);

impl Deref for DynNodeType {
    type Target = NodeTypeVTable;

    fn deref(&self) -> &Self::Target {
        self.0 .0
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
    /// # Safety
    /// The
    unsafe fn drop_in_place(&self, ptr: *mut u8);

    /// Cast `ptr` to a `*mut T` if `T` is statically known, or to a `*mut DynNode` otherwise.
    unsafe fn cast_mut_ptr(&self, ptr: *mut u8) -> *mut Self::RefTarget;
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

pub(super) struct NodeTypeVTable {
    type_info: fn() -> (std::any::TypeId, &'static str),
    name: &'static str,
    slot_size: usize,
    slot_align: usize,
    needs_drop: bool,
    drop_in_place: unsafe fn(*mut u8),
    dyn_vtable: *const u8,
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
            slot_size: std::mem::size_of::<super::nodes::ChunkSlot<T>>(),
            slot_align: std::mem::align_of::<super::nodes::ChunkSlot<T>>(),
            needs_drop: std::mem::needs_drop::<T>(),
            // SAFETY: field is an `unsafe fn`
            drop_in_place: unsafe { |ptr: *mut u8| ptr.cast::<T>().drop_in_place() },
            // SAFETY: see [`WidePtr`] documentation
            dyn_vtable: unsafe {
                std::mem::transmute::<*const DynNode, WidePtr>(std::ptr::null::<T>()).vtable
            },
        }
    }
}

/// Manually constructed representation of `*mut dyn Trait` pointers.
///
/// This is used as workaround for the lack of a stable pointer metadata API and should be replaced
/// by the use of such an API as soon as it becomes stable.
///
/// I'm assuming that some form of such an API will be stabilized before the representation of such
/// pointers ever changes. Just in case that assumption turns out to be incorrect, this includes
/// compile time checks that verify this struct against the actual pointer layout.
#[repr(C)]
struct WidePtr {
    ptr: *mut u8,
    vtable: *const u8,
}

/// This is a compile time check to ensure that `WidePtr` matches the layout of a `*mut dyn Trait`
/// pointer.
const _: () = {
    let u32_ptr = 0usize as *mut u32;
    let u32_dyn_ptr = u32_ptr as *mut dyn Debug;
    let char_ptr = 0usize as *mut char;
    let char_dyn_ptr = char_ptr as *mut dyn Debug;

    // SAFETY: If there is a mismatch in size, the transmute will fail to compile. With the given
    // size, there is just enough space for the underlying pointer and a single vtable pointer, with
    // two possible field orderings. If the field ordering doesn't match, on current rustc versions,
    // the comparison will fail because compile time integer vs pointer comparisons are not allowed.
    // In a potential future rustc versions that allows compile time null pointer checks, at least
    // one of the asserts will fail as it can't be the case that the `dyn Debug` vtables for both
    // `u32` and `char` are null pointers.
    unsafe {
        let wide_ptr: WidePtr = std::mem::transmute(u32_dyn_ptr);
        assert!(std::mem::transmute::<*mut u8, usize>(wide_ptr.ptr) == 0);

        let wide_ptr: WidePtr = std::mem::transmute(char_dyn_ptr);
        assert!(std::mem::transmute::<*mut u8, usize>(wide_ptr.ptr) == 0);
    }
};
