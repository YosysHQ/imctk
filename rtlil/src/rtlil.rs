use std::fmt::Write;
use std::hash::BuildHasherDefault;
use std::result::Result;

use crate::id_table::{Id, IdTable};
use crate::parser::{self, AlreadyDefinedError, DesignSink, WireRefError};
use bstr::{BStr, BString, ByteSlice};
use zwohash::ZwoHasher;

pub type BitUSize = u32;
pub type BitISize = i32;

type IndexMap<K, V> = indexmap::IndexMap<K, V, BuildHasherDefault<ZwoHasher>>;
type IndexSet<K> = indexmap::IndexSet<K, BuildHasherDefault<ZwoHasher>>;

#[derive(Default)]
pub struct Design {
    pub id_table: IdTable,
    pub modules: IndexMap<Id, Module>,
}

impl std::fmt::Debug for Design {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id_table.install(|| {
            f.debug_struct("Design")
                .field("modules", &self.modules)
                .finish()
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum State {
    S0 = 0,
    S1 = 1,
    Sx,
    Sz,
    Sa,
    Sm,
}

impl From<bool> for State {
    fn from(value: bool) -> Self {
        if value {
            State::S1
        } else {
            State::S0
        }
    }
}

impl State {
    pub fn is_fully_defined(self) -> bool {
        matches!(self, State::S0 | State::S1)
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(parser::State::from(*self) as u8 as char)
    }
}

impl From<parser::State> for State {
    fn from(value: parser::State) -> Self {
        match value {
            parser::State::S0 => State::S0,
            parser::State::S1 => State::S1,
            parser::State::Sx => State::Sx,
            parser::State::Sz => State::Sz,
            parser::State::Sm => State::Sm,
            parser::State::Sa => State::Sa,
        }
    }
}

impl From<State> for parser::State {
    fn from(value: State) -> Self {
        match value {
            State::S0 => parser::State::S0,
            State::S1 => parser::State::S1,
            State::Sx => parser::State::Sx,
            State::Sz => parser::State::Sz,
            State::Sm => parser::State::Sm,
            State::Sa => parser::State::Sa,
        }
    }
}

#[derive(Default, PartialEq, Eq, Hash)]
pub struct Const {
    inner: ConstInner,
}

impl std::fmt::Debug for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let ConstInner::String(value) = &self.inner {
            std::fmt::Debug::fmt(value, f)
        } else if let Some(value) = self.as_exact_width_i32() {
            std::fmt::Display::fmt(&value, f)
        } else {
            let len = self.len();
            f.write_fmt(format_args!("{}'", len))?;
            for i in (0..len).rev() {
                f.write_char(parser::State::from(self.get(i)) as u8 as char)?;
            }

            Ok(())
        }
    }
}

impl Const {
    pub fn len(&self) -> BitUSize {
        match &self.inner {
            ConstInner::Int(_) => 32,
            ConstInner::Bits(value) => value.len() as BitUSize,
            ConstInner::String(value) => (value.len() * 8) as BitUSize,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_fully_defined(&self) -> bool {
        match &self.inner {
            ConstInner::Int(_) => true,
            ConstInner::Bits(value) => value.iter().all(|&bit| bit.is_fully_defined()),
            ConstInner::String(_) => true,
        }
    }

    pub fn as_exact_width_i32(&self) -> Option<i32> {
        (self.len() == 32 && self.is_fully_defined()).then(|| match &self.inner {
            ConstInner::Bits(bits) => {
                let mut out = 0;
                for i in bits.iter().rev() {
                    out <<= 1;
                    out |= !matches!(i, State::S0) as i32;
                }
                out
            }
            ConstInner::String(bytes) => {
                let mut out = 0;
                for &b in bytes.iter().rev() {
                    out <<= 8;
                    out |= b as i32;
                }
                out
            }
            &ConstInner::Int(value) => value,
        })
    }

    pub fn get(&self, offset: BitUSize) -> State {
        match &self.inner {
            ConstInner::Int(value) => State::from((value >> offset) & 1 != 0),
            ConstInner::Bits(value) => value[offset as usize],
            ConstInner::String(value) => {
                State::from((value[(offset / 8) as usize] >> (offset % 8)) & 1 != 0)
            }
        }
    }

    pub fn as_bstr(&self) -> Option<&BStr> {
        match &self.inner {
            ConstInner::String(value) => Some(value.as_bstr()),
            _ => None,
        }
    }
}

impl From<i32> for Const {
    fn from(value: i32) -> Self {
        Const {
            inner: ConstInner::Int(value),
        }
    }
}

impl From<&BStr> for Const {
    fn from(value: &BStr) -> Self {
        Self {
            inner: ConstInner::String(value.to_owned()),
        }
    }
}

impl From<BString> for Const {
    fn from(value: BString) -> Self {
        assert!(value.len() < (BitUSize::MAX / 8) as usize);
        Self {
            inner: ConstInner::String(value),
        }
    }
}

impl From<Vec<State>> for Const {
    fn from(value: Vec<State>) -> Self {
        assert!(value.len() < BitUSize::MAX as usize);
        Self {
            inner: ConstInner::Bits(value),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
enum ConstInner {
    Int(i32),
    Bits(Vec<State>),
    String(BString),
}

impl Default for ConstInner {
    fn default() -> Self {
        Self::Bits(vec![])
    }
}

#[derive(PartialEq, Eq, Hash)]
pub enum SigChunk {
    Const(Const),
    Wire(WireChunk),
}

impl std::fmt::Debug for SigChunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            SigChunk::Const(value) => std::fmt::Debug::fmt(value, f),
            SigChunk::Wire(value) => std::fmt::Debug::fmt(value, f),
        }
    }
}

impl SigChunk {
    pub fn wire(name: Id, offset: BitUSize, len: BitUSize) -> Self {
        Self::Wire(WireChunk { name, offset, len })
    }
}

impl From<Const> for SigChunk {
    fn from(value: Const) -> Self {
        SigChunk::Const(value)
    }
}

impl Default for SigChunk {
    fn default() -> Self {
        Self::Const(Default::default())
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct WireChunk {
    pub name: Id,
    pub offset: BitUSize,
    pub len: BitUSize,
}

impl std::fmt::Debug for WireChunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?} [{}:{}]",
            self.name,
            self.offset + (self.len - 1),
            self.offset
        ))
    }
}

#[derive(Default, PartialEq, Eq, Hash)]
pub struct SigSpec {
    chunks: Vec<SigChunk>,
}

impl SigSpec {
    pub fn chunks(&self) -> &[SigChunk] {
        &self.chunks
    }
}

impl std::fmt::Debug for SigSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.chunks.iter().rev()).finish()
    }
}

#[derive(Default, Debug)]
pub struct Module {
    pub attributes: IndexMap<Id, Const>,
    pub parameters: IndexMap<Id, Option<Const>>,
    pub wires: IndexMap<Id, Wire>,
    pub cells: IndexMap<Id, Cell>,
    pub connections: IndexSet<[SigSpec; 2]>,
}

pub struct Wire {
    pub attributes: IndexMap<Id, Const>,
    pub len: BitUSize,
    pub offset: BitISize,
    pub is_upto: bool,
    pub is_signed: bool,
    pub port: Option<Port>,
}

impl std::fmt::Debug for Wire {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Wire");
        if !self.attributes.is_empty() {
            dbg.field("attributes", &self.attributes);
        }
        if self.len != 1 {
            dbg.field("len", &self.len);
        }
        if self.offset != 0 {
            dbg.field("offset", &self.offset);
        }
        if self.is_upto {
            dbg.field("is_upto", &self.is_upto);
        }
        if self.is_signed {
            dbg.field("is_signed", &self.is_signed);
        }
        if let Some(port) = &self.port {
            dbg.field("port", port);
        }
        dbg.finish()
    }
}

pub use crate::parser::Port;
pub use crate::parser::PortDir;

pub struct Cell {
    pub attributes: IndexMap<Id, Const>,
    pub ty: Id,
    pub parameters: IndexMap<Id, CellParameter>,
    pub connections: IndexMap<Id, SigSpec>,
}

impl std::fmt::Debug for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dbg = f.debug_struct("Cell");
        if !self.attributes.is_empty() {
            dbg.field("attributes", &self.attributes);
        }
        dbg.field("ty", &self.ty);
        if !self.parameters.is_empty() {
            dbg.field("parameters", &self.parameters);
        }
        if !self.connections.is_empty() {
            dbg.field("connections", &self.parameters);
        }
        dbg.finish()
    }
}

pub struct CellParameter {
    pub value: Const,
    pub ty: Option<CellParameterType>,
}

impl std::fmt::Debug for CellParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ty) = &self.ty {
            f.debug_struct("CellParameter")
                .field("value", &self.value)
                .field("ty", &ty)
                .finish()
        } else {
            std::fmt::Debug::fmt(&self.value, f)
        }
    }
}

pub use crate::parser::CellParameterType;

impl DesignSink for Design {
    type Name = Id;
    type Constant = Const;

    type Attribute = (Id, Const);

    type Attributes = IndexMap<Id, Const>;

    type Module = (Id, Module);

    type Cell = (Id, Cell);

    type SigChunk = SigChunk;

    type SigSpec = SigSpec;

    type SigSpecBuilder = SigSpec;

    type StringBuilder = BString;

    fn public(&mut self, name: &[u8]) -> Self::Name {
        self.id_table.insert(name.as_bstr())
    }

    fn private(&mut self, name: &[u8]) -> Self::Name {
        self.id_table.insert(name.as_bstr())
    }

    fn name(&self, name: &Self::Name) -> &BStr {
        &self.id_table[*name]
    }

    fn constant_int(&mut self, value: i32) -> Self::Constant {
        Const::from(value)
    }

    fn constant_str(&mut self, value: &[u8]) -> Self::Constant {
        Const::from(value.as_bstr())
    }

    fn constant_bits(&mut self, len: u32, bits: &[parser::State]) -> Self::Constant {
        let mut bits: Vec<_> = bits.iter().copied().map(State::from).collect();
        bits.reverse();

        let mut pad = bits.last().copied().unwrap_or(State::S0);
        if pad == State::S1 {
            pad = State::S0;
        }
        bits.resize(len as usize, pad);
        Const::from(bits)
    }

    fn constant_str_begin(&mut self) -> Self::StringBuilder {
        BString::default()
    }

    fn constant_str_append(&mut self, builder: &mut Self::StringBuilder, chunk: &[u8]) {
        builder.extend_from_slice(chunk.as_bstr())
    }

    fn constant_str_build(&mut self, builder: Self::StringBuilder) -> Self::Constant {
        Const::from(builder)
    }

    fn sig_chunk_constant(&mut self, constant: Self::Constant) -> Self::SigChunk {
        SigChunk::from(constant)
    }

    fn sig_chunk_wire(
        &mut self,
        (_module_name, module): &mut Self::Module,
        name: Self::Name,
    ) -> Result<Self::SigChunk, WireRefError> {
        let wire = module.wires.get(&name).ok_or(WireRefError::UnknownWire {
            name: b"[not supported]".as_bstr(),
        })?;

        Ok(SigChunk::wire(name, 0, wire.len))
    }

    fn sig_chunk_wire_slice(
        &mut self,
        (_module_name, module): &mut Self::Module,
        name: Self::Name,
        offset: u32,
        len: u32,
    ) -> Result<Self::SigChunk, WireRefError> {
        let wire = module.wires.get(&name).ok_or(WireRefError::UnknownWire {
            name: &self.id_table[name],
        })?;
        // Avoids overflows
        if offset > wire.len || (wire.len - offset) < len {
            return Err(WireRefError::WireSliceOutOfBounds {
                name: &self.id_table[name],
                offset,
                len,
            });
        }

        Ok(SigChunk::wire(name, offset, len))
    }

    fn sig_spec_single(&mut self, chunk: Self::SigChunk) -> Self::SigSpec {
        SigSpec {
            chunks: vec![chunk],
        }
    }

    fn sig_spec_new(&mut self) -> Self::SigSpecBuilder {
        SigSpec::default()
    }

    fn sig_spec_add_chunk(&mut self, builder: &mut Self::SigSpecBuilder, chunk: Self::SigChunk) {
        builder.chunks.push(chunk)
    }

    fn sig_spec_build(&mut self, mut builder: Self::SigSpecBuilder) -> Self::SigSpec {
        builder.chunks.reverse();
        builder
    }

    fn attribute(&mut self, name: Self::Name, value: Self::Constant) -> Self::Attribute {
        (name, value)
    }

    fn attributes(&mut self, initial: Self::Attribute) -> Self::Attributes {
        let mut attributes = IndexMap::default();
        attributes.insert(initial.0, initial.1);
        attributes
    }

    fn add_attribute(
        &mut self,
        attributes: &mut Self::Attributes,
        attribute: Self::Attribute,
    ) -> Result<(), parser::AlreadyDefinedError<'_>> {
        if attributes.insert(attribute.0, attribute.1).is_some() {
            Err(parser::AlreadyDefinedError {
                name: &self.id_table[attribute.0],
            })
        } else {
            Ok(())
        }
    }

    fn module(
        &mut self,
        attributes: Option<Self::Attributes>,
        name: Self::Name,
    ) -> Result<Self::Module, parser::AlreadyDefinedError<'_>> {
        if self.modules.contains_key(&name) {
            Err(parser::AlreadyDefinedError {
                name: &self.id_table[name],
            })
        } else {
            Ok((
                name,
                Module {
                    attributes: attributes.unwrap_or_default(),
                    parameters: Default::default(),
                    wires: Default::default(),
                    cells: Default::default(),
                    connections: Default::default(),
                },
            ))
        }
    }

    fn finish_module(&mut self, (name, module): Self::Module) {
        self.modules.insert(name, module);
    }

    fn module_paramter(
        &mut self,
        (_module_name, module): &mut Self::Module,
        name: Self::Name,
        value: Option<Self::Constant>,
    ) -> Result<(), parser::AlreadyDefinedError<'_>> {
        if module.parameters.insert(name, value).is_some() {
            Err(AlreadyDefinedError {
                name: &self.id_table[name],
            })
        } else {
            Ok(())
        }
    }

    fn module_connection(
        &mut self,
        (_module_name, module): &mut Self::Module,
        sig_sig: [Self::SigSpec; 2],
    ) {
        module.connections.insert(sig_sig);
    }

    fn module_wire(
        &mut self,
        (_module_name, module): &mut Self::Module,
        wire: parser::Wire<Self>,
    ) -> Result<(), parser::AlreadyDefinedError<'_>> {
        if module
            .wires
            .insert(
                wire.name,
                Wire {
                    attributes: wire.attributes.unwrap_or_default(),
                    len: wire.len,
                    offset: wire.offset,
                    is_upto: wire.is_upto,
                    is_signed: wire.is_signed,
                    port: wire.port,
                },
            )
            .is_some()
        {
            Err(AlreadyDefinedError {
                name: &self.id_table[wire.name],
            })
        } else {
            Ok(())
        }
    }

    fn module_cell(
        &mut self,
        (_module_name, module): &mut Self::Module,
        attributes: Option<Self::Attributes>,
        ty: Self::Name,
        name: Self::Name,
    ) -> Result<Self::Cell, parser::AlreadyDefinedError<'_>> {
        if module.cells.contains_key(&name) {
            Err(AlreadyDefinedError {
                name: &self.id_table[name],
            })
        } else {
            Ok((
                name,
                Cell {
                    attributes: attributes.unwrap_or_default(),
                    ty,
                    parameters: Default::default(),
                    connections: Default::default(),
                },
            ))
        }
    }

    fn cell_parameter(
        &mut self,
        _module: &mut Self::Module,
        (_cell_name, cell): &mut Self::Cell,
        name: Self::Name,
        value: Self::Constant,
        ty: Option<CellParameterType>,
    ) -> Result<(), parser::AlreadyDefinedError<'_>> {
        if cell
            .parameters
            .insert(name, CellParameter { value, ty })
            .is_some()
        {
            Err(AlreadyDefinedError {
                name: &self.id_table[name],
            })
        } else {
            Ok(())
        }
    }

    fn cell_connection(
        &mut self,
        _module: &mut Self::Module,
        (_cell_name, cell): &mut Self::Cell,
        port: Self::Name,
        sig_spec: Self::SigSpec,
    ) -> Result<(), parser::AlreadyDefinedError<'_>> {
        if cell.connections.insert(port, sig_spec).is_some() {
            Err(AlreadyDefinedError {
                name: &self.id_table[port],
            })
        } else {
            Ok(())
        }
    }

    fn finish_cell(
        &mut self,
        (_module_name, module): &mut Self::Module,
        (cell_name, cell): Self::Cell,
    ) {
        module.cells.insert(cell_name, cell);
    }
}
