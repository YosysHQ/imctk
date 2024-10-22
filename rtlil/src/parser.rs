use std::{
    io::{BufReader, Read},
    mem::replace,
};

use bstr::BStr;
use error::ParseError;
use flussab::{
    text::LineReader,
    DeferredReader,
    Parsed::{self, Fallthrough, Res},
};

pub mod error;

mod token;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u8)]
pub enum State {
    S0 = b'0',
    S1 = b'1',
    Sx = b'x',
    Sz = b'z',
    Sm = b'm',
    Sa = b'-',
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum PortDir {
    Input,
    Output,
    InOut,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum CellParameterType {
    Signed,
    Real,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Port {
    pub dir: PortDir,
    pub id: i32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Wire<Sink: DesignSink> {
    pub attributes: Option<Sink::Attributes>,
    pub name: Sink::Name,
    pub len: u32,
    pub offset: i32,
    pub port: Option<Port>,
    pub is_upto: bool,
    pub is_signed: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Expected {
    Token(&'static str),
    Description(&'static str),
}

pub struct Parser<'r, 's, Sink> {
    reader: LineReader<'r>,
    autidx: i32,
    sink: &'s mut Sink,
    expected: Vec<Expected>,
    expected_at: usize,
}

impl<'r, 's, Sink: DesignSink> Parser<'r, 's, Sink> {
    /// Creates a parser reading from a [`BufReader`].
    pub fn from_buf_reader(
        buf_reader: BufReader<impl Read + 'r>,
        sink: &'s mut Sink,
    ) -> Result<Self, ParseError> {
        Self::new(
            LineReader::new(DeferredReader::from_buf_reader(buf_reader)),
            sink,
        )
    }

    /// Creates a parser reading from a [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    pub fn from_read(read: impl Read + 'r, sink: &'s mut Sink) -> Result<Self, ParseError> {
        Self::new(LineReader::new(DeferredReader::from_read(read)), sink)
    }

    /// Creates a parser reading from a boxed [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    #[inline(never)]
    pub fn from_boxed_dyn_read(
        read: Box<dyn Read + 'r>,
        sink: &'s mut Sink,
    ) -> Result<Self, ParseError> {
        Self::new(
            LineReader::new(DeferredReader::from_boxed_dyn_read(read)),
            sink,
        )
    }

    pub fn new(mut reader: LineReader<'r>, sink: &'s mut Sink) -> Result<Self, ParseError> {
        token::skip_multiline_whitespace(&mut reader);

        let mut new = Self {
            reader,
            autidx: 1,
            sink,
            expected: vec![],
            expected_at: 0,
        };

        if let Some(autoidx) = new.try_autoidx().optional()? {
            new.autidx = autoidx;
        }

        Ok(new)
    }

    pub fn autoidx(&self) -> i32 {
        self.autidx
    }

    fn give_up(&mut self) -> ParseError {
        if self.expected_at != self.reader.reader.position() {
            self.expected.clear();
        }
        if self.expected.is_empty() {
            panic!();
        }
        self.expected
            .retain(|item| !matches!(item, Expected::Description("")));
        self.expected.sort_unstable();
        self.expected.dedup();

        let mut expected_str = String::new();

        let last = self.expected.pop();

        for (item, last) in self
            .expected
            .drain(..)
            .map(|item| (item, false))
            .chain(last.map(|item| (item, true)))
        {
            if !expected_str.is_empty() {
                expected_str.push_str(if last { " or " } else { ", " });
            }
            match item {
                Expected::Token(token) => {
                    expected_str.push('"');
                    expected_str.push_str(token);
                    expected_str.push('"');
                }
                Expected::Description(description) => expected_str.push_str(description),
            }
        }

        token::unexpected(&mut self.reader, &expected_str)
    }

    fn expecting(&mut self, what: Expected) {
        let pos = self.reader.reader.position();
        if replace(&mut self.expected_at, pos) != pos {
            self.expected.clear();
        }
        self.expected.push(what);
    }

    fn try_token(&mut self, word: &'static str) -> Parsed<(), ParseError> {
        self.expecting(Expected::Token(word));
        self.reader.reader.set_mark();
        token::word(&mut self.reader, word.as_bytes())
            .and_do(|_| token::skip_whitespace(&mut self.reader))
    }

    fn try_int(&mut self, what: &'static str) -> Parsed<i32, ParseError> {
        self.expecting(Expected::Description(what));
        self.reader.reader.set_mark();
        token::int(&mut self.reader).and_do(|_| token::skip_whitespace(&mut self.reader))
    }

    fn try_eol(&mut self) -> Parsed<(), ParseError> {
        self.expecting(Expected::Description("end of line"));
        token::end_of_line(&mut self.reader)
            .and_do(|_| token::skip_multiline_whitespace(&mut self.reader))
    }

    fn try_eof(&mut self) -> Parsed<(), ParseError> {
        self.expecting(Expected::Description("end of file"));
        if self.reader.reader.is_at_end() {
            Res(Ok(()))
        } else {
            Fallthrough
        }
    }

    fn try_autoidx(&mut self) -> Parsed<i32, ParseError> {
        self.try_token("autoidx").and_then(|_| {
            let index = self
                .try_int("autoidx value")
                .or_give_up(|| self.give_up())?;
            self.try_eol().or_give_up(|| self.give_up())?;

            Ok(index)
        })
    }

    fn try_id(&mut self, what: &'static str) -> Parsed<Sink::Name, ParseError> {
        self.expecting(Expected::Description(what));
        self.reader.reader.set_mark();
        token::identifier(&mut self.reader, self.sink)
            .and_do(|_| token::skip_whitespace(&mut self.reader))
    }

    fn try_constant(&mut self) -> Parsed<Sink::Constant, ParseError> {
        self.expecting(Expected::Description("constant value"));
        (token::int(&mut self.reader).map(|value| self.sink.constant_int(value)))
            .or_parse(|| token::string(&mut self.reader, self.sink))
            .or_parse(|| token::bits(&mut self.reader, self.sink))
            .and_do(|_| token::skip_whitespace(&mut self.reader))
    }

    fn try_sigchunk(&mut self, module: &mut Sink::Module) -> Parsed<Sink::SigChunk, ParseError> {
        self.try_constant()
            .map(|constant| self.sink.sig_chunk_constant(constant))
            .or_parse(|| {
                self.try_id("wire reference").and_then(|name| {
                    let slice_pos = self.reader.reader.position();
                    let slice = token::slice(&mut self.reader)
                        .and_do(|_| token::skip_whitespace(&mut self.reader))
                        .optional()?;

                    if let Some([high, low]) = slice {
                        if high < low {
                            return Err(self
                                .reader
                                .give_up_at(slice_pos, format!("invalid slice [{high}:{low}]")));
                        }
                        self.sink
                            .sig_chunk_wire_slice(module, name.clone(), low, high - low + 1)
                    } else {
                        self.sink.sig_chunk_wire(module, name.clone())
                    }
                    .map_err(|err| match err {
                        WireRefError::UnknownWire { name } => {
                            self.reader.give_up(format!("unknwon wire {name}"))
                        }
                        WireRefError::WireSliceOutOfBounds { name, offset, len } => {
                            self.reader.give_up_at(
                                slice_pos,
                                format!(
                                    "slice [{}:{offset}] out of bounds for wire {name}",
                                    offset + (len - 1)
                                ),
                            )
                        }
                    })
                })
            })
    }

    fn try_sigspec(&mut self, module: &mut Sink::Module) -> Parsed<Sink::SigSpec, ParseError> {
        self.try_sigchunk(module)
            .map(|chunk| self.sink.sig_spec_single(chunk))
            .or_parse(|| {
                self.try_token("{").and_then(|_| {
                    let mut builder = self.sink.sig_spec_new();

                    while !self.try_token("}").matches()? {
                        let chunk = self.try_sigchunk(module).or_give_up(|| self.give_up())?;
                        self.sink.sig_spec_add_chunk(&mut builder, chunk);
                    }
                    let spec = self.sink.sig_spec_build(builder);

                    Ok(spec)
                })
            })
    }

    fn try_attribute(&mut self) -> Parsed<Sink::Attribute, ParseError> {
        self.try_token("attribute").and_then(|_| {
            let name = self
                .try_id("attribute name")
                .or_give_up(|| self.give_up())?;
            let value = self.try_constant().or_give_up(|| self.give_up())?;
            self.try_eol().or_give_up(|| self.give_up())?;
            Ok(self.sink.attribute(name, value))
        })
    }

    // fn parse_attribute(&mut self) -> Parsed<Sink::Attribute, ParseError> {}

    fn try_module_parameter(&mut self, module: &mut Sink::Module) -> Parsed<(), ParseError> {
        self.try_token("parameter").and_then(|_| {
            let name = self
                .try_id("parameter name")
                .or_give_up(|| self.give_up())?;

            let value = self.try_constant().optional()?;

            self.try_eol().or_give_up(|| self.give_up())?;

            self.sink
                .module_paramter(module, name, value)
                .map_err(|err| Self::already_defined(&mut self.reader, "parameter", err))?;

            Ok(())
        })
    }

    fn try_module_connection(&mut self, module: &mut Sink::Module) -> Parsed<(), ParseError> {
        self.try_token("connect").and_then(|_| {
            let lhs = self.try_sigspec(module).or_give_up(|| self.give_up())?;
            let rhs = self.try_sigspec(module).or_give_up(|| self.give_up())?;

            self.try_eol().or_give_up(|| self.give_up())?;

            self.sink.module_connection(module, [lhs, rhs]);

            Ok(())
        })
    }

    fn try_port_direction(&mut self) -> Parsed<PortDir, ParseError> {
        (self.try_token("input").map(|_| PortDir::Input))
            .or_parse(|| self.try_token("output").map(|_| PortDir::Output))
            .or_parse(|| self.try_token("inout").map(|_| PortDir::InOut))
    }

    fn try_wire(
        &mut self,
        module: &mut Sink::Module,
        attributes: &mut Option<Sink::Attributes>,
    ) -> Parsed<(), ParseError> {
        self.try_token("wire").and_then(|_| {
            let mut width = None;
            let mut offset = None;
            let mut port = None;
            let mut upto = false;
            let mut signed = false;
            let name = loop {
                if let Some(name) = self.try_id("wire name").optional()? {
                    break name;
                } else if width.is_none() && self.try_token("width").matches()? {
                    let parsed_width = self.try_int("wire width").or_give_up(|| self.give_up())?;
                    if parsed_width < 0 {
                        return Err(self.reader.give_up_at(
                            self.reader.reader.mark(),
                            "wire width must be non-negative",
                        ));
                    }
                    width = Some(parsed_width as u32);
                } else if offset.is_none() && self.try_token("offset").matches()? {
                    offset = Some(self.try_int("wire offset").or_give_up(|| self.give_up())?);
                } else if let Some(dir) = {
                    if port.is_none() {
                        self.try_port_direction().optional()?
                    } else {
                        None
                    }
                } {
                    let id = self.try_int("port id").or_give_up(|| self.give_up())?;
                    port = Some(Port { dir, id })
                } else if !signed && self.try_token("signed").matches()? {
                    signed = true
                } else if !upto && self.try_token("upto").matches()? {
                    upto = true
                } else {
                    return Err(self.give_up());
                }
            };

            self.try_eol().or_give_up(|| self.give_up())?;

            self.sink
                .module_wire(
                    module,
                    Wire {
                        name,
                        attributes: attributes.take(),
                        len: width.unwrap_or(1),
                        offset: offset.unwrap_or(0),
                        port,
                        is_upto: upto,
                        is_signed: signed,
                    },
                )
                .map_err(|err| Self::already_defined(&mut self.reader, "wire", err))?;
            Ok(())
        })
    }

    fn try_cell_parameter(
        &mut self,
        module: &mut Sink::Module,
        cell: &mut Sink::Cell,
    ) -> Parsed<(), ParseError> {
        self.try_token("parameter").and_then(|_| {
            let name = self
                .try_id("parameter name")
                .or_give_up(|| self.give_up())?;

            let value = self.try_constant().or_give_up(|| self.give_up())?;

            let ty = self
                .try_token("signed")
                .map(|_| CellParameterType::Signed)
                .or_parse(|| self.try_token("real").map(|_| CellParameterType::Real))
                .optional()?;

            self.try_eol().or_give_up(|| self.give_up())?;

            self.sink
                .cell_parameter(module, cell, name, value, ty)
                .map_err(|err| Self::already_defined(&mut self.reader, "cell parameter", err))?;

            Ok(())
        })
    }

    fn try_cell_connection(
        &mut self,
        module: &mut Sink::Module,
        cell: &mut Sink::Cell,
    ) -> Parsed<(), ParseError> {
        self.try_token("connect").and_then(|_| {
            let port = self.try_id("cell port").or_give_up(|| self.give_up())?;

            let sigspec = self.try_sigspec(module).or_give_up(|| self.give_up())?;

            self.try_eol().or_give_up(|| self.give_up())?;

            self.sink
                .cell_connection(module, cell, port, sigspec)
                .map_err(|err| Self::already_defined(&mut self.reader, "port connection", err))?;

            Ok(())
        })
    }

    fn try_cell(
        &mut self,
        module: &mut Sink::Module,
        attributes: &mut Option<Sink::Attributes>,
    ) -> Parsed<(), ParseError> {
        self.try_token("cell").and_then(|_| {
            let ty = self.try_id("cell type").or_give_up(|| self.give_up())?;
            let name = self.try_id("cell name").or_give_up(|| self.give_up())?;

            self.try_eol().or_give_up(|| self.give_up())?;

            let mut cell = self
                .sink
                .module_cell(module, attributes.take(), ty, name)
                .map_err(|err| Self::already_defined(&mut self.reader, "cell", err))?;

            while !self.try_token("end").matches()? {
                if self.try_cell_parameter(module, &mut cell).matches()?
                    || self.try_cell_connection(module, &mut cell).matches()?
                {
                } else {
                    return Err(self.give_up());
                }
            }

            self.try_eol().or_give_up(|| self.give_up())?;

            self.sink.finish_cell(module, cell);

            Ok(())
        })
    }

    fn already_defined(
        reader: &mut LineReader,
        what: &str,
        err: AlreadyDefinedError,
    ) -> ParseError {
        reader.give_up(format!("{what} {} already defined", err.name))
    }

    fn try_module(&mut self, attributes: Option<Sink::Attributes>) -> Parsed<(), ParseError> {
        self.try_token("module").and_then(|_| {
            let name = self.try_id("module name").or_give_up(|| self.give_up())?;
            self.try_eol().or_give_up(|| self.give_up())?;

            let mut module = self
                .sink
                .module(attributes, name)
                .map_err(|err| Self::already_defined(&mut self.reader, "module", err))?;

            while !self.try_token("end").matches()? {
                let mut attributes = if let Some(attribute) = self.try_attribute().optional()? {
                    let mut attributes = self.sink.attributes(attribute);

                    while {
                        self.try_attribute()
                            .and_then(|attribute| {
                                self.sink
                                    .add_attribute(&mut attributes, attribute)
                                    .map_err(|err| {
                                        Self::already_defined(&mut self.reader, "attribute", err)
                                    })?;

                                Ok(())
                            })
                            .matches()?
                    } {}

                    Some(attributes)
                } else {
                    None
                };

                if (attributes.is_none() && (self.try_module_parameter(&mut module).matches()?)
                    || self.try_module_connection(&mut module).matches()?)
                    || self.try_wire(&mut module, &mut attributes).matches()?
                    || self.try_cell(&mut module, &mut attributes).matches()?
                {
                    continue;
                } else {
                    return Err(self.give_up());
                }
            }

            self.try_eol().or_give_up(|| self.give_up())?;
            self.sink.finish_module(module);
            Ok(())
        })
    }

    pub fn parse(mut self) -> Result<(), ParseError> {
        while !self.try_eof().matches()? {
            let attributes = if let Some(attribute) = self.try_attribute().optional()? {
                let mut attributes = self.sink.attributes(attribute);

                while let Some(attribute) = self.try_attribute().optional()? {
                    self.sink
                        .add_attribute(&mut attributes, attribute)
                        .map_err(|err| Self::already_defined(&mut self.reader, "attribute", err))?;
                }

                Some(attributes)
            } else {
                None
            };
            self.try_module(attributes).or_give_up(|| self.give_up())?;
        }

        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum WireRefError<'a> {
    UnknownWire {
        name: &'a BStr,
    },
    WireSliceOutOfBounds {
        name: &'a BStr,
        offset: u32,
        len: u32,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AlreadyDefinedError<'a> {
    pub name: &'a BStr,
}

pub trait DesignSink: Sized {
    type Name: Clone;
    type Constant;
    type Attribute;
    type Attributes;
    type Module;
    type Cell;
    type SigChunk;
    type SigSpec;

    type SigSpecBuilder;
    type StringBuilder;

    fn public(&mut self, name: &[u8]) -> Self::Name;
    fn private(&mut self, name: &[u8]) -> Self::Name;

    fn constant_int(&mut self, value: i32) -> Self::Constant;
    fn constant_str(&mut self, value: &[u8]) -> Self::Constant;
    fn constant_bits(&mut self, len: u32, bits: &[State]) -> Self::Constant;

    fn constant_str_begin(&mut self) -> Self::StringBuilder;
    fn constant_str_append(&mut self, builder: &mut Self::StringBuilder, chunk: &[u8]);
    fn constant_str_build(&mut self, builder: Self::StringBuilder) -> Self::Constant;

    fn name(&self, name: &Self::Name) -> &BStr;

    fn sig_chunk_constant(&mut self, constant: Self::Constant) -> Self::SigChunk;
    fn sig_chunk_wire(
        &mut self,
        module: &mut Self::Module,
        name: Self::Name,
    ) -> Result<Self::SigChunk, WireRefError>;
    fn sig_chunk_wire_slice(
        &mut self,
        module: &mut Self::Module,
        name: Self::Name,
        offset: u32,
        len: u32,
    ) -> Result<Self::SigChunk, WireRefError>;

    fn sig_spec_single(&mut self, chunk: Self::SigChunk) -> Self::SigSpec;

    fn sig_spec_new(&mut self) -> Self::SigSpecBuilder;
    fn sig_spec_add_chunk(&mut self, builder: &mut Self::SigSpecBuilder, chunk: Self::SigChunk);
    fn sig_spec_build(&mut self, builder: Self::SigSpecBuilder) -> Self::SigSpec;

    fn attribute(&mut self, name: Self::Name, value: Self::Constant) -> Self::Attribute;
    fn attributes(&mut self, initial: Self::Attribute) -> Self::Attributes;

    fn add_attribute(
        &mut self,
        attributes: &mut Self::Attributes,
        attribute: Self::Attribute,
    ) -> Result<(), AlreadyDefinedError>;

    fn module(
        &mut self,
        attributes: Option<Self::Attributes>,
        name: Self::Name,
    ) -> Result<Self::Module, AlreadyDefinedError>;

    fn finish_module(&mut self, module: Self::Module);

    fn module_paramter(
        &mut self,
        module: &mut Self::Module,
        name: Self::Name,
        value: Option<Self::Constant>,
    ) -> Result<(), AlreadyDefinedError>;

    fn module_connection(&mut self, module: &mut Self::Module, sig_sig: [Self::SigSpec; 2]);

    fn module_wire(
        &mut self,
        module: &mut Self::Module,
        wire: Wire<Self>,
    ) -> Result<(), AlreadyDefinedError>;

    fn module_cell(
        &mut self,
        module: &mut Self::Module,
        attributes: Option<Self::Attributes>,
        ty: Self::Name,
        name: Self::Name,
    ) -> Result<Self::Cell, AlreadyDefinedError>;

    fn cell_parameter(
        &mut self,
        module: &mut Self::Module,
        cell: &mut Self::Cell,
        name: Self::Name,
        value: Self::Constant,
        ty: Option<CellParameterType>,
    ) -> Result<(), AlreadyDefinedError>;

    fn cell_connection(
        &mut self,
        module: &mut Self::Module,
        cell: &mut Self::Cell,
        port: Self::Name,
        sig_spec: Self::SigSpec,
    ) -> Result<(), AlreadyDefinedError>;

    fn finish_cell(&mut self, module: &mut Self::Module, cell: Self::Cell);
}
