use bstr::{BStr, ByteSlice};
use flussab::{write::text::ascii_digits, DeferredWriter};

use crate::{
    id_table::{Id, IdTable},
    parser, rtlil,
};

pub fn write_design(writer: &mut DeferredWriter, design: &rtlil::Design) {
    for (&name, module) in &design.modules {
        write_module(writer, &design.id_table, name, module);
    }
}

pub fn write_module(writer: &mut DeferredWriter, ids: &IdTable, name: Id, module: &rtlil::Module) {
    for (&attr_name, attr_value) in &module.attributes {
        writer.write_all_defer_err(b"attribute ");
        writer.write_all_defer_err(&ids[attr_name]);
        writer.write_all_defer_err(b" ");
        write_constant(writer, attr_value);
        writer.write_all_defer_err(b"\n");
    }
    writer.write_all_defer_err(b"module ");
    writer.write_all_defer_err(&ids[name]);
    writer.write_all_defer_err(b"\n");

    for (&name, parameter) in &module.parameters {
        writer.write_all_defer_err(b"  parameter ");
        writer.write_all_defer_err(&ids[name]);
        if let Some(parameter) = parameter {
            writer.write_all_defer_err(b" ");
            write_constant(writer, parameter);
        }
        writer.write_all_defer_err(b"\n");
    }

    for (&name, wire) in &module.wires {
        write_wire(writer, ids, name, wire);
    }

    for (&name, cell) in &module.cells {
        write_cell(writer, ids, module, name, cell);
    }

    for [lhs, rhs] in &module.connections {
        writer.write_all_defer_err(b"  connect ");
        write_sig_spec(writer, ids, module, lhs);
        writer.write_all_defer_err(b" ");
        write_sig_spec(writer, ids, module, rhs);
        writer.write_all_defer_err(b"\n");
    }

    writer.write_all_defer_err(b"end\n");
}

pub fn write_wire(writer: &mut DeferredWriter, ids: &IdTable, name: Id, wire: &rtlil::Wire) {
    for (&attr_name, attr_value) in &wire.attributes {
        writer.write_all_defer_err(b"  attribute ");
        writer.write_all_defer_err(&ids[attr_name]);
        writer.write_all_defer_err(b" ");
        write_constant(writer, attr_value);
        writer.write_all_defer_err(b"\n");
    }

    writer.write_all_defer_err(b"  wire ");

    if wire.len != 1 {
        writer.write_all_defer_err(b"width ");
        ascii_digits(writer, wire.len);
        writer.write_all_defer_err(b" ");
    }
    if wire.offset != 0 {
        writer.write_all_defer_err(b"offset ");
        ascii_digits(writer, wire.offset);
    }
    if wire.is_upto {
        writer.write_all_defer_err(b"upto ");
    }
    if wire.is_signed {
        writer.write_all_defer_err(b"signed ");
    }
    if let Some(port) = wire.port {
        writer.write_all_defer_err(match port.dir {
            rtlil::PortDir::Input => b"input ",
            rtlil::PortDir::Output => b"output ",
            rtlil::PortDir::InOut => b"inout ",
        });
        ascii_digits(writer, port.id);
        writer.write_all_defer_err(b" ");
    }
    writer.write_all_defer_err(&ids[name]);
    writer.write_all_defer_err(b"\n");
}

pub fn write_cell(
    writer: &mut DeferredWriter,
    ids: &IdTable,
    module: &rtlil::Module,
    name: Id,
    cell: &rtlil::Cell,
) {
    for (&attr_name, attr_value) in &cell.attributes {
        writer.write_all_defer_err(b"  attribute ");
        writer.write_all_defer_err(&ids[attr_name]);
        writer.write_all_defer_err(b" ");
        write_constant(writer, attr_value);
        writer.write_all_defer_err(b"\n");
    }

    writer.write_all_defer_err(b"  cell ");
    writer.write_all_defer_err(&ids[cell.ty]);
    writer.write_all_defer_err(b" ");
    writer.write_all_defer_err(&ids[name]);
    writer.write_all_defer_err(b"\n");

    for (&name, parameter) in &cell.parameters {
        writer.write_all_defer_err(b"    parameter ");
        if let Some(ty) = parameter.ty {
            match ty {
                rtlil::CellParameterType::Signed => writer.write_all_defer_err(b"signed "),
                rtlil::CellParameterType::Real => writer.write_all_defer_err(b"real "),
            }
        }
        writer.write_all_defer_err(&ids[name]);
        writer.write_all_defer_err(b" ");
        write_constant(writer, &parameter.value);
        writer.write_all_defer_err(b"\n");
    }

    for (&port, sig_spec) in &cell.connections {
        writer.write_all_defer_err(b"    connect ");
        writer.write_all_defer_err(&ids[port]);
        writer.write_all_defer_err(b" ");
        write_sig_spec(writer, ids, module, sig_spec);
        writer.write_all_defer_err(b"\n");
    }

    writer.write_all_defer_err(b"  end\n");
}
pub fn write_escaped_string_contents(writer: &mut DeferredWriter, mut chunk: &BStr) {
    loop {
        let mut first_escaped = match std::str::from_utf8(chunk) {
            Ok(str_chunk) => str_chunk.len(),
            Err(err) => err.valid_up_to(),
        };

        first_escaped = first_escaped.min(
            chunk[..first_escaped]
                .iter()
                .position(|c| matches!(c, b'\\' | b'\n' | b'\r' | b'"' | (0..=0x1f)))
                .unwrap_or(first_escaped),
        );
        let (prefix, rest) = chunk.split_at(first_escaped);
        writer.write_all_defer_err(prefix);
        if let Some((&to_escape, rest)) = rest.split_first() {
            chunk = rest.as_bstr();
            match to_escape {
                b'\\' | b'\"' => writer.write_all_defer_err(&[b'\\', to_escape]),
                b'\n' => writer.write_all_defer_err(b"\\n"),
                _ => {
                    let a = b'0' + (to_escape & 7);
                    let b = b'0' + ((to_escape >> 3) & 7);
                    let c = b'0' + ((to_escape >> 6) & 7);
                    writer.write_all_defer_err(&[b'\\', c, b, a]);
                }
            }
        } else {
            break;
        }
    }
}

pub fn write_constant(writer: &mut DeferredWriter, constant: &rtlil::Const) {
    if let Some(string) = constant.as_bstr() {
        writer.write_all_defer_err(b"\"");
        write_escaped_string_contents(writer, string);
        writer.write_all_defer_err(b"\"");
    } else {
        ascii_digits(writer, constant.len());
        writer.write_all_defer_err(b"'");
        for i in (0..constant.len()).rev() {
            writer.write_all_defer_err(&[parser::State::from(constant.get(i)) as u8]);
        }
    }
}

pub fn write_sig_spec(
    writer: &mut DeferredWriter,
    ids: &IdTable,
    module: &rtlil::Module,
    sig_spec: &rtlil::SigSpec,
) {
    match sig_spec.chunks() {
        [single] => write_sig_chunk(writer, ids, module, single),
        chunks => {
            writer.write_all_defer_err(b"{");
            for chunk in chunks {
                writer.write_all_defer_err(b" ");
                write_sig_chunk(writer, ids, module, chunk);
            }

            writer.write_all_defer_err(b" }");
        }
    }
}

pub fn write_sig_chunk(
    writer: &mut DeferredWriter,
    ids: &IdTable,
    module: &rtlil::Module,
    sig_spec: &rtlil::SigChunk,
) {
    match sig_spec {
        rtlil::SigChunk::Const(constant) => write_constant(writer, constant),
        rtlil::SigChunk::Wire(wire_chunk) => {
            writer.write_all_defer_err(&ids[wire_chunk.name]);
            let len = module
                .wires
                .get(&wire_chunk.name)
                .map(|wire| wire.len)
                .unwrap_or(1);
            if len != wire_chunk.len || wire_chunk.offset != 0 {
                writer.write_all_defer_err(b" [");
                ascii_digits(writer, wire_chunk.offset + (wire_chunk.len - 1));
                if wire_chunk.len != 1 {
                    writer.write_all_defer_err(b":");
                    ascii_digits(writer, wire_chunk.offset);
                }
                writer.write_all_defer_err(b"]");
            }
        }
    }
}
