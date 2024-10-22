use std::{io::Write, result::Result};

use flussab::DeferredWriter;

use imctk_rtlil::{
    parser::{error::ParseError, Parser},
    rtlil,
    writer::write_design,
};

fn main() {
    if let Err(err) = main_err() {
        eprintln!("error: {err}");
    }
}

fn main_err() -> Result<(), ParseError> {
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();

    let mut writer = DeferredWriter::from_write(stdout.lock());

    let mut design = rtlil::Design::default();
    let reader = Parser::from_read(stdin.lock(), &mut design)?;

    let autoidx = reader.autoidx();

    reader.parse()?;

    writeln!(&mut writer, "autoidx {autoidx}")?;

    write_design(&mut writer, &design);

    writer.flush()?;
    Ok(())
}
