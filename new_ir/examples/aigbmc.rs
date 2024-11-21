#![allow(missing_docs)]
use clap::Parser;
use imctk_new_ir::aiger::{AigerImporter, AigerMapping};
use imctk_new_ir::bmc::{Bmc, BmcWitness, Frame};
use imctk_new_ir::ir::BitIr;
use std::io::BufReader;

fn opt_to_char(b: Option<bool>) -> char {
    match b {
        None => 'x',
        Some(true) => '1',
        Some(false) => '0',
    }
}

fn print_witness(
    mut f: impl std::io::Write,
    witness: &BmcWitness,
    aiger_map: &AigerMapping,
) -> Result<(), std::io::Error> {
    writeln!(f, "1")?;
    writeln!(f, "b{}", witness.property_index())?;
    for &lit in aiger_map.latch_outputs() {
        write!(f, "{}", opt_to_char(witness.get_lit(Frame(0), lit)))?;
    }
    writeln!(f)?;
    for frame in witness.frames() {
        for input_id in aiger_map.inputs() {
            write!(f, "{}", opt_to_char(witness.get_input(frame, input_id)))?;
        }
        writeln!(f)?;
    }
    writeln!(f, ".")?;
    Ok(())
}

#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    aiger_file: String,
    depth: Option<u32>,
}

fn main() -> color_eyre::Result<()> {
    let args = Args::parse();
    color_eyre::install()?;
    imctk_logger::setup();

    let depth = args.depth.unwrap_or(10);
    let file = std::fs::File::open(args.aiger_file)?;
    let file = BufReader::new(file);
    let mut seq_ir = BitIr::default();
    let aiger_map = AigerImporter::default().import_binary(&mut seq_ir, file)?;
    seq_ir.refresh();
    let mut bmc = Bmc::new();
    bmc.assume(aiger_map.invariant_constraints());
    if let Some(witness) = bmc.run(&seq_ir, depth, &aiger_map.bad_state_properties()) {
        print_witness(std::io::stdout(), &witness, &aiger_map)?;
    } else {
        println!("2");
    }
    Ok(())
}
