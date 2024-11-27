#![allow(missing_docs)]
use clap::Parser;
use imctk_new_ir::{
    btor::BtorImporter,
    ir::WordIr,
};

#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    btor_file: String,
    depth: Option<u32>,
}

fn main() -> color_eyre::Result<()> {
    let args = Args::parse();
    color_eyre::install()?;
    imctk_logger::setup();

    let depth = args.depth.unwrap_or(10);
    let file = std::fs::File::open(args.btor_file)?;
    let mut word_ir = WordIr::default();
    BtorImporter::default().import(&mut word_ir, file)?;
    word_ir.refresh();
    Ok(())
}
