#![allow(missing_docs)]

use std::ffi::OsString;

use clap::Parser;
use imctk_ids::Id;
use imctk_lit::{Lit, Var};
use imctk_new_ir::{
    bitlevel::{BitlevelTerm, InputId, Node},
    dag::{AlwaysInsert, IrDag},
    egraph,
    ir::BitIr,
};
use imctk_paged_storage::index::IndexedTerm;
use imctk_util::hash::BuildHasherDefault;
use stable_set::StableSet;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: OsString,
}

fn main() -> color_eyre::Result<()> {
    let args = Args::parse();

    color_eyre::install()?;
    imctk_logger::setup();

    let importer = imctk_new_ir::aiger::AigerImporter::default();

    let mut ir = BitIr::default();

    let mut primary_defs = <IrDag<_, AlwaysInsert>>::new(&mut ir.egraph_mut());

    let false_lit = ir
        .egraph_mut()
        .insert_term(BitlevelTerm::Input(InputId::MAX_ID));

    assert_eq!(false_lit, Lit::FALSE);

    let input = std::fs::File::open(&args.input)?;

    let (mapping, aiger) = importer.import_binary(&mut ir, input)?;

    primary_defs.refresh(&ir.egraph_ref());

    // println!("{:?}", primary_defs);

    for &output in aiger.outputs.iter() {
        let output_lit = output.lookup(|var| mapping[var]);

        let mut seen: StableSet<Var, BuildHasherDefault> = StableSet::from_iter([output_lit.var()]);

        let mut pos = 0;

        while let Some(&current) = seen.get_index(pos) {
            pos += 1;
            let node_id = primary_defs.def(current);
            let node = ir.egraph_ref().get::<Node<BitlevelTerm>>(node_id);
            for input in node.term.nonguarding_vars() {
                println!("{node_id:?}: {node:?}");
                seen.insert(input);
            }
        }

        println!("{:?}", primary_defs.def(output_lit.var()));
    }

    // let mut unrolled_ir = BitIr::default();

    // println!("{:?}", ir);

    Ok(())
}
