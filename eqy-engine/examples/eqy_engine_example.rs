use std::{
    ffi::OsString,
    fmt::{Debug, Display, Write},
};

use clap::Parser;
use imctk_eqy_engine::{
    refinement::{
        bmc::BmcRefinement, rarity_sim::RaritySimRefinement, EnvVarRefinement, RefinementContext,
    },
    seq_sim, TimeStep,
};
use imctk_ir::{env::Env, var::Lit};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: OsString,
    #[clap(short = 'r', long, default_value = "5")]
    rarity_sim_rounds: usize,
    #[clap(short = 'b', long, default_value = "5")]
    bmc_depth: usize,
}

fn main() -> color_eyre::Result<()> {
    let args = Args::parse();

    color_eyre::install()?;
    imctk_logger::setup();

    log::info!("{args:?}");

    log::info!("parsing {:?}", &args.input);
    let binary_aiger = std::fs::File::open(&args.input).unwrap();

    let parser =
        flussab_aiger::binary::Parser::<Lit>::from_read(binary_aiger, Default::default()).unwrap();
    let header = parser.header();
    log::info!(
        "AIGER inputs = {}, outputs = {}, ands = {}, latches = {}",
        header.input_count,
        header.output_count,
        header.and_gate_count,
        header.latch_count,
    );

    let parsed = parser.parse()?;

    let mut env = Env::default();

    let _seq_env_from_aiger = imctk::import::aiger::import_ordered_aig(&mut env, &parsed);

    env.rebuild_egraph();

    log::info!("");
    log::info!("# Stats");
    for (node_type, count) in env.nodes().node_type_stats() {
        log::info!("{}: {}", node_type.name(), count);
    }

    let mut refine = EnvVarRefinement::default();
    for var in env.var_defs().repr_vars() {
        refine.insert_item(var);
    }

    let sim_model = seq_sim::model::extract_sim_model(&env, env.var_defs().repr_vars());

    let mut refinement_env = RefinementContext {
        env: &mut env,
        refine: &mut refine,
        sim_model: &sim_model,
    };

    if args.rarity_sim_rounds != 0 {
        log::info!("{} refinement by rarity simulation {0}", sep('=', 6));
        let mut rarity_sim_refine = RaritySimRefinement::new(&mut refinement_env);
        for round in 1..=args.rarity_sim_rounds {
            rarity_sim_refine.run_round(&mut refinement_env);
            log::info!(
                "rarity sim round {round}: {}",
                refinement_env.refine.stats()
            );
        }
    }

    if args.bmc_depth != 0 {
        log::info!("{} refinement by bmc {0}", sep('=', 6));
        let mut bmc_refine = BmcRefinement::default();
        for time in TimeStep::first_n(args.bmc_depth) {
            log::info!("{} {time} {0}", sep('-', 6));
            bmc_refine.refine_for_time_step(&mut refinement_env, time);
            log::info!("bmc {time}: {}", refinement_env.refine.stats());
        }
    }

    Ok(())
}

fn sep(c: char, len: usize) -> impl Debug + Display {
    imctk_util::fmt::fmt_closure(move |f| {
        for _ in 0..len {
            f.write_char(c)?;
        }
        Ok(())
    })
}
