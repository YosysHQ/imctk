#![allow(unexpected_cfgs)] // for coverage_nightly
#![cfg_attr(coverage_nightly, feature(coverage_attribute))]
#![deny(unsafe_code)]

#[allow(unsafe_code)] // Required for calling directly into abc
pub mod pdr_wrapper;

pub mod comb_sim;
pub mod seq_sim;

pub mod bit_matrix;

pub mod circuit_sat;

pub mod refinement;

pub mod unroll;

pub mod fragment;

pub mod engine;

mod time_step;

pub use time_step::TimeStep;

use std::{ffi::OsString, mem::take};

use crate::{
    engine::{run_eqy_engine, EngineOptions},
    pdr_wrapper::PdrResult,
};
use clap::Parser;
use imctk_ids::IdRange;
use imctk_ir::{
    env::Env,
    var::{Lit, Var},
};
use serde_json::json;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: OsString,
    #[clap(short = 'r', long, default_value = "5")]
    rarity_sim_rounds: usize,
    #[clap(short = 'b', long, default_value = "5")]
    bmc_depth: usize,

    #[clap(short = 'w', long, default_value = "3")]
    window_min: usize,
    #[clap(short = 'W', long, default_value = "8")]
    window_max: usize,

    #[clap(long)]
    jsonl_output: bool,
}

#[cfg_attr(coverage_nightly, coverage(off))]
fn main() -> color_eyre::Result<()> {
    let args = Args::parse();

    color_eyre::install()?;
    imctk_logger::setup();

    let binary_aiger = std::fs::File::open(&args.input).unwrap();

    let parser =
        flussab_aiger::binary::Parser::<Lit>::from_read(binary_aiger, Default::default()).unwrap();
    let header = parser.header();
    if header.invariant_constraint_count != 0 {
        log::error!("AIGER invariant constraints are not supported");
        std::process::exit(1);
    } else if header.fairness_constraint_count != 0 || header.justice_property_count != 0 {
        log::warn!("AIGER fairness constraints and justice properties will be ignored");
    }

    let parsed = parser.parse()?;

    let mut env = Env::default();

    let seq_env_from_aiger = imctk_aiger::import::import_ordered_aig(&mut env, &parsed);

    log::info!("# Stats");
    for (node_type, count) in env.nodes().node_type_stats() {
        log::info!("{}: {}", node_type.name(), count);
    }

    env.rebuild_egraph();

    let target_properties = Vec::from_iter(
        parsed
            .outputs
            .iter()
            .chain(parsed.bad_state_properties.iter())
            .map(|&lit| lit.lookup(|var| seq_env_from_aiger[var])),
    );

    let result = run_eqy_engine(
        &mut env,
        &target_properties,
        EngineOptions {
            rarity_sim_rounds: args.rarity_sim_rounds,
            bmc_depth: args.bmc_depth,
            window: args.window_min..args.window_max + 1,
        },
    );

    let status;
    match result {
        Some(PdrResult::Unreachable) => {
            status = "pass";
        }
        Some(PdrResult::Cex(mut cex)) => {
            status = "fail";

            for (_, inputs) in cex.inputs.iter_mut() {
                for input in take(inputs) {
                    inputs.insert(env.lit_repr(input));
                }
            }

            let mut aiw_row = String::default();

            for aig_var in <IdRange<Var>>::from_index_range(
                parsed.input_count + 1..parsed.input_count + 1 + parsed.latches.len(),
            ) {
                let env_lit = env.lit_repr(seq_env_from_aiger[aig_var]);
                aiw_row.push(if cex.inputs[TimeStep::FIRST].contains(&env_lit) {
                    '1'
                } else {
                    '0'
                });
            }

            if args.jsonl_output {
                println!(
                    "{}",
                    serde_json::to_string(&json!({ "aiw": aiw_row })).unwrap()
                );
            }

            for (_, inputs) in cex.inputs.iter() {
                aiw_row.clear();
                for aig_var in <IdRange<Var>>::from_index_range(1..parsed.input_count + 1) {
                    let env_lit = env.lit_repr(seq_env_from_aiger[aig_var]);
                    aiw_row.push(if inputs.contains(&env_lit) { '1' } else { '0' });
                }

                if args.jsonl_output {
                    println!(
                        "{}",
                        serde_json::to_string(&json!({ "aiw": aiw_row })).unwrap()
                    );
                }
            }
        }
        None => {
            status = "unknown";
        }
    }

    log::info!("status: {status}");

    if args.jsonl_output {
        println!(
            "{}",
            serde_json::to_string(&json!({ "status": status })).unwrap()
        );
    }

    Ok(())
}
