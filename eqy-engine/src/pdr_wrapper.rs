use std::{
    cell::Cell,
    ffi::{c_char, c_int},
};

use imctk_ids::{id_vec::IdVec, Id, IdRange};
use imctk_ir::{
    env::Env,
    node::fine::circuit::{Input, SteadyInput},
    var::{Lit, Var},
};
use zwohash::HashSet;

use crate::{
    circuit_sat::CircuitSat,
    unroll::{Unroll, UnrollMode},
    TimeStep,
};

thread_local! {
    pub static COUNTER: Cell<usize> = const { Cell::new(0) };
    pub static LIMIT: Cell<Option<usize>> = const { Cell::new(None) };
}

#[derive(Debug)]
pub enum PdrResult {
    Cex(PdrCex),
    Unreachable,
}

#[derive(Default, Debug)]
pub struct PdrStats {
    pub pre_check_success: bool,
}

#[derive(Clone, Default, Debug)]
pub struct PdrCex {
    pub inputs: IdVec<TimeStep, HashSet<Lit>>,
}

impl PdrCex {
    pub fn minimize(&mut self, env: &Env, target: Lit) {
        let mut sat = CircuitSat::default();

        let mut bmc = Unroll::new(UnrollMode::Bmc);
        let mut bmc_env = Env::default();

        let t = self.inputs.next_unused_key().prev().unwrap();
        let bmc_target = bmc.unroll(env, &mut bmc_env, t, target);

        let mut assumptions = vec![];
        let mut required_assumptions = HashSet::from_iter([!bmc_target]);
        let mut required_seq_inputs = <HashSet<Var>>::default();

        for (t, inputs) in self.inputs.iter_mut() {
            for input in inputs.drain() {
                assumptions.push(bmc.unroll(env, &mut bmc_env, t, input));
            }
        }

        while let Some(candidate) = assumptions.pop() {
            if sat
                .query_cube(
                    &mut bmc_env,
                    required_assumptions
                        .iter()
                        .chain(assumptions.iter())
                        .copied(),
                )
                .unwrap()
            {
                let (_time, seq_lit) = bmc.find_seq_input(env, &bmc_env, candidate).unwrap();
                required_seq_inputs.insert(seq_lit.var());
                required_assumptions.insert(candidate);

                assumptions.retain(|&bmc_lit| {
                    let (_time, seq_lit) = bmc.find_seq_input(env, &bmc_env, bmc_lit).unwrap();

                    let retain = !required_seq_inputs.contains(&seq_lit.var());
                    if !retain {
                        required_assumptions.insert(bmc_lit);
                    }
                    retain
                });
            } else {
                assumptions.clear();
                assumptions.extend(
                    sat.unsat_cubes()
                        .next()
                        .unwrap()
                        .iter()
                        .copied()
                        .filter(|&lit| !required_assumptions.contains(&lit)),
                );
            }
        }

        assert!(!sat
            .query_cube(&mut bmc_env, required_assumptions.iter().copied())
            .unwrap());

        for &lit in required_assumptions.iter() {
            if lit == !bmc_target {
                continue;
            }
            let (time, seq_lit) = bmc.find_seq_input(env, &bmc_env, lit).unwrap();
            self.inputs[time].insert(seq_lit);
        }
    }
}

#[derive(Default)]
pub struct PdrOptions {
    pub limit: Option<usize>,
    pub frame_limit: Option<usize>,
    pub run_scorr: bool,
    pub run_dc2: bool,
    pub abc_output: bool,

    pub pre_limit: Option<usize>,
    pub pre_bmc: usize,
    pub pre_k_induction: usize,
}

#[inline(never)]
pub fn solve_with_pdr(
    env: &Env,
    targets: impl IntoIterator<Item = Lit>,
    expand: impl FnMut(Var) -> bool,
    options: &PdrOptions,
) -> (Option<PdrResult>, PdrStats) {
    let mut stats = PdrStats::default();
    let targets = Vec::from_iter(targets);
    let bmc_depth = options.pre_bmc.max(options.pre_k_induction);
    if bmc_depth > 0 {
        let mut sat = CircuitSat::default();
        sat.cumulative_conflict_limit(options.pre_limit);

        {
            let mut bmc = Unroll::new(UnrollMode::Bmc);
            let mut bmc_env = Env::default();

            for t in TimeStep::first_n(bmc_depth) {
                for &target in targets.iter() {
                    let bmc_target = bmc.unroll(env, &mut bmc_env, t, target);

                    if sat.query_lit(&mut bmc_env, bmc_target) == Some(true) {
                        stats.pre_check_success = true;
                        let mut inputs = <IdVec<TimeStep, HashSet<Lit>>>::default();
                        inputs.grow_for_key_with(t, HashSet::default);

                        for &lit in sat.input_model() {
                            let (time, seq_lit) = bmc.find_seq_input(env, &bmc_env, lit).unwrap();
                            inputs[time].insert(seq_lit);
                        }
                        return (Some(PdrResult::Cex(PdrCex { inputs })), stats);
                    }
                }
            }
        }

        if options.pre_k_induction > 0 {
            let mut ind = Unroll::new(UnrollMode::Induction);
            let mut ind_env = Env::default();
            sat.reset();

            'outer: for t in TimeStep::first_n(3 + options.pre_k_induction)
                .iter()
                .skip(3)
            {
                for &target in targets.iter() {
                    let ind_target = ind.unroll(env, &mut ind_env, t, target);

                    if sat.query_lit(&mut ind_env, ind_target) != Some(false) {
                        continue 'outer;
                    }
                }
                stats.pre_check_success = true;
                return (Some(PdrResult::Unreachable), stats);
            }
        }
    }

    let aiger = imctk_aiger::extract::extract_aiger(env, targets.iter().copied(), expand);

    let mut steady_inputs_used = vec![];

    unsafe {
        use imctk_abc_sys as abc;

        abc::Abc_FrameGetGlobalFrame();

        let mut gia = abc::Gia_ManStart(100);

        for _ in 0..aiger.aig.input_count + aiger.aig.latches.len() {
            abc::Gia_ManAppendCi(gia);
        }

        for and in aiger.aig.and_gates {
            let [a, b] = and.inputs;
            abc::Gia_ManAppendAnd(gia, a.code() as _, b.code() as _);
        }
        for &output in aiger.aig.outputs.iter() {
            abc::Gia_ManAppendCo(gia, output.code() as _);
        }
        let mut inits = vec![];

        for (i, latch) in aiger.aig.latches.iter().enumerate() {
            abc::Gia_ManAppendCo(gia, latch.next_state.code() as _);
            match latch.initialization {
                Some(false) => inits.push(b'0'),
                Some(true) => inits.push(b'1'),
                None => {
                    steady_inputs_used.push(SteadyInput::from_id_index(i));
                    inits.push(b'x');
                }
            }
        }
        inits.push(0);
        abc::Gia_ManSetRegNum(gia, aiger.aig.latches.len() as _);

        let zero_undc_gia =
            abc::Gia_ManDupZeroUndc(gia, inits.as_mut_ptr() as *mut c_char, 0, 0, 0);
        abc::Gia_ManStop(gia);
        gia = zero_undc_gia;

        if options.run_scorr && !aiger.aig.latches.is_empty() {
            let mut pars: abc::Cec_ParCor_t = std::mem::zeroed();
            abc::Cec_ManCorSetDefaultParams(&mut pars);

            pars.nFrames = 1;
            pars.nBTLimit = 1000;

            let scorr_gia = abc::Cec_ManLSCorrespondence(gia, &mut pars);
            abc::Gia_ManStop(gia);
            gia = scorr_gia;
        }

        if options.run_dc2 {
            let dc2_gia = abc::Gia_ManCompress2(gia, 1, 0);
            abc::Gia_ManStop(gia);
            gia = dc2_gia;
        }

        let mut any_left = false;

        for i in 0..aiger.aig.outputs.len() {
            if abc::Gia_ManPoIsConst0(gia, i as _) == 0 {
                any_left = true;
                break;
            }
        }

        if !any_left {
            return (Some(PdrResult::Unreachable), stats);
        }

        let aig = abc::Gia_ManToAig(gia, 0);

        let mut pdr_pars: abc::Pdr_Par_t = std::mem::zeroed();

        let pdr_pars_p: *mut _ = &mut pdr_pars;

        abc::Pdr_ManSetDefaultParams(pdr_pars_p);
        pdr_pars.fTwoRounds = 1;
        if let Some(frame_limit) = options.frame_limit {
            pdr_pars.nFrameMax = frame_limit as _;
        }

        unsafe extern "C" fn callback(_: c_int) -> c_int {
            let counter = COUNTER.get() + 1;
            COUNTER.set(counter);
            if counter % 1000 == 0 {
                log::debug!("pdr {counter}...");
            }

            if let Some(limit) = LIMIT.get() {
                if counter >= limit {
                    return 1;
                }
            }
            0
        }

        COUNTER.set(0);
        LIMIT.set(options.limit);

        pdr_pars.pFuncStop = Some(callback);

        abc::enable_dbg_outs = options.abc_output as _;

        if options.abc_output {
            abc::imctk_abc_line_buffer_stdout();
        }

        let status = abc::Pdr_ManSolve(aig, &mut pdr_pars);

        abc::enable_dbg_outs = 0;

        let cex = (*aig).pSeqModel;

        let result = if status == 1 {
            Some(PdrResult::Unreachable)
        } else if status == 0 {
            assert!(!cex.is_null());
            assert_ne!((*cex).nBits, 0);
            assert!(((*cex).nPis as usize) >= aiger.aig.input_count);

            let mut inputs = <IdVec<TimeStep, HashSet<Lit>>>::default();

            let input_lits: Vec<Option<Lit>> =
                <IdRange<Input>>::from_index_range(0..aiger.aig.input_count)
                    .iter()
                    .map(|input| env.lookup_term(&input).map(|(_term, output)| output))
                    .collect();

            for f in 0..(*cex).iFrame as usize + 1 {
                let mut frame_inputs = HashSet::default();
                for (i, &input_lit) in input_lits.iter().enumerate() {
                    let offset = (*cex).nRegs as usize + i + f * ((*cex).nPis as usize);

                    if let Some(input_lit) = input_lit {
                        let value = abc::Abc_InfoHasBit(
                            (&mut (*cex).pData) as *mut _ as *mut _,
                            offset as c_int,
                        ) != 0;
                        frame_inputs.insert(input_lit ^ !value);
                    };
                }
                inputs.push(frame_inputs);
            }

            for (i, &steady_input) in steady_inputs_used.iter().enumerate() {
                let Some((_term, input_lit)) = env.lookup_term(&steady_input) else { continue };
                let offset = (*cex).nRegs as usize + i + aiger.aig.input_count;
                let value =
                    abc::Abc_InfoHasBit((&mut (*cex).pData) as *mut _ as *mut _, offset as c_int)
                        != 0;

                inputs[TimeStep::FIRST].insert(input_lit ^ !value);
            }

            Some(PdrResult::Cex(PdrCex { inputs }))
        } else {
            None
        };

        abc::Aig_ManStop(aig);
        abc::Gia_ManStop(gia);
        (result, stats)
    }
}
