use super::model::{SimModel, Step};
use crate::bit_matrix::BitMatrix;
use imctk_ir::var::Var;

pub use crate::bit_matrix::{Word, WordVec};

pub struct BitSlicedSim {
    lanes: usize,
    reg_len: usize,
    comb_len: usize,

    pub(super) reg_state: BitMatrix,
    pub(super) comb_state: Vec<WordVec>,
}

impl BitSlicedSim {
    pub fn new(model: &SimModel, bit_lanes: usize) -> Self {
        let lanes = bit_lanes.div_ceil(WordVec::BITS as usize);
        Self {
            lanes,
            reg_len: model.read_state.len(),
            comb_len: model.init_steps.len(),
            reg_state: BitMatrix::zeroed(
                model.read_state.len() + 1,
                lanes * WordVec::BITS as usize,
            ),
            comb_state: vec![WordVec::default(); model.init_steps.len() * lanes],
        }
    }

    pub fn reg_len(&self) -> usize {
        self.reg_len
    }

    pub fn comb_len(&self) -> usize {
        self.comb_len
    }

    pub fn lanes(&self) -> usize {
        self.lanes
    }

    pub fn bit_lanes(&self) -> usize {
        self.lanes * WordVec::BITS as usize
    }

    pub fn reset_state(&mut self) -> &mut [WordVec] {
        self.reg_state.packed_row_mut(self.reg_len)
    }

    pub fn reset_all(&mut self) {
        self.reset_state().fill(!WordVec::ZERO);
    }

    pub fn sim(&mut self, model: &SimModel, provide_input: impl FnMut(Var, usize) -> WordVec) {
        self.sim_comb(model, provide_input);
        self.sim_regs(model);
    }

    pub fn comb_state(&self, var: Var) -> &[WordVec] {
        &self.comb_state[var.index() * self.lanes..][..self.lanes]
    }

    pub fn reg_state(&self, var: Var) -> &[WordVec] {
        self.reg_state.packed_row(var.index())
    }

    pub fn sim_comb(
        &mut self,
        model: &SimModel,
        mut provide_input: impl FnMut(Var, usize) -> WordVec,
    ) {
        #![allow(clippy::needless_range_loop)]

        for (step_var, &step) in model.next_steps.iter() {
            let offset_y = step_var.index() * self.lanes;

            let init_step = model.init_steps[step_var];
            if step != init_step {
                let reset_mask = self.reg_state.packed_row(model.read_state.len());

                match init_step {
                    Step::Xaig(step) => {
                        let [a, b] = step.inputs;

                        let offset_a = a.index() * self.lanes;
                        let offset_b = b.index() * self.lanes;

                        let mask_a = WordVec::splat((a.is_pos() as u64).wrapping_sub(1));
                        let mask_b = WordVec::splat((b.is_pos() as u64).wrapping_sub(1));

                        assert!(self.comb_state.len() >= offset_a + self.lanes);
                        assert!(self.comb_state.len() >= offset_b + self.lanes);
                        assert!(self.comb_state.len() >= offset_y + self.lanes);

                        if step.is_and() {
                            for w in 0..self.lanes {
                                let word_a = self.comb_state[offset_a + w] ^ mask_a;
                                let word_b = self.comb_state[offset_b + w] ^ mask_b;
                                self.comb_state[offset_y + w] = (word_a & word_b) & reset_mask[w];
                            }
                        } else {
                            for w in 0..self.lanes {
                                let word_a = self.comb_state[offset_a + w] ^ mask_a;
                                let word_b = self.comb_state[offset_b + w] ^ mask_b;
                                self.comb_state[offset_y + w] = (word_a ^ word_b) & reset_mask[w];
                            }
                        }
                    }
                    Step::Other(Some(_state_var)) => {
                        unreachable!()
                    }
                    Step::Other(None) => {
                        for w in 0..self.lanes {
                            self.comb_state[offset_y + w] =
                                provide_input(step_var, w) & reset_mask[w];
                        }
                    }
                }

                match step {
                    Step::Xaig(step) => {
                        let [a, b] = step.inputs;

                        let offset_a = a.index() * self.lanes;
                        let offset_b = b.index() * self.lanes;
                        let offset_y = step_var.index() * self.lanes;

                        let mask_a = WordVec::splat((a.is_pos() as u64).wrapping_sub(1));
                        let mask_b = WordVec::splat((b.is_pos() as u64).wrapping_sub(1));

                        assert!(self.comb_state.len() >= offset_a + self.lanes);
                        assert!(self.comb_state.len() >= offset_b + self.lanes);
                        assert!(self.comb_state.len() >= offset_y + self.lanes);

                        if step.is_and() {
                            for w in 0..self.lanes {
                                let word_a = self.comb_state[offset_a + w] ^ mask_a;
                                let word_b = self.comb_state[offset_b + w] ^ mask_b;
                                self.comb_state[offset_y + w] |= (word_a & word_b) & !reset_mask[w];
                            }
                        } else {
                            for w in 0..self.lanes {
                                let word_a = self.comb_state[offset_a + w] ^ mask_a;
                                let word_b = self.comb_state[offset_b + w] ^ mask_b;
                                self.comb_state[offset_y + w] |= (word_a ^ word_b) & !reset_mask[w];
                            }
                        }
                    }
                    Step::Other(Some(state_var)) => {
                        let state_input_slice = self.reg_state.packed_row(state_var.index());

                        for w in 0..self.lanes {
                            self.comb_state[offset_y + w] |= state_input_slice[w] & !reset_mask[w];
                        }
                    }
                    Step::Other(None) => {
                        for w in 0..self.lanes {
                            self.comb_state[offset_y + w] |=
                                provide_input(step_var, w) & !reset_mask[w];
                        }
                    }
                }
            } else {
                match step {
                    Step::Xaig(step) => {
                        let [a, b] = step.inputs;

                        let offset_a = a.index() * self.lanes;
                        let offset_b = b.index() * self.lanes;

                        let mask_a = WordVec::splat((a.is_pos() as u64).wrapping_sub(1));
                        let mask_b = WordVec::splat((b.is_pos() as u64).wrapping_sub(1));

                        assert!(self.comb_state.len() >= offset_a + self.lanes);
                        assert!(self.comb_state.len() >= offset_b + self.lanes);
                        assert!(self.comb_state.len() >= offset_y + self.lanes);

                        if step.is_and() {
                            for w in 0..self.lanes {
                                let word_a = self.comb_state[offset_a + w] ^ mask_a;
                                let word_b = self.comb_state[offset_b + w] ^ mask_b;
                                self.comb_state[offset_y + w] = word_a & word_b;
                            }
                        } else {
                            for w in 0..self.lanes {
                                let word_a = self.comb_state[offset_a + w] ^ mask_a;
                                let word_b = self.comb_state[offset_b + w] ^ mask_b;
                                self.comb_state[offset_y + w] = word_a ^ word_b;
                            }
                        }
                    }
                    Step::Other(Some(state_var)) => {
                        self.comb_state[step_var.index() * self.lanes..][..self.lanes]
                            .copy_from_slice(self.reg_state.packed_row(state_var.index()));
                    }
                    Step::Other(None) => {
                        for w in 0..self.lanes {
                            self.comb_state[offset_y + w] = provide_input(step_var, w);
                        }
                    }
                }
            }
        }
    }

    pub fn sim_regs(&mut self, model: &SimModel) {
        for (state_var, &step_var) in model.read_state.iter() {
            self.reg_state
                .packed_row_mut(state_var.index())
                .copy_from_slice(&self.comb_state[step_var.index() * self.lanes..][..self.lanes])
        }
        self.reset_state().fill(WordVec::ZERO);
    }
}
