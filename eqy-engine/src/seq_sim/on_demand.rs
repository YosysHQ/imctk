use imctk_ids::Id;
use imctk_ir::var::{Lit, Pol, Var};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use zwohash::HashMap;

use crate::{seq_sim::model::Step, time_step::TimeStep};

use super::model::SimModel;

pub struct OnDemandSeqSim {
    cached: HashMap<(TimeStep, Var), bool>,
    induction_mode: bool,
    rng: SmallRng,
}

impl Default for OnDemandSeqSim {
    fn default() -> Self {
        Self {
            cached: Default::default(),
            induction_mode: Default::default(),
            rng: SmallRng::seed_from_u64(0),
        }
    }
}

impl OnDemandSeqSim {
    pub fn reset(&mut self) {
        self.cached.clear();
    }

    pub fn set_induction_mode(&mut self, enable: bool) {
        self.induction_mode = enable
    }

    pub fn fix_lit_value(&mut self, time: TimeStep, lit: Lit, value: bool) -> Option<bool> {
        self.cached
            .insert((time, lit.var()), value ^ lit.pol())
            .map(|value| value ^ lit.pol())
    }

    pub fn fix_input_lit_value(
        &mut self,
        model: &SimModel,
        time: TimeStep,
        lit: Lit,
        value: bool,
    ) -> Option<Option<bool>> {
        if lit.is_const() {
            return None;
        }

        if self.induction_mode && time.id_index() == 1 {
            Some(self.fix_lit_value(time, lit, value))
        } else {
            let steps = if time.id_index() == 0 {
                &model.init_steps
            } else {
                &model.next_steps
            };

            match steps[lit.var()] {
                Step::Other(None) => Some(self.fix_lit_value(time, lit, value)),
                _ => None,
            }
        }
    }

    pub fn input_lit_value(&mut self, model: &SimModel, time: TimeStep, lit: Lit) -> Option<bool> {
        if lit.is_const() {
            return None;
        }

        if self.induction_mode && time.id_index() == 1 {
            Some(self.lit_value(model, time, lit))
        } else {
            let steps = if time.id_index() == 0 {
                &model.init_steps
            } else {
                &model.next_steps
            };

            match steps[lit.var()] {
                Step::Other(None) => Some(self.lit_value(model, time, lit)),
                _ => None,
            }
        }
    }

    pub fn cached_lit_value(&self, time: TimeStep, lit: Lit) -> Option<bool> {
        self.cached_var_value(time, lit.var())
            .map(|value| value ^ lit.pol())
    }

    pub fn cached_var_value(&self, time: TimeStep, var: Var) -> Option<bool> {
        if var == Var::FALSE {
            return Some(false);
        }

        self.cached.get(&(time, var)).copied()
    }

    pub fn lit_value(&mut self, model: &SimModel, time: TimeStep, lit: Lit) -> bool {
        self.var_value(model, time, lit.var()) ^ lit.pol()
    }

    pub fn var_value(&mut self, model: &SimModel, time: TimeStep, var: Var) -> bool {
        if var == Var::FALSE {
            return false;
        }

        if let Some(&cached) = self.cached.get(&(time, var)) {
            return cached;
        }

        let value = if self.induction_mode && time.id_index() == 1 {
            self.rng.gen()
        } else {
            let steps = if time.id_index() == 0 {
                &model.init_steps
            } else {
                &model.next_steps
            };

            match steps[var] {
                Step::Xaig(xaig) => {
                    let [a, b] = xaig
                        .inputs
                        .map(|input_lit| self.lit_value(model, time, input_lit));
                    if xaig.is_and() {
                        a & b
                    } else {
                        a ^ b
                    }
                }
                Step::Other(None) => self.rng.gen(),
                Step::Other(Some(reg)) => self.lit_value(
                    model,
                    time.prev().unwrap(),
                    model.read_state[reg.var()] ^ reg.pol(),
                ),
            }
        };

        self.cached.insert((time, var), value);

        value
    }

    pub fn values(&self) -> impl Iterator<Item = (TimeStep, Lit)> + '_ {
        self.cached
            .iter()
            .map(|(&(step, var), &value)| (step, var ^ Pol::pos_if(value)))
    }
}
