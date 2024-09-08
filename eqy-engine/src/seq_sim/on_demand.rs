use imctk_ids::Id;
use imctk_ir::var::{Lit, Var};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use zwohash::HashMap;

use crate::{seq_sim::model::Step, time_step::TimeStep};

use super::model::SimModel;

pub struct OnDemandSeqSim {
    cached: HashMap<(TimeStep, Var), bool>,
    rng: SmallRng,
}

impl Default for OnDemandSeqSim {
    fn default() -> Self {
        Self {
            cached: Default::default(),
            rng: SmallRng::seed_from_u64(0),
        }
    }
}

impl OnDemandSeqSim {
    pub fn reset(&mut self) {
        self.cached.clear();
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

    pub fn input_lit_value(&mut self, model: &SimModel, time: TimeStep, lit: Lit) -> Option<bool> {
        if lit.is_const() {
            return None;
        }

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

        let steps = if time.id_index() == 0 {
            &model.init_steps
        } else {
            &model.next_steps
        };

        let value = match steps[var] {
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
        };

        self.cached.insert((time, var), value);

        value
    }
}
