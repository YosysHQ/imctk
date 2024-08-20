//! Rarity simulation, randomized simulation with a bias for novel states.
//!
//! This uses [the bit sliced simulation engine][crate::seq_sim::bit_sliced] to perform rarity
//! simulation. It follows the strategy described by Mishchenko, Brayton and Een[^1].
//!
//! [^1]: [Mishchenko, Alan, Robert Brayton, and Niklas Een. "Using speculation for sequential
//! equivalence checking." Proceedings of International Workshop on Logic & Synthesis. 2012.
//! (PDF)](https://people.eecs.berkeley.edu/~alanmi/publications/2012/iwls12_sec.pdf)
use rand::{rngs::SmallRng, Rng, SeedableRng};

use super::{
    bit_sliced::{BitSlicedSim, WordVec},
    model::SimModel,
};
use crate::bit_matrix::BitMatrix;

struct Counters {
    counters: Vec<[u32; 256]>,
    seen: usize,
}

impl Counters {
    fn new(len: usize) -> Self {
        Self {
            counters: vec![[0; 256]; len.next_multiple_of(WordVec::BITS as usize) / 8],
            seen: 0,
        }
    }

    fn bump(&mut self, words: &[WordVec], count: u32) {
        let mut counters = self.counters.iter_mut();

        for &vec in words {
            for &word in vec.as_array_ref() {
                for subbyte in word.to_le_bytes().into_iter() {
                    let counter = counters.next().unwrap();
                    let byte_counter = &mut counter[subbyte as usize];
                    self.seen += (*byte_counter == 0) as usize;
                    (*byte_counter) = (*byte_counter).wrapping_add(count);
                }
            }
        }
    }

    fn score(&self, words: &[WordVec]) -> f32 {
        let mut score = 0.0f32;

        let mut counters = self.counters.iter();

        for &vec in words {
            for &word in vec.as_array_ref() {
                for subbyte in word.to_le_bytes().into_iter() {
                    let counter = counters.next().unwrap();
                    let byte_counter = counter[subbyte as usize] as f32;
                    score += 1.0 / byte_counter.powi(2);
                }
            }
        }

        score
    }
}

/// Rarity simulation engine.
///
/// See the [module level documentation][self] for an overview.
pub struct RaritySim {
    inner: BitSlicedSim,
    transposed: BitMatrix,
    rng: rand::rngs::SmallRng,
    counters: Counters,
    order: Vec<(f32, usize)>,
    steps: usize,
    reduce: usize,
    stuck_counter: usize,
}

impl RaritySim {
    pub fn new(inner: BitSlicedSim, steps: usize, reduce: usize) -> Self {
        assert!(reduce >= 2);
        assert!(steps >= 1);
        let mut new = Self {
            transposed: BitMatrix::new(inner.reg_len() + 1),
            counters: Counters::new(inner.reg_len() + 1),
            order: vec![],
            inner,
            rng: SmallRng::seed_from_u64(0),
            steps,
            reduce,
            stuck_counter: 0,
        };
        new.reset();
        new
    }

    pub fn reset(&mut self) {
        self.inner.reset_all();
        self.stuck_counter = 0;
    }

    pub fn sim_round(&mut self, model: &SimModel, mut callback: impl FnMut(&BitSlicedSim)) {
        for _ in 0..self.steps {
            self.inner.sim(model, |_var, _lane| {
                WordVec::from(std::array::from_fn(|_| self.rng.gen()))
            });

            callback(&self.inner);
        }

        self.inner.reg_state.transpose_into(&mut self.transposed);

        let mut bit_lane_scores = vec![];

        let mut bit_lanes_with_new_patterns = 0;

        for bit_lane in 0..self.transposed.rows() {
            let before = self.counters.seen;
            self.counters.bump(self.transposed.packed_row(bit_lane), 1);
            let added = self.counters.seen - before;
            bit_lane_scores.push(added);
            if added != 0 {
                bit_lanes_with_new_patterns += 1;
            }
        }

        self.order.clear();

        self.order
            .extend((0..self.transposed.rows()).map(|bit_lane| {
                (
                    self.counters.score(self.transposed.packed_row(bit_lane)),
                    bit_lane,
                )
            }));

        self.order
            .sort_unstable_by(|(score_a, _), (score_b, _)| score_b.total_cmp(score_a));

        if bit_lanes_with_new_patterns != 0 {
            self.stuck_counter = 0;
        } else {
            self.stuck_counter += 1;
        }

        let keep = if self.stuck_counter >= 4 {
            self.transposed.rows() / 2
        } else {
            self.transposed.rows() / self.reduce
        };

        let (keep, replace) = self.order.split_at(keep);

        for (&(_, source), &(_, dest)) in keep.iter().cycle().zip(replace.iter()) {
            self.transposed.copy_row(source, dest);
        }

        self.transposed.transpose_into(&mut self.inner.reg_state);

        log::debug!(
            "seen: {:?} new pat: {:?} stuck: {}, {}",
            self.counters.seen,
            bit_lanes_with_new_patterns,
            self.stuck_counter,
            if bit_lanes_with_new_patterns > 0 {
                ""
            } else {
                " ######## stuck ########"
            }
        );
    }

    pub fn stuck_counter(&self) -> usize {
        self.stuck_counter
    }
}
