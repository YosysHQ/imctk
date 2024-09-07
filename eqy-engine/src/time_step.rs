use imctk_ids::{Id, Id32, IdRange};

/// Represent a time step in the evaluation of a sequential circuit.
#[derive(Id)]
#[repr(transparent)]
pub struct TimeStep(Id32);

impl std::fmt::Debug for TimeStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "step {}", self.id_index())
    }
}

impl std::fmt::Display for TimeStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl TimeStep {
    pub const FIRST: Self = Self::MIN_ID;

    pub fn prev(self) -> Option<Self> {
        self.id_index().checked_sub(1).map(Self::from_id_index)
    }

    pub fn next(self) -> Self {
        Self::from_id_index(self.id_index() + 1)
    }

    pub fn first_n(n: usize) -> IdRange<Self> {
        IdRange::from_index_range(0..n)
    }
}
