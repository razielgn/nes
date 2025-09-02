use std::{cell::Cell, rc::Rc};

#[derive(Debug, Clone, Default)]
pub struct Pin(Rc<Cell<(usize, bool)>>);

impl Pin {
    pub fn pull(&self) {
        self.0.set((0, true));
    }

    pub fn pull_with_delay(&self, delay: usize) {
        self.0.set((delay, true));
    }

    pub fn clear(&self) {
        self.0.set((0, false));
    }

    pub fn is_pulled(&self) -> bool {
        self.0.get() == (0, true)
    }

    pub fn decr_delay(&self) {
        self.0
            .update(|(delay, state)| (delay.saturating_sub(1), state));
    }
}
