use std::{cell::Cell, rc::Rc};

#[derive(Debug, Clone, Default)]
pub struct Pin(Rc<Cell<bool>>);

impl Pin {
    pub fn pull(&self) {
        self.0.set(true);
    }

    pub fn clear(&self) {
        self.0.set(false);
    }

    pub fn is_pulled(&self) -> bool {
        self.0.get()
    }
}
