use crate::bits::BitOps;
use log::*;

#[derive(Clone, Copy, Debug)]
pub enum Button {
    A = 0,
    B = 1,
    Select = 2,
    Start = 3,
    Up = 4,
    Down = 5,
    Left = 6,
    Right = 7,
}

#[derive(Clone, Copy, Default)]
pub struct Controller {
    buttons: [bool; 8],
    idx: usize,
    strobe: bool,
}

impl Controller {
    pub fn set_button(&mut self, button: Button) {
        self.buttons[button as usize] = true;
    }

    pub fn unset_button(&mut self, button: Button) {
        self.buttons[button as usize] = false;
    }

    pub fn read(&mut self) -> u8 {
        let val = if self.idx < 8 && self.buttons[self.idx] {
            1
        } else {
            0
        };

        self.idx = if self.strobe { 0 } else { self.idx + 1 };

        val
    }

    pub fn write(&mut self, val: u8) {
        self.strobe = val.is_bit_set(0);
        if self.strobe {
            self.idx = 0;
        }
    }
}
