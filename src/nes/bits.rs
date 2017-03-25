pub trait BitOps: Copy {
    fn set_bit(&mut self, i: u8);
    fn get_bit(&self, i: u8) -> u8;

    fn is_bit_set(&self, i: u8) -> bool {
        self.get_bit(i) == 1
    }
}

impl BitOps for u8 {
    fn set_bit(&mut self, i: u8) {
        *self |= 1 << i
    }

    fn get_bit(&self, i: u8) -> u8 {
        (self >> i) & 1
    }
}

pub trait HighLowBits {
    fn high(&self) -> u8;
    fn low(&self) -> u8;

    fn set_high(&mut self, byte: u8);
    fn set_low(&mut self, byte: u8);
}

impl HighLowBits for u16 {
    fn high(&self) -> u8 {
        (*self >> 8) as u8
    }

    fn low(&self) -> u8 {
        *self as u8
    }

    fn set_high(&mut self, byte: u8) {
        *self |= (byte as u16) << 8;
    }

    fn set_low(&mut self, byte: u8) {
        *self |= byte as u16;
    }
}
