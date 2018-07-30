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

pub trait HighLowBits: Sized {
    fn from_hilo(hi: u8, lo: u8) -> Self;
    fn high(&self) -> u8;
    fn low(&self) -> u8;
    fn split(&self) -> (u8, u8);
}

impl HighLowBits for u16 {
    fn from_hilo(hi: u8, lo: u8) -> Self {
        u16::from(hi) << 8 | u16::from(lo)
    }

    fn high(&self) -> u8 {
        (*self >> 8) as u8
    }

    fn low(&self) -> u8 {
        *self as u8
    }

    fn split(&self) -> (u8, u8) {
        (self.high(), self.low())
    }
}
