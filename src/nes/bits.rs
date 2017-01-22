pub trait BitOps: Copy {
    fn set_bit(self, bit: u8) -> Self;
    fn get_bit(&self, nth: u8) -> u8;

    fn is_bit_set(&self, nth: u8) -> bool {
        self.get_bit(nth) == 1
    }
}

impl BitOps for u8 {
    fn set_bit(self, bit: u8) -> Self {
        self | 1 << bit
    }

    fn get_bit(&self, bit: u8) -> u8 {
        (self >> bit) & 1
    }
}
