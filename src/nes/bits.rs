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
