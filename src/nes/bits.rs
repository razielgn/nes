use std::fmt;

pub trait BitOps: Copy + fmt::Binary {
    fn set_bit(&mut self, i: u8);
    fn get_bit(&self, i: u8) -> u8;
    fn clear_bit(&mut self, i: u8);

    fn is_bit_set(&self, i: u8) -> bool {
        self.get_bit(i) == 1
    }

    fn to_bitstring(&self) -> String {
        format!("{:08b}", self)
    }
}

impl BitOps for u8 {
    fn set_bit(&mut self, i: u8) {
        *self |= 1 << i
    }

    fn clear_bit(&mut self, i: u8) {
        *self &= !(1 << i)
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
        Self::from(hi) << 8 | Self::from(lo)
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bit_ops() {
        let mut byte = 0u8;
        byte.set_bit(7);
        assert_eq!(1, byte.get_bit(7));

        byte.clear_bit(7);
        assert_eq!(0, byte.get_bit(7));
    }
}
