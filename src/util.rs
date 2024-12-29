use std::ops::RangeInclusive;

pub trait Extractable {
    fn extract(self, range: RangeInclusive<u8>) -> Self;
    fn extract_flag(self, index: usize) -> bool;
}

impl Extractable for u16 {
    fn extract(self, range: std::ops::RangeInclusive<u8>) -> u16 {
        let bits = (range.end() - range.start() + 1) as u16;
        let mask = ((1u16 << bits) - 1) << range.start();
        (self & mask) >> range.start()
    }

    fn extract_flag(self, index: usize) -> bool {
        (self & (1 << index)) != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extr() {
        let sample: u16 = 0b0000_0000_1001_0000;
        let extracted = sample.extract(4..=8);
        assert_eq!(9, extracted);
    }
}
