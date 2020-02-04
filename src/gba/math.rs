#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Result {
    pub value: u8,
    pub zero: Option<bool>,
    pub add_sub: Option<bool>,
    pub half_carry: Option<bool>,
    pub carry: Option<bool>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Result16 {
    pub value: u16,
    pub zero: Option<bool>,
    pub add_sub: Option<bool>,
    pub half_carry: Option<bool>,
    pub carry: Option<bool>,
}

pub fn add(a: u8, b: u8) -> Result {
    let (res, did_overflow) = a.overflowing_add(b);
    Result {
        value: res,
        zero: Some(res == 0),
        add_sub: Some(false),
        half_carry: Some(self::half_carry_add(a, b)),
        carry: Some(did_overflow),
    }
}

pub fn add16(a: u16, b: u16) -> Result16 {
    let (res, did_overflow) = a.overflowing_add(b);
    let half_carry = (((a & 0x0FFF) + (b & 0x0FFF)) & 0x1000) != 0;
    Result16 {
        value: res,
        zero: None,
        add_sub: Some(false),
        half_carry: Some(half_carry),
        carry: Some(did_overflow),
    }
}

pub fn sub(a: u8, b: u8) -> Result {
    let (res, did_overflow) = a.overflowing_sub(b);
    Result {
        value: res,
        zero: Some(res == 0),
        add_sub: Some(true),
        half_carry: Some(self::half_carry_sub(a, b)),
        carry: Some(did_overflow),
    }
}

pub fn cp(a: u8, b: u8) -> Result {
    let (res, did_overflow) = a.overflowing_sub(b);
    Result {
        value: a,
        zero: Some(res == 0),
        add_sub: Some(true),
        half_carry: Some(self::half_carry_sub(a, b)),
        carry: Some(did_overflow),
    }
}

pub fn and(a: u8, b: u8) -> Result {
    let value = a & b;
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(true),
        carry: Some(false),
    }
}

pub fn or(a: u8, b: u8) -> Result {
    let value = a | b;
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(false),
    }
}

pub fn xor(a: u8, b: u8) -> Result {
    let value = a ^ b;
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(false),
    }
}

fn half_carry_add(a: u8, b: u8) -> bool {
    (((0xf & a) + (0xf & b)) & 0x10) == 0x10
}

fn half_carry_sub(a: u8, b: u8) -> bool {
    (a & 0xf) < (b & 0xf)
}

pub fn sla(a: u8) -> Result {
    let value = a << 1;
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(a & 0x80 == 0x80),
    }
}

pub fn sra(a: u8) -> Result {
    let value = a >> 1 | (a & 0x80);
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(a & 0x01 == 0x01),
    }
}

pub fn srl(a: u8) -> Result {
    let value = a >> 1;
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(a & 0x01 == 0x01),
    }
}

pub fn swap(a: u8) -> Result {
    let value = ((a & 0xf0) >> 4) | ((a & 0x0f) << 4);
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(false),
    }
}

pub fn rlc(a: u8) -> Result {
    let value = ((a & 0x7f) << 1) | ((a & 0x80) >> 7);
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(a & 0x80 == 0x80),
    }
}

pub fn rrc(a: u8) -> Result {
    let value = ((a & 0xfe) >> 1) | ((a & 0x1) << 7);
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(a & 0x1 == 0x1),
    }
}

pub fn rr(a: u8, carry: bool) -> Result {
    let value = ((a & 0xfe) >> 1) | (if carry { 0x80 } else { 0x0 });
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(a & 0x1 == 0x1),
    }
}

pub fn rl(a: u8, carry: bool) -> Result {
    let value = ((a & 0x7f) << 1) | (if carry { 0x1 } else { 0x0 });
    Result {
        value,
        zero: Some(value == 0),
        add_sub: Some(false),
        half_carry: Some(false),
        carry: Some(a & 0x80 == 0x80),
    }
}

pub fn sp_add(sp: u16, delta: i8) -> Result16 {
    let b = (delta as i16) as u16;
    let half_carry = (0xf & sp) + (0xf & b) >= 0x10;
    let carry = (0xff & sp) + (0xff & b) >= 0x100;
    let value = sp.wrapping_add(b);
    Result16 {
        value,
        zero: Some(false),
        add_sub: Some(false),
        half_carry: Some(half_carry),
        carry: Some(carry),
    }
}

pub fn adc(a: u8, b: u8, c: bool) -> Result {
    let c_value = if c { 1 } else { 0 };
    let (res, did_overflow) = a.overflowing_add(b);
    let (res2, did_overflow2) = res.overflowing_add(c_value);
    let half_carry = (((a & 0x0f) + (b & 0x0f) + c_value) & 0x10) == 0x10;
    Result {
        value: res2,
        zero: Some(res2 == 0),
        add_sub: Some(false),
        half_carry: Some(half_carry),
        carry: Some(did_overflow || did_overflow2),
    }
}

pub fn sbc(a: u8, b: u8, c: bool) -> Result {
    let c_value = if c { 1 } else { 0 };
    let (res, did_overflow) = a.overflowing_sub(b);
    let (res2, did_overflow2) = res.overflowing_sub(c_value);
    let half_carry = (a & 0xf) < ((b & 0xf) + c_value);
    Result {
        value: res2,
        zero: Some(res2 == 0),
        add_sub: Some(true),
        half_carry: Some(half_carry),
        carry: Some(did_overflow || did_overflow2),
    }
}

pub fn daa(a: u8, carry: bool, half: bool, subtraction: bool) -> Result {
    if !subtraction {
        let left = (a & 0xf0) >> 4;
        let right = a & 0x0f;
        let right_add = if half || right >= 0xa { 0x06 } else { 0x00 };
        let left_add = if carry || left >= 0xa || (left >= 0x9 && right >= 0xa) {
            0x60
        } else {
            0x00
        };
        let (value, _did_overflow) = a.overflowing_add(left_add + right_add);
        Result {
            value,
            zero: Some(value == 0),
            add_sub: None,
            half_carry: Some(false),
            carry: Some(left_add >= 0x60),
        }
    } else {
        let adder = match (carry, half) {
            (false, false) => 0x0,
            (false, true) => 0xFA,
            (true, false) => 0xA0,
            (true, true) => 0x9A,
        };
        let (value, _did_overflow) = a.overflowing_add(adder);
        Result {
            value,
            zero: Some(value == 0),
            add_sub: None,
            half_carry: Some(false),
            carry: Some(carry),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(
            add(1, 1),
            Result {
                value: 2,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            add(0x3A, 0xC6),
            Result {
                value: 0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(true)
            }
        );
        assert_eq!(
            add(0xFF, 0x02),
            Result {
                value: 1,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(true)
            }
        );
    }

    #[test]
    fn test_add16() {
        assert_eq!(
            add16(0x8A23, 0x0605),
            Result16 {
                value: 0x9028,
                zero: None,
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(false)
            }
        );
        assert_eq!(
            add16(0x8A23, 0x8A23),
            Result16 {
                value: 0x1446,
                zero: None,
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(true)
            }
        );
    }

    #[test]
    fn test_sub() {
        assert_eq!(
            sub(0x3E, 0x3E),
            Result {
                value: 0,
                zero: Some(true),
                add_sub: Some(true),
                half_carry: Some(false),
                carry: Some(false)
            }
        );
        assert_eq!(
            sub(0x3E, 0x0F),
            Result {
                value: 0x2F,
                zero: Some(false),
                add_sub: Some(true),
                half_carry: Some(true),
                carry: Some(false),
            }
        );
        assert_eq!(
            sub(0x3E, 0x40),
            Result {
                value: 0xFE,
                zero: Some(false),
                add_sub: Some(true),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
    }

    #[test]
    fn test_cp() {
        assert_eq!(
            cp(0x3C, 0x2F),
            Result {
                value: 0x3C,
                zero: Some(false),
                add_sub: Some(true),
                half_carry: Some(true),
                carry: Some(false),
            }
        );
        assert_eq!(
            cp(0x3C, 0x3C),
            Result {
                value: 0x3C,
                zero: Some(true),
                add_sub: Some(true),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            cp(0x3C, 0x40),
            Result {
                value: 0x3C,
                zero: Some(false),
                add_sub: Some(true),
                half_carry: Some(false),
                carry: Some(true),
            }
        );
    }

    #[test]
    fn test_and() {
        assert_eq!(
            and(0x5A, 0x3F),
            Result {
                value: 0x1A,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(false)
            }
        );
        assert_eq!(
            and(0x5A, 0x38),
            Result {
                value: 0x18,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(false),
            }
        );
        assert_eq!(
            and(0x5A, 0x0),
            Result {
                value: 0x0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(false)
            }
        );
    }

    #[test]
    fn test_sla() {
        assert_eq!(
            sla(0x80),
            Result {
                value: 0x00,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
        assert_eq!(
            sla(0xFF),
            Result {
                value: 0xFE,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
    }

    #[test]
    fn test_sra() {
        assert_eq!(
            sra(0x8A),
            Result {
                value: 0xC5,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false)
            }
        );
        assert_eq!(
            sra(0x01),
            Result {
                value: 0x00,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
    }

    #[test]
    fn test_srl() {
        assert_eq!(
            srl(0x01),
            Result {
                value: 0x0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
        assert_eq!(
            srl(0xFF),
            Result {
                value: 0x7F,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
    }

    #[test]
    fn test_swap() {
        assert_eq!(
            swap(0x00),
            Result {
                value: 0x0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false)
            }
        );
        assert_eq!(
            swap(0xF0),
            Result {
                value: 0x0F,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false)
            }
        );
    }

    #[test]
    fn test_rlc() {
        assert_eq!(
            rlc(0x85),
            Result {
                value: 0x0B,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
        assert_eq!(
            rlc(0x0),
            Result {
                value: 0x0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false)
            }
        );
        assert_eq!(
            rlc(0x80),
            Result {
                value: 0x01,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
    }

    #[test]
    fn test_rrc() {
        assert_eq!(
            rrc(0x1),
            Result {
                value: 0x80,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true)
            }
        );
        assert_eq!(
            rrc(0x0),
            Result {
                value: 0x0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false)
            }
        );
    }

    #[test]
    fn test_rr() {
        assert_eq!(
            rr(0x1, false),
            Result {
                value: 0x0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true),
            }
        );
        assert_eq!(
            rr(0x8A, false),
            Result {
                value: 0x45,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            rr(0x0, true),
            Result {
                value: 0x80,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
    }

    #[test]
    fn test_rl() {
        assert_eq!(
            rl(0x80, false),
            Result {
                value: 0x0,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true),
            }
        );
        assert_eq!(
            rl(0x11, false),
            Result {
                value: 0x22,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            rl(0x0, true),
            Result {
                value: 0x01,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
    }

    #[test]
    fn test_adc() {
        assert_eq!(
            adc(0xE1, 0x0F, true),
            Result {
                value: 0xF1,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(false),
            }
        );
        assert_eq!(
            adc(0xE1, 0x3B, true),
            Result {
                value: 0x1D,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true),
            }
        );
        assert_eq!(
            adc(0xE1, 0x1E, true),
            Result {
                value: 0x00,
                zero: Some(true),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(true),
            }
        );
    }

    #[test]
    fn test_sbc() {
        assert_eq!(
            sbc(0x3b, 0x2a, true),
            Result {
                value: 0x10,
                zero: Some(false),
                add_sub: Some(true),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            sbc(0x3b, 0x3a, true),
            Result {
                value: 0x00,
                zero: Some(true),
                add_sub: Some(true),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            sbc(0x3b, 0x4f, true),
            Result {
                value: 0xeb,
                zero: Some(false),
                add_sub: Some(true),
                half_carry: Some(true),
                carry: Some(true),
            }
        );
    }

    #[test]
    fn test_daa() {
        assert_eq!(
            daa(0x7d, false, false, false),
            Result {
                value: 0x83,
                zero: Some(false),
                add_sub: None,
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            daa(0x20, true, false, false),
            Result {
                value: 0x80,
                zero: Some(false),
                add_sub: None,
                half_carry: Some(false),
                carry: Some(true),
            }
        );
    }

    #[test]
    fn test_sp_add() {
        assert_eq!(
            sp_add(0x000f, 0x1),
            Result16 {
                value: 0x0010,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(false),
            }
        );
        assert_eq!(
            sp_add(0x00f0, 0x10),
            Result16 {
                value: 0x0100,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true),
            }
        );
        assert_eq!(
            sp_add(0x0ff0, 0x10),
            Result16 {
                value: 0x1000,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(true),
            }
        );
        assert_eq!(
            sp_add(0xfffe, -1),
            Result16 {
                value: 0xfffd,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(true),
                carry: Some(true),
            }
        );
        assert_eq!(
            sp_add(0xff00, -2),
            Result16 {
                value: 0xfefe,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            sp_add(0xff00, -127),
            Result16 {
                value: 0xfe81,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
        assert_eq!(
            sp_add(0x0, -1),
            Result16 {
                value: 0xffff,
                zero: Some(false),
                add_sub: Some(false),
                half_carry: Some(false),
                carry: Some(false),
            }
        );
    }
}
