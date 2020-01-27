#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Result {
    pub value: u8,
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
}
