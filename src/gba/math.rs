
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

#[cfg(test)]
mod tests {
    use super::add;
    use super::and;
    use super::sub;
    use super::Result;

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
}
