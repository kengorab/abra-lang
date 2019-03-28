#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    Constant = 0,
    IAdd,
    ISub,
    IMul,
    IDiv,
    FAdd,
    FSub,
    FMul,
    FDiv,
    I2F,
    F2I,
    StrConcat,
    Negate,
    Return,
}

impl From<u8> for Opcode {
    fn from(i: u8) -> Self {
        match i {
            0 => Opcode::Constant,
            1 => Opcode::IAdd,
            2 => Opcode::ISub,
            3 => Opcode::IMul,
            4 => Opcode::IDiv,
            5 => Opcode::FAdd,
            6 => Opcode::FSub,
            7 => Opcode::FMul,
            8 => Opcode::FDiv,
            9 => Opcode::I2F,
            10 => Opcode::F2I,
            11 => Opcode::StrConcat,
            12 => Opcode::Negate,
            13 => Opcode::Return,
            _ => unreachable!()
        }
    }
}
