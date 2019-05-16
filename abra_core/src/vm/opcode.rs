#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    Constant = 0,
    Nil,
    IConst0,
    IConst1,
    IConst2,
    IConst3,
    IConst4,
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
    Invert,
    StrConcat,
    T,
    F,
    And,
    Or,
    Negate,
    Coalesce,
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    Neq,
    ArrMk,
    ArrLoad,
    ArrSlc,
    Store0,
    Store1,
    Store2,
    Store3,
    Store4,
    Store,
    Load0,
    Load1,
    Load2,
    Load3,
    Load4,
    Load,
    Jump,
    JumpIfF,
    Return,
}

impl From<u8> for Opcode {
    fn from(i: u8) -> Self {
        match i {
            0 => Opcode::Constant,
            1 => Opcode::Nil,
            2 => Opcode::IConst0,
            3 => Opcode::IConst1,
            4 => Opcode::IConst2,
            5 => Opcode::IConst3,
            6 => Opcode::IConst4,
            7 => Opcode::IAdd,
            8 => Opcode::ISub,
            9 => Opcode::IMul,
            10 => Opcode::IDiv,
            11 => Opcode::FAdd,
            12 => Opcode::FSub,
            13 => Opcode::FMul,
            14 => Opcode::FDiv,
            15 => Opcode::I2F,
            16 => Opcode::F2I,
            17 => Opcode::Invert,
            18 => Opcode::StrConcat,
            19 => Opcode::T,
            20 => Opcode::F,
            21 => Opcode::And,
            22 => Opcode::Or,
            23 => Opcode::Negate,
            24 => Opcode::Coalesce,
            25 => Opcode::LT,
            26 => Opcode::LTE,
            27 => Opcode::GT,
            28 => Opcode::GTE,
            29 => Opcode::Eq,
            30 => Opcode::Neq,
            31 => Opcode::ArrMk,
            32 => Opcode::ArrLoad,
            33 => Opcode::ArrSlc,
            34 => Opcode::Store0,
            35 => Opcode::Store1,
            36 => Opcode::Store2,
            37 => Opcode::Store3,
            38 => Opcode::Store4,
            39 => Opcode::Store,
            40 => Opcode::Load0,
            41 => Opcode::Load1,
            42 => Opcode::Load2,
            43 => Opcode::Load3,
            44 => Opcode::Load4,
            45 => Opcode::Load,
            46 => Opcode::Jump,
            47 => Opcode::JumpIfF,
            48 => Opcode::Return,
            _ => unreachable!()
        }
    }
}
