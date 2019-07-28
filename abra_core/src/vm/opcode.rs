#[derive(Display, Debug, PartialEq)]
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
    GStore,
    LStore0,
    LStore1,
    LStore2,
    LStore3,
    LStore4,
    LStore,
    GLoad,
    LLoad0,
    LLoad1,
    LLoad2,
    LLoad3,
    LLoad4,
    LLoad,
    Jump,
    JumpIfF,
    Invoke,
    Pop,
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
            34 => Opcode::GStore,
            35 => Opcode::LStore0,
            36 => Opcode::LStore1,
            37 => Opcode::LStore2,
            38 => Opcode::LStore3,
            39 => Opcode::LStore4,
            40 => Opcode::LStore,
            41 => Opcode::GLoad,
            42 => Opcode::LLoad0,
            43 => Opcode::LLoad1,
            44 => Opcode::LLoad2,
            45 => Opcode::LLoad3,
            46 => Opcode::LLoad4,
            47 => Opcode::LLoad,
            48 => Opcode::Jump,
            49 => Opcode::JumpIfF,
            50 => Opcode::Invoke,
            51 => Opcode::Pop,
            52 => Opcode::Return,
            _ => unreachable!()
        }
    }
}

impl Opcode {
    pub fn expects_imm(&self) -> bool {
        match self {
            Opcode::Constant |
            Opcode::Jump |
            Opcode::JumpIfF |
            Opcode::ArrMk |
            Opcode::LStore |
            Opcode::LLoad |
            Opcode::Invoke => true,
            _ => false
        }
    }
}
