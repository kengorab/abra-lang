#[derive(Clone, Display, Debug, PartialEq)]
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
    IMod,
    FMod,
    I2F,
    F2I,
    Invert,
    StrConcat,
    T,
    F,
    Negate,
    LT,
    LTE,
    GT,
    GTE,
    Eq,
    Neq,
    New,
    Init,
    GetField,
    SetField,
    MapMk,
    MapLoad,
    MapStore,
    ArrMk,
    ArrLoad,
    ArrStore,
    ArrSlc,
    GStore,
    LStore0,
    LStore1,
    LStore2,
    LStore3,
    LStore4,
    LStore,
    UStore0,
    UStore1,
    UStore2,
    UStore3,
    UStore4,
    UStore,
    GLoad,
    LLoad0,
    LLoad1,
    LLoad2,
    LLoad3,
    LLoad4,
    LLoad,
    ULoad0,
    ULoad1,
    ULoad2,
    ULoad3,
    ULoad4,
    ULoad,
    Jump,
    JumpIfF,
    JumpB,
    Invoke,
    ClosureMk,
    CloseUpvalue,
    CloseUpvalueAndPop,
    Pop,
    PopN,
    MarkLocal,
    Dup,
    Return,
}

impl From<&u8> for Opcode {
    fn from(i: &u8) -> Self {
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
            15 => Opcode::IMod,
            16 => Opcode::FMod,
            17 => Opcode::I2F,
            18 => Opcode::F2I,
            19 => Opcode::Invert,
            20 => Opcode::StrConcat,
            21 => Opcode::T,
            22 => Opcode::F,
            23 => Opcode::Negate,
            24 => Opcode::LT,
            25 => Opcode::LTE,
            26 => Opcode::GT,
            27 => Opcode::GTE,
            28 => Opcode::Eq,
            29 => Opcode::Neq,
            30 => Opcode::New,
            31 => Opcode::Init,
            32 => Opcode::GetField,
            33 => Opcode::SetField,
            34 => Opcode::MapMk,
            35 => Opcode::MapLoad,
            36 => Opcode::MapStore,
            37 => Opcode::ArrMk,
            38 => Opcode::ArrLoad,
            39 => Opcode::ArrStore,
            40 => Opcode::ArrSlc,
            41 => Opcode::GStore,
            42 => Opcode::LStore0,
            43 => Opcode::LStore1,
            44 => Opcode::LStore2,
            45 => Opcode::LStore3,
            46 => Opcode::LStore4,
            47 => Opcode::LStore,
            48 => Opcode::UStore0,
            49 => Opcode::UStore1,
            50 => Opcode::UStore2,
            51 => Opcode::UStore3,
            52 => Opcode::UStore4,
            53 => Opcode::UStore,
            54 => Opcode::GLoad,
            55 => Opcode::LLoad0,
            56 => Opcode::LLoad1,
            57 => Opcode::LLoad2,
            58 => Opcode::LLoad3,
            59 => Opcode::LLoad4,
            60 => Opcode::LLoad,
            61 => Opcode::ULoad0,
            62 => Opcode::ULoad1,
            63 => Opcode::ULoad2,
            64 => Opcode::ULoad3,
            65 => Opcode::ULoad4,
            66 => Opcode::ULoad,
            67 => Opcode::Jump,
            68 => Opcode::JumpIfF,
            69 => Opcode::JumpB,
            70 => Opcode::Invoke,
            71 => Opcode::ClosureMk,
            72 => Opcode::CloseUpvalue,
            73 => Opcode::CloseUpvalueAndPop,
            74 => Opcode::Pop,
            75 => Opcode::PopN,
            76 => Opcode::MarkLocal,
            77 => Opcode::Dup,
            78 => Opcode::Return,
            _ => unreachable!()
        }
    }
}

impl Opcode {
    pub fn num_expected_imms(&self) -> u8 {
        match self {
            Opcode::Constant |
            Opcode::Jump |
            Opcode::JumpIfF |
            Opcode::JumpB |
            Opcode::ArrMk |
            Opcode::MapMk |
            Opcode::LStore |
            Opcode::UStore |
            Opcode::PopN |
            Opcode::LLoad |
            Opcode::ULoad |
            Opcode::New |
            Opcode::MarkLocal |
            Opcode::GetField |
            Opcode::Invoke |
            Opcode::SetField => 1,
            _ => 0
        }
    }
}
