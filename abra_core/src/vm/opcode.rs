#[derive(Clone, Copy, Display, Debug, Hash, PartialEq, Eq)]
pub enum Opcode {
    Constant(usize),
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
    Pow,
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
    Xor,
    New(usize),
    GetField(usize),
    GetMethod(usize),
    SetField(usize),
    MapMk(usize),
    MapLoad,
    MapStore,
    ArrMk(usize),
    ArrLoad,
    ArrStore,
    ArrSlc,
    TupleMk(usize),
    TupleLoad,
    TupleStore,
    SetMk(usize),
    GStore(usize),
    LStore(usize),
    UStore(usize),
    GLoad(usize),
    LLoad(usize),
    ULoad(usize),
    Jump(usize),
    JumpIfF(usize),
    JumpB(usize),
    Invoke(usize),
    ClosureMk,
    CloseUpvalue,
    CloseUpvalueAndPop,
    Pop(usize),
    MarkLocal(usize),
    Dup,
    Typeof,
    Return,
}

impl Opcode {
    pub fn dis(&self) -> String {
        let base = self.to_string();

        let imm = match self {
            Opcode::Constant(const_idx) => Some(const_idx),
            Opcode::New(num_fields) => Some(num_fields),
            Opcode::GetField(field_idx) => Some(field_idx),
            Opcode::GetMethod(method_idx) => Some(method_idx),
            Opcode::SetField(field_idx) => Some(field_idx),
            Opcode::MapMk(size) |
            Opcode::ArrMk(size) |
            Opcode::TupleMk(size) |
            Opcode::SetMk(size) => Some(size),
            Opcode::GStore(slot) |
            Opcode::LStore(slot) => Some(slot),
            Opcode::UStore(upvalue_idx) => Some(upvalue_idx),
            Opcode::GLoad(slot) |
            Opcode::LLoad(slot) => Some(slot),
            Opcode::ULoad(upvalue_idx) => Some(upvalue_idx),
            Opcode::Jump(offset) |
            Opcode::JumpIfF(offset) |
            Opcode::JumpB(offset) => Some(offset),
            Opcode::Invoke(arity) => Some(arity),
            Opcode::Pop(num_pops) => Some(num_pops),
            Opcode::MarkLocal(local_idx) => Some(local_idx),
            _ => None
        };
        match imm {
            Some(imm) => format!("{} {}", base, imm),
            None => base
        }
    }
}
