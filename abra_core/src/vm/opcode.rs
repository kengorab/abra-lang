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
    Invoke(usize, bool),
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
            Opcode::Constant(const_idx) => Some(const_idx.to_string()),
            Opcode::New(num_fields) => Some(num_fields.to_string()),
            Opcode::GetField(field_idx) => Some(field_idx.to_string()),
            Opcode::GetMethod(method_idx) => Some(method_idx.to_string()),
            Opcode::SetField(field_idx) => Some(field_idx.to_string()),
            Opcode::MapMk(size) |
            Opcode::ArrMk(size) |
            Opcode::TupleMk(size) |
            Opcode::SetMk(size) => Some(size.to_string()),
            Opcode::GStore(slot) |
            Opcode::LStore(slot) => Some(slot.to_string()),
            Opcode::UStore(upvalue_idx) => Some(upvalue_idx.to_string()),
            Opcode::GLoad(slot) |
            Opcode::LLoad(slot) => Some(slot.to_string()),
            Opcode::ULoad(upvalue_idx) => Some(upvalue_idx.to_string()),
            Opcode::Jump(offset) |
            Opcode::JumpIfF(offset) |
            Opcode::JumpB(offset) => Some(offset.to_string()),
            Opcode::Invoke(arity, has_return) => Some(format!("{} {}", arity, has_return)),
            Opcode::Pop(num_pops) => Some(num_pops.to_string()),
            Opcode::MarkLocal(local_idx) => Some(local_idx.to_string()),
            _ => None
        };
        match imm {
            Some(imm) => format!("{} {}", base, imm),
            None => base
        }
    }
}
