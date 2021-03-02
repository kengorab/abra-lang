use itertools::Itertools;

#[derive(Clone, Copy, Display, Debug, Hash, PartialEq, Eq)]
pub enum Opcode {
    Constant(/* module_idx: */ usize, /* const_idx: */usize),
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
    GStore(/* module_id: */ usize, /* const_idx: */ usize),
    LStore(usize),
    UStore(usize),
    GLoad(/* module_id: */ usize, /* const_idx: */ usize),
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
            Opcode::Constant(module_idx, const_idx) => Some(vec![module_idx, const_idx]),
            Opcode::New(num_fields) => Some(vec![num_fields]),
            Opcode::GetField(field_idx) => Some(vec![field_idx]),
            Opcode::GetMethod(method_idx) => Some(vec![method_idx]),
            Opcode::SetField(field_idx) => Some(vec![field_idx]),
            Opcode::MapMk(size) |
            Opcode::ArrMk(size) |
            Opcode::TupleMk(size) |
            Opcode::SetMk(size) => Some(vec![size]),
            Opcode::GStore(module_idx, slot) |
            Opcode::GLoad(module_idx, slot) => Some(vec![module_idx, slot]),
            Opcode::LStore(slot) |
            Opcode::LLoad(slot) => Some(vec![slot]),
            Opcode::UStore(upvalue_idx) |
            Opcode::ULoad(upvalue_idx) => Some(vec![upvalue_idx]),
            Opcode::Jump(offset) |
            Opcode::JumpIfF(offset) |
            Opcode::JumpB(offset) => Some(vec![offset]),
            Opcode::Invoke(arity) => Some(vec![arity]),
            Opcode::Pop(num_pops) => Some(vec![num_pops]),
            Opcode::MarkLocal(local_idx) => Some(vec![local_idx]),
            _ => None
        };
        match imm {
            Some(imm) => format!("{} {}", base, imm.iter().join(" ")),
            None => base
        }
    }
}
