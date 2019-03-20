#[derive(Debug, PartialEq)]
pub enum Opcode {
    Constant,
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Return,
}

impl Into<u8> for Opcode {
    fn into(self) -> u8 {
        match &self {
            Opcode::Constant => 0,
            Opcode::Add => 1,
            Opcode::Sub => 2,
            Opcode::Mul => 3,
            Opcode::Div => 4,
            Opcode::Negate => 5,
            Opcode::Return => 6,
        }
    }
}

impl From<u8> for Opcode {
    fn from(i: u8) -> Self {
        match i {
            0 => Opcode::Constant,
            1 => Opcode::Add,
            2 => Opcode::Sub,
            3 => Opcode::Mul,
            4 => Opcode::Div,
            5 => Opcode::Negate,
            6 => Opcode::Return,
            _ => unimplemented!()
        }
    }
}
