macro_rules! define_opecode {
    ($($enum_const:ident)*) => {
        #[repr(u8)]
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum Opcode {
            $($enum_const = ${index()}),*,
            Unknown,
        }
        impl From<u8> for Opcode {
            fn from(value: u8) -> Self {
                if value < ${count($enum_const)} {
                    unsafe { ::std::mem::transmute::<u8, Opcode>(value) }
                } else {
                    Opcode::Unknown
                }
            }
        }
    };
}

define_opecode! {
    Nop

    LoadInt
    LoadFloat
    LoadSring
    LoadBool
    LoadNil
    LoadLocal
    Unload

    StoreLoal
    StoreNewLoal
    DropLocal

    MakeArray
    MakeTable

    Jump
    JumpIfTrue
    JumpIfFalse

    Call
    CallMethod
    Leave

    SetItem
    GetItem
    SetItemS
    GetItemS
    SetItemI
    GetItemI

    Add
    Sub
    Mul
    Div
    Mod
    Unm
    Unp
    Not
    Eq
    Ne
    Le
    Lt
    Ge
    Gt
    Concat
    BitAnd
    BitOr
    BitXor
    BitNot
    ShiftL
    ShiftR

    GetIter
    IterMoveNext
    IterCurrent
}
