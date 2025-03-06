use super::*;

#[repr(C, align(8))]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Instr(Opcode, [u8; 15]);

const fn _static_assert() {
    const {
        assert!(size_of::<Instr>() == 2 * size_of::<u64>());
        assert!(align_of::<Instr>() == align_of::<u64>());
    }
}

impl Instr {
    pub const fn new(opcode: Opcode) -> Self {
        Instr(opcode, [0; 15])
    }

    pub const fn opcode(&self) -> Opcode {
        self.0
    }
}

// | opcode | b00 | b01 | b02 |
//    | b03 | b04 | b05 | b06 |
//    | b07 | b08 | b09 | b10 |
//    | b11 | b12 | b13 | b14 |
macro_rules! instr_operand_rw {
    ($($u:ident <$n:literal> $s:literal .. $e:literal)*) => {
    impl Instr {
        $(
            // To clarify type annotations from rust-analyzer, we use Operand<$n> instead of
            // Operand<{$e - $s}>.
            // Currently, if we use Operand<{$e - $s}>, rust-analyzer will show the type as
            // Operand<{const}>.

            pub fn ${concat(read_operand_, $u)}(&self) -> Operand<$n> {
                let ptr = self.1[$s .. $e].as_ptr() as *const [u8; $e - $s];
                Operand::new(unsafe { *ptr })
            }

            pub fn ${concat(write_operand_, $u)}(&mut self, operand: impl Into<Operand<$n>>) {
                let ptr = self.1[$s .. $e].as_mut_ptr() as *mut [u8; $e - $s];
                unsafe { *ptr = operand.into().into_raw() }
            }
        )*
    }
    };
}
instr_operand_rw! {
    x  <8>  7 .. 15

    w0 <4>  3 ..  7
    w1 <4>  7 .. 11
    w2 <4> 11 .. 15

    h0 <2>  1 ..  3
    h1 <2>  3 ..  5
    h2 <2>  5 ..  7
    h3 <2>  7 ..  9
    h4 <2>  9 .. 11
    h5 <2> 11 .. 13
    h6 <2> 13 .. 15

    b0 <1>  0 ..  1
    b1 <1>  1 ..  2
    b2 <1>  2 ..  3
    b3 <1>  3 ..  4
    b4 <1>  4 ..  5
    b5 <1>  5 ..  6
    b6 <1>  6 ..  7
    b7 <1>  7 ..  8
    b8 <1>  8 ..  9
    b9 <1>  9 .. 10
    ba <1> 10 .. 11
    bb <1> 11 .. 12
    bc <1> 12 .. 13
    bd <1> 13 .. 14
    be <1> 14 .. 15
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instr").field("opcode", &self.0).field("operand", &self.1).finish()
    }
}
