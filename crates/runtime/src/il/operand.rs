#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Operand<const N: usize>([u8; N]);

impl<const N: usize> Operand<N> {
    pub const fn new(value: [u8; N]) -> Self {
        Operand(value)
    }

    pub const fn raw(&self) -> &[u8; N] {
        &self.0
    }

    pub const fn into_raw(self) -> [u8; N] {
        self.0
    }

    pub const fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    pub const fn as_mut_bytes(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

macro_rules! impl_num_convert {
    (N = $n:literal : $($t:ident),*) => {
        impl Operand<$n> {
            $(
                pub fn ${concat(as_, $t)}(&self) -> $t {
                    <$t>::from_le_bytes(self.0)
                }
            )*
        }
        $(
            impl From<$t> for Operand<$n> {
                fn from(value: $t) -> Self {
                    Operand(value.to_le_bytes())
                }
            }
        )*
    };
}
impl_num_convert!(N = 8 : i64, u64, f64);
impl_num_convert!(N = 4 : i32, u32, f32);
impl_num_convert!(N = 2 : i16, u16);
