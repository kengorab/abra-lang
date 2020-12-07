use rand::{thread_rng, distributions, Rng};
use std::iter;

pub fn random_string(length: usize) -> String {
    let mut rng = thread_rng();
    let chars: String = iter::repeat(())
        .map(|()| rng.sample(distributions::Alphanumeric))
        .take(length)
        .collect();
    return chars;
}

// This code has been deprecated from the Rust stdlib, it's copied from here: https://github.com/rust-lang/rust/blob/5c674a11471ec0569f616854d715941757a48a0a/src/libcore/num/f64.rs#L203-L216
pub fn integer_decode(float: f64) -> (u64, i16, i8) {
    let bits: u64 = unsafe { std::mem::transmute(float) };
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };
    // Exponent bias + mantissa shift
    exponent -= 1023 + 52;
    (mantissa, exponent, sign)
}
