use rand::{thread_rng, distributions, Rng};
use std::{env, io, iter};
use std::io::ErrorKind;
use std::path::PathBuf;

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

// Borrowed from https://github.com/neilwashere/rust-project-root
pub fn get_project_root() -> io::Result<PathBuf> {
    let path = env::current_dir()?;
    let mut path_ancestors = path.as_path().ancestors();

    while let Some(p) = path_ancestors.next() {
        let has_cargo = std::fs::read_dir(p)?
            .into_iter()
            .any(|p| p.unwrap().file_name().to_str() == Some("Cargo.lock"));
        if has_cargo {
            return Ok(PathBuf::from(p));
        }
    }
    Err(io::Error::new(ErrorKind::NotFound, "Ran out of places to find Cargo.toml"))
}
