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