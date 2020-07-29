use std::sync::atomic::{AtomicUsize, Ordering};

pub static ANON_IDX: AtomicUsize = AtomicUsize::new(0);

pub fn get_anon_name() -> String {
    format!("$anon_{}", ANON_IDX.fetch_add(1, Ordering::Relaxed))
}
