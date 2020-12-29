use std::sync::atomic::{AtomicUsize, Ordering};

pub static ANON_IDX: AtomicUsize = AtomicUsize::new(0);

pub fn get_anon_name() -> String {
    format!("$anon_{}", ANON_IDX.fetch_add(1, Ordering::Relaxed))
}

pub static TEMP_IDX: AtomicUsize = AtomicUsize::new(0);

pub fn get_temp_name() -> String {
    format!("$temp_{}", TEMP_IDX.fetch_add(1, Ordering::Relaxed))
}
