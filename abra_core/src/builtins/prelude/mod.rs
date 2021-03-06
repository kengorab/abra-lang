#[cfg(test)]
pub use index::{PRELUDE_PRINTLN_INDEX, PRELUDE_STRING_INDEX};
pub use index::load_module;
pub use native_array::NativeArray;
pub use native_float::NativeFloat;
pub use native_int::NativeInt;
pub use native_map::NativeMap;
pub use native_set::NativeSet;
pub use native_string::NativeString;

mod index;
mod native_array;
mod native_float;
mod native_int;
mod native_map;
mod native_set;
mod native_string;
