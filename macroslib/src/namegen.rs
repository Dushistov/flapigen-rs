use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::fmt::Write;

pub(crate) fn new_unique_name(names: &FxHashSet<SmolStr>, templ: &str) -> SmolStr {
    let mut new_name: String = templ.into();
    let mut idx = 0_u64;
    loop {
        if !names.contains(new_name.as_str()) {
            return new_name.into();
        }
        write!(&mut new_name, "{}{}", templ, idx).expect("write to String failed, no free mem?");
        idx += 1;
        if idx == u64::max_value() {
            panic!("it is impossible find name for {}", templ);
        }
    }
}
