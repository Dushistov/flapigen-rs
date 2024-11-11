use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::{borrow::Borrow, fmt::Write, hash::Hash};

pub(crate) fn new_unique_name<T>(names: &FxHashSet<T>, templ: &str) -> SmolStr
where
    T: Borrow<str> + Eq + Hash,
{
    let mut new_name: String = templ.into();
    let mut idx = 0_u64;
    loop {
        if !names.contains(new_name.as_str()) {
            return new_name.into();
        }
        new_name.clear();
        write!(&mut new_name, "{}{}", templ, idx).expect("write to String failed, no free mem?");
        idx += 1;
        if idx == u64::MAX {
            panic!("it is impossible find name for {templ}");
        }
    }
}
