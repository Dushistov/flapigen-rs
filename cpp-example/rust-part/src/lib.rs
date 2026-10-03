// ANCHOR: connect
mod cpp_glue;
pub use crate::cpp_glue::*;
// ANCHOR_END: connect

// ANCHOR: rust_class
pub struct Foo {
    data: i32,
}

impl Foo {
    fn new(val: i32) -> Foo {
        Foo { data: val }
    }

    fn f(&self, a: i32, b: i32) -> i32 {
        self.data + a + b
    }

    fn set_field(&mut self, v: i32) {
        self.data = v;
    }
}
// ANCHOR_END: rust_class

fn f2(a: i32) -> i32 {
    a * 2
}

// ANCHOR: shared_counter_rust
pub struct SharedCounter {
    value: i32,
}

impl SharedCounter {
    fn new() -> std::rc::Rc<std::cell::RefCell<Self>> {
        std::rc::Rc::new(std::cell::RefCell::new(Self { value: 0 }))
    }

    fn increment(&mut self) {
        self.value += 1;
    }

    fn value(&self) -> i32 {
        self.value
    }
}
// ANCHOR_END: shared_counter_rust

// ANCHOR: plain_class_rust
pub struct ScoreAdjustment {
    bonus: i32,
}

impl ScoreAdjustment {
    fn new(bonus: i32) -> Self {
        Self { bonus }
    }

    fn apply(&self, base: i32) -> i32 {
        base + self.bonus
    }
}
// ANCHOR_END: plain_class_rust

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let foo = Foo::new(5);
        assert_eq!(8, foo.f(1, 2));
    }
}
