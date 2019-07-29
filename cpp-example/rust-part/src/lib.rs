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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let foo = Foo::new(5);
        assert_eq!(8, foo.f(1, 2));
    }
}
