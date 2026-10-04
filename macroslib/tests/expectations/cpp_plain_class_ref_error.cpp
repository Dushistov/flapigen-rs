@@expect {"file":"Foo.hpp","kind":"item","name":"get","form":"declaration"}
const FooOpaque * get() const noexcept;
@@end

@@expect {"file":"Foo.hpp","kind":"item","name":"get_mut","form":"declaration"}
const FooOpaque * get_mut() noexcept;
@@end

@@expect {"file":"Foo.hpp","kind":"item","name":"read_other","form":"declaration"}
void read_other(const Foo & other) const noexcept;
@@end

@@expect {"file":"Foo.hpp","kind":"item","name":"update_other","form":"declaration"}
void update_other(Foo & other) noexcept;
@@end
