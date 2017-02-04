package com.example;

import com.example.Foo;

class Main {
    public static void main(String[] args) {
	try {
	    System.loadLibrary("test");
	} catch (UnsatisfiedLinkError e) {
	    System.out.println("Can not load library");
	    throw e;
	}
	Foo foo = new Foo(5, "Me is foo");
	int res = foo.f(1, 2);
	assert res == 8;
        System.out.println("res: " + Integer.toString(res));
    }
}
