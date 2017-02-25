package com.example;

import com.example.Foo;
import com.example.Boo;

class Main {
    public static void main(String[] args) {
	try {
	    System.loadLibrary("test");
	} catch (UnsatisfiedLinkError e) {
	    System.out.println("Can not load library");
	    throw e;
	}
        final String FOO_NAME = "Me is foo";
	Foo foo = new Foo(5, FOO_NAME);
	final int res = foo.f(1, 2);
	assert res == 8;
        System.out.println("res: " + Integer.toString(res));
        final double resf = foo.f_double(1.0, 1.0);
        assert Math.abs(Math.hypot(1.0, 1.0) + 5.0 - resf) < 1e-10;
        System.out.println("resf: " + Double.toString(resf));
        assert foo.name().equals(FOO_NAME);
        System.out.println("name from java: " + foo.name());
        foo = null;
        System.gc();
        Boo boo = new Boo();
        System.out.println("E: " + Float.toString(boo.test(true)));
        assert Math.abs((float )Math.E - boo.test(true)) < 1e-10;
        assert Math.abs((float )Math.PI - boo.test(false)) < 1e-10;
        boo = null;
        System.gc();
    }
}
