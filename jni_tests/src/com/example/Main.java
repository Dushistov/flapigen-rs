package com.example;

import com.example.Foo;
import com.example.Boo;
import com.example.TestPathAndResult;

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
	final int res = foo.calcF(1, 2);
	assert res == 8;
        System.out.println("res: " + Integer.toString(res));
        final double resf = foo.f_double(1.0, 1.0);
        assert Math.abs(Math.hypot(1.0, 1.0) + 5.0 - resf) < 1e-10;
        System.out.println("resf: " + Double.toString(resf));
        assert foo.getName().equals(FOO_NAME);
        System.out.println("name from java: " + foo.getName());
        foo = null;
        System.gc();
        Boo boo = new Boo();
        System.out.println("E: " + Float.toString(boo.test(true)));
        assert Math.abs((float )Math.E - boo.test(true)) < 1e-10;
        assert Math.abs((float )Math.PI - boo.test(false)) < 1e-10;
        boo = null;
        System.gc();
        boolean haveException = false;
        try {
            TestPathAndResult tpr1 = new TestPathAndResult();
        } catch (Exception ex) {
            System.out.println("Have exception: " + ex);
            haveException = true;
        }
        assert haveException;
        System.gc();

        haveException = false;
        TestPathAndResult tpr2 = null;
        try {
            tpr2 = new TestPathAndResult("/tmp/a.txt");
        } catch (Exception ex) {
            System.out.println("Have exception: " + ex);
            haveException = true;
        }
        assert !haveException;
        assert tpr2.getPath().equals("/tmp/a.txt");
        Boo booMember = tpr2.getBoo();
        assert booMember.getA() == 17;
        booMember.setA(18);
        assert booMember.getA() == 18;
        tpr2 = null;
        System.gc();

        System.out.println("ALL tests PASSED");
    }
}
