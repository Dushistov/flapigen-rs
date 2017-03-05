package com.example;

import java.util.Date;
import java.util.Calendar;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import com.example.Foo;
import com.example.Boo;
import com.example.TestPathAndResult;
import com.example.TestInner;

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
        assert Math.abs(Math.hypot(1.0, 1.0) - Foo.fHypot(1.0, 1.0)) < 1e-10;
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
        assert tpr2.javaFunc() == 17;
        {
            Boo []arrByHands = tpr2.testHandArrayReturn();
            assert arrByHands.length == 10;
            for (int i = 0; i < arrByHands.length; ++i) {
                System.out.println(String.format("getA(%d) == %d", i, arrByHands[i].getA()));
                assert arrByHands[i].getA() == i;
            }
        }
        {
            Foo []arr = tpr2.get_foo_list();
            assert arr.length == 10;
            for (int i = 0; i < arr.length; ++i) {
                assert arr[i].calcF(0, 0) == i;
                assert arr[i].getName().equals(String.format("foo arr: %d", i));
            }
        }
        tpr2 = null;
        System.gc();

        final Date now = Foo.now();
        final DateFormat df = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
        System.out.println("now: " + df.format(now));
        final Date today = Calendar.getInstance().getTime();
        assert (today.getTime() - now.getTime()) < 1000;

        TestInner.Inner testInner = TestInner.getInner();
        assert testInner.name.equals("Boo Boo");

        assert Boo.test_u8((short) 1) == (short) 2;
        assert Boo.test_i8((byte) -1) == (byte) 0;
        assert Boo.test_u16((int) 1) == (int) 2;
        assert Boo.test_i16((short) -1) == (short) 0;
        assert Boo.test_u32((long) 1) == (long) 2;
        assert Boo.test_i32((int) -1) == (int) 0;
        assert Boo.test_u64((long) 1) == (long) 2;
        assert Boo.test_i64((long) -1) == (long) 0;
        assert Math.abs(Boo.test_f32((float) 1.1) - (float) 2.1) < 1e-12;
        assert Math.abs(Boo.test_f64((double) -1.0)) < 1e-12;

        System.out.println("ALL tests PASSED");
    }
}
