package com.example;

import java.util.Date;
import java.util.Calendar;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import com.example.rust.Foo;
import com.example.rust.Boo;
import com.example.rust.TestPathAndResult;
import com.example.rust.TestInner;
import com.example.rust.Xyz;
import com.example.rust.TestContainers;
import com.example.rust.TestIntArrays;
import com.example.rust.TestPassObjectsAsParams;
import com.example.rust.MyEnum;
import com.example.rust.TestEnumClass;
import com.example.rust.Observable;
import com.example.rust.MyObserver;

class Main {
    private static void testDoubleOverload() {
        new Xyz();
        new Xyz(1., 2., 3.);
    }

    public static void main(String[] args) {
        try {
            System.loadLibrary("rust_swig_test_jni");
        } catch (UnsatisfiedLinkError e) {
            System.out.println("Can not load library");
            throw e;
        }
	try {
	{
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
	}
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

	testDateTime();
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

        testDoubleOverload();

	{
	    TestContainers testContainers = new TestContainers();
	    Foo[] sv = testContainers.get_struct_vec();
	    assert sv.length == 2;
	    assert sv[0].getName().equals("1");
	    assert sv[0].calcF(0, 0) == 1;
	    assert sv[1].getName().equals("2");
	    assert sv[1].calcF(0, 0) == 2;
	    assert testContainers.get_empty_struct_vec().length == 0;
	    String[] strv = testContainers.get_string_vec();
	    assert strv.length == 7;
	    assert strv[0].equals("The");
	    assert strv[1].equals("was");
	    assert strv[2].equals("a");
	    assert strv[3].equals("young");
	    assert strv[4].equals("lady");
	    assert strv[5].equals("whose");
	    assert strv[6].equals("nose");
	}

	{
	    System.out.println("check null handling for String");
	    Foo foo = new Foo(17, null);
	    assert foo.calcF(0, 0) == 17;
	    assert foo.getName().equals("");
	}

	{
	    //	    TestIntArrays test = new TestIntArrays();
	    int[] arr1 = {1, 2, 3};
	    int[] arr2 = TestIntArrays.arr_pass_thorough(arr1);
	    int []empty = {};
	    assert Arrays.equals(arr1, arr2);
	    assert Arrays.equals(new TestIntArrays().get_ref(), empty);
	    int[] bigArray = new int[20000];
	    for (int i = 0; i < bigArray.length; ++i) {
		bigArray[i] = 17;
	    }
	    assert Arrays.equals(bigArray, new TestIntArrays(17, bigArray.length).get_ref());
	}
	testPassObjectsAsParams();
	testTestEnumClass();
	testCallbacks();
	} catch (java.lang.AssertionError ex) {
	    ex.printStackTrace();
	    throw ex;
	}
        System.out.println("ALL tests PASSED");
    }

    private static void testPassObjectsAsParams() {
	TestPassObjectsAsParams x = new TestPassObjectsAsParams();
	assert x.get_data() == 0;
	assert x.get_name().equals("");

	Foo foo1 = new Foo(5, "aaa");
	assert foo1.calcF(0, 0) == 5;
	assert foo1.getName().equals("aaa");
	x.f1(foo1);
	assert x.get_data() == 5;
	assert x.get_name().equals("aaa");
	assert foo1.calcF(0, 0) == 5;
	assert foo1.getName().equals("aaa");

	x.f1(new Foo(0, ""));
	assert x.get_data() == 0;
	assert x.get_name().equals("");
	assert foo1.calcF(0, 0) == 5;
	assert foo1.getName().equals("aaa");

	x.f2(foo1);
	assert x.get_data() == 5;
	assert x.get_name().equals("aaa");
	assert foo1.calcF(0, 0) == 0;
	assert foo1.getName().equals("");

	x.f1(new Foo(0, ""));
	Foo foo2 = new Foo(17, "bbb");
	x.f3(foo2);
	foo2 = null;
        System.gc();

	foo1 = new Foo(5, "aaa");
	assert TestPassObjectsAsParams.f4(foo1).equals("aaa5");
	assert foo1.calcF(0, 0) == 5;
	assert foo1.getName().equals("aaa");

	assert TestPassObjectsAsParams.f5(1, "ccc", foo1).equals("aaa5");
	assert foo1.calcF(0, 0) == 1;
	assert foo1.getName().equals("ccc");

	assert TestPassObjectsAsParams.f6(foo1).equals("ccc1");

	foo1 = new Foo(5, "aaa");
	TestPassObjectsAsParams y = new TestPassObjectsAsParams(foo1);
	assert y.get_data() == 5;
	assert y.get_name().equals("aaa");

	TestPassObjectsAsParams z = new TestPassObjectsAsParams(new Foo(7, "7"));
	assert z.get_data() == 7;
	assert z.get_name().equals("7");
	z.f_get_like_me(y);
	assert z.get_data() == 5;
	assert z.get_name().equals("aaa");

	z = TestPassObjectsAsParams.factory_method(13, "13");
	assert z.get_data() == 13;
	assert z.get_name().equals("13");
    }

    private static void testDateTime() {
        final Date now = Foo.now();
        final DateFormat df = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
        System.out.println("now: " + df.format(now));
        final Date today = Calendar.getInstance().getTime();
	System.out.println("now: " + now);
	System.out.println("today: " + today);
        assert (today.getTime() - now.getTime()) < 2000;
    }

    private static void testTestEnumClass() {
	MyEnum v1 = MyEnum.ITEM1;
	TestEnumClass o = new TestEnumClass();
	assert o.f1(v1) == -5;
	assert o.f1(MyEnum.ITEM2) == 17;
    }

    private static class TestObserver implements MyObserver {
	int x;
	public void onStateChanged(int x) {
	    System.out.println(String.format("TestObserver.onStateChange %d", x));
	    this.x = x;
	}
    }

    private static void testCallbacks() {
	Observable events = new Observable();
	TestObserver eventHandler = new TestObserver();
	assert eventHandler.x == 0;
	events.subscribe(eventHandler);
	for (int i = 0; i < 1000; ++i) {
	    events.change(i);
	    assert eventHandler.x == i;
	}
    }
}
