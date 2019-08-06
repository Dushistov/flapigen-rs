package net.akaame.myapplication;

import android.content.Context;
import android.support.test.InstrumentationRegistry;
import android.support.test.runner.AndroidJUnit4;
import android.util.Log;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

/**
 * Instrumentation test, which will execute on an Android device.
 *
 * @see <a href="http://d.android.com/tools/testing">Testing documentation</a>
 */
@RunWith(AndroidJUnit4.class)
public class ExampleInstrumentedTest {
    private static final String TAG = ExampleInstrumentedTest.class.getSimpleName();

    @Before
    public void setUp() throws Exception {
        Log.d(TAG, "setUp");
        System.loadLibrary("mobcore");
    }
    @Test
    public void useAppContext() throws Exception {
        // Context of the app under test.
        Context appContext = InstrumentationRegistry.getTargetContext();

        assertEquals("net.akaame.myapplication", appContext.getPackageName());
    }
    @Test
    public void testSession() {
        Session session = new Session();
        assertEquals(5, session.add_and1(2));
    }
}
