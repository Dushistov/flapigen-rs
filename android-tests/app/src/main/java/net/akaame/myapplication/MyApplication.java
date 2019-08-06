package net.akaame.myapplication;

import android.app.Application;
import android.util.Log;

/**
 * Created by evgeniy on 16.03.17.
 */

public final class MyApplication extends Application {
    private static MyApplication sSelf;
    private Session mSession;
    private static final String TAG = "exm MyApplication";

    public MyApplication() {
        super();
        sSelf = this;
    }

    @Override
    public void onCreate() {
        Log.i(TAG, "onCreate");
        super.onCreate();
        try {
            System.loadLibrary("mobcore");
        } catch (UnsatisfiedLinkError e) {
            Log.e(TAG, "Load libary ERROR: " + e);
            return;
        }
        mSession = new Session();
    }

    public static MyApplication get() {
        return sSelf;
    }

    public Session getSession() {
        return mSession;
    }
}
