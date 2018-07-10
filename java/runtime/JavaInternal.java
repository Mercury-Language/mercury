// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2003, 2009 The University of Melbourne.
// Copyright (C) 2014, 2017-2018 The Mercury Team.
// This file is distributed under the terms specified in COPYING.LIB.
//
// All modifications to this file will require changes to:
// compiler/mlds_to_java.m

package jmercury.runtime;

/**
 * Internals and static objects for Mercury's runtime system on the Java
 * backend.
 * This class is used to store the main module's name (progname), command
 * line arguments and the exit status.  We can't put them in one of the
 * library modules because we need to hold them in a class variable in a top
 * level class.
 *
 * The class also contains utility methods and other objects such as a
 * reference to the thread pool.
 *
 * No instance of this class is ever created, all of its members and methods
 * are static.
 */
public class JavaInternal {

    /**
     * Private constructor.
     * This private constructor doesn't do anything and isn't called by
     * anyone.  It exists only to prevent people from creating an instance.
     */
    private JavaInternal() {
    }

    private static MercuryThreadPool    thread_pool = null;
    private static MercuryOptions       options = null;

    public static synchronized MercuryThreadPool getThreadPool() {
        if (thread_pool == null) {
            thread_pool = new MercuryThreadPool(
                getOptions().getNumProcessors());
        }
        return thread_pool;
    }

    public static synchronized MercuryOptions getOptions() {
        if (options == null) {
            options = new MercuryOptions();
            options.process();
        }
        return options;
    }

    public static java.lang.String      progname;
    public static java.lang.String[]    args;
    public static int                   exit_status;
    private static ExceptionReporter    exception_reporter;

    private static java.util.List<Runnable> finalisers
        = new java.util.ArrayList<Runnable>();

    public static void register_finaliser(Runnable hook) {
        finalisers.add(hook);
    }

    public static void run_finalisers() {
        for (Runnable r : finalisers) {
            r.run();
        }
    }

    /**
     * Run the main task.
     * The main task is executed by the thread pool so that when it blocks
     * the thread pool is notified correctly.
     */
    public static void runMain(Runnable main)
    {
        getThreadPool().runMain(main);
    }

    /**
     * Set the exception reporter if one is not set already.
     * The specified object will be used by reportUncaughtException to
     * report exceptions.  See the comment for reportUncaughtException()
     % @param reporter The new exception reporter object.
     */
    public static void setExceptionReporter(ExceptionReporter reporter)
    {
        if (null == exception_reporter) {
            exception_reporter = reporter;
        }
    }

    /**
     * Report uncaught exceptions.
     * This reports exceptions using the exception reporter object, if it is
     * set.  Otherwise we make a best effort at reporting it.
     *
     * Exception reporting code is written in exception.m which we cannot
     * link to from here.  When that code starts it should set an exception
     * reporter object.
     */
    public static void reportUncaughtException(jmercury.runtime.Exception e)
    {
        if (null != exception_reporter) {
            exception_reporter.reportUncaughtException(e);
        } else {
            System.out.flush();
            System.err.println("Uncaught exception: " + e.getMessage());
            System.err.flush();
        }

        if (shouldPrintStackTrace()) {
            e.printStackTrace(System.err);
        }
        if (exit_status == 0) {
            exit_status = 1;
        }
    }

    /**
     * Should we print Java stack traces when we catch Mercury exceptions?
     */
    public static boolean shouldPrintStackTrace() {
        return System.getenv("MERCURY_SUPPRESS_STACK_TRACE") == null;
    }

    /**
     * Interface for reporting exceptions.
     */
    public interface ExceptionReporter {
        public void reportUncaughtException(jmercury.runtime.Exception e);
    }
}
