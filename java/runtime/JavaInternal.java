//
// Copyright (C) 2001-2003, 2009 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
// All modifications to this file will require changes to:
// compiler/mlds_to_java.m

package jmercury.runtime;

/**
 * Internals for Mercury's runtime system on the Java backend.
 * At the moment this class is used to store the main module's name (progname),
 * command line arguments and the exit status.  We can't put them in one of the
 * library modules because we need to hold them in a class variable in a top
 * level class.
 *
 * The class also contains utility methods.
 */
public class JavaInternal {

    private static JavaInternal         instance;

    private JavaInternal() {
        options = new MercuryOptions();
        options.process();
        thread_pool = new MercuryThreadPool(options.getNumProcessors());
    }

    private MercuryThreadPool thread_pool;
    private MercuryOptions options;

    public static MercuryThreadPool getThreadPool() {
        return instance.thread_pool;
    }

    public static MercuryOptions getOptions() {
        return instance.options;
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
     * Run the main task using the thread pool.
     */
    public static void runMain(Runnable main)
    {
        instance = new JavaInternal();
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
