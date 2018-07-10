// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2014, 2016, 2018 The Mercury Team
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

/**
 * Threads for the Mercury code running in Java.
 */
public class MercuryWorkerThread extends MercuryThread
{
    private MercuryThreadPool   pool;

    private ThreadStatus        status;

    /**
     * Construct a new MercuryThread with the given ID and runnable.
     * @param pool The Mercury thread pool.
     * @param id A numeric identifier (should be unique).
     */
    public MercuryWorkerThread(MercuryThreadPool pool, int id)
    {
        super("Mercury Worker Thread", id);
        this.pool = pool;
        this.status = ThreadStatus.OTHER;
    }

    /**
     * Run.
     * The worker thread executes tasks that it retrieves from the pool.
     */
    public void run()
    {
        Task task;

        try {
            do {
                task = null;
                try {
                    if (status != ThreadStatus.IDLE) {
                        setStatus(ThreadStatus.IDLE);
                    }
                    task = pool.workerGetTask();
                }
                catch (InterruptedException e) {
                    // A worker thread has no semantics for this,
                    // so we continue looping.
                    continue;
                }
                if (task != null) {
                    try {
                        setStatus(ThreadStatus.WORKING);
                        task.run();
                        pool.taskDone(task);
                    } catch (jmercury.runtime.Exception e) {
                        // The task threw a Mercury exception.
                        pool.taskFailed(task, e);
                        JavaInternal.reportUncaughtException(e);
                        // Make the thread exit after throwing an exception.
                        break;
                    } catch (Throwable e) {
                        // Some other error occured. bail out.
                        System.err.println("Uncaught exception: " +
                            e.toString());
                        System.err.println(e.getMessage());
                        e.printStackTrace();
                        System.exit(1);
                    } finally {
                        setStatus(ThreadStatus.OTHER);
                    }
                }
            } while (task != null);
        } finally {
            pool.threadShutdown(this, status);
        }
    }

    protected void setStatus(ThreadStatus new_status) {
        pool.updateThreadCounts(status, new_status);
        status = new_status;
    }

    public void blocked() {
        pool.updateThreadCounts(status, ThreadStatus.BLOCKED);
        status = ThreadStatus.BLOCKED;
    }

    public void running() {
        pool.updateThreadCounts(status, ThreadStatus.WORKING);
        status = ThreadStatus.WORKING;
    }
}
