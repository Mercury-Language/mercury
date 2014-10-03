//
// Copyright (C) 2014 The Mercury Team 
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
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
     * The worker thread executes tasks that it retrives from the pool.
     */
    public void run()
    {
        Task task;

        do {
            task = null;
            try {
                if (status != ThreadStatus.IDLE) {
                    setStatus(ThreadStatus.IDLE);
                }
                task = pool.workerGetTask();
            }
            catch (InterruptedException e) {
                /*
                ** A worker thread has no semantics for this, so we continue
                ** looping.
                */
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
                } catch (Throwable e) {
                    // Some other error occured. bail out.
                    System.err.println("Uncaught exception: " + e.toString());
                    System.err.println(e.getMessage());
                    e.printStackTrace();
                    System.exit(1);
                } finally {
                    setStatus(ThreadStatus.OTHER);
                }
            }
        } while (task != null);

        pool.threadShutdown(this, status);
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

