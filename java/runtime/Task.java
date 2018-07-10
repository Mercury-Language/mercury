// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2014, 2018 The Mercury Team
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

/**
 * Task is a task being managed by MercuryThreadPool.
 * Callers can use this object to wait for the task's completion.
 */
public class Task implements Runnable
{
    private static long next_id;

    private long        id;
    private Runnable    target;
    private Status      status;

    public enum Status {
        NEW,
        SCHEDULED,
        RUNNING,
        FINISHED
    }

    /**
     * Create a new task.
     */
    public Task(Runnable target) {
        id = allocateTaskId();
        this.target = target;
        status = Status.NEW;
    }

    private static synchronized long allocateTaskId() {
        return next_id++;
    }

    public void run() {
        updateStatus(Status.RUNNING);
        target.run();
        updateStatus(Status.FINISHED);
    }

    public long getId() {
        return id;
    }

    public void scheduled() {
        updateStatus(Status.SCHEDULED);
    }

    /**
     * Update the task's status and notify any threads waiting on the
     * status change.
     */
    protected synchronized void updateStatus(Status status) {
        if (MercuryThreadPool.debug) {
            System.err.println("Thread: " + Thread.currentThread() +
                "task: " + this + " status " + status);
        }
        this.status = status;
        notifyAll();
    }

    /**
     * Wait for the task to complete.
     * This waits on the task's monitor. Callers should not be holding
     * any other monitors.
     */
    public synchronized void waitForTask()
        throws InterruptedException
    {
        while (status != Status.FINISHED) {
            wait();
        }
    }

    public String toString() {
        return "Task " + id;
    }
}
