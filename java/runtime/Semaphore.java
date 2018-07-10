// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2014, 2018 The Mercury Team
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

import java.util.concurrent.TimeUnit;


public class Semaphore
    extends java.util.concurrent.Semaphore
{
    public Semaphore(int permits, boolean fair)
    {
        super(permits, fair);
    }

    public Semaphore(int permits)
    {
        super(permits);
    }

    public void acquire()
        throws InterruptedException
    {
        acquire(1);
    }

    public void acquire(int permits)
        throws InterruptedException
    {
        boolean blocked = false;

        try {
            if (tryAcquire(permits, 0, TimeUnit.SECONDS)) {
                return;
            } else {
                // This thread will (probably) block.
                blocked();
                blocked = true;
                super.acquire(permits);
            }
        } finally {
            if (blocked) {
                running();
                // The thread isn't blocked anymore.
            }
        }
    }

    public void acquireUninterruptibly()
    {
        acquireUninterruptibly(1);
    }

    public void acquireUninterruptibly(int permits)
    {
        boolean interrupted_once = false;
        boolean keep_trying;
        boolean success;

        // Avoid a warning with the loop below.
        success = false;

        // tryAcquire is interruptable so we keep trying until we have
        // executed it at least once and it was not interrupted. We also track
        // if we were interrupted, so we can raise this condition again.
        do {
            keep_trying = true;
            try {
                success = tryAcquire(permits, 0, TimeUnit.SECONDS);
                keep_trying = false;
            } catch (InterruptedException e) {
                interrupted_once = true;
            }
        } while (keep_trying);

        if (!success) {
            // This thread will (probably) block because tryAcquire failed.
            blocked();
            super.acquireUninterruptibly(permits);
            running();
        }

        if (interrupted_once) {
            // Because this was supposed to be uninterruptable,
            // we need to raise the interrupt that we received earlier.
            Thread.currentThread().interrupt();
        }
    }

    public boolean tryAcquire(long timeout, TimeUnit unit)
        throws InterruptedException
    {
        return tryAcquire(1, timeout, unit);
    }

    public boolean tryAcquire(int permits, long timeout, TimeUnit unit)
        throws InterruptedException
    {
        if (timeout < 1) {
            return super.tryAcquire(permits, timeout, unit);
        } else {
            boolean result;

            result = tryAcquire(permits, 0, unit);
            if (result) {
                // Blocking wasn't necessary.
                return true;
            } else {
                // Blocking required, notify thread.
                blocked();
                try {
                    // Block.
                    return tryAcquire(permits, timeout, unit);
                } finally {
                    running();
                }
            }
        }
    }

    protected void blocked()
    {
        MercuryThread   mer_thread;

        mer_thread = MercuryThread.currentThread();
        if (mer_thread != null) {
            mer_thread.blocked();
        }
    }

    protected void running()
    {
        MercuryThread   mer_thread;

        mer_thread = MercuryThread.currentThread();
        if (mer_thread != null) {
            mer_thread.running();
        }
    }
}
