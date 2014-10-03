//
// Copyright (C) 2014 The Mercury Team 
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

/**
 * A thread with some Mercury specific support.
 */
public abstract class MercuryThread extends Thread
{
    private int                 id;

    /**
     * Construct a new MercuryThread with the given ID.
     * @param name A string that identifies the type of thread.
     * @param id A numeric identifier (should be unique).
     */
    public MercuryThread(String name, int id)
    {
        super(name + " " + id);
        this.id = id;
    }

    /**
     * The thread has become blocked.
     */
    public abstract void blocked();

    /**
     * The thread is unblocked and is now running again.
     */
    public abstract void running();

    /**
     * If the current thread is a MercuryThread then return a reference to
     * it.
     */
    public static MercuryThread currentThread()
    {
        Thread          thread;

        thread = Thread.currentThread();
        if (thread instanceof MercuryThread) {
            return (MercuryThread)thread;
        } else {
            return null;
        }
    }

}

