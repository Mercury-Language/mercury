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
     * Construct a new MercuryThread with the given ID and runnable.
     * @param name A string that identifies the type of thread.
     * @param id A numeric identifier (should be unique).
     */
    public MercuryThread(String name, int id)
    {
        super(name + " " + id);
        this.id = id;
    }

}

