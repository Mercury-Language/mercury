//
// Copyright (C) 2014 The Mercury Team 
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package jmercury.runtime;

public enum ThreadStatus {
    WORKING,
    IDLE,
    BLOCKED,

    /*
     * A thread in any other state, currently the only possibility is after
     * a thread has been created but before checks the runqueue for work.
     */
    OTHER
}

