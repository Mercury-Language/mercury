//
// Copyright (C) 2014, 2018 The Mercury Team
// This file is distributed under the terms specified in COPYING.LIB.
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

