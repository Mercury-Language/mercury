//
// Copyright (C) 2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
// This is a throwable class used for the Java implementation of commits.
// 

package jmercury.runtime;

public class Commit extends java.lang.Error {
    public Throwable fillInStackTrace() {
        // Do nothing.
        return this;
    }
}

