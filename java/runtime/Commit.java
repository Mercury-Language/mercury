// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2002 The University of Melbourne.
// Copyright (C) 2017-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
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

