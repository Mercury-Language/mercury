// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2002 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//
// This exception signals when an unreachable default case of of a switch
// statement is reached.
//

package jmercury.runtime;

public class UnreachableDefault extends java.lang.RuntimeException {

    public UnreachableDefault() {
        super();
    }

    public UnreachableDefault(String s) {
        super(s);
    }
}
