// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2004 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class PredFunc {

    public static final int MR_PREDICATE = 0;
    public static final int MR_FUNCTION = 1;

    public int value;

    public PredFunc(int arg) {
        this.value = arg;
    }
}
