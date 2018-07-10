// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2016, 2018 The Mercury Team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class FunctorSubtypeInfo implements java.io.Serializable {

    public int value;

    public FunctorSubtypeInfo(int arg) {
        this.value = arg;
    }
}
