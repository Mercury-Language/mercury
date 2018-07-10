// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2011 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class DuArgLocn implements java.io.Serializable {

    public int arg_offset;
    public int arg_shift;
    public int arg_bits;

    public DuArgLocn(int arg_offset, int arg_shift, int arg_bits)
    {
        this.arg_offset = arg_offset;
        this.arg_shift = arg_shift;
        this.arg_bits = arg_bits;
    }
}
