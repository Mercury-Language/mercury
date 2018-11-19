// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2011 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class DuArgLocn implements java.io.Serializable {

    public short arg_offset;
    public byte arg_shift;
    public byte arg_bits;

    public DuArgLocn(short arg_offset, byte arg_shift, byte arg_bits)
    {
        this.arg_offset = arg_offset;
        this.arg_shift = arg_shift;
        this.arg_bits = arg_bits;
    }
}
