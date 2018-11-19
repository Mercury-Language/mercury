// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2003 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

// Corresponds to MR_DuExistLocn in runtime/mercury_type_info.h.

public class DuExistLocn implements java.io.Serializable {
    public short exist_arg_num;
    public short exist_offset_in_tci;

    public DuExistLocn(short arg_num, short offset_in_tci) {
        exist_arg_num = arg_num;
        exist_offset_in_tci = offset_in_tci;
    }
}
