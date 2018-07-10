// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2003-2004 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

// This corresponds to the C type MR_ReservedAddrFunctorDesc
// in runtime/mercury_type_info.h.

package jmercury.runtime;

public class ReservedAddrFunctorDesc implements java.io.Serializable {
    public java.lang.String     ra_functor_name;
    public int                  ra_ordinal;
    public java.lang.Object     ra_reserved_addr;

    public ReservedAddrFunctorDesc(java.lang.String functor_name,
        int ordinary,
        java.lang.Object reserved_addr)
    {
        ra_functor_name = functor_name;
        ra_ordinal = ordinary;
        ra_reserved_addr = reserved_addr;
    }
}
