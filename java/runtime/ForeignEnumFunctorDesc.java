// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2007 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class ForeignEnumFunctorDesc implements java.io.Serializable {

    public java.lang.String foreign_enum_functor_name;
    public int              foreign_enum_functor_ordinal;
    public int              foreign_enum_functor_value;

    public ForeignEnumFunctorDesc() {
    }

    public void init(String name, int ordinal, int value) {
        foreign_enum_functor_name = name;
        foreign_enum_functor_ordinal = ordinal;
        foreign_enum_functor_value = value;
    }
}
