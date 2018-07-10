// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2003 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class TypeLayout implements java.io.Serializable {
    // This should hold a value of one of the types
    // accessible by the access functions that follow.
    public java.lang.Object layout_init;

    // In runtime/mercury_type_info.h:
    // typedef MR_DuPtagLayout *MR_DuTypeLayout;
    // so here we just use DuPtagLayout[].
    public DuPtagLayout[] layout_du() {
        return (DuPtagLayout[]) layout_init;
    }

    // In runtime/mercury_type_info.h:
    // typedef MR_EnumFunctorDesc **EnumTypeLayout;
    // so here we just use EnumFunctorDesc[][]
    public EnumFunctorDesc[] layout_enum() {
        return (EnumFunctorDesc[]) layout_init;
    }

    // In runtime/mercury_type_info.h:
    // typedef MR_NotagFunctorDesc *MR_NotagTypeLayout;
    // so here we just us NotagFunctorDesc[]
    public NotagFunctorDesc[] layout_notag() {
        return (NotagFunctorDesc[]) layout_init;
    }
    // In runtime/mercury_type_info.h:
    // typedef MR_PseudoTypeInfo MR_EquivType;
    // so here we just use MR_PseudoTypeInfo
    public PseudoTypeInfo layout_equiv() {
        return (PseudoTypeInfo) layout_init;
    }

    public TypeLayout(java.lang.Object init) {
        layout_init = init;
    }
}
