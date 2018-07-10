// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2004 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class DuExistInfo implements java.io.Serializable {

    public int exist_typeinfos_plain;
    public int exist_typeinfos_in_tci;
    public int exist_tcis;
    public /* final */ DuExistLocn[] exist_typeinfo_locns;
    public /* final */ TypeClassConstraint[] exist_constraints;

    public DuExistInfo()
    {
    }

    public void init(int typeinfos_plain,
        int typeinfos_in_tci,
        int tcis,
        DuExistLocn[] typeinfo_locns,
        TypeClassConstraint constraints[])
    {
        exist_typeinfos_plain = typeinfos_plain;
        exist_typeinfos_in_tci = typeinfos_in_tci;
        exist_tcis = tcis;
        exist_typeinfo_locns = typeinfo_locns;
        exist_constraints = constraints;
    }
}
