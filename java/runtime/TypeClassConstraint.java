// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2004 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

// This corresponds to the C type MR_TypeClassConstraint
// in runtime/mercury_type_info.h.

public class TypeClassConstraint implements java.io.Serializable {
    public TypeClassDeclStruct  tc_constr_type_class;
    public PseudoTypeInfo       tc_constr_arg_ptis[];

    public TypeClassConstraint()
    {
    }

    public void init(TypeClassDeclStruct type_class,
        // XXX Object[] should be PseudoTypeInfo[],
        //     but mlds_to_java.m generates Object[] since
        //     init_array/1 doesn't give type info
        Object[] ptis)
    {
        tc_constr_type_class = type_class;
        tc_constr_arg_ptis = new PseudoTypeInfo[ptis.length];
        for (int i = 0; i < ptis.length; i++) {
            tc_constr_arg_ptis[i] = (PseudoTypeInfo) ptis[i];
        }
    }
}
