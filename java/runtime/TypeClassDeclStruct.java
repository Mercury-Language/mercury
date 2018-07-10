// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2004 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

// This corresponds to the C typedef "MR_TypeClassDeclStruct"
// in runtime/mercury_types.h, i.e. the C struct
// "struct MR_TypeClassDecl_Struct" in runtime/mercury_typeclass_info.h.

public class TypeClassDeclStruct implements java.io.Serializable {
    public TypeClassId          tc_decl_id;
    public int                  tc_decl_version_number;
    public int                  tc_decl_num_supers; // redundant
    public TypeClassConstraint  tc_decl_supers;

    public TypeClassDeclStruct()
    {
    }

    public void init(TypeClassId id,
        int version_number,
        int num_supers,
        TypeClassConstraint supers)
    {
        tc_decl_id = id;
        tc_decl_version_number = version_number;
        tc_decl_num_supers = num_supers;
        tc_decl_supers = supers;
    }
}
