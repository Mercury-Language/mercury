// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2001-2004, 2009 The University of Melbourne.
// Copyright (C) 2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

// This corresponds to the C type "struct MR_TypeCtorInfo_Struct"
// in runtime/mercury_type_info.h.

public class TypeCtorInfo_Struct extends PseudoTypeInfo
    implements java.io.Serializable
{
    public int                  arity;
    public byte                 type_ctor_version;
    public byte                 type_ctor_num_ptags; // if DU
    public TypeCtorRep          type_ctor_rep;
    public MethodPtr            unify_pred;
    public MethodPtr            compare_pred;
    public java.lang.String     type_ctor_module_name;
    public java.lang.String     type_ctor_name;
    public TypeFunctors         type_functors;
    public TypeLayout           type_layout;
    public int                  type_ctor_num_functors;
    public short                type_ctor_flags;
    public int[]                type_functor_number_map;

    private final short MR_TYPE_CTOR_FLAG_LAYOUT_INDEXABLE = 0x8;

    public TypeCtorInfo_Struct()
    {
    }

    // Constructor for variable arity type_ctor_infos,
    // i.e. predicates, functions and tuples.
    public TypeCtorInfo_Struct(TypeCtorInfo_Struct other, int arity)
    {
        this.init(
            arity,
            other.type_ctor_version,
            other.type_ctor_num_ptags,
            other.type_ctor_rep.value,
            other.unify_pred,
            other.compare_pred,
            other.type_ctor_module_name,
            other.type_ctor_name,
            other.type_functors,
            other.type_layout,
            other.type_ctor_num_functors,
            other.type_ctor_flags,
            other.type_functor_number_map
        );
    }

    public void init(
        int type_arity,
        byte version,
        byte num_ptags,
        int rep,
        Object unify_proc,
        Object compare_proc,
        String module,
        String name,
        java.lang.Object name_ordered_functor_descs, // TypeFunctors
        java.lang.Object ordinal_ordered_functor_descs, // TypeLayout
        int num_functors,
        short flags,
        int[] functor_number_map)
    {
        arity = type_arity;
        type_ctor_version = version;
        type_ctor_num_ptags = num_ptags;
        type_ctor_rep = new TypeCtorRep(rep);
        unify_pred = (MethodPtr) unify_proc;
        compare_pred = (MethodPtr) compare_proc;
        type_ctor_module_name = module;
        type_ctor_name = name;
        type_functors = (TypeFunctors) name_ordered_functor_descs;
        type_layout = (TypeLayout) ordinal_ordered_functor_descs;
        type_ctor_num_functors = num_functors;
        type_ctor_flags = flags;
        type_functor_number_map = functor_number_map;
    }

    // XXX this should be renamed `equals'
    public boolean unify(TypeCtorInfo_Struct tci) {
        if (this == tci) {
            return true;
        }
        return type_ctor_module_name.equals(tci.type_ctor_module_name)
                && type_ctor_name.equals(tci.type_ctor_name)
                && arity == tci.arity;
    }

    // Return the functor ordinal for the given enum functor value,
    // or -1 if not found.
    public int index_or_search_enum_functor_ordinal(int enum_value) {
        if (flags_is_layout_indexable()) {
            return enum_value; // same as ordinal
        }

        final EnumFunctorDesc[] layout_enum = type_layout.layout_enum();
        for (int i = 0; i < layout_enum.length; i++) {
            if (layout_enum[i].enum_functor_value == enum_value) {
                return i;
            }
        }
        return -1;
    }

    // Return the ptag layout with the given ptag, or null if not found.
    public DuPtagLayout index_or_search_ptag_layout(byte ptag) {
        final DuPtagLayout[] layout_du = type_layout.layout_du();

        if (flags_is_layout_indexable()) {
            return layout_du[ptag];
        }

        for (DuPtagLayout ptag_layout : layout_du) {
            if (ptag_layout.du_ptag == ptag) {
                return ptag_layout;
            }
        }
        return null;
    }

    private boolean flags_is_layout_indexable() {
        return (type_ctor_flags & MR_TYPE_CTOR_FLAG_LAYOUT_INDEXABLE) != 0;
    }
}
