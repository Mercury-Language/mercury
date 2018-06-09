// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002-2004, 2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module exists to handle user-visible descriptions of types and type
// constructors.

#include "mercury_conf.h"
#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif
#include "mercury_type_info.h"
#include "mercury_type_desc.h"
#include "mercury_heap.h"       // for MR_offset_incr_hp_msg()
#include "mercury_misc.h"       // for MR_fatal_error()

MR_TypeCtorDesc
MR_make_type_ctor_desc(MR_TypeInfo type_info, MR_TypeCtorInfo type_ctor_info)
{
    MR_TypeCtorDesc type_ctor_desc;

    if (MR_TYPE_CTOR_INFO_IS_HO_PRED(type_ctor_info)) {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_PRED(
            MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info));
        if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
            MR_fatal_error("MR_make_type_ctor_desc - arity out of range.");
        }
    } else if (MR_TYPE_CTOR_INFO_IS_HO_FUNC(type_ctor_info)) {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FUNC(
            MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info));
        if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
            MR_fatal_error("MR_make_type_ctor_desc - arity out of range.");
        }
    } else if (MR_TYPE_CTOR_INFO_IS_TUPLE(type_ctor_info)) {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_TUPLE(
            MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info));
        if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
            MR_fatal_error("MR_make_type_ctor_desc - arity out of range.");
        }
    } else {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FIXED_ARITY(type_ctor_info);
    }

    return type_ctor_desc;
}

MR_TypeCtorDesc
MR_make_type_ctor_desc_pseudo(MR_PseudoTypeInfo pseudo,
    MR_TypeCtorInfo type_ctor_info)
{
    MR_TypeCtorDesc type_ctor_desc;

    if (MR_TYPE_CTOR_INFO_IS_HO_PRED(type_ctor_info)) {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_PRED(
            MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pseudo));
        if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
            MR_fatal_error("MR_make_type_ctor_desc - arity out of range.");
        }
    } else if (MR_TYPE_CTOR_INFO_IS_HO_FUNC(type_ctor_info)) {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FUNC(
            MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pseudo));
        if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
            MR_fatal_error("MR_make_type_ctor_desc - arity out of range.");
        }
    } else if (MR_TYPE_CTOR_INFO_IS_TUPLE(type_ctor_info)) {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_TUPLE(
            MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(pseudo));
        if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
            MR_fatal_error("MR_make_type_ctor_desc - arity out of range.");
        }
    } else {
        type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FIXED_ARITY(type_ctor_info);
    }

    return type_ctor_desc;
}

void
MR_type_ctor_and_args(MR_TypeInfo type_info, MR_bool collapse_equivalences,
    MR_TypeCtorDesc *type_ctor_desc_ptr, MR_Word *arg_type_info_list_ptr)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_TypeCtorDesc type_ctor_desc;
    MR_Integer      arity;

    if (collapse_equivalences) {
        type_info = MR_collapse_equivalences(type_info);
    }

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    type_ctor_desc = MR_make_type_ctor_desc(type_info, type_ctor_info);
    *type_ctor_desc_ptr = type_ctor_desc;

    if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
        arity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
        *arg_type_info_list_ptr = MR_type_params_vector_to_list(arity,
            MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info));
    } else {
        arity = type_ctor_info->MR_type_ctor_arity;
        *arg_type_info_list_ptr = MR_type_params_vector_to_list(arity,
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info));
    }
}

MR_bool
MR_pseudo_type_ctor_and_args(MR_PseudoTypeInfo pseudo_type_info,
    MR_bool collapse_equivalences, MR_TypeCtorDesc *type_ctor_desc_ptr,
    MR_Word *arg_type_info_list_ptr)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_TypeCtorDesc type_ctor_desc;
    MR_Integer      arity;

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info)) {
        return MR_FALSE;
    }

    if (collapse_equivalences) {
        pseudo_type_info = MR_collapse_equivalences_pseudo(pseudo_type_info);
    }

    if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info)) {
        return MR_FALSE;
    }

    type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pseudo_type_info);
    type_ctor_desc = MR_make_type_ctor_desc_pseudo(pseudo_type_info,
        type_ctor_info);
    *type_ctor_desc_ptr = type_ctor_desc;

    if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
        arity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
        *arg_type_info_list_ptr = MR_pseudo_type_params_vector_to_list(arity,
            MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(pseudo_type_info));
    } else {
        arity = type_ctor_info->MR_type_ctor_arity;
        *arg_type_info_list_ptr = MR_pseudo_type_params_vector_to_list(arity,
            MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(pseudo_type_info));
    }

    return MR_TRUE;
}

MR_TypeInfo
MR_make_type(int arity, MR_TypeCtorDesc type_ctor_desc, MR_Word arg_types_list)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_Word         *new_type_info_arena;
    MR_Word         new_type_info_arena_word;
    MR_TypeInfo     *new_type_info_args;
    int             i;

    // We need to treat variable-arity types as a special case here.

    if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
        type_ctor_info = MR_TYPECTOR_DESC_GET_VA_TYPE_CTOR_INFO(
            type_ctor_desc);

        MR_restore_transient_registers();
        MR_offset_incr_hp_msg(new_type_info_arena_word, 0,
            MR_var_arity_type_info_size(arity),
            MR_ALLOC_SITE_TYPE_INFO, NULL);
        new_type_info_arena = (MR_Word *) new_type_info_arena_word;
        MR_save_transient_registers();
        MR_fill_in_var_arity_type_info(new_type_info_arena, type_ctor_info,
            arity, new_type_info_args);
    } else {
        type_ctor_info =
            MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(type_ctor_desc);

        if (arity == 0) {
            return (MR_TypeInfo) type_ctor_info;
        }

        MR_restore_transient_registers();
        MR_offset_incr_hp_msg(new_type_info_arena_word, 0,
            MR_fixed_arity_type_info_size(arity),
            MR_ALLOC_SITE_TYPE_INFO, NULL);
        new_type_info_arena = (MR_Word *) new_type_info_arena_word;
        MR_save_transient_registers();
        MR_fill_in_fixed_arity_type_info(new_type_info_arena, type_ctor_info,
            new_type_info_args);
    }

    for (i = 1; i <= arity; i++) {
        new_type_info_args[i] = (MR_TypeInfo) MR_list_head(arg_types_list);
        arg_types_list = MR_list_tail(arg_types_list);
    }

    return (MR_TypeInfo) new_type_info_arena;
}

int
MR_compare_type_ctor_desc(MR_TypeCtorDesc tcd1, MR_TypeCtorDesc tcd2)
{
    MR_TypeCtorInfo tci1;
    MR_TypeCtorInfo tci2;
    int             arity1;
    int             arity2;
    int             result;

    // We use this algorithm to get comparison results that are
    // consistent with MR_compare_type_ctor_info.

    tci1 = MR_TYPECTOR_DESC_GET_TYPE_CTOR_INFO(tcd1);
    tci2 = MR_TYPECTOR_DESC_GET_TYPE_CTOR_INFO(tcd2);

    result = MR_compare_type_ctor_info(tci1, tci2);
    if (result != MR_COMPARE_EQUAL) {
        return result;
    }

    if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(tcd1)) {
        // We already know that the two type_ctor_descs refer to
        // the same variable-arity type constructor, so they can differ
        // only in the arity.

        arity1 = MR_TYPECTOR_DESC_GET_VA_ARITY(tcd1);
        arity2 = MR_TYPECTOR_DESC_GET_VA_ARITY(tcd2);

        if (arity1 < arity2) {
            return MR_COMPARE_LESS;
        } else if (arity1 > arity2) {
            return MR_COMPARE_GREATER;
        } else {
            return MR_COMPARE_EQUAL;
        }
    } else {
        return result;
    }
}

MR_bool
MR_unify_type_ctor_desc(MR_TypeCtorDesc tcd1, MR_TypeCtorDesc tcd2)
{
    MR_TypeCtorInfo tci1;
    MR_TypeCtorInfo tci2;
    int             arity1;
    int             arity2;

    // We use this algorithm to get comparison results that are
    // consistent with MR_unify_type_ctor_info.

    tci1 = MR_TYPECTOR_DESC_GET_TYPE_CTOR_INFO(tcd1);
    tci2 = MR_TYPECTOR_DESC_GET_TYPE_CTOR_INFO(tcd2);

    if (! MR_unify_type_ctor_info(tci1, tci2)) {
        return MR_FALSE;
    }

    if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(tcd1)) {
        // We already know that the two type_ctor_descs refer to
        // the same variable-arity type constructor, so they can differ
        // only in the arity.

        arity1 = MR_TYPECTOR_DESC_GET_VA_ARITY(tcd1);
        arity2 = MR_TYPECTOR_DESC_GET_VA_ARITY(tcd2);

        if (arity1 == arity2) {
            return MR_TRUE;
        } else {
            return MR_FALSE;
        }
    } else {
        return MR_TRUE;
    }
}
