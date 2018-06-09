// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002-2005, 2007 The University of Melbourne.
// Copyright (C) 2014, 2016-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_construct.c
//
// This file provides utility functions for constructing terms, for use by
// the standard library.

#include "mercury_conf.h"
#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif
#include "mercury_type_info.h"
#include "mercury_construct.h"
#include "mercury_univ.h"
#include "mercury_misc.h"   // for MR_fatal_error()

static  int  MR_get_functor_info(MR_TypeInfo type_info, int functor_number,
                MR_Construct_Info *construct_info);

// MR_get_functor_info:
//
// Extract the information for functor number `functor_number',
// for the type represented by type_info.
// We succeed if the type is some sort of discriminated union.
//
// You need to save and restore transient registers around
// calls to this function.

static int
MR_get_functor_info(MR_TypeInfo type_info, int functor_number,
    MR_Construct_Info *construct_info)
{
    MR_TypeCtorInfo     type_ctor_info;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    construct_info->type_ctor_rep = MR_type_ctor_rep(type_ctor_info);

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error("MR_get_functor_info: term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {

    case MR_TYPECTOR_REP_DU:
    case MR_TYPECTOR_REP_DU_USEREQ:
        {
            const MR_DuFunctorDesc    *functor_desc;

            if (functor_number < 0 ||
                functor_number >= MR_type_ctor_num_functors(type_ctor_info))
            {
                MR_fatal_error("MR_get_functor_info: "
                    "du functor_number out of range");
            }

            functor_desc = MR_type_ctor_functors(type_ctor_info).
                MR_functors_du[functor_number];
            construct_info->functor_info.du_functor_desc = functor_desc;
            construct_info->functor_name = functor_desc->MR_du_functor_name;
            construct_info->arity = functor_desc->MR_du_functor_orig_arity;
            construct_info->arg_pseudo_type_infos =
                functor_desc->MR_du_functor_arg_types;
            construct_info->arg_names =
                functor_desc->MR_du_functor_arg_names;
        }
        return MR_TRUE;

    case MR_TYPECTOR_REP_ENUM:
    case MR_TYPECTOR_REP_ENUM_USEREQ:
    case MR_TYPECTOR_REP_DUMMY:
        {
            const MR_EnumFunctorDesc  *functor_desc;

            if (functor_number < 0 ||
                functor_number >= MR_type_ctor_num_functors(type_ctor_info))
            {
                MR_fatal_error("MR_get_functor_info: "
                    "enum functor_number out of range");
            }

            functor_desc = MR_type_ctor_functors(type_ctor_info).
                MR_functors_enum[functor_number];
            construct_info->functor_info.enum_functor_desc = functor_desc;
            construct_info->functor_name = functor_desc->MR_enum_functor_name;
            construct_info->arity = 0;
            construct_info->arg_pseudo_type_infos = NULL;
            construct_info->arg_names = NULL;
        }
        return MR_TRUE;

    case MR_TYPECTOR_REP_FOREIGN_ENUM:
    case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
        {
            const MR_ForeignEnumFunctorDesc  *functor_desc;

            if (functor_number < 0 ||
                functor_number >= MR_type_ctor_num_functors(type_ctor_info))
            {
                MR_fatal_error("MR_get_functor_info: "
                    "foreign enum functor_number out of range");
            }
            functor_desc = MR_type_ctor_functors(type_ctor_info).
                MR_functors_foreign_enum[functor_number];
            construct_info->functor_info.foreign_enum_functor_desc
                = functor_desc;
            construct_info->functor_name =
                functor_desc->MR_foreign_enum_functor_name;
            construct_info->arity = 0;
            construct_info->arg_pseudo_type_infos = NULL;
            construct_info->arg_names = NULL;
        }
        return MR_TRUE;

    case MR_TYPECTOR_REP_NOTAG:
    case MR_TYPECTOR_REP_NOTAG_USEREQ:
    case MR_TYPECTOR_REP_NOTAG_GROUND:
    case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        {
            const MR_NotagFunctorDesc *functor_desc;

            if (functor_number != 0) {
                MR_fatal_error("MR_get_functor_info: "
                    "notag functor_number out of range");
            }

            functor_desc = MR_type_ctor_functors(type_ctor_info).
                MR_functors_notag;
            construct_info->functor_info.notag_functor_desc = functor_desc;
            construct_info->functor_name = functor_desc->MR_notag_functor_name;
            construct_info->arity = 1;
            construct_info->arg_pseudo_type_infos =
                &functor_desc->MR_notag_functor_arg_type;
            construct_info->arg_names =
                &functor_desc->MR_notag_functor_arg_name;
        }
        return MR_TRUE;

    case MR_TYPECTOR_REP_EQUIV_GROUND:
    case MR_TYPECTOR_REP_EQUIV:
        return MR_get_functor_info(
            MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv),
            functor_number, construct_info);

    case MR_TYPECTOR_REP_TUPLE:
        construct_info->functor_name = "{}";
        construct_info->arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);

        // Tuple types don't have pseudo-type_infos for the functors.
        construct_info->arg_pseudo_type_infos = NULL;
        construct_info->arg_names = NULL;
        return MR_TRUE;

    case MR_TYPECTOR_REP_INT:
    case MR_TYPECTOR_REP_UINT:
    case MR_TYPECTOR_REP_INT8:
    case MR_TYPECTOR_REP_UINT8:
    case MR_TYPECTOR_REP_INT16:
    case MR_TYPECTOR_REP_UINT16:
    case MR_TYPECTOR_REP_INT32:
    case MR_TYPECTOR_REP_UINT32:
    case MR_TYPECTOR_REP_INT64:
    case MR_TYPECTOR_REP_UINT64:
    case MR_TYPECTOR_REP_CHAR:
    case MR_TYPECTOR_REP_FLOAT:
    case MR_TYPECTOR_REP_STRING:
    case MR_TYPECTOR_REP_BITMAP:
    case MR_TYPECTOR_REP_FUNC:
    case MR_TYPECTOR_REP_PRED:
    case MR_TYPECTOR_REP_SUBGOAL:
    case MR_TYPECTOR_REP_VOID:
    case MR_TYPECTOR_REP_C_POINTER:
    case MR_TYPECTOR_REP_STABLE_C_POINTER:
    case MR_TYPECTOR_REP_TYPEINFO:
    case MR_TYPECTOR_REP_TYPECTORINFO:
    case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
    case MR_TYPECTOR_REP_TYPEDESC:
    case MR_TYPECTOR_REP_TYPECTORDESC:
    case MR_TYPECTOR_REP_TYPECLASSINFO:
    case MR_TYPECTOR_REP_BASETYPECLASSINFO:
    case MR_TYPECTOR_REP_ARRAY:
    case MR_TYPECTOR_REP_SUCCIP:
    case MR_TYPECTOR_REP_HP:
    case MR_TYPECTOR_REP_CURFR:
    case MR_TYPECTOR_REP_MAXFR:
    case MR_TYPECTOR_REP_REDOFR:
    case MR_TYPECTOR_REP_REDOIP:
    case MR_TYPECTOR_REP_TRAIL_PTR:
    case MR_TYPECTOR_REP_TICKET:
    case MR_TYPECTOR_REP_FOREIGN:
    case MR_TYPECTOR_REP_STABLE_FOREIGN:
    case MR_TYPECTOR_REP_REFERENCE:
        return MR_FALSE;

    case MR_TYPECTOR_REP_UNUSED1:
    case MR_TYPECTOR_REP_UNUSED2:
    case MR_TYPECTOR_REP_UNKNOWN:
        MR_fatal_error("MR_get_functor_info: unknown type_ctor_rep");
    }

    MR_fatal_error("MR_get_functor_info: unexpected fallthrough");
}

// MR_typecheck_arguments:
//
// Given a list of univs (`arg_list'), and a vector of type_infos
// (`arg_vector'), checks that they are all of the same type;
// if so, returns MR_TRUE, otherwise returns MR_FALSE;
// `arg_vector' may contain type variables, these will be filled in
// by the type arguments of `type_info'.
//
// Assumes the length of the list has already been checked.
//
// You need to save and restore transient registers around
// calls to this function.

MR_bool
MR_typecheck_arguments(MR_TypeInfo type_info, int arity, MR_Word arg_list,
    const MR_PseudoTypeInfo *arg_pseudo_type_infos)
{
    MR_TypeInfo     arg_type_info;
    MR_TypeInfo     list_arg_type_info;
    int             comp;
    int             i;

    // Type check the list of arguments.

    for (i = 0; i < arity; i++) {
        if (MR_list_is_empty(arg_list)) {
            return MR_FALSE;
        }

        list_arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
            MR_list_head(arg_list), MR_UNIV_OFFSET_FOR_TYPEINFO);

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
                MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            arg_type_info =
                MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info)[i + 1];
        } else {
            arg_type_info = MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                arg_pseudo_type_infos[i]);
        }

        comp = MR_compare_type_info(list_arg_type_info, arg_type_info);
        if (comp != MR_COMPARE_EQUAL) {
            return MR_FALSE;
        }
        arg_list = MR_list_tail(arg_list);
    }

    // List should now be empty.
    return MR_list_is_empty(arg_list);
}

// MR_get_functors_check_range:
//
// Check that functor_number is in range, and get the functor info if it is.
// Return MR_FALSE if it is out of range, or if MR_get_functor_info returns
// MR_FALSE, otherwise return MR_TRUE.
//
// You need to save and restore transient registers around
// calls to this function.

MR_bool
MR_get_functors_check_range(int functor_number, MR_TypeInfo type_info,
    MR_Construct_Info *construct_info)
{
    // Check range of functor_number, get functors vector.
    return functor_number < MR_get_num_functors(type_info) &&
        functor_number >= 0 &&
        MR_get_functor_info(type_info, functor_number, construct_info);
}

// MR_get_num_functors:
//
// Get the number of functors for a type. If it isn't a discriminated union,
// return -1.
//
// You need to save and restore transient registers around
// calls to this function.

int
MR_get_num_functors(MR_TypeInfo type_info)
{
    MR_TypeCtorInfo type_ctor_info;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error("MR_get_num_functors: term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
        case MR_TYPECTOR_REP_DUMMY:
        case MR_TYPECTOR_REP_FOREIGN_ENUM:
        case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
            return MR_type_ctor_num_functors(type_ctor_info);

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        case MR_TYPECTOR_REP_TUPLE:
            return 1;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
        case MR_TYPECTOR_REP_EQUIV:
            return MR_get_num_functors(
                MR_create_type_info((MR_TypeInfo *) type_info,
                    MR_type_ctor_layout(type_ctor_info).MR_layout_equiv));

        case MR_TYPECTOR_REP_INT:
        case MR_TYPECTOR_REP_UINT:
        case MR_TYPECTOR_REP_INT8:
        case MR_TYPECTOR_REP_UINT8:
        case MR_TYPECTOR_REP_INT16:
        case MR_TYPECTOR_REP_UINT16:
        case MR_TYPECTOR_REP_INT32:
        case MR_TYPECTOR_REP_UINT32:
        case MR_TYPECTOR_REP_INT64:
        case MR_TYPECTOR_REP_UINT64:
        case MR_TYPECTOR_REP_CHAR:
        case MR_TYPECTOR_REP_FLOAT:
        case MR_TYPECTOR_REP_STRING:
        case MR_TYPECTOR_REP_BITMAP:
        case MR_TYPECTOR_REP_FUNC:
        case MR_TYPECTOR_REP_PRED:
        case MR_TYPECTOR_REP_SUBGOAL:
        case MR_TYPECTOR_REP_VOID:
        case MR_TYPECTOR_REP_C_POINTER:
        case MR_TYPECTOR_REP_STABLE_C_POINTER:
        case MR_TYPECTOR_REP_TYPEINFO:
        case MR_TYPECTOR_REP_TYPECTORINFO:
        case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
        case MR_TYPECTOR_REP_TYPEDESC:
        case MR_TYPECTOR_REP_TYPECTORDESC:
        case MR_TYPECTOR_REP_TYPECLASSINFO:
        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        case MR_TYPECTOR_REP_ARRAY:
        case MR_TYPECTOR_REP_SUCCIP:
        case MR_TYPECTOR_REP_HP:
        case MR_TYPECTOR_REP_CURFR:
        case MR_TYPECTOR_REP_MAXFR:
        case MR_TYPECTOR_REP_REDOFR:
        case MR_TYPECTOR_REP_REDOIP:
        case MR_TYPECTOR_REP_TRAIL_PTR:
        case MR_TYPECTOR_REP_TICKET:
        case MR_TYPECTOR_REP_FOREIGN:
        case MR_TYPECTOR_REP_STABLE_FOREIGN:
        case MR_TYPECTOR_REP_REFERENCE:
            return -1;

        case MR_TYPECTOR_REP_UNUSED1:
        case MR_TYPECTOR_REP_UNUSED2:
        case MR_TYPECTOR_REP_UNKNOWN:
            MR_fatal_error("MR_get_num_functors: unknown type_ctor_rep");
    }

    MR_fatal_error("MR_get_num_functors: unexpected fallthrough");
}
