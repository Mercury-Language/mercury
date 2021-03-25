// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2005, 2007 The University of Melbourne.
// Copyright (C) 2016, 2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_construct.h
//
// This module provides utility functions for constructing terms, for use by
// the standard library.

#ifndef MERCURY_CONSTRUCT_H
#define MERCURY_CONSTRUCT_H

#include "mercury_types.h"
#include "mercury_type_info.h"
#include "mercury_stack_layout.h"

union MR_Construct_Functor_Union {
    const MR_EnumFunctorDesc            *enum_functor_desc;
    const MR_ForeignEnumFunctorDesc     *foreign_enum_functor_desc;
    const MR_NotagFunctorDesc           *notag_functor_desc;
    const MR_DuFunctorDesc              *du_functor_desc;
};

typedef struct MR_Construct_Info_Struct {
    MR_ConstString                      functor_name;
    MR_Integer                          functor_ordinal;
    MR_Integer                          arity;
    const MR_PseudoTypeInfo             *arg_pseudo_type_infos;
    const MR_ConstString                *arg_names;
    MR_TypeCtorRep                      type_ctor_rep;
    union MR_Construct_Functor_Union    functor_info;
} MR_Construct_Info;

// MR_get_num_functors:
//
// Get the number of functors for a type. If it isn't a discriminated union,
// return -1.
//
// You need to save and restore transient registers around
// calls to this function.

extern  int     MR_get_num_functors(MR_TypeInfo type_info);

// MR_get_functors_check_range:
//
// Check that functor_number is in range, and get the functor info if it is.
// Return MR_FALSE if it is out of range, or if MR_get_functor_info returns
// MR_FALSE, otherwise return MR_TRUE.
//
// You need to save and restore transient registers around
// calls to this function.

extern  MR_bool MR_get_functors_check_range(int functor_number,
                    MR_TypeInfo type_info, MR_bool compute_ordinal,
                    MR_Construct_Info *construct_info);

// MR_typecheck_arguments:
//
// Given a list of univs (`arg_list'), and a vector of type_infos
// (`arg_vector'), checks that they are all of the same type; if so,
// returns MR_TRUE, otherwise returns MR_FALSE. `arg_vector' may contain
// type variables; these will be filled in by the type arguments
// of `type_info'.
//
// Assumes the length of the list has already been checked.
//
// You need to save and restore transient registers around
// calls to this function.

extern  MR_bool MR_typecheck_arguments(MR_TypeInfo type_info,
                    int arity, MR_Word arg_list,
                    const MR_PseudoTypeInfo *arg_pseudo_type_infos);

#endif  // MERCURY_CONSTRUCT_H
