// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2005, 2007, 2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_deconstruct.h
//
// This file declares utility functions for deconstructing terms,
// for use by the standard library and the debugger.

#ifndef MERCURY_DECONSTRUCT_H
#define MERCURY_DECONSTRUCT_H

#include "mercury_imp.h"
#include <stdio.h>

typedef struct {
    int                     num_extra_args;
    MR_Word                 *arg_values;
    const MR_DuArgLocn      *arg_locns;
    MR_TypeInfo             *arg_type_infos;
    MR_bool                 can_free_arg_type_infos;
} MR_ExpandArgsFields;

typedef struct {
    int                     arity;
    int                     functor_number;
    MR_ConstString          functor;
    MR_ExpandArgsFields     args;
} MR_ExpandFunctorArgsInfo;

typedef struct {
    int                     arity;
    int                     functor_number;
    MR_ConstString          functor;
    MR_ExpandArgsFields     args;
    MR_bool                 limit_reached;
} MR_ExpandFunctorArgsLimitInfo;

typedef struct {
    int                     arity;
    int                     functor_number;
    MR_ConstString          functor_only;
} MR_ExpandFunctorOnlyInfo;

typedef struct {
    int                     arity;
    MR_ExpandArgsFields     args_only;
} MR_ExpandArgsOnlyInfo;

typedef struct {
    int                     arity;
    MR_bool                 chosen_index_exists;
    MR_Word                 *chosen_value_ptr;
    const MR_DuArgLocn      *chosen_arg_locn;
    MR_TypeInfo             chosen_type_info;
} MR_ExpandChosenArgOnlyInfo;

// MR_NONCANON_ABORT asks that deconstructions of noncanonical types should
// cause a runtime abort.
//
// MR_NONCANON_ALLOW asks that deconstructions of noncanonical types should
// return a constant that indicates this fact.
//
// MR_NONCANON_CC asks that deconstruction of noncanonical types should
// deconstruct the term as if it were canonical. Since by definition,
// noncanonical types may have more than one representation for the same value,
// this requires the caller to be in a committed choice context.

typedef enum {
    MR_NONCANON_ABORT,
    MR_NONCANON_ALLOW,
    MR_NONCANON_CC
} MR_noncanon_handling;

extern  void    MR_expand_functor_args(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, MR_noncanon_handling noncanon,
                    MR_ExpandFunctorArgsInfo *expand_info);

extern  void    MR_expand_functor_args_limit(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, MR_noncanon_handling noncanon,
                    int max_arity,
                    MR_ExpandFunctorArgsLimitInfo *expand_info);

extern  void    MR_expand_functor_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, MR_noncanon_handling noncanon,
                    MR_ExpandFunctorOnlyInfo *expand_info);

extern  void    MR_expand_args_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, MR_noncanon_handling noncanon,
                    MR_ExpandArgsOnlyInfo *expand_info);

extern  void    MR_expand_chosen_arg_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, MR_noncanon_handling noncanon,
                    int chosen, MR_ExpandChosenArgOnlyInfo *expand_info);

extern  void    MR_expand_named_arg_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, MR_noncanon_handling noncanon,
                    MR_ConstString chosen_name,
                    MR_ExpandChosenArgOnlyInfo *expand_info);

// MR_arg() takes the address of a term, its type, and an
// argument position (the first argument being at position 1).
// If the given term has an argument at that position, MR_arg
// returns MR_TRUE and fills in the locations pointed to by the
// argument_ptr and arg_type_info_ptr arguments with the value
// and type of the argument at the selected position.
// If it doesn't, it fails (i.e. returns MR_FALSE).
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_arg(MR_TypeInfo type_info, MR_Word *term, int arg_index,
                    MR_TypeInfo *arg_type_info_ptr, MR_Word **argument_ptr,
                    const MR_DuArgLocn **arg_locn_ptr,
                    MR_noncanon_handling noncanon);

// MR_named_arg() is just like MR_arg, except the argument
// is selected by name, not by position.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_named_arg(MR_TypeInfo type_info, MR_Word *term,
                    MR_ConstString arg_name, MR_TypeInfo *arg_type_info_ptr,
                    MR_Word **argument_ptr,
                    const MR_DuArgLocn **arg_locn_ptr,
                    MR_noncanon_handling noncanon);

// MR_named_arg_num() takes the address of a term, its type, and an argument
// name. If the given term has an argument with the given name, it succeeds
// and returns the argument number (counted starting from 0) of the argument.
// If it doesn't, it fails (i.e. returns MR_FALSE).
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_named_arg_num(MR_TypeInfo type_info, MR_Word *term_ptr,
                    const char *arg_name, int *arg_num_ptr);

#define MR_arg_value(arg_ptr, arg_locn)                                 \
    ( ((arg_locn) == NULL || (arg_locn)->MR_arg_bits == 0)              \
    ? *(arg_ptr)                                                        \
    : MR_arg_value_uncommon(arg_ptr, arg_locn)                          \
    )

extern  MR_Word MR_arg_value_uncommon(MR_Word *arg_ptr,
                    const MR_DuArgLocn *arg_locn);

#endif // MERCURY_DECONSTRUCT_H
