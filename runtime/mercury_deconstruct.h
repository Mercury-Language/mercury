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
    int                     arity;
    int                     functor_number;
    MR_ConstString          functor;
    MR_Word                 arg_univs_list;
} MR_ExpandFunctorArgsInfo;

typedef struct {
    int                     arity;
    int                     functor_number;
    MR_ConstString          functor;
    MR_Word                 arg_univs_list;
    MR_bool                 limit_reached;
} MR_ExpandFunctorArgsLimitInfo;

typedef struct {
    int                     arity;
    int                     functor_number;
    MR_ConstString          functor_only;
} MR_ExpandFunctorOnlyInfo;

typedef struct {
    int                     arity;
    MR_Word                 arg_univs_list;
} MR_ExpandArgsOnlyInfo;

typedef struct {
    int                     arity;
    MR_bool                 chosen_index_exists;
    MR_TypeInfo             chosen_arg_type_info;
    MR_Word                 chosen_arg_term;
    MR_Word                 *chosen_arg_word_sized_ptr;
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

// The MR_expand_* functions do the heavy lifting in the implementation
// of the main predicates of library/deconstruct.m. Given a term and its type,
// they can find out and return the name and the arity of the term's top
// function symbol, a list of the values of all its arguments, or the value
// of just one argument, chosen by index or by field name. Each variant
// returns some subset of this information. The subset is given by the fields
// of the MR_Expand* type referred to by its last argument.

// XXX In each of these functions, the name of the argument that represents
// the whole term is
//
// - Term in deconstruct.m in the library,
// - term_ptr in mercury_deconstruct.c, and
// - data_word_ptr here and in mercury_ml_expand_body.h.
//
// It would be nice if the terminology was consistent.

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

// MR_arg() takes the address of a term, its type, and an argument position
// (the first argument being at position 1), as well as an indication
// of the desired `canonicality' of the decomposition.
//
// If the given term has an argument at the specified position, MR_arg returns
// MR_TRUE, and fills in *arg_type_info_ptr and *arg_term_ptr with the
// type_info and value of that argument at the selected position. It also
// fills in *word_sized_arg_ptr, with the address of the argument
// if the argument's size is exactly one word, or with NULL if the size
// is anything else (double word, subword, or nothing for dummies).
//
// If the given term does not have an argument at the specified position,
// MR_arg fails, i.e. it returns MR_FALSE.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_arg(MR_TypeInfo type_info, MR_Word *term,
                    MR_noncanon_handling noncanon, int arg_index,
                    MR_TypeInfo *arg_type_info_ptr, MR_Word *arg_term_ptr,
                    MR_Word **word_sized_arg_ptr);

// MR_named_arg() is just like MR_arg, except the argument is selected by name,
// not by position.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_named_arg(MR_TypeInfo type_info, MR_Word *term,
                    MR_noncanon_handling noncanon, MR_ConstString arg_name,
                    MR_TypeInfo *arg_type_info_ptr, MR_Word *arg_term_ptr,
                    MR_Word **word_sized_arg_ptr);

// MR_named_arg_num() takes the address of a term, its type, and an argument
// name. If the given term has an argument with the given name, it succeeds
// and returns the argument number (counted starting from 0) of the argument.
// If it doesn't, it fails, i.e. returns MR_FALSE.
//
// You need to wrap MR_{save/restore}_transient_hp() around
// calls to this function.

extern  MR_bool MR_named_arg_num(MR_TypeInfo type_info, MR_Word *term_ptr,
                    const char *arg_name, int *arg_num_ptr);

#endif // MERCURY_DECONSTRUCT_H
