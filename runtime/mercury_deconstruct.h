/*
** vim:ts=4 sw=4 expandtab
*/

/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_deconstruct.h
**
** This file declares utility functions for deconstructing terms,
** for use by the standard library and the debugger.
*/

#ifndef MERCURY_DECONSTRUCT_H
#define MERCURY_DECONSTRUCT_H

#include "mercury_imp.h"
#include <stdio.h>

typedef struct {
    int                     num_extra_args;
    MR_Word                 *arg_values;
    MR_TypeInfo             *arg_type_infos;
    bool                    can_free_arg_type_infos;
} MR_Expand_Args_Fields;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    MR_ConstString          functor;
    MR_Expand_Args_Fields   args;
} MR_Expand_Functor_Args_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    MR_ConstString          functor;
    MR_Expand_Args_Fields   args;
    bool                    limit_reached;
} MR_Expand_Functor_Args_Limit_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    MR_ConstString          functor_only;
} MR_Expand_Functor_Only_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    MR_Expand_Args_Fields   args_only;
} MR_Expand_Args_Only_Info;

typedef struct {
    bool                    non_canonical_type;
    int                     arity;
    bool                    chosen_index_exists;
    MR_Word                 *chosen_value_ptr;
    MR_TypeInfo             chosen_type_info;
} MR_Expand_Chosen_Arg_Only_Info;

extern  void    MR_expand_functor_args(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr,
                    MR_Expand_Functor_Args_Info *expand_info);

extern  void    MR_expand_functor_args_limit(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, int max_arity,
                    MR_Expand_Functor_Args_Limit_Info *expand_info);

extern  void    MR_expand_functor_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr,
                    MR_Expand_Functor_Only_Info *expand_info);

extern  void    MR_expand_args_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr,
                    MR_Expand_Args_Only_Info *expand_info);

extern  void    MR_expand_chosen_arg_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, int chosen,
                    MR_Expand_Chosen_Arg_Only_Info *expand_info);

extern  void    MR_expand_named_arg_only(MR_TypeInfo type_info,
                    MR_Word *data_word_ptr, MR_ConstString chosen_name,
                    MR_Expand_Chosen_Arg_Only_Info *expand_info);

typedef enum {
    MR_ALLOW_NONCANONICAL,
    MR_FAIL_ON_NONCANONICAL,
    MR_ABORT_ON_NONCANONICAL
} MR_non_canon_handling;

                /*
                ** MR_arg() takes the address of a term, its type, and an
                ** argument position (the first argument being at position 1).
                ** If the given term has an argument at that position, MR_arg
                ** returns TRUE and fills in the locations pointed to by the
                ** argument_ptr and arg_type_info_ptr arguments with the value
                ** and type of the argument at the selected position.
                **
                ** The noncanon argument says how MR_arg should behave if the
                ** term being deconstructed is of a non-canonical type. The msg
                ** is for use if noncanon is MR_ABORT_ON_NONCANONICAL.
                */

extern  bool    MR_arg(MR_TypeInfo type_info, MR_Word *term, int arg_index,
                    MR_TypeInfo *arg_type_info_ptr, MR_Word **argument_ptr,
                    MR_non_canon_handling noncanon, MR_ConstString msg);

                /*
                ** MR_named_arg() is just like MR_arg, except the argument
                ** is selected by name, not by position.
                */

extern  bool    MR_named_arg(MR_TypeInfo type_info, MR_Word *term,
                    MR_ConstString arg_name, MR_TypeInfo *arg_type_info_ptr,
                    MR_Word **argument_ptr, MR_non_canon_handling noncanon,
                    MR_ConstString msg);

                /*
                ** MR_named_arg_num() takes the address of a term, its type,
                ** and an argument name. If the given term has an argument
                ** with the given name, it succeeds and returns the argument
                ** number (counted starting from 0) of the argument; if it
                ** doesn't, it fails (i.e. returns FALSE).
                **
                ** You need to wrap MR_{save/restore}_transient_hp() around
                ** calls to this function.
                */

extern  bool    MR_named_arg_num(MR_TypeInfo type_info, MR_Word *term_ptr,
                    const char *arg_name, int *arg_num_ptr);

#endif /* MERCURY_DECONSTRUCT_H */
