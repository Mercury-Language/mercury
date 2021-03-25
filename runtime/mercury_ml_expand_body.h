// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2007, 2012 The University of Melbourne.
// Copyright (C) 2013, 2015-2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_ml_expand_body.h
//
// This file is included several times in runtime/mercury_deconstruct.c. Each
// inclusion defines the body of one of several variants of the old ML_expand
// function, which, given a data word and its type_info, returned its functor,
// arity, argument vector and a type_info vector describing its arguments.
// One variant still does all that. The others perform different subsets of
// this task. The reason for having those specialized variants is that
// executing the full task can be extremely time consuming, especially when
// large arrays are involved. (Simply allocating and filling in an array of
// a million typeinfos can cause a system to start paging.) Therefore we try to
// make sure that in every circumstance we perform the minimum work possible.
//
// The code including this file must define these macros:
//
// EXPAND_FUNCTION_NAME     Gives the name of the function being defined.
//
// EXPAND_TYPE_NAME         Gives the name of the type of the expand_info
//                          argument.
//
// The code including this file may define these macros:
//
// EXPAND_FUNCTOR_FIELD     If defined, gives the name of the field in the
//                          expand_info structure that contains the name of the
//                          functor. This field should be of type
//                          MR_ConstString. The function will fill in this
//                          field.
//
// EXPAND_ARGS              If defined, the expand_info structure should have
//                          a field named arg_univs_list whose type is MR_Word.
//                          This function will fill in this field with a
//                          Mercury list that contains one univ for each
//                          of the functor's arguments in order.
//
// EXPAND_CHOSEN_ARG        If defined, the function will have an extra
//                          argument, chosen, which specifies the position of
//                          the one desired argument (with the first argument
//                          having position 0), and the function will fill in
//                          the fields of the MR_ExpandChosenArgOnlyInfo
//                          structure.
//
// EXPAND_NAMED_ARG         If defined, the function will have an extra
//                          argument, chosen_name, which specifies the name
//                          of the one desired argument, and the function
//                          will fill in the fields of the
//                          MR_ExpandChosenArgOnlyInfo structure.
//
// EXPAND_APPLY_LIMIT       If defined, the function will have an extra
//                          argument, max_arity. If the number of arguments
//                          exceeds this limit, the function will store
//                          MR_TRUE in the limit_reached field of expand_info
//                          and will not fill in the other fields about the
//                          arguments.
//
//
// Most combinations are allowed, but
//
// - only one of EXPAND_ARGS, EXPAND_CHOSEN_ARG and EXPAND_NAMED_ARG
//   may be defined at once, and
// - EXPAND_APPLY_LIMIT should be defined only if EXPAND_ARGS is also defined.
//
// Each variant of the function will fill in all the fields of the expand_info
// structure passed to it, although the set of fields in that structure will
// be different for different variants. The type in EXPAND_TYPE_NAME must be
// consistent with the set of defined optional macros.
//
// All variants contain the integer field arity, which will be set to
// the number of arguments the functor has.
//
// Please note:
//
// These functions increment the heap pointer; however, on some platforms
// the register windows mean that transient Mercury registers may be lost.
// Before calling these functions, call MR_save_transient_registers(), and
// afterwards, call MR_restore_transient_registers().
//
// If you change this code, you may also have to reflect your changes
// in runtime/mercury_deep_copy_body.h and runtime/mercury_table_type_body.h.
//
// In several places, we call MR_fatal_error to signal inappropriate
// deconstruction of noncanonical terms. These should all throw exceptions
// instead, but it is not yet safe to throw exceptions across the C interface.

#include <stdio.h>
#include <inttypes.h>
#include "mercury_library_types.h" // for MR_ArrayType
#include "mercury_layout_util.h"   // for MR_materialize_closure_type_params
#include "mercury_ho_call.h"       // for MR_ClosureId etc

#ifdef MR_DEEP_PROFILING
  #include  "mercury_deep_profiling.h"
#endif

// set up for recursive calls
#ifdef  EXPAND_APPLY_LIMIT
  #define   EXTRA_ARG1  max_arity,
#else
  #define   EXTRA_ARG1
#endif
#ifdef  EXPAND_CHOSEN_ARG
  #define   EXTRA_ARG2  chosen,
#else
  #define   EXTRA_ARG2
#endif
#ifdef  EXPAND_NAMED_ARG
  #define   EXTRA_ARG3  chosen_name,
#else
  #define   EXTRA_ARG3
#endif
#define EXTRA_ARGS  EXTRA_ARG1 EXTRA_ARG2 EXTRA_ARG3

#if defined(EXPAND_CHOSEN_ARG) || defined(EXPAND_NAMED_ARG)
  #define   EXPAND_ONE_ARG
#else   // defined(EXPAND_CHOSEN_ARG) || defined(EXPAND_NAMED_ARG)
  #undef    EXPAND_ONE_ARG
#endif  // defined(EXPAND_CHOSEN_ARG) || defined(EXPAND_NAMED_ARG)

///////////////////

// Macros for setting the values of the fields in expand_infos that specify
// the top functor's name, its functor number (i.e. its position in the
// type's list of function symbols in declaration order), and its arity.

#ifdef  EXPAND_FUNCTOR_FIELD
  #define copy_and_handle_functor_name(name)                                \
        do {                                                                \
            MR_restore_transient_hp();                                      \
            MR_make_aligned_string_copy(expand_info->EXPAND_FUNCTOR_FIELD,  \
                name);                                                      \
            MR_save_transient_hp();                                         \
        } while (0)
  #define handle_functor_name(name)                                         \
        do {                                                                \
            MR_restore_transient_hp();                                      \
            MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD, name);\
            MR_save_transient_hp();                                         \
        } while (0)
  #define handle_noncanonical_type_ctor_name(tci)                           \
        do {                                                                \
            MR_ConstString  name;                                           \
                                                                            \
            name = MR_expand_type_name(tci, MR_TRUE);                       \
            MR_restore_transient_hp();                                      \
            MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD, name);\
            MR_save_transient_hp();                                         \
        } while (0)
  #define handle_type_ctor_name(tci)                                        \
        do {                                                                \
            MR_ConstString  name;                                           \
                                                                            \
            name = MR_expand_type_name(tci, MR_FALSE);                      \
            MR_restore_transient_hp();                                      \
            MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD, name);\
            MR_save_transient_hp();                                         \
        } while (0)
  #define handle_functor_number(num)                                        \
        do {                                                                \
            expand_info->functor_number = (num);                            \
        } while (0)
  #define handle_type_functor_number(tci, ordinal)                          \
        do {                                                                \
            expand_info->functor_number =                                   \
                (tci)->MR_type_ctor_functor_number_map[ordinal];            \
        } while (0)
#else   // EXPAND_FUNCTOR_FIELD
  #define copy_and_handle_functor_name(name)                                \
        ((void) 0)
  #define handle_functor_name(name)                                         \
        ((void) 0)
  #define handle_noncanonical_type_ctor_name(tci)                                     \
        ((void) 0)
  #define handle_type_ctor_name(tci)                                        \
        ((void) 0)
  #define handle_functor_number(num)                                        \
        ((void) 0)
  #define handle_type_functor_number(tci, ordinal)                          \
        ((void) 0)
#endif  // EXPAND_FUNCTOR_FIELD

#define handle_functor_name_number_arity(ei, tci, fdesc)                    \
        do {                                                                \
            handle_functor_name(fdesc->MR_du_functor_name);                 \
            handle_type_functor_number(tci, fdesc->MR_du_functor_ordinal);  \
            ei->arity = fdesc->MR_du_functor_orig_arity;                    \
        } while (0)

///////////////////

// Many type_ctor_reps represent (classes of) types in which *all*
// function symbols have arity zero. These macros set up the results
// for terms of such types to requests either for all arguments, or for
// one selected argument.

#ifdef  EXPAND_ARGS
  #define handle_zero_arity_all_args()                                      \
        do {                                                                \
            expand_info->arg_univs_list = MR_list_empty();                  \
        } while (0)
#else   // EXPAND_ARGS
  #define handle_zero_arity_all_args()                                      \
        ((void) 0)
#endif  // EXPAND_ARGS

#ifdef  EXPAND_ONE_ARG
  #define handle_zero_arity_one_arg()                                       \
        do {                                                                \
            expand_info->chosen_index_exists = MR_FALSE;           \
        } while (0)
#else   // EXPAND_ONE_ARG
  #define handle_zero_arity_one_arg()                                       \
        ((void) 0)
#endif  // EXPAND_ONE_ARG

#define handle_zero_arity_args()                                            \
        do {                                                                \
            expand_info->arity = 0;                                         \
            handle_zero_arity_all_args();                                   \
            handle_zero_arity_one_arg();                                    \
        } while (0)

///////////////////

// Return, in index, the argument number of an argument with the given
// field name. Leaves index unchanged if there is no argument with the
// given field name. (This is ok because we initialize the variable passed
// as index to -1, which means no match.)

#ifdef  EXPAND_NAMED_ARG
  #define set_chosen_for_arg_name(fdesc, arity, name, index)                \
        do {                                                                \
            if (fdesc->MR_du_functor_arg_names != NULL) {                   \
                int max = arity;                                            \
                int i;                                                      \
                                                                            \
                for (i = 0; i < max; i++) {                                 \
                    MR_ConstString name_i =                                 \
                        fdesc->MR_du_functor_arg_names[i];                  \
                    if (name_i != NULL && MR_streq(name_i, name)) {         \
                        index = i;                                          \
                        break;                                              \
                    }                                                       \
                }                                                           \
            }                                                               \
        } while (0)
#else
  #define set_chosen_for_arg_name(fdesc, arity, name, index)                \
        ((void) 0)
#endif  // EXPAND_NAMED_ARG

///////////////////

// If we are implementing the limited arity version of deconstruct
// and the current term is above the limit arity, say so and return.
// We rely on the default initialization of the limit_reached field
// to MR_FALSE If we are below the limit.

#ifdef  EXPAND_APPLY_LIMIT
  #define maybe_set_limit_reached_and_return(ei, max)                       \
        do {                                                                \
            if ((ei)->arity > max) {                                        \
                (ei)->limit_reached = MR_TRUE;                              \
                return;                                                     \
            }                                                               \
        } while (0)
#else
  #define maybe_set_limit_reached_and_return(ei, max)                       \
        ((void) 0)
#endif  // EXPAND_APPLY_LIMIT

///////////////////

// Fill the extra_args parameter with the number of type_infos and/or
// typeclass_infos polymorphism has inserted into the memory cell
// between the remote secondary tag and the argument values.

#if     defined(EXPAND_ARGS) || defined(EXPAND_ONE_ARG)
  #define set_exist_info_extra_args(fdesc, exist_info, extra_args)          \
        do {                                                                \
            exist_info = (fdesc)->MR_du_functor_exist_info;                 \
            if (exist_info != NULL) {                                       \
                extra_args =                                                \
                    exist_info->MR_exist_typeinfos_plain +                  \
                    exist_info->MR_exist_tcis;                              \
            } else {                                                        \
                extra_args = 0;                                             \
            }                                                               \
        } while (0)
#else
  #define set_exist_info_extra_args(fdesc, exist_info, extra_args)          \
        do {                                                                \
            exist_info = NULL;                                              \
            extra_args = 0;                                                 \
        } while (0)
#endif  // defined(EXPAND_ARGS) || defined(EXPAND_ONE_ARG)

// Assert that there are no such type_infos or typeclass_infos
// in the memory cell.

#define assert_no_exist_info(fdesc, st_desc)                                \
        do {                                                                \
            if (functor_desc->MR_du_functor_exist_info != NULL) {           \
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)           \
                    ": exist_info with " st_desc);                          \
            }                                                               \
        } while (0)

///////////////////

// XXX SUBTYPE
// We maintain compatibility with RTTI structures generated by older versions
// of the Mercury compiler by not reading from fields that were introduced in
// newer versions of the RTTI structures. These version checks can be deleted
// after some time.
#define tci_version_no_subtypes(tci)                                        \
    ((tci)->MR_type_ctor_version < MR_RTTI_VERSION__SUBTYPES)

// This performs a linear search for subtype enums. If a subtype has a
// large number of functors, it may be worth performing a binary search.
// If you update this, you will need to update MR_get_enum_functor_ordinal.
#define index_or_search_enum_functor(data, functor_name, functor_ordinal)   \
    do {                                                                    \
        MR_TypeLayout       type_layout;                                    \
        MR_EnumTypeLayout   enum_layout;                                    \
                                                                            \
        type_layout = MR_type_ctor_layout(type_ctor_info);                  \
        enum_layout = type_layout.MR_layout_enum;                           \
                                                                            \
        if (tci_version_no_subtypes(type_ctor_info) ||                      \
              MR_type_ctor_is_layout_indexable(type_ctor_info)) {           \
            functor_name = enum_layout[data]->MR_enum_functor_name;         \
            functor_ordinal = data;                                         \
        } else {                                                            \
            int     num_functors;                                           \
            int     i;                                                      \
                                                                            \
            num_functors = MR_type_ctor_num_functors(type_ctor_info);       \
            for (i = 0; i < num_functors; i++) {                            \
                if (enum_layout[i]->MR_enum_functor_value == data) {        \
                    functor_name = enum_layout[i]->MR_enum_functor_name;    \
                    functor_ordinal = i;                                    \
                    break;                                                  \
                }                                                           \
            }                                                               \
            MR_assert(i < num_functors);                                    \
        }                                                                   \
    } while (0)                                                             \

#define index_or_search_ptag_layout(ptag, ptag_layout)                      \
    do {                                                                    \
        MR_TypeLayout       type_layout;                                    \
        MR_DuTypeLayout     du_type_layout;                                 \
                                                                            \
        type_layout = MR_type_ctor_layout(type_ctor_info);                  \
        du_type_layout = type_layout.MR_layout_du;                          \
                                                                            \
        if (tci_version_no_subtypes(type_ctor_info) ||                      \
              MR_type_ctor_is_layout_indexable(type_ctor_info)) {           \
            ptag_layout = &du_type_layout[ptag];                            \
        } else {                                                            \
            int     num_ptags;                                              \
            int     i;                                                      \
                                                                            \
            num_ptags = MR_type_ctor_num_ptags(type_ctor_info);             \
            for (i = 0; i < num_ptags; i++) {                               \
                ptag_layout = &du_type_layout[i];                           \
                if (ptag_layout->MR_du_ptag == ptag) {                      \
                    break;                                                  \
                }                                                           \
            }                                                               \
                                                                            \
            MR_assert(i < num_ptags);                                       \
        }                                                                   \
    } while (0)

#define index_or_search_sectag_functor(ptag_layout, sectag, functor_desc)   \
    do {                                                                    \
        if (tci_version_no_subtypes(type_ctor_info) ||                      \
            (ptag_layout->MR_du_ptag_flags                                  \
                & MR_DU_PTAG_FLAG_SECTAG_ALTERNATIVES_INDEXABLE)) {         \
            functor_desc = ptag_layout->MR_sectag_alternatives[sectag];     \
        } else {                                                            \
            int     num_sharers;                                            \
            int     i;                                                      \
                                                                            \
            num_sharers = ptag_layout->MR_sectag_sharers;                   \
            for (i = 0; i < num_sharers; i++) {                             \
                functor_desc = ptag_layout->MR_sectag_alternatives[i];      \
                if (functor_desc->MR_du_functor_secondary == sectag) {      \
                    break;                                                  \
                }                                                           \
            }                                                               \
            MR_assert(i < num_sharers);                                     \
        }                                                                   \
    } while (0)                                                             \

#define search_foreign_enum_functor(data, functor_name, functor_ordinal)    \
    do {                                                                    \
        int                         i;                                      \
        int                         num_functors;                           \
        MR_TypeLayout               type_layout;                            \
        MR_ForeignEnumTypeLayout    fe_layout;                              \
                                                                            \
        num_functors = MR_type_ctor_num_functors(type_ctor_info);           \
        type_layout = MR_type_ctor_layout(type_ctor_info);                  \
        fe_layout = type_layout.MR_layout_foreign_enum;                     \
                                                                            \
        for (i = 0; i < num_functors; i++) {                                \
            if (fe_layout[i]->MR_foreign_enum_functor_value == data) {      \
                functor_name = fe_layout[i]->MR_foreign_enum_functor_name;  \
                functor_ordinal =                                           \
                    fe_layout[i]->MR_foreign_enum_functor_ordinal;          \
                break;                                                      \
            }                                                               \
        }                                                                   \
        MR_assert(i < num_functors);                                        \
    } while (0)

///////////////////

// These macros set up the results for terms of notag types to requests
// either for all arguments, or for one selected argument.

#define notag_arg_build_univ_list(ei, arg_ti_expr, dw_ptr)                  \
        do {                                                                \
            MR_Word arg_univ;                                               \
            MR_Word list;                                                   \
                                                                            \
            list = MR_list_empty();                                         \
            MR_new_univ_on_hp(arg_univ, (arg_ti_expr), (dw_ptr)[0]);        \
            list = MR_univ_list_cons(arg_univ, list);                       \
                                                                            \
            (ei)->arg_univs_list = list;                                    \
        } while (0)

// This has max_arity as an *implicit* argument. We cannot pass max_arity
// explicitly, since it is not defined if EXPAND_APPLY_LIMIT is not set.
#if defined(EXPAND_ARGS) && defined(EXPAND_APPLY_LIMIT)
  #define maybe_notag_arg_build_univ_list(ei, ate, dw_ptr)                  \
        do {                                                                \
            if ((ei)->arity > max_arity) {                                  \
                (ei)->limit_reached = MR_TRUE;                              \
            } else {                                                        \
                notag_arg_build_univ_list(ei, ate, dw_ptr);                 \
            }                                                               \
        } while (0)
#elif defined(EXPAND_ARGS)
  #define maybe_notag_arg_build_univ_list(ei, ate, dw_ptr)                  \
        do {                                                                \
            notag_arg_build_univ_list(ei, ate, dw_ptr);                     \
        } while (0)
#else
  #define maybe_notag_arg_build_univ_list(ei, ate, dw_ptr)                  \
        ((void) 0)
#endif  // EXPAND_ARGS

// This has chosen_name as an *implicit* argument. We cannot pass chosen_name
// explicitly, since it is not defined if EXPAND_NAMED_ARG is not set.
#ifdef    EXPAND_NAMED_ARG
  #define set_chosen_for_notag_arg_name(tci, chosen)                        \
        do {                                                                \
            MR_ConstString arg_name = MR_type_ctor_layout(tci).             \
                MR_layout_notag ->MR_notag_functor_arg_name;                \
                                                                            \
            if (arg_name != NULL && MR_streq(arg_name, chosen_name)) {      \
                chosen = 0;                                                 \
            }                                                               \
        } while (0)
#else
  #define set_chosen_for_notag_arg_name(tci, chosen)                        \
        ((void) 0)
#endif  // EXPAND_NAMED_ARG

#define notag_arg_get_chosen(ei, arg_ti_expr, dw_ptr, chosen)               \
        do {                                                                \
            if (chosen == 0) {                                              \
                (ei)->chosen_index_exists = MR_TRUE;               \
                                                                            \
                (ei)->chosen_arg_type_info = (arg_ti_expr);        \
                (ei)->chosen_arg_term = (dw_ptr)[0];               \
                (ei)->chosen_arg_word_sized_ptr = (dw_ptr);        \
            } else {                                                        \
                (ei)->chosen_index_exists = MR_FALSE;              \
            }                                                               \
        } while (0)

#ifdef  EXPAND_ONE_ARG
  #define maybe_notag_arg_get_chosen(ei, tci, ate, dw_ptr, chosen)          \
        do {                                                                \
            set_chosen_for_notag_arg_name(tci, chosen);                     \
            notag_arg_get_chosen(ei, ate, dw_ptr, chosen);                  \
        } while (0)
#else
  #define maybe_notag_arg_get_chosen(ei, tci, ate, dw_ptr, chosen)          \
        ((void) 0)
#endif  // EXPAND_ONE_ARG

// This has chosen and chosen_name as *implicit* arguments. We cannot pass
// them explicitly, since they are not always defined.
#define notag_arg_build_univ_list_or_get_chosen(ei, tci, ate, dw_ptr)       \
        do {                                                                \
            maybe_notag_arg_build_univ_list(ei, ate, dw_ptr);               \
            maybe_notag_arg_get_chosen(ei, tci, ate, dw_ptr, chosen);       \
        } while (0)

///////////////////

// These macros set up the results for terms of types in which all arguments
// are of the same type to requests either for all arguments, or for
// one selected argument.

#define same_type_args_build_univ_list(ei, arg_ti, arg_vector)              \
        do {                                                                \
            MR_Word arg_value;                                              \
            MR_Word arg_univ;                                               \
            MR_Word list;                                                   \
            int     i;                                                      \
                                                                            \
            list = MR_list_empty();                                         \
            i = (ei)->arity;                                                \
            while (--i >= 0) {                                              \
                arg_value = (arg_vector)[i];                                \
                MR_new_univ_on_hp(arg_univ, (arg_ti), arg_value);           \
                list = MR_univ_list_cons(arg_univ, list);                   \
            }                                                               \
                                                                            \
            (ei)->arg_univs_list = list;                                    \
        } while (0)

// This has max_arity as an *implicit* argument. We cannot pass max_arity
// explicitly, since it is not defined if EXPAND_APPLY_LIMIT is not set.
#if defined(EXPAND_ARGS) && defined(EXPAND_APPLY_LIMIT)
  #define maybe_same_type_args_build_univ_list(ei, ati, av)                 \
        do {                                                                \
            if ((ei)->arity > max_arity) {                                  \
                (ei)->limit_reached = MR_TRUE;                              \
            } else {                                                        \
                same_type_args_build_univ_list(ei, ati, av);                \
            }                                                               \
        } while (0)
#elif defined(EXPAND_ARGS)
  #define maybe_same_type_args_build_univ_list(ei, ati, av)                 \
        do {                                                                \
            same_type_args_build_univ_list(ei, ati, av)    ;                \
        } while (0)
#else
  #define maybe_same_type_args_build_univ_list(ei, ati, av)                 \
        ((void) 0)
#endif  // EXPAND_ARGS

#define same_type_args_get_chosen(ei, arg_ti, arg_vector, chosen)           \
        do {                                                                \
            if (0 <= chosen && chosen < (ei)->arity) {                      \
                (ei)->chosen_index_exists = MR_TRUE;               \
                                                                            \
                (ei)->chosen_arg_type_info = (arg_ti);             \
                (ei)->chosen_arg_term = (arg_vector)[chosen];      \
                (ei)->chosen_arg_word_sized_ptr =                  \
                    &((arg_vector)[chosen]);                                \
            } else {                                                        \
                (ei)->chosen_index_exists = MR_FALSE;              \
            }                                                               \
        } while (0)

#ifdef  EXPAND_ONE_ARG
  #define maybe_same_type_args_get_chosen(ei, ati, av, chosen)              \
        same_type_args_get_chosen(ei, ati, av, chosen)
#else
  #define maybe_same_type_args_get_chosen(ei, ati, av, chosen)              \
        ((void) 0)
#endif  // EXPAND_ONE_ARG

// This has chosen as an *implicit* argument. We cannot pass chosen
// explicitly, since it is not defined if EXPAND_ONE_ARG is not set.
#define same_type_args_build_univ_list_or_get_chosen(ei, ati, av)           \
        do {                                                                \
            maybe_same_type_args_build_univ_list(ei, ati, av);              \
            maybe_same_type_args_get_chosen(ei, ati, av, chosen);           \
        } while (0)

///////////////////

// In hlc grades, closures have a closure_layout field but it is not filled in.
// Since deconstructing closures is not possible without the information in
// this field, we must canonicalize all closures in hlc grades. We do this by
// overriding the test for canonicalization, so it always succeeds.
// XXX This approach to the problem prevents us from simply switching
// on the value of noncanon.

#ifdef  MR_HIGHLEVEL_CODE
  #define   higher_order_test(test)   (MR_TRUE)
#else
  #define   higher_order_test(test)   (test)
#endif

////////////////////////////////////////////////////////////////////////////

void
EXPAND_FUNCTION_NAME(MR_TypeInfo type_info, MR_Word *data_word_ptr,
    MR_noncanon_handling noncanon,
#ifdef  EXPAND_APPLY_LIMIT
    int max_arity,
#endif  // EXPAND_APPLY_LIMIT
#ifdef  EXPAND_CHOSEN_ARG
    int chosen,
#endif  // EXPAND_CHOSEN_ARG
#ifdef  EXPAND_NAMED_ARG
    MR_ConstString chosen_name,
#endif  // EXPAND_NAMED_ARG
    EXPAND_TYPE_NAME *expand_info)
{
    MR_TypeCtorInfo type_ctor_info;
#ifdef EXPAND_NAMED_ARG
    // No arm of the switch on type_ctor_rep handles named arguments by
    // default. Only those type_ctor_reps that support named arguments
    // need have code for searching for argument names. For the rest,
    // initializing chosen to -1 ensures that no argument will be returned.

    int chosen = -1;
#endif // EXPAND_NAMED_ARG

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
#ifdef  EXPAND_APPLY_LIMIT
    expand_info->limit_reached = MR_FALSE;
#endif  // EXPAND_APPLY_LIMIT

    handle_functor_number(-1);

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
            ": term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {

    case MR_TYPECTOR_REP_ENUM_USEREQ:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX should throw an exception
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        } else if (noncanon == MR_NONCANON_ALLOW) {
            handle_noncanonical_type_ctor_name(type_ctor_info);
            handle_zero_arity_args();
            return;
        }
        // else fall through

    case MR_TYPECTOR_REP_ENUM:
    {
        MR_Word             data;
        MR_ConstString      functor_name = NULL;
        MR_int_least32_t    functor_ordinal = -1;

        data = *data_word_ptr;
        index_or_search_enum_functor(data, functor_name, functor_ordinal);
        handle_functor_name(functor_name);
        handle_type_functor_number(type_ctor_info, functor_ordinal);
        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        } else if (noncanon == MR_NONCANON_ALLOW) {
            handle_noncanonical_type_ctor_name(type_ctor_info);
            handle_zero_arity_args();
            return;
        }
        // else fall through

    case MR_TYPECTOR_REP_FOREIGN_ENUM:
    {
        MR_Word             data;
        MR_ConstString      functor_name = NULL;
        MR_int_least32_t    functor_ordinal = -1;

        // For foreign enumerations, we cannot use the value as an index
        // into the type layout, so we just have to do a linear search.
        data = *data_word_ptr;
        search_foreign_enum_functor(data, functor_name, functor_ordinal);
        MR_assert(functor_name != NULL);
        MR_assert(functor_ordinal != -1);

        handle_functor_name(functor_name);
        handle_type_functor_number(type_ctor_info, functor_ordinal);
        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_DUMMY:
    {
        // We must not refer to the "value" we are asked to deconstruct,
        // *data_word_ptr, since it contains garbage.
        const MR_Word       data = 0;
        MR_ConstString      functor_name = NULL;
        MR_int_least32_t    functor_ordinal = -1; /* unused */

        index_or_search_enum_functor(data, functor_name, functor_ordinal);
        handle_functor_name(functor_name);
        handle_zero_arity_args();
        handle_functor_number(data);
        return;
    }

    case MR_TYPECTOR_REP_DU_USEREQ:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        } else if (noncanon == MR_NONCANON_ALLOW) {
            handle_noncanonical_type_ctor_name(type_ctor_info);
            handle_zero_arity_args();
            return;
        }
        // else fall through

    case MR_TYPECTOR_REP_DU:
    {
        const MR_DuPtagLayout   *ptag_layout;
        const MR_DuFunctorDesc  *functor_desc;
        const MR_DuArgLocn      *arg_locns;
        const MR_DuExistInfo    *exist_info;
        int                     num_extra_args;
        MR_Word                 data;
        int                     ptag;
        MR_Word                 sectag;
        // We use the argument vector for two purposes.
        //
        // The first is computing the type_infos of the arguments.
        // This requires accessing the type parameters that are stored
        // between the remote secondary tag (if any) and the argument values.
        // For this, we use ti_arg_vector, which should point to the start
        // of these type parameters (if there are any).
        //
        // The second is accessing the values of the arguments.
        // For this, we use ao_arg_vector, which should point to the start
        // of the part of the memory cell that stores only arg values.
        MR_Word                 *ti_arg_vector;
        MR_Word                 *ao_arg_vector;
        MR_Word                 *word_size_arg_ptr;
        MR_Word                 direct_arg;
        int                     arg_num;

        data = *data_word_ptr;
        ptag = MR_tag(data);
        index_or_search_ptag_layout(ptag, ptag_layout);

        switch (ptag_layout->MR_sectag_locn) {

        case MR_SECTAG_NONE:
            functor_desc = ptag_layout->MR_sectag_alternatives[0];
            handle_functor_name_number_arity(expand_info, type_ctor_info,
                functor_desc);
            set_exist_info_extra_args(functor_desc, exist_info, num_extra_args);
            ti_arg_vector = ((MR_Word *) MR_body(data, ptag));
            ao_arg_vector = ti_arg_vector + num_extra_args;
            break;

        case MR_SECTAG_NONE_DIRECT_ARG:
            functor_desc = ptag_layout->MR_sectag_alternatives[0];
            handle_functor_name_number_arity(expand_info, type_ctor_info,
                functor_desc);
            assert_no_exist_info(functor_desc, "MR_SECTAG_NONE_DIRECT_ARG");
            direct_arg = MR_body(data, ptag);
            // The word containing the direct arg in effect forms an argument
            // vector with just one element.
            ti_arg_vector = &direct_arg;
            ao_arg_vector = &direct_arg;
            break;

        case MR_SECTAG_LOCAL_REST_OF_WORD:
            sectag = MR_unmkbody(data);
            index_or_search_sectag_functor(ptag_layout, sectag, functor_desc);
            handle_functor_name_number_arity(expand_info, type_ctor_info,
                functor_desc);
            assert_no_exist_info(functor_desc, "MR_SECTAG_LOCAL_REST_OF_WORD");
            handle_zero_arity_args();
            return;

        case MR_SECTAG_LOCAL_BITS:
            sectag = MR_unmkbody(data) &
            // XXX ARG_PACK
            // Consider storing this mask in the ptag_layout.
                ((1 << ptag_layout->MR_sectag_numbits) - 1);
            index_or_search_sectag_functor(ptag_layout, sectag, functor_desc);
            handle_functor_name_number_arity(expand_info, type_ctor_info,
                functor_desc);
            assert_no_exist_info(functor_desc, "MR_SECTAG_LOCAL_BITS");
            arg_locns = functor_desc->MR_du_functor_arg_locns;
            MR_assert(arg_locns != NULL);

#ifdef  EXPAND_ARGS
  #ifdef    EXPAND_APPLY_LIMIT
            if (expand_info->arity > max_arity) {
                expand_info->limit_reached = MR_TRUE;
            } else
  #endif    // EXPAND_APPLY_LIMIT
            {
                MR_Word     list;
                MR_TypeInfo arg_type_info;
                MR_Word     arg_value;
                MR_Word     arg_univ;

                list = MR_list_empty_msg(MR_ALLOC_ID);
                arg_num = expand_info->arity;
                while (--arg_num >= 0) {
                    arg_type_info = MR_get_arg_type_info(type_info,
                        functor_desc, ti_arg_vector, arg_num);
                    MR_get_tagword_arg_value(arg_locns[arg_num], data,
                        arg_value);
                    MR_new_univ_on_hp(arg_univ, arg_type_info, arg_value);
                    list = MR_univ_list_cons(arg_univ, list);
                }

                expand_info->arg_univs_list = list;
            }
#endif  // EXPAND_ARGS

#ifdef  EXPAND_ONE_ARG
  #ifdef  EXPAND_NAMED_ARG
            set_chosen_for_arg_name(functor_desc, expand_info->arity,
                chosen_name, chosen);
  #endif  // EXPAND_NAMED_ARG

            if (0 <= chosen && chosen < expand_info->arity) {
                expand_info->chosen_index_exists = MR_TRUE;
                expand_info->chosen_arg_type_info =
                    MR_get_arg_type_info(type_info, functor_desc,
                        ti_arg_vector, chosen);
                {
                    MR_Word             arg_value;

                    MR_get_tagword_arg_value(arg_locns[chosen], data,
                        arg_value);
                    expand_info->chosen_arg_term = arg_value;
                    expand_info->chosen_arg_word_sized_ptr = NULL;
                }
            } else {
                expand_info->chosen_index_exists = MR_FALSE;
            }
#endif  // EXPAND_ONE_ARG

            return;

        case MR_SECTAG_REMOTE_FULL_WORD:
            sectag = MR_field(ptag, data, 0);
            index_or_search_sectag_functor(ptag_layout, sectag, functor_desc);
            handle_functor_name_number_arity(expand_info, type_ctor_info,
                functor_desc);
            set_exist_info_extra_args(functor_desc, exist_info,
                num_extra_args);
            ti_arg_vector = (MR_Word *) MR_body(data, ptag) + 1;
            ao_arg_vector = ti_arg_vector + num_extra_args;
            break;

        case MR_SECTAG_REMOTE_BITS:
            sectag = MR_field(ptag, data, 0) &
            // XXX ARG_PACK
            // Consider storing this mask in the ptag_layout.
                ((1 << ptag_layout->MR_sectag_numbits) - 1);
            index_or_search_sectag_functor(ptag_layout, sectag, functor_desc);
            handle_functor_name_number_arity(expand_info, type_ctor_info,
                functor_desc);
            assert_no_exist_info(functor_desc, "MR_SECTAG_LOCAL_BITS");
            ti_arg_vector = (MR_Word *) MR_body(data, ptag) + 1;
            ao_arg_vector = ti_arg_vector;
            break;

        case MR_SECTAG_VARIABLE:
            if (noncanon != MR_NONCANON_CC) {
                // XXX should throw an exception
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct variable");
            }

            handle_functor_name("<<variable>>");
            handle_zero_arity_args();
            return;

        default:
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                 ": invalid sectag_locn");
            return;
        }

#ifdef  EXPAND_ARGS
  #ifdef    EXPAND_APPLY_LIMIT
        if (expand_info->arity > max_arity) {
            expand_info->limit_reached = MR_TRUE;
        } else
  #endif    // EXPAND_APPLY_LIMIT
        {
            MR_Word     list;
            MR_TypeInfo arg_type_info;
            MR_Word     arg_value;
            MR_Word     arg_univ;

            list = MR_list_empty_msg(MR_ALLOC_ID);
            arg_num = expand_info->arity;
            arg_locns = functor_desc->MR_du_functor_arg_locns;
            if (arg_locns == NULL) {
                while (--arg_num >= 0) {
                    arg_type_info = MR_get_arg_type_info(type_info,
                        functor_desc, ti_arg_vector, arg_num);
                    arg_value = ao_arg_vector[arg_num];
                    MR_new_univ_on_hp(arg_univ, arg_type_info, arg_value);
                    list = MR_univ_list_cons(arg_univ, list);
                }
            } else {
                while (--arg_num >= 0) {
                    arg_type_info = MR_get_arg_type_info(type_info,
                        functor_desc, ti_arg_vector, arg_num);
                    // Here we ignore the value put into word_size_arg_ptr.
                    MR_get_non_tagword_arg_value(arg_locns[arg_num],
                        ao_arg_vector, arg_value, word_size_arg_ptr);
                    MR_new_univ_on_hp(arg_univ, arg_type_info, arg_value);
                    list = MR_univ_list_cons(arg_univ, list);
                }
            }

            expand_info->arg_univs_list = list;
        }
#endif  // EXPAND_ARGS

#ifdef  EXPAND_ONE_ARG
  #ifdef  EXPAND_NAMED_ARG
        set_chosen_for_arg_name(functor_desc, expand_info->arity, chosen_name,
            chosen);
  #endif  // EXPAND_NAMED_ARG

        if (0 <= chosen && chosen < expand_info->arity) {
            expand_info->chosen_index_exists = MR_TRUE;
            expand_info->chosen_arg_type_info =
                MR_get_arg_type_info(type_info, functor_desc,
                    ti_arg_vector, chosen);
            arg_locns = functor_desc->MR_du_functor_arg_locns;
            if (arg_locns == NULL) {
                expand_info->chosen_arg_term = ao_arg_vector[chosen];
                expand_info->chosen_arg_word_sized_ptr =
                    &ao_arg_vector[chosen];
            } else {
                MR_Word             arg_value;

                MR_get_non_tagword_arg_value(arg_locns[chosen],
                    ao_arg_vector, arg_value, word_size_arg_ptr);
                expand_info->chosen_arg_term = arg_value;
                expand_info->chosen_arg_word_sized_ptr =
                    word_size_arg_ptr;
            }
        } else {
            expand_info->chosen_index_exists = MR_FALSE;
        }
#endif  // EXPAND_ONE_ARG
        return;
    }

    case MR_TYPECTOR_REP_NOTAG_USEREQ:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        } else if (noncanon == MR_NONCANON_ALLOW) {
            handle_noncanonical_type_ctor_name(type_ctor_info);
            handle_zero_arity_args();
            return;
        }
        // else fall through

    case MR_TYPECTOR_REP_NOTAG:
        expand_info->arity = 1;
        handle_functor_number(0);
        handle_functor_name(MR_type_ctor_layout(type_ctor_info).
            MR_layout_notag->MR_notag_functor_name);

        notag_arg_build_univ_list_or_get_chosen(expand_info, type_ctor_info,
            MR_create_type_info(
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                    MR_notag_functor_arg_type),
            data_word_ptr);
        return;

    case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        } else if (noncanon == MR_NONCANON_ALLOW) {
            handle_noncanonical_type_ctor_name(type_ctor_info);
            handle_zero_arity_args();
            return;
        }
        // else fall through

    case MR_TYPECTOR_REP_NOTAG_GROUND:
        expand_info->arity = 1;
        handle_functor_number(0);
        handle_functor_name(MR_type_ctor_layout(type_ctor_info).
            MR_layout_notag->MR_notag_functor_name);

        notag_arg_build_univ_list_or_get_chosen(expand_info, type_ctor_info,
            MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                    MR_notag_functor_arg_type),
            data_word_ptr);
        return;

    case MR_TYPECTOR_REP_EQUIV:
    {
        MR_TypeInfo eqv_type_info;

        eqv_type_info = MR_create_type_info(
            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
            MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
        EXPAND_FUNCTION_NAME(eqv_type_info, data_word_ptr, noncanon,
            EXTRA_ARGS expand_info);
        return;
    }

    case MR_TYPECTOR_REP_EQUIV_GROUND:
        EXPAND_FUNCTION_NAME(MR_pseudo_type_info_is_ground(
            MR_type_ctor_layout(type_ctor_info).MR_layout_equiv),
            data_word_ptr, noncanon, EXTRA_ARGS expand_info);
        return;

    case MR_TYPECTOR_REP_INT:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "d",
            (MR_Integer) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_UINT:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu",
            (MR_Unsigned) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_INT8:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "di8",
            (MR_Integer) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_UINT8:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu8",
            (MR_Unsigned) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_INT16:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "di16",
            (MR_Integer) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_UINT16:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu16",
            (MR_Unsigned) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_INT32:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "di32",
            (MR_Integer) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_UINT32:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu32",
            (MR_Unsigned) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_INT64:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        int64_t i64;
        char    *str;

        data_word = *data_word_ptr;
        i64 = MR_word_to_int64(data_word);
        sprintf(buf, "%" PRId64 "i64", i64);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_UINT64:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word     data_word;
        char        buf[500];
        uint64_t    u64;
        char        *str;

        data_word = *data_word_ptr;
        u64 = MR_word_to_uint64(data_word);
        sprintf(buf, "%" PRIu64 "u64", u64);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_CHAR:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        // Any changes to this code need to be reflected in the
        // predicate deconstruct_2/9 in library/rtti_implementation.m.
        char        buf[8];
        MR_Word     data_word;
        const char  *str_ptr;
        char        *str;

        data_word = *data_word_ptr;
        switch (data_word) {
            case '\\': str_ptr = "'\\\\'"; break;
            case '\'': str_ptr = "'\\''";  break;
            case '\a': str_ptr = "'\\a'";  break;
            case '\b': str_ptr = "'\\b'";  break;
            case '\r': str_ptr = "'\\r'";  break;
            case '\f': str_ptr = "'\\f'";  break;
            case '\t': str_ptr = "'\\t'";  break;
            case '\n': str_ptr = "'\\n'";  break;
            case '\v': str_ptr = "'\\v'";  break;
            default:
                // Print remaining control characters using octal escapes.
                if (MR_is_control(data_word)) {
                    sprintf(buf, "\'\\%03" MR_INTEGER_LENGTH_MODIFIER "o\\\'",
                        data_word);
                } else if (MR_is_ascii(data_word)) {
                    sprintf(buf, "\'%c\'", (char) data_word);
                } else if (MR_is_surrogate(data_word)) {
                    // XXX Should throw an exception.
                    MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                        ": attempt to deconstruct surrogate code point");
                } else {
                    size_t n = MR_utf8_encode(buf + 1, (MR_Char)data_word);
                    // XXX Should throw an exception.
                    if (n == 0) {
                        MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                            ": attempt to deconstruct illegal code point");
                    }
                    buf[0] = '\'';
                    buf[n + 1] = '\'';
                    buf[n + 2] = '\0';
                }
                str_ptr = buf;
                break;
        }
        MR_make_aligned_string_copy_saved_hp(str, str_ptr, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_FLOAT:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word     data_word;
        char        buf[MR_SPRINTF_FLOAT_BUF_SIZE];
        MR_Float    f;
        char        *str;

        data_word = *data_word_ptr;
        f = MR_word_to_float(data_word);
        MR_sprintf_float(buf, f);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_STRING:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    *str;

        data_word = *data_word_ptr;
        if (MR_escape_string_quote(&str, (MR_ConstString)data_word)) {
            expand_info->EXPAND_FUNCTOR_FIELD = str;
        } else {
            // XXX should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": invalid string encoding");
        }
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_BITMAP:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        MR_String str;

        data_word = *data_word_ptr;
        str = MR_bitmap_to_quoted_string_saved_hp(
                (MR_ConstBitmapPtr) data_word, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_FUNC:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX should throw an exception
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        } else if (higher_order_test(noncanon == MR_NONCANON_ALLOW)) {
            handle_functor_name("<<function>>");
            handle_zero_arity_args();
            return;
        } else {
            goto predfunc;
        }

    case MR_TYPECTOR_REP_PRED:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        } else if (higher_order_test(noncanon == MR_NONCANON_ALLOW)) {
            handle_functor_name("<<predicate>>");
            handle_zero_arity_args();
            return;
        } else {
            goto predfunc;
        }

    // This label handles the MR_NONCANON_CC case of both predicates
    // and functions.
    predfunc:
    {
        MR_Closure          *closure;
        MR_Closure_Layout   *closure_layout;
        MR_ProcId           *proc_id;
        MR_UserProcId       *user_proc_id;
        MR_UCIProcId        *uci_proc_id;
        MR_ConstString      name;
        int                 num_r_args;
        int                 num_f_args;
        int                 num_args;
        int                 i;

        closure = (MR_Closure *) *data_word_ptr;
        closure_layout = closure->MR_closure_layout;
        num_r_args = MR_closure_num_hidden_r_args(closure);
        num_f_args = MR_closure_num_hidden_f_args(closure);
        num_args = num_r_args + num_f_args;
        expand_info->arity = num_args;

#ifdef  EXPAND_FUNCTOR_FIELD
        proc_id = &closure_layout->MR_closure_id->MR_closure_proc_id;
        if (proc_id->MR_proc_user.MR_user_arity < 0) {
            name = "dynlink_proc";  // XXX
        } else if (MR_PROC_ID_IS_UCI(*proc_id)) {
            name = proc_id->MR_proc_uci.MR_uci_pred_name;
        } else {
            name = proc_id->MR_proc_user.MR_user_name;
        }
        handle_functor_name(name);
#endif  // EXPAND_FUNCTOR_FIELD

#ifdef  EXPAND_ARGS
  #ifdef    EXPAND_APPLY_LIMIT
        if (num_args > max_arity) {
            expand_info->limit_reached = MR_TRUE;
        } else
  #endif    // EXPAND_APPLY_LIMIT
        {
            MR_TypeInfo *type_params;
            MR_Word     *arg_vector;
            MR_bool     free_arg_vector;
            MR_Word     list;
            MR_TypeInfo arg_type_info;
            MR_Word     arg_value;
            MR_Word     arg_univ;

            type_params = MR_materialize_closure_type_params(closure);
  #ifdef    MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS
            // If hidden arguments may have been reordered, create
            // a new vector with arguments in the correct order.
            if (num_r_args != 0 && num_f_args != 0) {
                int r_offset = 0;
                int f_offset = num_r_args;

                arg_vector = MR_malloc(sizeof(MR_Word) * num_args);
                free_arg_vector = MR_TRUE;
                for (i = 0; i < num_args; i++) {
                    MR_PseudoTypeInfo   arg_pti;
                    int                 offset;

                    arg_pti =
                        closure_layout->MR_closure_arg_pseudo_type_info[i];
                    if (MR_unify_pseudo_type_info_float(arg_pti)) {
                        offset = f_offset++;
                    } else {
                        offset = r_offset++;
                    }
                    arg_vector[i] = closure->MR_closure_hidden_args_0[offset];
                }
            } else {
                arg_vector = &closure->MR_closure_hidden_args_0[0];
                free_arg_vector = MR_FALSE;
            }
  #else
            arg_vector = &closure->MR_closure_hidden_args_0[0];
            free_arg_vector = MR_FALSE;
  #endif    // MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS

            list = MR_list_empty_msg(MR_ALLOC_ID);
            i = num_args;
            while (--i >= 0) {
                arg_type_info = MR_create_type_info(type_params,
                    closure_layout->MR_closure_arg_pseudo_type_info[i]);
                arg_value = arg_vector[i];
                MR_new_univ_on_hp(arg_univ, arg_type_info, arg_value);
                list = MR_univ_list_cons(arg_univ, list);
            }
            expand_info->arg_univs_list = list;

            if (type_params != NULL) {
                MR_free(type_params);
            }
            if (free_arg_vector) {
                MR_free(arg_vector);
            }
        }
#endif  // EXPAND_ARGS

#ifdef  EXPAND_CHOSEN_ARG
        if (0 <= chosen && chosen < num_args) {
            MR_TypeInfo *type_params;
            MR_Unsigned offset;
            MR_Unsigned r_offset;
            MR_Unsigned f_offset;

            expand_info->chosen_index_exists = MR_TRUE;
#ifdef MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS
            r_offset = 0;
            f_offset = MR_closure_num_hidden_r_args(closure);
            for (i = 0; i <= chosen; i++) {
                MR_PseudoTypeInfo arg_pti =
                    closure_layout->MR_closure_arg_pseudo_type_info[i];
                if (MR_unify_pseudo_type_info_float(arg_pti)) {
                    offset = f_offset++;
                } else {
                    offset = r_offset++;
                }
            }
#else
            offset = chosen;
#endif
            type_params = MR_materialize_closure_type_params(closure);
            // The following code could be improved.
            expand_info->chosen_arg_type_info =
                MR_create_type_info(type_params,
                    closure_layout->MR_closure_arg_pseudo_type_info[chosen]);
            expand_info->chosen_arg_term =
                closure->MR_closure_hidden_args_0[offset];
            expand_info->chosen_arg_word_sized_ptr =
                &(closure->MR_closure_hidden_args_0[offset]);
            if (type_params != NULL) {
                MR_free(type_params);
            }
        } else {
            expand_info->chosen_index_exists = MR_FALSE;
        }
#endif  // EXPAND_CHOSEN_ARG
#ifdef  EXPAND_NAMED_ARG
        expand_info->chosen_index_exists = MR_FALSE;
#endif  // EXPAND_NAMED_ARG

        return;
    }

    case MR_TYPECTOR_REP_TUPLE:
        expand_info->arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
        handle_functor_number(0);
        handle_functor_name("{}");

#ifdef  EXPAND_ARGS
  #ifdef    EXPAND_APPLY_LIMIT
        if (expand_info->arity > max_arity) {
            expand_info->limit_reached = MR_TRUE;
        } else
  #endif    // EXPAND_APPLY_LIMIT
        {
            MR_TypeInfo *arg_type_infos;
            MR_Word     *arg_vector;
            MR_Word     list;
            MR_TypeInfo arg_type_info;
            MR_Word     arg_value;
            MR_Word     arg_univ;
            int         i;

            // Type-infos are normally counted from one,
            // but the users of this vector count from zero.
            arg_type_infos =
                MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info) + 1;
            arg_vector = (MR_Word *) *data_word_ptr;

            list = MR_list_empty_msg(MR_ALLOC_ID);
            i = expand_info->arity;
            while (--i >= 0) {
                MR_new_univ_on_hp(arg_univ, arg_type_infos[i], arg_vector[i]);
                list = MR_univ_list_cons(arg_univ, list);
            }

            expand_info->arg_univs_list = list;
        }
#endif  // EXPAND_ARGS

#ifdef  EXPAND_ONE_ARG
        if (0 <= chosen && chosen < expand_info->arity) {
            MR_TypeInfo *arg_type_infos;
            MR_Word     *arg_vector;

            expand_info->chosen_index_exists = MR_TRUE;

            // Type-infos are normally counted from one,
            // but the users of this vector count from zero.
            arg_type_infos =
                MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info) + 1;
            arg_vector = (MR_Word *) *data_word_ptr;

            expand_info->chosen_arg_type_info =
                arg_type_infos[chosen];
            expand_info->chosen_arg_term = arg_vector[chosen];
            expand_info->chosen_arg_word_sized_ptr =
                &arg_vector[chosen];
        } else {
            expand_info->chosen_index_exists = MR_FALSE;
        }
#endif  // EXPAND_ONE_ARG
        return;

    case MR_TYPECTOR_REP_SUBGOAL:
#if MR_USE_MINIMAL_MODEL_STACK_COPY
        if (noncanon == MR_NONCANON_CC) {
            handle_functor_name(MR_subgoal_addr_name(
                (MR_SubgoalPtr) *data_word_ptr));
        } else {
            handle_functor_name("<<subgoal>>");
        }
#else
        handle_functor_name("<<subgoal>>");
#endif
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_VOID:
        // There is no way to create values of type `void',
        // so this should never happen.
        MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
            ": cannot expand void types");

    case MR_TYPECTOR_REP_C_POINTER:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "c_pointer(0x%lX)", (long) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_STABLE_C_POINTER:
    {
#ifdef  EXPAND_FUNCTOR_FIELD
        MR_Word data_word;
        char    buf[500];
        char    *str;

        data_word = *data_word_ptr;
        sprintf(buf, "stable_c_pointer(0x%lX)", (long) data_word);
        MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
        expand_info->EXPAND_FUNCTOR_FIELD = str;
#endif  // EXPAND_FUNCTOR_FIELD

        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_TYPEINFO:
    case MR_TYPECTOR_REP_TYPEDESC:
    {
        MR_TypeInfo         data_type_info;
        MR_TypeCtorInfo     data_type_ctor_info;
        MR_Word             *arg_type_infos;
        int                 num_args;

        // Most changes here should also be made in the code for
        // MR_TYPECTOR_REP_PSEUDOTYPEDESC below.

        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
        }

        // The only source of noncanonicality in type_infos is due
        // to type equivalences, so we can eliminate noncanonicality
        // by expanding out equivalences.

        data_type_info = (MR_TypeInfo) *data_word_ptr;
        if (noncanon == MR_NONCANON_ALLOW) {
            data_type_info = MR_collapse_equivalences(data_type_info);
        }

        data_type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(data_type_info);
        handle_functor_name(MR_type_ctor_name(data_type_ctor_info));

        if (MR_type_ctor_has_variable_arity(data_type_ctor_info)) {
            num_args = MR_TYPEINFO_GET_VAR_ARITY_ARITY(data_type_info);
            arg_type_infos = (MR_Word *)
                MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(data_type_info);
        } else {
            num_args = data_type_ctor_info->MR_type_ctor_arity;
            arg_type_infos = (MR_Word *)
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(data_type_info);
        }

        expand_info->arity = num_args;
        // The arguments of a type_info are themselves of type ``type_info''.
        // The +1 is to switch from 1-based to 0-based array indexing.
        same_type_args_build_univ_list_or_get_chosen(expand_info,
            type_info, (arg_type_infos+1));
        return;
    }

    case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
    {
        MR_PseudoTypeInfo   data_pseudo_type_info;
        MR_TypeCtorInfo     data_type_ctor_info;
        MR_Word             *arg_type_infos;
        int                 num_args;

        // Most changes here should also be made in the code for
        // MR_TYPECTOR_REP_TYPEDESC above.

        if (noncanon == MR_NONCANON_ABORT) {
            // XXX should throw an exception
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
        }

        // The only source of noncanonicality in pseudo_type_infos
        // is due to type equivalences, so we can eliminate
        // noncanonicality by expanding out equivalences.

        data_pseudo_type_info = (MR_PseudoTypeInfo) *data_word_ptr;
        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(data_pseudo_type_info)) {
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                char    buf[500];
                char    *str;

                sprintf(buf, "tvar%" MR_INTEGER_LENGTH_MODIFIER "d",
                    (MR_Integer) data_pseudo_type_info);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;
        }

        if (noncanon == MR_NONCANON_ALLOW) {
            data_pseudo_type_info = MR_collapse_equivalences_pseudo(
                data_pseudo_type_info);
        }

        data_type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(
            data_pseudo_type_info);
        handle_functor_name(MR_type_ctor_name(data_type_ctor_info));

        if (MR_type_ctor_has_variable_arity(data_type_ctor_info)) {
            num_args = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(
                data_pseudo_type_info);
            arg_type_infos = (MR_Word *)
                MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(
                    data_pseudo_type_info);
        } else {
            num_args = data_type_ctor_info->MR_type_ctor_arity;
            arg_type_infos = (MR_Word *)
                MR_PSEUDO_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(
                    data_pseudo_type_info);
        }

        expand_info->arity = num_args;
        // The arguments of a pseudo_type_info are themselves of type
        // ``pseudo_type_info''.
        // The +1 is to switch from 1-based to 0-based array indexing.
        same_type_args_build_univ_list_or_get_chosen(expand_info,
            type_info, (arg_type_infos+1));
        return;
    }

    case MR_TYPECTOR_REP_TYPECTORINFO:
    {
        MR_TypeCtorInfo data_type_ctor_info;

        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
        }

        data_type_ctor_info = (MR_TypeCtorInfo) *data_word_ptr;
        handle_type_ctor_name(data_type_ctor_info);
        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_TYPECTORDESC:
    {
        MR_TypeCtorDesc data_type_ctor_desc;
        MR_TypeCtorInfo data_type_ctor_info;

        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
        }

        data_type_ctor_desc = (MR_TypeCtorDesc) *data_word_ptr;
        if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(data_type_ctor_desc)) {
            handle_functor_name(MR_TYPECTOR_DESC_GET_VA_NAME(
                data_type_ctor_desc));
        } else {
            data_type_ctor_info =
                MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(
                    data_type_ctor_desc);
            handle_type_ctor_name(data_type_ctor_info);
        }
        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_TYPECLASSINFO:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        }

        handle_functor_name("<<typeclassinfo>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        }

        handle_functor_name("<<basetypeclassinfo>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_ARRAY:
    {
        MR_ArrayType        *array;
        MR_TypeInfoParams   ti_params;
        MR_TypeInfo         elt_type_info;

        array = (MR_ArrayType *) *data_word_ptr;
        expand_info->arity = array->size;
        handle_functor_name("<<array>>");

        ti_params = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
        elt_type_info = ti_params[1];

        same_type_args_build_univ_list_or_get_chosen(expand_info,
            elt_type_info, array->elements);
        return;
    }

    case MR_TYPECTOR_REP_SUCCIP:
        handle_functor_name("<<succip>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_HP:
        handle_functor_name("<<hp>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_CURFR:
        handle_functor_name("<<curfr>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_MAXFR:
        handle_functor_name("<<maxfr>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_REDOFR:
        handle_functor_name("<<redofr>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_REDOIP:
        handle_functor_name("<<redoip>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_TRAIL_PTR:
        handle_functor_name("<<trail_ptr>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_TICKET:
        handle_functor_name("<<ticket>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_FOREIGN:
    {
        char buf[MR_FOREIGN_NAME_BUF_SIZE];
        MR_snprintf(buf, MR_FOREIGN_NAME_BUF_SIZE,
            "<<foreign(%s, %p)>>",
            type_ctor_info->MR_type_ctor_name,
            (void *) *data_word_ptr);
        // The contents of the memory occupied by buf may change.
        copy_and_handle_functor_name(buf);
        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_STABLE_FOREIGN:
    {
        char buf[MR_FOREIGN_NAME_BUF_SIZE];
        MR_snprintf(buf, MR_FOREIGN_NAME_BUF_SIZE,
            "<<stable_foreign(%s, %p)>>",
            type_ctor_info->MR_type_ctor_name,
            (void *) *data_word_ptr);
        // The contents of the memory occupied by buf may change.
        copy_and_handle_functor_name(buf);
        handle_zero_arity_args();
        return;
    }

    case MR_TYPECTOR_REP_REFERENCE:
        if (noncanon == MR_NONCANON_ABORT) {
            // XXX Should throw an exception.
            MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                ": attempt to deconstruct noncanonical term");
            return;
        }

        handle_functor_name("<<reference>>");
        handle_zero_arity_args();
        return;

    case MR_TYPECTOR_REP_UNUSED1:
    case MR_TYPECTOR_REP_UNUSED2:
    case MR_TYPECTOR_REP_UNKNOWN:
        MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
            ": cannot expand -- unknown data type");
    }

    MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
        ": unexpected fallthrough");
}

#undef  EXTRA_ARG1
#undef  EXTRA_ARG2
#undef  EXTRA_ARG3
#undef  EXTRA_ARGS
#undef  EXPAND_ONE_ARG

#undef  copy_and_handle_functor_name
#undef  handle_functor_name
#undef  handle_noncanonical_type_ctor_name
#undef  handle_type_ctor_name
#undef  handle_functor_number
#undef  handle_type_functor_number
#undef  handle_functor_name_number_arity

#undef  handle_zero_arity_all_args
#undef  handle_zero_arity_one_arg
#undef  handle_zero_arity_args

#undef  set_chosen_for_arg_name

#undef  maybe_set_limit_reached_and_return

#undef  set_exist_info_extra_args
#undef  assert_no_exist_info

#undef  tci_version_no_subtypes
#undef  index_or_search_enum_functor
#undef  index_or_search_ptag_layout
#undef  index_or_search_sectag_functor
#undef  search_foreign_enum_functor

#undef  notag_arg_build_univ_list
#undef  maybe_notag_arg_build_univ_list
#undef  set_chosen_for_notag_arg_name
#undef  notag_arg_get_chosen
#undef  maybe_notag_arg_get_chosen
#undef  notag_arg_build_univ_list_or_get_chosen

#undef  same_type_args_build_univ_list
#undef  maybe_same_type_args_build_univ_list
#undef  same_type_args_get_chosen
#undef  maybe_same_type_args_get_chosen
#undef  same_type_args_build_univ_list_or_get_chosen

#undef  higher_order_test
