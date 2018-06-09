// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2001-2007, 2012 The University of Melbourne.
// Copyright (C) 2013, 2015-2018 The Mercury team.
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
// EXPAND_ARGS_FIELD        If defined, gives the name of the field in the
//                          expand_info structure that contains information
//                          about all the functor's arguments. This field
//                          should be of type MR_ExpandArgsFields. The
//                          function will fill in this field.
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
// - only one of EXPAND_ARGS_FIELD, EXPAND_CHOSEN_ARG and EXPAND_NAMED_ARG
//   may be defined at once, and
// - EXPAND_APPLY_LIMIT should be defined only if EXPAND_ARGS_FIELD is also
//   defined.
//
// Each variant of the function will fill in all the fields of the expand_info
// structure passed to it, although the set of fields in that structure will
// be different for different variants. The type in EXPAND_TYPE_NAME must be
// consistent with the set of defined optional macros.
//
// All variants contain the integer field arity, which will be set to
// the number of arguments the functor has.
//
// The variants that return all the arguments do so in a field of type
// MR_ExpandArgsFields. Its arg_type_infos subfield will contain a pointer
// to an array of arity MR_TypeInfos, one for each user-visible field of the
// cell. The arg_values field will contain a pointer to a block of MR_Words,
// one for each field of the cell. The first num_extra_args words will be
// the type infos and/or typeclass infos added by the implementation to
// describe the types of the existentially typed fields, while the rest
// will hold the user-visible constructor arguments, some of which may be
// _packed_. The caller must unpack the actual values using the arg_locns
// field. arg_locns will be NULL if there is no argument packing, or
// otherwise point to an array of MR_DuArgLocns, one for every user-visible
// argument.
//
// If the can_free_arg_type_infos field is true, then the array returned
// in the arg_type_infos field was allocated by this function, and should be
// freed by the caller when it has finished using the information it contains.
// Since the array will have been allocated using MR_GC_malloc(), it should be
// freed with MR_GC_free. (We need to use MR_GC_malloc() rather than
// MR_malloc() or malloc(), since this vector may contain pointers into the
// Mercury heap, and memory allocated with MR_malloc() or malloc() will not be
// traced by the Boehm collector.) The elements of the array should not be
// freed, since they point to previously allocated data, which is either
// on the heap or is in constant storage (e.g. type_ctor_infos).
// If the can_free_arg_type_infos field is false, then the array returned in
// the arg_type_infos field was not allocated by the function (it came from the
// type_info argument passed to it) and must not be freed.
//
// Please note:
//
// These functions increment the heap pointer; however, on some platforms
// the register windows mean that transient Mercury registers may be lost.
// Before calling these functions, call MR_save_transient_registers(), and
// afterwards, call MR_restore_transient_registers().
//
// If you change this code, you may also have to reflect your changes
// in runtime/mercury_deep_copy_body.h and runtime/mercury_tabling.c.
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

// Set up macro for setting field names without #ifdefs.
#ifdef  EXPAND_FUNCTOR_FIELD
  #define copy_and_handle_functor_name(name)                                  \
            do {                                                              \
                MR_restore_transient_hp();                                    \
                MR_make_aligned_string_copy(expand_info->EXPAND_FUNCTOR_FIELD,\
                    name);                                                    \
                MR_save_transient_hp();                                       \
            } while (0)
  #define handle_functor_name(name)                                      \
            do {                                                         \
                MR_restore_transient_hp();                               \
                MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD,\
                    name);                                               \
                MR_save_transient_hp();                                  \
            } while (0)
  #define handle_noncanonical_name(tci)                                  \
            do {                                                         \
                MR_ConstString  name;                                    \
                                                                         \
                name = MR_expand_type_name(tci, MR_TRUE);                \
                MR_restore_transient_hp();                               \
                MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD,\
                    name);                                               \
                MR_save_transient_hp();                                  \
            } while (0)
  #define handle_type_ctor_name(tci)                                     \
            do {                                                         \
                MR_ConstString  name;                                    \
                                                                         \
                name = MR_expand_type_name(tci, MR_FALSE);               \
                MR_restore_transient_hp();                               \
                MR_make_aligned_string(expand_info->EXPAND_FUNCTOR_FIELD,\
                    name);                                               \
                MR_save_transient_hp();                                  \
            } while (0)
  #define handle_functor_number(num)                                    \
            do {                                                        \
                expand_info->functor_number = (num);                    \
            } while (0)
  #define handle_type_functor_number(tci, ordinal)                      \
            do {                                                        \
                expand_info->functor_number =                           \
                    (tci)->MR_type_ctor_functor_number_map[ordinal];    \
            } while (0)
#else   // EXPAND_FUNCTOR_FIELD
  #define copy_and_handle_functor_name(name)                            \
            ((void) 0)
  #define handle_functor_name(name)                                     \
            ((void) 0)
  #define handle_noncanonical_name(tci)                                 \
            ((void) 0)
  #define handle_type_ctor_name(tci)                                    \
            ((void) 0)
  #define handle_functor_number(num)                                    \
            ((void) 0)
  #define handle_type_functor_number(tci, ordinal)                      \
            ((void) 0)
#endif  // EXPAND_FUNCTOR_FIELD

// Set up macros for the common code handling zero arity terms.

#ifdef  EXPAND_ARGS_FIELD
  #define handle_zero_arity_all_args()                                  \
            do {                                                        \
                expand_info->EXPAND_ARGS_FIELD.arg_values = NULL;       \
                expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;        \
                expand_info->EXPAND_ARGS_FIELD.arg_type_infos = NULL;   \
                expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;      \
            } while (0)
#else   // EXPAND_ARGS_FIELD
  #define handle_zero_arity_all_args()                                  \
            ((void) 0)
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
  #define handle_zero_arity_one_arg()                                   \
            do {                                                        \
                expand_info->chosen_index_exists = MR_FALSE;            \
            } while (0)
#else   // EXPAND_ONE_ARG
  #define handle_zero_arity_one_arg()                                   \
            ((void) 0)
#endif  // EXPAND_ONE_ARG

#define handle_zero_arity_args()                                        \
            do {                                                        \
                expand_info->arity = 0;                                 \
                handle_zero_arity_all_args();                           \
                handle_zero_arity_one_arg();                            \
            } while (0)

// In hlc grades, closures have a closure_layout field but it is not filled in.
// Since deconstructing closures is not possible without the information in
// this field, we must canonicalize all closures in hlc grades. We do this by
// overriding the test for canonicalization, so it always succeeds.

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
#ifdef  EXPAND_ARGS_FIELD
    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos = MR_FALSE;
#endif  // EXPAND_ARGS_FIELD
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
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                return;
            }
            // else fall through

        case MR_TYPECTOR_REP_ENUM:
            handle_functor_name(MR_type_ctor_layout(type_ctor_info).
                MR_layout_enum[*data_word_ptr]->MR_enum_functor_name);
            handle_type_functor_number(type_ctor_info,
                MR_type_ctor_layout(type_ctor_info).
                    MR_layout_enum[*data_word_ptr]->MR_enum_functor_ordinal);
            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                // XXX Should throw an exception.
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                return;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                return;
            }
            // else fall through

        case MR_TYPECTOR_REP_FOREIGN_ENUM:
            {
                // For foreign enumerations we cannot use the value as
                // an index into the type layout, so we just have to do a
                // linear search.

                int                 i;
                int                 num_functors;
                MR_ConstString      functor_name = NULL;
                MR_int_least32_t    functor_ordinal = -1;
                MR_Integer          functor_value;

                num_functors = MR_type_ctor_num_functors(type_ctor_info);

                for (i = 0; i < num_functors; i++) {
                    functor_value = MR_type_ctor_layout(type_ctor_info).
                      MR_layout_foreign_enum[i]->MR_foreign_enum_functor_value;

                    if (functor_value == *data_word_ptr) {

                        functor_name = MR_type_ctor_layout(type_ctor_info).
                            MR_layout_foreign_enum[i]->
                                MR_foreign_enum_functor_name;

                        functor_ordinal = MR_type_ctor_layout(type_ctor_info).
                            MR_layout_foreign_enum[i]->
                                MR_foreign_enum_functor_ordinal;

                        break;
                    }
                }

                MR_assert(functor_name != NULL);
                MR_assert(functor_ordinal != -1);

                handle_functor_name(functor_name);
                handle_type_functor_number(type_ctor_info, functor_ordinal);
                handle_zero_arity_args();
            }
            return;

        case MR_TYPECTOR_REP_DUMMY:
            // We must not refer to the "value" we are asked to deconstruct,
            // *data_word_ptr, since it contains garbage.

            handle_functor_name(MR_type_ctor_layout(type_ctor_info).
                MR_layout_enum[0]->MR_enum_functor_name);
            handle_zero_arity_args();
            handle_functor_number(0);
            return;

        case MR_TYPECTOR_REP_DU_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                // XXX Should throw an exception.
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                return;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                return;
            }
            // else fall through

        case MR_TYPECTOR_REP_DU:
            {
                MR_DuTypeLayout         du_type_layout;
                const MR_DuPtagLayout   *ptag_layout;
                const MR_DuFunctorDesc  *functor_desc;
                const MR_DuExistInfo    *exist_info;
                int                     extra_args;
                MR_Word                 data;
                int                     ptag;
                MR_Word                 sectag;
                MR_Word                 *arg_vector;
                MR_Word                 direct_arg;

                du_type_layout =
                    MR_type_ctor_layout(type_ctor_info).MR_layout_du;
                data = *data_word_ptr;
                ptag = MR_tag(data);
                ptag_layout = &du_type_layout[ptag];

                switch (ptag_layout->MR_sectag_locn) {
                    case MR_SECTAG_NONE:
                        functor_desc = ptag_layout->MR_sectag_alternatives[0];
                        arg_vector = (MR_Word *) MR_body(data, ptag);
                        break;
                    case MR_SECTAG_NONE_DIRECT_ARG:
                        functor_desc = ptag_layout->MR_sectag_alternatives[0];
                        direct_arg = MR_body(data, ptag);
                        arg_vector = &direct_arg;
                        break;
                    case MR_SECTAG_LOCAL:
                        sectag = MR_unmkbody(data);
                        functor_desc =
                            ptag_layout->MR_sectag_alternatives[sectag];
                        arg_vector = NULL;
                        break;
                    case MR_SECTAG_REMOTE:
                        sectag = MR_field(ptag, data, 0);
                        functor_desc =
                            ptag_layout->MR_sectag_alternatives[sectag];
                        arg_vector = (MR_Word *) MR_body(data, ptag) + 1;
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
                }

                handle_functor_name(functor_desc->MR_du_functor_name);
                handle_type_functor_number(type_ctor_info,
                    functor_desc->MR_du_functor_ordinal);
                expand_info->arity = functor_desc->MR_du_functor_orig_arity;

#if     defined(EXPAND_ARGS_FIELD) || defined(EXPAND_ONE_ARG)
                exist_info = functor_desc->MR_du_functor_exist_info;
                if (exist_info != NULL) {
                    extra_args = exist_info->MR_exist_typeinfos_plain
                        + exist_info->MR_exist_tcis;
                } else {
                    extra_args = 0;
                }
#endif  // defined(EXPAND_ARGS_FIELD) || defined(EXPAND_ONE_ARG)

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
                if (expand_info->arity > max_arity) {
                    expand_info->limit_reached = MR_TRUE;
                } else
  #endif    // EXPAND_APPLY_LIMIT
                {
                    int i;

                    expand_info->EXPAND_ARGS_FIELD.num_extra_args = extra_args;
                    expand_info->EXPAND_ARGS_FIELD.arg_values = arg_vector;
                    expand_info->EXPAND_ARGS_FIELD.arg_locns =
                        functor_desc->MR_du_functor_arg_locns;
                    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos =
                        MR_TRUE;
                    expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_GC_NEW_ARRAY(MR_TypeInfo, expand_info->arity);

                    for (i = 0; i < expand_info->arity; i++) {
                        if (MR_arg_type_may_contain_var(functor_desc, i)) {
                            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                                MR_create_type_info_maybe_existq(
                                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(
                                        type_info),
                                    functor_desc->MR_du_functor_arg_types[i],
                                    arg_vector, functor_desc);
                        } else {
                            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                                MR_pseudo_type_info_is_ground(
                                    functor_desc->MR_du_functor_arg_types[i]);
                        }
                    }
                }
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
  #ifdef  EXPAND_NAMED_ARG
                {
                    int i;

                    for (i = 0; i < expand_info->arity; i++) {
                        if (functor_desc->MR_du_functor_arg_names != NULL
                            && functor_desc->MR_du_functor_arg_names[i] != NULL
                            && MR_streq(
                                functor_desc->MR_du_functor_arg_names[i],
                                chosen_name))
                        {
                            chosen = i;
                            break;
                        }
                    }
                }
  #endif  // EXPAND_NAMED_ARG

                if (0 <= chosen && chosen < expand_info->arity) {
                    const MR_DuArgLocn *arg_locn;
                    int                 slot;

                    if (functor_desc->MR_du_functor_arg_locns == NULL) {
                        arg_locn = NULL;
                        slot = extra_args + chosen;
                    } else {
                        arg_locn =
                            &functor_desc->MR_du_functor_arg_locns[chosen];
                        slot = extra_args + arg_locn->MR_arg_offset;
                    }

                    expand_info->chosen_index_exists = MR_TRUE;
                    expand_info->chosen_value_ptr = &arg_vector[slot];
                    expand_info->chosen_arg_locn = arg_locn;

                    if (MR_arg_type_may_contain_var(functor_desc, chosen)) {
                        expand_info->chosen_type_info =
                            MR_create_type_info_maybe_existq(
                                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(
                                    type_info),
                                functor_desc->MR_du_functor_arg_types[chosen],
                                arg_vector, functor_desc);
                    } else {
                        expand_info->chosen_type_info =
                            MR_pseudo_type_info_is_ground(
                                functor_desc->MR_du_functor_arg_types[chosen]);
                    }
                } else {
                    expand_info->chosen_index_exists = MR_FALSE;
                }
#endif  // EXPAND_ONE_ARG
            }
            return;

        case MR_TYPECTOR_REP_NOTAG_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                // XXX Should throw an exception.
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                return;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                return;
            }
            // else fall through

        case MR_TYPECTOR_REP_NOTAG:
            expand_info->arity = 1;
            handle_functor_number(0);
            handle_functor_name(MR_type_ctor_layout(type_ctor_info).
                MR_layout_notag->MR_notag_functor_name);

#ifdef  EXPAND_ARGS_FIELD
            expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
            expand_info->EXPAND_ARGS_FIELD.arg_values = data_word_ptr;
            expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;
            expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos = MR_TRUE;
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                MR_GC_NEW_ARRAY(MR_TypeInfo, 1);
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[0] =
                MR_create_type_info(
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                        MR_notag_functor_arg_type);
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
  #ifdef    EXPAND_NAMED_ARG
            if (MR_type_ctor_layout(type_ctor_info).MR_layout_notag
                    ->MR_notag_functor_arg_name != NULL
               && MR_streq(chosen_name, MR_type_ctor_layout(type_ctor_info).
                    MR_layout_notag->MR_notag_functor_arg_name))
            {
                chosen = 0;
            }
  #endif    // EXPAND_NAMED_ARG

            if (chosen == 0) {
                expand_info->chosen_index_exists = MR_TRUE;
                expand_info->chosen_value_ptr = data_word_ptr;
                expand_info->chosen_arg_locn = NULL;
                expand_info->chosen_type_info =
                    MR_create_type_info(
                        MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                        MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                            MR_notag_functor_arg_type);
            } else {
                expand_info->chosen_index_exists = MR_FALSE;
            }
#endif  // EXPAND_ONE_ARG
            return;

        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            if (noncanon == MR_NONCANON_ABORT) {
                // XXX Should throw an exception.
                MR_fatal_error(MR_STRINGIFY(EXPAND_FUNCTION_NAME)
                    ": attempt to deconstruct noncanonical term");
                return;
            } else if (noncanon == MR_NONCANON_ALLOW) {
                handle_noncanonical_name(type_ctor_info);
                handle_zero_arity_args();
                return;
            }
            // else fall through

        case MR_TYPECTOR_REP_NOTAG_GROUND:
            expand_info->arity = 1;
            handle_functor_number(0);
            handle_functor_name(MR_type_ctor_layout(type_ctor_info).
                MR_layout_notag->MR_notag_functor_name);

#ifdef  EXPAND_ARGS_FIELD
            expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
            expand_info->EXPAND_ARGS_FIELD.arg_values = data_word_ptr;
            expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;
            expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos = MR_TRUE;
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                MR_GC_NEW_ARRAY(MR_TypeInfo, 1);
            expand_info->EXPAND_ARGS_FIELD.arg_type_infos[0] =
                MR_pseudo_type_info_is_ground(
                    MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                        MR_notag_functor_arg_type);
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
  #ifdef    EXPAND_NAMED_ARG
            if (MR_type_ctor_layout(type_ctor_info).MR_layout_notag
                    ->MR_notag_functor_arg_name != NULL
               && MR_streq(chosen_name, MR_type_ctor_layout(type_ctor_info).
                    MR_layout_notag->MR_notag_functor_arg_name))
            {
                chosen = 0;
            }
  #endif    // EXPAND_NAMED_ARG

            if (chosen == 0) {
                expand_info->chosen_index_exists = MR_TRUE;
                expand_info->chosen_value_ptr = data_word_ptr;
                expand_info->chosen_arg_locn = NULL;
                expand_info->chosen_type_info =
                    MR_pseudo_type_info_is_ground(
                        MR_type_ctor_layout(type_ctor_info).MR_layout_notag
                            ->MR_notag_functor_arg_type);
            } else {
                expand_info->chosen_index_exists = MR_FALSE;
            }
#endif  // EXPAND_ONE_ARG
            return;

        case MR_TYPECTOR_REP_EQUIV:
            {
                MR_TypeInfo eqv_type_info;

                eqv_type_info = MR_create_type_info(
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).MR_layout_equiv);
                EXPAND_FUNCTION_NAME(eqv_type_info, data_word_ptr, noncanon,
                    EXTRA_ARGS expand_info);
            }
            return;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            EXPAND_FUNCTION_NAME(MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv),
                data_word_ptr, noncanon, EXTRA_ARGS expand_info);
            return;

        case MR_TYPECTOR_REP_INT:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "d",
                    (MR_Integer) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_UINT:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu",
                    (MR_Unsigned) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_INT8:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "di8",
                    (MR_Integer) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_UINT8:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu8",
                    (MR_Unsigned) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_INT16:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "di16",
                    (MR_Integer) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_UINT16:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu16",
                    (MR_Unsigned) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_INT32:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "di32",
                    (MR_Integer) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_UINT32:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "%" MR_INTEGER_LENGTH_MODIFIER "uu32",
                    (MR_Unsigned) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;
        
        case MR_TYPECTOR_REP_INT64:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                int64_t i64;
                char    *str;

                data_word = *data_word_ptr;
                i64 = MR_word_to_int64(data_word);
                sprintf(buf, "%" PRId64 "i64", i64);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;
        
        case MR_TYPECTOR_REP_UINT64:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word     data_word;
                char        buf[500];
                uint64_t    u64;
                char        *str;

                data_word = *data_word_ptr;
                u64 = MR_word_to_uint64(data_word);
                sprintf(buf, "%" PRIu64 "u64", u64);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_CHAR:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                char    buf[8];
                MR_Word data_word;
                const char  *str_ptr;
                char        *str;

                data_word = *data_word_ptr;
                // XXX what should we do with other non-printable characters.
                switch (data_word) {
                    case '\\': str_ptr = "'\\\\'"; break;
                    case '\'': str_ptr = "'\\''"; break;
                    case '\a': str_ptr = "'\\a'";  break;
                    case '\b': str_ptr = "'\\b'";  break;
                    case '\r': str_ptr = "'\\r'";  break;
                    case '\f': str_ptr = "'\\f'";  break;
                    case '\t': str_ptr = "'\\t'";  break;
                    case '\n': str_ptr = "'\\n'";  break;
                    case '\v': str_ptr = "'\\v'";  break;
                    default:
                        sprintf(buf, "\'%c\'", (char) data_word);
                        str_ptr = buf;
                }
                MR_make_aligned_string_copy_saved_hp(str, str_ptr, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_FLOAT:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word     data_word;
                char        buf[MR_SPRINTF_FLOAT_BUF_SIZE];
                MR_Float    f;
                char        *str;

                data_word = *data_word_ptr;
                f = MR_word_to_float(data_word);
                MR_sprintf_float(buf, f);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_STRING:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                // XXX Should escape characters correctly.
                MR_Word data_word;
                char    *str;

                data_word = *data_word_ptr;
                MR_make_aligned_string_copy_saved_hp_quote(str,
                        (MR_String) data_word, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_BITMAP:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                MR_String str;

                data_word = *data_word_ptr;
                str = MR_bitmap_to_quoted_string_saved_hp(
                        (MR_ConstBitmapPtr) data_word, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

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

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
                if (num_args > max_arity) {
                    expand_info->limit_reached = MR_TRUE;
                } else
  #endif    // EXPAND_APPLY_LIMIT
                {
                    MR_TypeInfo *type_params;

                    type_params =
                        MR_materialize_closure_type_params(closure);
                    expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
                    expand_info->EXPAND_ARGS_FIELD.arg_values =
                        &closure->MR_closure_hidden_args_0[0];
  #ifdef MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS
                    // If hidden arguments may have been reordered, create an
                    // new vector with arguments in the correct order.

                    if (num_r_args != 0 && num_f_args != 0) {
                        int r_offset = 0;
                        int f_offset = num_r_args;

                        expand_info->EXPAND_ARGS_FIELD.arg_values =
                            MR_GC_NEW_ARRAY(MR_Word, num_args);
                        for (i = 0; i < num_args; i++) {
                            MR_PseudoTypeInfo arg_pti;
                            int offset;

                            arg_pti = closure_layout->
                                MR_closure_arg_pseudo_type_info[i];
                            if (MR_unify_pseudo_type_info_float(arg_pti)) {
                                offset = f_offset++;
                            } else {
                                offset = r_offset++;
                            }
                            expand_info->EXPAND_ARGS_FIELD.arg_values[i] =
                                closure->MR_closure_hidden_args_0[offset];
                        }
                    }
  #endif    // MR_MAY_REORDER_CLOSURE_HIDDEN_ARGS
                    expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;
                    expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_GC_NEW_ARRAY(MR_TypeInfo, num_args);
                    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos =
                        MR_TRUE;
                    for (i = 0; i < num_args; i++) {
                        expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                            MR_create_type_info(type_params,
                                closure_layout->
                                    MR_closure_arg_pseudo_type_info[i]);
                    }
                    if (type_params != NULL) {
                        MR_free(type_params);
                    }
                }
#endif  // EXPAND_ARGS_FIELD

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
                    expand_info->chosen_value_ptr =
                        &closure->MR_closure_hidden_args_0[offset];
                    expand_info->chosen_arg_locn = NULL;
                    // The following code could be improved.
                    type_params = MR_materialize_closure_type_params(closure);
                    expand_info->chosen_type_info =
                        MR_create_type_info(type_params,
                            closure_layout->
                                MR_closure_arg_pseudo_type_info[chosen]);
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
            }

            return;

        case MR_TYPECTOR_REP_TUPLE:
            expand_info->arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
            handle_functor_number(0);
            handle_functor_name("{}");

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
            if (expand_info->arity > max_arity) {
                expand_info->limit_reached = MR_TRUE;
            } else
  #endif    // EXPAND_APPLY_LIMIT
            {
                expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
                expand_info->EXPAND_ARGS_FIELD.arg_values =
                    (MR_Word *) *data_word_ptr;
                expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;

                // Type-infos are normally counted from one, but
                // the users of this vector count from zero.

                expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info) + 1;
            }
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
            if (0 <= chosen && chosen < expand_info->arity) {
                MR_Word *arg_vector;

                arg_vector = (MR_Word *) *data_word_ptr;
                expand_info->chosen_index_exists = MR_TRUE;
                expand_info->chosen_value_ptr = &arg_vector[chosen];
                expand_info->chosen_arg_locn = NULL;
                expand_info->chosen_type_info =
                    MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info)[chosen + 1];
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
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "c_pointer(0x%lX)", (long) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

        case MR_TYPECTOR_REP_STABLE_C_POINTER:
#ifdef  EXPAND_FUNCTOR_FIELD
            {
                MR_Word data_word;
                char    buf[500];
                char    *str;

                data_word = *data_word_ptr;
                sprintf(buf, "stable_c_pointer(0x%lX)", (long) data_word);
                MR_make_aligned_string_copy_saved_hp(str, buf, NULL);
                expand_info->EXPAND_FUNCTOR_FIELD = str;
            }
#endif  // EXPAND_FUNCTOR_FIELD

            handle_zero_arity_args();
            return;

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

                data_type_ctor_info =
                    MR_TYPEINFO_GET_TYPE_CTOR_INFO(data_type_info);
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
                // Switch from 1-based to 0-based array indexing.
                arg_type_infos++;

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
                if (num_args > max_arity) {
                    expand_info->limit_reached = MR_TRUE;
                } else
  #endif    // EXPAND_APPLY_LIMIT
                {
                    int i;

                    expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
                    expand_info->EXPAND_ARGS_FIELD.arg_values = arg_type_infos;
                    expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;

                    expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_GC_NEW_ARRAY(MR_TypeInfo, num_args);
                    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos =
                        MR_TRUE;
                    for (i = 0; i < num_args ; i++) {
                        // The arguments of a type_info are themselves of type
                        // ``type_info''.

                        expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                            type_info;
                    }
                }
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
                if (0 <= chosen && chosen < expand_info->arity) {
                    MR_Word *arg_vector;

                    arg_vector = (MR_Word *) data_type_info;
                    expand_info->chosen_index_exists = MR_TRUE;
                    expand_info->chosen_value_ptr = &arg_type_infos[chosen];
                    expand_info->chosen_arg_locn = NULL;
                    expand_info->chosen_type_info = type_info;
                } else {
                    expand_info->chosen_index_exists = MR_FALSE;
                }
#endif  // EXPAND_ONE_ARG
            }

            return;

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
                // Switch from 1-based to 0-based array indexing.
                arg_type_infos++;

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
                if (num_args > max_arity) {
                    expand_info->limit_reached = MR_TRUE;
                } else
  #endif    // EXPAND_APPLY_LIMIT
                {
                    int i;

                    expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
                    expand_info->EXPAND_ARGS_FIELD.arg_values = arg_type_infos;
                    expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;

                    expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_GC_NEW_ARRAY(MR_TypeInfo, num_args);
                    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos =
                        MR_TRUE;
                    for (i = 0; i < num_args ; i++) {
                        // The arguments of a pseudo_type_info are themselves
                        // of type ``pseudo_type_info''.

                        expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                            type_info;
                    }
                }
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
                if (0 <= chosen && chosen < expand_info->arity) {
                    MR_Word *arg_vector;

                    arg_vector = (MR_Word *) data_pseudo_type_info;
                    expand_info->chosen_index_exists = MR_TRUE;
                    expand_info->chosen_value_ptr = &arg_type_infos[chosen];
                    expand_info->chosen_arg_locn = NULL;
                    expand_info->chosen_type_info = type_info;
                } else {
                    expand_info->chosen_index_exists = MR_FALSE;
                }
#endif  // EXPAND_ONE_ARG
            }

            return;

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
            }

            return;

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
            }

            return;

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
                MR_ArrayType    *array;

                array = (MR_ArrayType *) *data_word_ptr;
                expand_info->arity = array->size;

                handle_functor_name("<<array>>");

#ifdef  EXPAND_ARGS_FIELD
  #ifdef    EXPAND_APPLY_LIMIT
                if (expand_info->arity > max_arity) {
                    expand_info->limit_reached = MR_TRUE;
                } else
  #endif    // EXPAND_APPLY_LIMIT
                {
                    MR_TypeInfoParams   params;
                    int                 i;

                    params = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
                    expand_info->EXPAND_ARGS_FIELD.num_extra_args = 0;
                    expand_info->EXPAND_ARGS_FIELD.arg_values =
                        &array->elements[0];
                    expand_info->EXPAND_ARGS_FIELD.arg_locns = NULL;
                    expand_info->EXPAND_ARGS_FIELD.can_free_arg_type_infos =
                        MR_TRUE;
                    expand_info->EXPAND_ARGS_FIELD.arg_type_infos =
                        MR_GC_NEW_ARRAY(MR_TypeInfo, array->size);
                    for (i = 0; i < array->size; i++) {
                        expand_info->EXPAND_ARGS_FIELD.arg_type_infos[i] =
                            params[1];
                    }
                }
#endif  // EXPAND_ARGS_FIELD

#ifdef  EXPAND_ONE_ARG
                if (0 <= chosen && chosen < array->size) {
                    MR_TypeInfoParams   params;

                    params = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
                    expand_info->chosen_value_ptr = &array->elements[chosen];
                    expand_info->chosen_arg_locn = NULL;
                    expand_info->chosen_type_info = params[1];
                    expand_info->chosen_index_exists = MR_TRUE;
                } else {
                    expand_info->chosen_index_exists = MR_FALSE;
                }
#endif  // EXPAND_ONE_ARG
            }
            return;

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
#ifdef  MR_HAVE_SNPRINTF
                snprintf(buf, MR_FOREIGN_NAME_BUF_SIZE,
                    "<<foreign(%s, %p)>>",
                    type_ctor_info->MR_type_ctor_name,
                    (void *) *data_word_ptr);
#else
                sprintf(buf,
                    "<<foreign(%s, %p)>>",
                    type_ctor_info->MR_type_ctor_name,
                    (void *) *data_word_ptr);
#endif
                // The contents of the memory occupied by buf may change.
                copy_and_handle_functor_name(buf);
                handle_zero_arity_args();
            }
            return;

        case MR_TYPECTOR_REP_STABLE_FOREIGN:
            {
                char buf[MR_FOREIGN_NAME_BUF_SIZE];
#ifdef  MR_HAVE_SNPRINTF
                snprintf(buf, MR_FOREIGN_NAME_BUF_SIZE,
                    "<<stable_foreign(%s, %p)>>",
                    type_ctor_info->MR_type_ctor_name,
                    (void *) *data_word_ptr);
#else
                sprintf(buf,
                    "<<stable_foreign(%s, %p)>>",
                    type_ctor_info->MR_type_ctor_name,
                    (void *) *data_word_ptr);
#endif
                // The contents of the memory occupied by buf may change.
                copy_and_handle_functor_name(buf);
                handle_zero_arity_args();
            }
            return;

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
#undef  handle_functor_number
#undef  handle_type_functor_number
#undef  handle_noncanonical_name
#undef  handle_type_ctor_name
#undef  handle_zero_arity_args
#undef  handle_zero_arity_all_args
#undef  handle_zero_arity_one_arg
#undef  higher_order_test
