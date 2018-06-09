// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002-2004, 2007, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_deconstruct_macros.h
//
// This file defines macros for performing tasks that are useful when
// deconstructing terms,

#ifndef MERCURY_DECONSTRUCT_MACROS_H
#define MERCURY_DECONSTRUCT_MACROS_H

// Check for attempts to deconstruct a non-canonical type.
// Such deconstructions must be cc_multi, which is why we treat
// violations of this as runtime errors in det deconstruction predicates.
// (There ought to be cc_multi versions of those predicates.)

#define MR_abort_if_type_is_noncanonical(ei, msg)                       \
    do {                                                                \
        if ((ei).non_canonical_type) {                                  \
            MR_fatal_error(msg);                                        \
        }                                                               \
    } while (0)

#define MR_noncanon_msg(predname)                                       \
    "called " predname " for non-canonical type"

#define MR_deconstruct_get_functor(ei, functor_field, var)              \
    do {                                                                \
        MR_make_aligned_string(var, (ei).functor_field);                \
    } while (0)

#define MR_deconstruct_get_functor_number(ei, var)                      \
    do {                                                                \
        var = (ei).functor_number;                                      \
    } while (0)

#define MR_deconstruct_get_arity(ei, var)                               \
    do {                                                                \
        var = (ei).arity;                                               \
    } while (0)

#define MR_deconstruct_get_arg_list(ei, args_field, var)                \
    do {                                                                \
        int     i;                                                      \
                                                                        \
        var = MR_list_empty_msg(MR_ALLOC_ID);                           \
        i = (ei).arity;                                                 \
                                                                        \
        while (--i >= 0) {                                              \
            MR_Word arg;                                                \
            MR_Word val;                                                \
                                                                        \
            if ((ei).args_field.arg_locns == NULL) {                    \
                val = (ei).args_field.arg_values[i +                    \
                    (ei).args_field.num_extra_args];                    \
            } else {                                                    \
                const MR_DuArgLocn *locn =                              \
                    &(ei).args_field.arg_locns[i];                      \
                MR_Word *argp = &(ei).args_field.arg_values[            \
                    locn->MR_arg_offset +                               \
                    (ei).args_field.num_extra_args];                    \
                val = MR_arg_value(argp, locn);                         \
            }                                                           \
                                                                        \
            /* Create an argument on the heap. */                       \
            MR_new_univ_on_hp(arg,                                      \
                (ei).args_field.arg_type_infos[i], val);                \
                                                                        \
            /* Join the argument to the front of the list. */           \
            var = MR_univ_list_cons_msg(arg, var, MR_ALLOC_ID);         \
        }                                                               \
    } while (0)

// Free any arg_type_infos allocated by the MR_expand variant.
// Should be called after we have used them for the last time.

#define MR_deconstruct_free_allocated_arg_type_infos(ei, args_field)    \
    do {                                                                \
        if ((ei).args_field.can_free_arg_type_infos) {                  \
            MR_GC_free((ei).args_field.arg_type_infos);                 \
        }                                                               \
    } while (0)

#endif // MERCURY_DECONSTRUCT_MACROS_H
