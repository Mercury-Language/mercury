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

///////////////////

// Check for attempts to deconstruct a non-canonical type.
// Such deconstructions must be cc_multi, which is why we treat
// violations of this as runtime errors in det deconstruction predicates.
// (There ought to be cc_multi versions of those predicates.)

#define MR_abort_if_type_is_noncanonical(ei, msg)                           \
    do {                                                                    \
        if ((ei).non_canonical_type) {                                      \
            MR_fatal_error(msg);                                            \
        }                                                                   \
    } while (0)

#define MR_noncanon_msg(predname)                                           \
    "called " predname " for non-canonical type"

///////////////////

// Extract fields of some of the expand_info structures.

#define MR_deconstruct_get_functor(ei, functor_field, var)                  \
    do {                                                                    \
        MR_make_aligned_string(var, (ei).functor_field);                    \
    } while (0)

#define MR_deconstruct_get_functor_number(ei, var)                          \
    do {                                                                    \
        var = (ei).functor_number;                                          \
    } while (0)

#define MR_deconstruct_get_arity(ei, var)                                   \
    do {                                                                    \
        var = (ei).arity;                                                   \
    } while (0)

#define MR_deconstruct_get_arg_univs_list(ei, var)                          \
    do {                                                                    \
        var = (ei).arg_univs_list;                                          \
    } while (0)

///////////////////

// Given the type_info for the type of a whole term, the functor
// descriptor of its top function symbol, the argument vector of its
// arguments, and the number of an argument, return the type_info
// of that argument.

#define MR_get_arg_type_info(ti, fdesc, argvec, argnum)                     \
        ( MR_arg_type_may_contain_var(functor_desc, argnum) ?               \
            MR_create_type_info_maybe_existq(                               \
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti),                 \
                fdesc->MR_du_functor_arg_types[argnum], argvec, fdesc)      \
        :                                                                   \
            MR_pseudo_type_info_is_ground(                                  \
                fdesc->MR_du_functor_arg_types[argnum])                     \
        )

#define MR_get_arg_type_info_alloc(ti, fdesc, argvec, argnum, alloc)        \
        ( MR_arg_type_may_contain_var(functor_desc, argnum) ?               \
            MR_make_type_info_maybe_existq(                                 \
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti),                 \
                fdesc->MR_du_functor_arg_types[argnum], argvec, fdesc,      \
                &alloc)                                                     \
        :                                                                   \
            MR_pseudo_type_info_is_ground(                                  \
                fdesc->MR_du_functor_arg_types[argnum])                     \
        )

///////////////////

// Convert a 64-bit argument to a word, boxing it on 32-bit architectures,
// and aborting if the address of that 64-bit argument does not exist.
// The latter would happen if an arg_locn mistakenly said that such a 64-bit
// argument occurred inside a tagword, which should never happen.

#ifdef MR_HIGHLEVEL_CODE
  #define MR_float_to_value(f, v)   v = (MR_Word) MR_box_float(f)
  #define MR_int64_to_value(i, v)   v = (MR_Word) MR_box_int64(i)
  #define MR_uint64_to_value(u, v)  v = (MR_Word) MR_box_uint64(u)
#else
  #define MR_float_to_value(f, v)   v = MR_float_to_word(f)
  #define MR_int64_to_value(i, v)   v = MR_int64_to_word(i)
  #define MR_uint64_to_value(u, v)  v = MR_uint64_to_word(u)
#endif

#ifdef MR_BOXED_FLOAT
  #define MR_word_pair_to_float_value(have_addr, addr, val)                 \
        do {                                                                \
            MR_Float    flt;                                                \
                                                                            \
            if (have_addr) {                                                \
                flt = MR_float_from_dword(addr[0], addr[1]);                \
                MR_float_to_value(flt, val);                                \
            } else {                                                        \
                MR_fatal_error("double-word float in tagword");             \
            }                                                               \
        } while (0)
#else
  #define MR_word_pair_to_float_value(have_addr, addr, val)                 \
        do {                                                                \
            MR_fatal_error("double-word floats "                            \
                "should not exist in this grade");                          \
        } while (0)
#endif

#ifdef MR_BOXED_INT64S
  #define MR_word_pair_to_int64_value(have_addr, addr, val)                 \
        do {                                                                \
            MR_Float    flt;                                                \
                                                                            \
            if (have_addr) {                                                \
                flt = MR_int64_from_dword(addr[0], addr[1]);                \
                MR_int64_to_value(flt, val);                                \
            } else {                                                        \
                MR_fatal_error("double-word int64 in tagword");             \
            }                                                               \
        } while (0)
  #define MR_word_pair_to_uint64_value(have_addr, addr, val)                \
        do {                                                                \
            MR_Float    flt;                                                \
                                                                            \
            if (have_addr) {                                                \
                flt = MR_uint64_from_dword(addr[0], addr[1]);               \
                MR_uint64_to_value(flt, val);                               \
            } else {                                                        \
                MR_fatal_error("double-word uint64 in tagword");            \
            }                                                               \
        } while (0)
#else
  #define MR_word_pair_to_int64_value(have_addr, addr, val)                 \
        do {                                                                \
            MR_fatal_error("double-word int64s "                            \
                "should not exist in this grade");                          \
        } while (0)
  #define MR_word_pair_to_uint64_value(have_addr, addr, val)                \
        do {                                                                \
            MR_fatal_error("double-word uint64s "                           \
                "should not exist in this grade");                          \
        } while (0)
#endif

///////////////////

// Given the arg_locn of an argument, and either the address at which
// it can be found (if have_addr is true) or the value of the tagword
// containing it (if have_addr is false), return its value. If the argument's
// size is exactly one word, then set wsa_ptr (word-sized-argument) to
// its address; if its size is anything else, then set wsa_ptr to NULL.
// This is used by tabling and by store.m to reserve unique word addresses
// for (copies of) the arguments that do not naturally have them.
//
// This macro should be used only by MR_get_non_tagword_arg_value and
// MR_get_tagword_arg_value.

#define MR_get_arg_value(arg_locn, have_addr, word_addr, word, value, wsa_ptr) \
    switch ((arg_locn).MR_arg_bits) {                                       \
                                                                            \
    case -1:                                                                \
        /* MR_arg_bits == -1: arg is a double stored in two words. */       \
        MR_word_pair_to_float_value(have_addr, word_addr, value);           \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -2:                                                                \
        /* MR_arg_bits == -2: arg is an int64 stored in two words. */       \
        MR_word_pair_to_int64_value(have_addr, word_addr, value);           \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -3:                                                                \
        /* MR_arg_bits == -3: arg is a uint64 stored in two words. */       \
        MR_word_pair_to_uint64_value(have_addr, word_addr, value);          \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -4:                                                                \
        /* MR_arg_bits == -4: arg is an int8 subword arg. */                \
        value = (MR_Word) (int8_t)                                          \
            (word >> (arg_locn).MR_arg_shift) & ((MR_Word) 0xff);           \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -5:                                                                \
        /* MR_arg_bits == -5: arg is a uint8 subword arg. */                \
        value = (MR_Word) (uint8_t)                                         \
            (word >> (arg_locn).MR_arg_shift) & ((MR_Word) 0xff);           \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -6:                                                                \
        /* MR_arg_bits == -6: arg is an int16 subword arg. */               \
        value = (MR_Word) (int16_t)                                         \
            (word >> (arg_locn).MR_arg_shift) & ((MR_Word) 0xffff);         \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -7:                                                                \
        /* MR_arg_bits == -7: arg is a uint16 subword arg. */               \
        value = (MR_Word) (uint16_t)                                        \
            (word >> (arg_locn).MR_arg_shift) & ((MR_Word) 0xffff);         \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -8:                                                                \
        /* MR_arg_bits == -8: arg is an int32 subword arg. */               \
        value = (MR_Word) (int32_t)                                         \
            (word >> (arg_locn).MR_arg_shift) & ((MR_Word) 0xffffffff);     \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -9:                                                                \
        /* MR_arg_bits == -9: arg is a uint32 subword arg. */               \
        value = (MR_Word) (uint32_t)                                        \
            (word >> (arg_locn).MR_arg_shift) & ((MR_Word) 0xffffffff);     \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    case -10:                                                               \
        /* MR_arg_bits == -10: arg is a value of a dummy type. */           \
        value = 0;                                                          \
        wsa_ptr = NULL;                                                     \
        break;                                                              \
                                                                            \
    default:                                                                \
        if (arg_locn.MR_arg_bits == 0) {                                    \
            /* The arg is a full word value. */                             \
            value = word;                                                   \
            wsa_ptr = word_addr;                                            \
        } else if (arg_locn.MR_arg_bits > 0) {                              \
            /* The arg is a packed enumeration value. */                    \
            value = (word >> (arg_locn).MR_arg_shift)                       \
                & ((MR_Word) (1 << (arg_locn).MR_arg_bits) - 1);            \
            wsa_ptr = NULL;                                                 \
        } else {                                                            \
            /* The only negative values of MR_arg_bits that make sense */   \
            /* are the ones we have cases for above. */                     \
            MR_fatal_error("unexpected negative value of MR_arg_bits");     \
        }                                                                   \
        break;                                                              \
    }

///////////////////

// Given the arg_locn of an argument and the starting address of
// the non-extra, non-tagword arguments of its memory cell,
// return the value of the argument. Also return wsa_ptr, whose meaning
// and purpose is described above.
//
// The offset of the argument is taken from the arg_locn. This offset
// will be -1 for arguments stored in the tagword of a remote secondary tag.
// This works because we do such packing only for function symbols that
// have no type_infos or typeclass_infos between the tagword and the
// argument values themselves.

#define MR_get_non_tagword_arg_value(arg_locn, av, value, wsa_ptr)          \
    do {                                                                    \
        MR_int_least16_t    off = (arg_locn).MR_arg_offset;                 \
        MR_Word             *word_addr = &((MR_Word *) av)[off];            \
        MR_get_arg_value(arg_locn, MR_TRUE, word_addr, (*word_addr),        \
            value, wsa_ptr);                                                \
    } while (0)

// Given the arg_locn of an argument and the tagword it occurs in,
// return the value of the argument. Such arguments cannot be word sized.

#define MR_get_tagword_arg_value(arg_locn, tagword, value)                  \
    do {                                                                    \
        MR_Word             *wsa_ptr; /* ignored */                         \
        MR_get_arg_value(arg_locn, MR_FALSE, ((MR_Word *) NULL), tagword,   \
            value, wsa_ptr);                                                \
    } while (0)

#endif // MERCURY_DECONSTRUCT_MACROS_H
