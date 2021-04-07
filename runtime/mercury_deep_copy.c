// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2006 The University of Melbourne.
// Copyright (C) 2016, 2018, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module defines the MR_deep_copy() functions.
//
// Deep copy is used for a number of different purposes. Each variant
// has the same basic control structure, but differs in how memory
// is allocated, or whether forwarding pointers are left behind.

#include "mercury_imp.h"
#include "mercury_builtin_types.h"
#include "mercury_deep_copy.h"
#include "mercury_type_info.h"
#include "mercury_ho_call.h"
#include "mercury_layout_util.h"
#include "mercury_memory.h"
#include "mercury_accurate_gc.h"
#include "mercury_deconstruct_macros.h"

// MR_deep_copy(): see mercury_deep_copy.h for documentation.

#undef  in_range
#define in_range(X)             (lower_limit == NULL || \
                                    ((X) >= lower_limit && (X) <= upper_limit))

#undef  copy
#define copy                    MR_deep_copy

#undef  copy_arg
#define copy_arg                MR_deep_copy_arg

#undef  copy_type_info
#define copy_type_info          MR_deep_copy_type_info

#undef  copy_pseudo_type_info
#define copy_pseudo_type_info   MR_deep_copy_pseudo_type_info

#undef  copy_typeclass_info
#define copy_typeclass_info     MR_deep_copy_typeclass_info

#undef  if_forwarding_pointer
#define if_forwarding_pointer(Data, ACTION)

#undef  leave_forwarding_pointer
#define leave_forwarding_pointer(Data, Offset, NewData)

#undef  found_out_of_range_pointer
#define found_out_of_range_pointer(Data)

#include "mercury_deep_copy_body.h"

// agc_deep_copy(): see mercury_deep_copy.h for documentation.

#ifdef MR_NATIVE_GC

// in_range() is true iff X is in the from-space.
#undef  in_range
#define in_range(X)             ((X) >= lower_limit && (X) <= upper_limit)

#undef  copy
#define copy                    MR_agc_deep_copy

#undef  copy_arg
#define copy_arg                MR_agc_deep_copy_arg

#undef  copy_type_info
#define copy_type_info          MR_agc_deep_copy_type_info

#undef  copy_pseudo_type_info
#define copy_pseudo_type_info   MR_agc_deep_copy_pseudo_type_info

#undef  copy_typeclass_info
#define copy_typeclass_info     MR_agc_deep_copy_typeclass_info

#ifdef MR_DEBUG_AGC_FORWARDING
  #define FORWARD_DEBUG_MSG(Msg, Data)                                        \
    fprintf(stderr, Msg, Data)
#else
  #define FORWARD_DEBUG_MSG(Msg, Data) ((void)0)
#endif

// This points to a bitmap, which is used to record which objects
// have already been copied and now hold forwarding pointers.

MR_Word *MR_has_forwarding_pointer;

#define mark_as_forwarding_pointer(Data)                                      \
    do {                                                                      \
        size_t fwdptr_offset = (MR_Word *)(Data) - (MR_Word *) lower_limit;   \
        size_t fwdptr_word = fwdptr_offset / MR_WORDBITS;                     \
        size_t fwdptr_bit = fwdptr_offset % MR_WORDBITS;                      \
        MR_has_forwarding_pointer[fwdptr_word] |= (1 << fwdptr_bit);          \
    } while (0)

#undef  if_forwarding_pointer
#define if_forwarding_pointer(Data, ACTION)                                   \
    do {                                                                      \
        size_t fwdptr_offset = (MR_Word *)(Data) - (MR_Word *) lower_limit;   \
        size_t fwdptr_word = fwdptr_offset / MR_WORDBITS;                     \
        size_t fwdptr_bit = fwdptr_offset % MR_WORDBITS;                      \
        if (MR_has_forwarding_pointer[fwdptr_word] & (1 << fwdptr_bit)) {     \
            ACTION;                                                           \
        }                                                                     \
    } while (0)

#undef  leave_forwarding_pointer
#define leave_forwarding_pointer(Data, Offset, NewData)                       \
    do {                                                                      \
        FORWARD_DEBUG_MSG("forwarding to %lx\n", (long) NewData);             \
        * (((MR_Word *) Data) + Offset) = NewData;                            \
        mark_as_forwarding_pointer(Data);                                     \
    } while (0)

#undef  found_out_of_range_pointer
#define found_out_of_range_pointer(Data)                                      \
            FORWARD_DEBUG_MSG("not on this heap: %lx\n", (long) Data)

#include "mercury_deep_copy_body.h"

#endif

////////////////////////////////////////////////////////////////////////////

#define SWAP(val1, val2, type)                                                \
    do {                                                                      \
        type swap_tmp;                                                        \
        swap_tmp = (val1);                                                    \
        (val1) = (val2);                                                      \
        (val2) = swap_tmp;                                                    \
    } while (0)

// The above macro won't suffice when we want to swap the pointers to the
// heap and the global heap since it will cause gcc to emit warnings about
// lvalue casts being deprecated. In that case we use the following macro.

#define SWAP_HEAP_AND_GLOBAL_HEAP                                             \
    do {                                                                      \
        MR_Word *swap_tmp;                                                    \
        swap_tmp = MR_hp;                                                     \
        MR_hp_word = (MR_Word) MR_global_hp;                                  \
        MR_global_hp = swap_tmp;                                              \
    } while (0)

#ifdef MR_MIGHT_RECLAIM_HP_ON_FAILURE

// MR_make_long_lived(): see mercury_deep_copy.h for documentation.

MR_Word
MR_make_long_lived(MR_Word term, MR_TypeInfo type_info, MR_Word *lower_limit)
{
    MR_Word result;

    MR_restore_transient_hp();  // Because we play with MR_hp.

    if (lower_limit < MR_ENGINE(MR_eng_heap_zone)->MR_zone_bottom ||
        lower_limit > MR_ENGINE(MR_eng_heap_zone)->MR_zone_top)
    {
        lower_limit = MR_ENGINE(MR_eng_heap_zone)->MR_zone_bottom;
    }

    // Temporarily swap the heap with the global heap.
    SWAP(MR_ENGINE(MR_eng_heap_zone), MR_ENGINE(MR_eng_global_heap_zone),
        MR_MemoryZone *);
    SWAP_HEAP_AND_GLOBAL_HEAP;

    // Copy values from the heap to the global heap.
    MR_save_transient_hp();
    result = MR_deep_copy(term, type_info, lower_limit,
        MR_ENGINE(MR_eng_global_heap_zone)->MR_zone_top);
    MR_restore_transient_hp();

    // swap the heap and global heap back again.
    SWAP(MR_ENGINE(MR_eng_heap_zone), MR_ENGINE(MR_eng_global_heap_zone),
        MR_MemoryZone *);
    SWAP_HEAP_AND_GLOBAL_HEAP;

    MR_save_transient_hp(); // Because we played with MR_hp.

    return result;
}

#endif  // MIGHT_RECLAIM_HP_ON_FAILURE
