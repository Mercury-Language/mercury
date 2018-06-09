// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2006, 2010-2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_heap.h - definitions for manipulating the Mercury heap.
//
// This file defines several levels of macros for allocating space on
// the Mercury heap.
//
// It would be simpler if all these macros expanded out to statements. However,
// many cannot, since they must be usable inside expressions. The ultimate
// reason for this is MR_float_to_word, which is used not just as an operand
// in expressions, but also as an initializer in static cells generated
// by the compiler.

#ifndef MERCURY_HEAP_H
#define MERCURY_HEAP_H

#include "mercury_conf.h"               // for MR_CONSERVATIVE_GC
#include "mercury_conf_param.h"         // for MR_RECORD_TERM_SIZES
#include "mercury_types.h"              // for `MR_Word'
#include "mercury_context.h"            // for min_heap_reclamation_point()
#include "mercury_heap_profile.h"       // for MR_record_allocation()
#include "mercury_deep_profiling.h"     // for MR_current_call_site_dynamic
#include "mercury_std.h"                // for MR_EXTERN_INLINE
#include "mercury_reg_workarounds.h"    // for MR_memcpy
#include "mercury_debug.h"              // for MR_debugtagoffsetincrhp*
#ifdef MR_HIGHLEVEL_CODE
  #include "mercury.h"                  // for MR_new_object()
#endif

#ifdef MR_CONSERVATIVE_GC
  #ifdef MR_HGC
    #include "mercury_hgc.h"
  #endif
  #ifdef MR_BOEHM_GC
    #define GC_I_HIDE_POINTERS
    #include "gc.h"
    #include "gc_mark.h"                // for GC_least_plausible_heap_addr
                                        // GC_greatest_plausible_heap_addr
  #endif
#endif

////////////////////////////////////////////////////////////////////////////

// The first level of heap allocation macros. These are concerned with
// the raw business of allocating memory, taking and restoring snapshots
// of the state of the heap, and recording profiling information for the
// profdeep grade and complexity experiments.

#ifdef MR_DEBUG_HEAP_ALLOC
  #define   MR_tag_offset_sanity_check(offset, count)                       \
            ( ((offset) >= (count))                                         \
                ? MR_fatal_error("MR_tag_offset_sanity_check failed")       \
                : ((void) 0)                                                \
            )
#else
  #define   MR_tag_offset_sanity_check(offset, count)                       \
            ((void) 0)
#endif

#ifdef  MR_RECORD_TERM_SIZES
  #define   MR_maybe_increment_complexity_counters(count)                   \
            (                                                               \
                MR_complexity_word_counter += (count),                      \
                MR_complexity_cell_counter += 1                             \
            )
#else
  #define   MR_maybe_increment_complexity_counters(count)                   \
            ((void) 0)
#endif

#if defined (MR_DEEP_PROFILING) && defined(MR_DEEP_PROFILING_MEMORY)
  #define   MR_profdeep_maybe_record_allocation(count)                      \
            (                                                               \
                MR_current_call_site_dynamic->MR_csd_own.MR_own_allocs += 1,\
                MR_current_call_site_dynamic->MR_csd_own.MR_own_words       \
                    += (count)                                              \
            )
#else
  #define   MR_profdeep_maybe_record_allocation(count)                      \
            ((void) 0)
#endif

#ifdef MR_CONSERVATIVE_GC

  #define   MR_tag_offset_incr_hp_base(dest, tag, offset, count, alloc,     \
                is_atomic)                                                  \
            (                                                               \
                MR_tag_offset_sanity_check((offset), (count)),              \
                MR_maybe_increment_complexity_counters((count)),            \
                MR_profdeep_maybe_record_allocation((count)),               \
                (dest) = (MR_Word) MR_mkword((tag), (MR_Word)               \
                    (((MR_Word *) alloc((count) * sizeof(MR_Word)))         \
                        + (offset))),                                       \
                MR_debug_tag_offset_incr_hp_base((dest), (tag), (offset),   \
                    (count), (is_atomic)),                                  \
                ((void) 0)                                                  \
            )

  #define   MR_tag_offset_incr_hp_n(dest, tag, offset, count)               \
            MR_tag_offset_incr_hp_base(dest, tag, offset, count,            \
                GC_MALLOC, 0)
  #define   MR_tag_offset_incr_hp_atomic(dest, tag, offset, count)          \
            MR_tag_offset_incr_hp_base(dest, tag, offset, count,            \
                GC_MALLOC_ATOMIC, 1)

  #ifdef MR_INLINE_ALLOC

    // The following stuff uses the macros in the `gc_inline.h' header file in
    // the Boehm garbage collector. They improve performance a little for
    // highly allocation-intensive programs (e.g. the `nrev' benchmark).
    // You will probably need to fool around with the `-I' options to get this
    // to work. Also, you must make sure that you compile with the same
    // setting for -DSILENT that the boehm_gc directory was compiled with.
    //
    // We only want to inline allocations if the allocation size is a
    // compile-time constant. This should be true for almost all the code that
    // we generate, but with GCC we can use the `__builtin_constant_p()'
    // extension to find out.
    //
    // The inline allocation macros are used only for allocating amounts
    // of less than 16 words, to avoid fragmenting memory by creating too
    // many distinct free lists. The garbage collector also requires that
    // if we are allocating more than one word, we round up to an even number
    // of words.

    #ifndef MR_GNUC
      // Without the gcc extensions __builtin_constant_p() and ({...}),
      // MR_INLINE_ALLOC would probably be a performance _loss_.

      #error "MR_INLINE_ALLOC requires the use of GCC"
    #endif
    #ifdef MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
      #error "MR_INLINE_ALLOC and MR_MPROF_PROFILE_MEMORY_ATTRIBUTION both defined"
    #endif

    #include "gc_inline.h"
    #define MR_tag_offset_incr_hp(dest, tag, offset, count)                 \
            ( __builtin_constant_p(count) && (count) < 16                   \
            ? ({                                                            \
                void    *temp;                                              \
                                                                            \
                MR_tag_offset_sanity_check((offset), (count)),              \
                MR_maybe_increment_complexity_counters((count)),            \
                MR_profdeep_maybe_record_allocation((count)),               \
                /* If size > 1, round up to an even number of words. */     \
                MR_Word num_words = ((count) == 1 ? 1 :                     \
                    2 * (((count) + 1) / 2));                               \
                GC_MALLOC_WORDS(temp, num_words);                           \
                temp = (void *) (((MR_Word *) temp) + (offset));            \
                (dest) = (MR_Word) MR_mkword((tag), temp);                  \
              })                                                            \
            : MR_tag_offset_incr_hp_n((dest), (tag), (offset), (count))     \
            )

  #else // !MR_INLINE_ALLOC

    #define MR_tag_offset_incr_hp(dest, tag, offset, count)                 \
            MR_tag_offset_incr_hp_n((dest), (tag), (offset), (count))

  #endif // MR_INLINE_ALLOC

  #define   MR_mark_hp(dest)        ((void) 0)
  #define   MR_restore_hp(src)      ((void) 0)
  #define   MR_free_heap(ptr)                                               \
            do {                                                            \
                MR_Word *tmp = (MR_Word *) (ptr);                           \
                if (MR_in_heap_range(tmp)) {                                \
                    GC_FREE(tmp);                                           \
                }                                                           \
            } while (0)

#else // not MR_CONSERVATIVE_GC

  #define   MR_tag_offset_incr_hp_base(dest, tag, offset, count, is_atomic) \
            (                                                               \
                MR_tag_offset_sanity_check((offset), (count)),              \
                MR_maybe_increment_complexity_counters((count)),            \
                MR_profdeep_maybe_record_allocation((count)),               \
                (dest) = (MR_Word) MR_mkword(tag, (MR_Word)                 \
                    (((MR_Word *) MR_hp) + (offset))),                      \
                MR_debug_tag_offset_incr_hp_base((dest), (tag), (offset),   \
                    (count), (is_atomic)),                                  \
                MR_hp_word = (MR_Word) (MR_hp + (count)),                   \
                MR_heap_overflow_check(),                                   \
                (void) 0                                                    \
            )

  #define   MR_tag_offset_incr_hp(dest, tag, offset, count)                 \
            MR_tag_offset_incr_hp_base((dest), (tag), (offset), (count), 0)
  #define   MR_tag_offset_incr_hp_atomic(dest, tag, offset, count)          \
            MR_tag_offset_incr_hp_base((dest), (tag), (offset), (count), 1)

  #define   MR_mark_hp(dest)      ((dest) = MR_hp_word)

  // When restoring MR_hp, we must make sure that we don't truncate the heap
  // further than it is safe to. We can only truncate it as far as
  // min_heap_reclamation_point. See the comments in mercury_context.h next to
  // the MR_set_min_heap_reclamation_point() macro.

  #define   MR_restore_hp(src)                                              \
            (                                                               \
                MR_hp_word = (MR_Word) (src),                               \
                (void) 0                                                    \
            )

#if 0
  #define   MR_restore_hp(src)                                              \
            (                                                               \
                MR_hp_word = (MR_Word)                                      \
                  ( (MR_Word) MR_min_hp_rec < (src) ?                       \
                  (src) : (MR_Word) MR_min_hp_rec ),                        \
                (void) 0                                                    \
            )
#endif

  #define   MR_free_heap(ptr)     ((void) 0)

#endif // not MR_CONSERVATIVE_GC

////////////////////////////////////////////////////////////////////////////

// The second level of heap allocation macros. These are concerned with
// recording profiling information for memory profiling grades. Memory
// attribution profiling adds an extra word at the start of each object.

#if defined(MR_MPROF_PROFILE_MEMORY)
  #define   MR_profmem_record_allocation(count, alloc_id, type)             \
            MR_record_allocation((count), (alloc_id), (type))
#else
  #define   MR_profmem_record_allocation(count, alloc_id, type)             \
            ((void) 0)
#endif

#if defined(MR_MPROF_PROFILE_MEMORY_ATTRIBUTION)
  #define   MR_profmem_attrib_word   (1)
  #define   MR_profmem_set_attrib(dest, tag, alloc_id)                      \
            ((MR_Word *) MR_strip_tag(dest))[-1] = (MR_Word) (alloc_id)
            // XXX This version causes gcc 4.4.4 on x86 to abort when
            // compiling mercury_bitmap.c.
            // MR_field((tag), (dest), 0) = (MR_Word) (alloc_id)

            // Hand-written code must set the MR_asi_type field at runtime.
            // When the type argument is NULL, as it is for generated code,
            // the C compiler can optimise away the condition and assignment.

  #define   MR_profmem_set_alloc_type(alloc_id, type)                       \
            ((alloc_id) != NULL && (type) != NULL &&                        \
             (((MR_AllocSiteInfo *) (alloc_id))->MR_asi_type = (type)))
#else
  #define   MR_profmem_attrib_word   (0)
  #define   MR_profmem_set_attrib(dest, tag, alloc_id)                      \
            ((void) 0)
  #define   MR_profmem_set_alloc_type(alloc_id, type)                       \
            ((void) 0)
#endif

#define     MR_tag_offset_incr_hp_msg(dest, tag, offset, count,             \
                alloc_id, type)                                             \
            (                                                               \
                MR_tag_offset_incr_hp((dest), (tag),                        \
                    (offset) + MR_profmem_attrib_word,                      \
                    (count) + MR_profmem_attrib_word),                      \
                MR_profmem_set_attrib((dest), (tag), (alloc_id)),           \
                MR_profmem_set_alloc_type((alloc_id), (type)),              \
                MR_profmem_record_allocation((count), (alloc_id), (type))   \
            )
#define     MR_tag_offset_incr_hp_atomic_msg(dest, tag, offset, count,      \
                alloc_id, type)                                             \
            (                                                               \
                MR_tag_offset_incr_hp_atomic((dest), (tag),                 \
                    (offset) + MR_profmem_attrib_word,                      \
                    (count) + MR_profmem_attrib_word),                      \
                MR_profmem_set_attrib((dest), (tag), (alloc_id)),           \
                MR_profmem_set_alloc_type((alloc_id), (type)),              \
                MR_profmem_record_allocation((count), (alloc_id), (type))   \
            )

////////////////////////////////////////////////////////////////////////////

// The third level of heap allocation macros. These are shorthands, supplying
// default values of some of the parameters of the previous macros.

#define     MR_tag_incr_hp(dest, tag, count)                                \
            MR_tag_offset_incr_hp((dest), (tag), 0, (count))
#define     MR_tag_incr_hp_atomic(dest, tag, count)                         \
            MR_tag_offset_incr_hp_atomic((dest), (tag), 0, (count))
#define     MR_tag_incr_hp_msg(dest, tag, count, alloc_id, type)            \
            MR_tag_offset_incr_hp_msg((dest), (tag), 0, (count),            \
                (alloc_id), (type))
#define     MR_tag_incr_hp_atomic_msg(dest, tag, count, alloc_id, type)     \
            MR_tag_offset_incr_hp_atomic_msg((dest), (tag), 0, (count),     \
                (alloc_id), (type))

// The MR_offset_incr_hp*() macros are defined in terms of the
// MR_tag_offset_incr_hp*() macros.

#define     MR_offset_incr_hp(dest, offset, count)                          \
            MR_tag_offset_incr_hp((dest), MR_mktag(0), (offset), (count))
#define     MR_offset_incr_hp_msg(dest, offset, count, alloc_id, type)      \
            MR_tag_offset_incr_hp_msg((dest), MR_mktag(0),                  \
                (offset), (count), (alloc_id), (type))
#define     MR_offset_incr_hp_atomic(dest, offset, count)                   \
            MR_tag_offset_incr_hp_atomic((dest), MR_mktag(0), (offset),     \
                (count))
#define     MR_offset_incr_hp_atomic_msg(dest, offset, count, alloc_id,     \
                type)                                                       \
            MR_tag_offset_incr_hp_atomic_msg((dest), MR_mktag(0), (offset), \
                (count), (alloc_id), (type))

#ifdef  MR_CONSERVATIVE_GC
            // We use `MR_hp_word' as a convenient temporary here.
  #define   MR_hp_alloc(count) (                                            \
                MR_offset_incr_hp(MR_hp_word, 0, (count)),                  \
                MR_hp_word = (MR_Word) (MR_hp + (count)),                   \
                (void) 0                                                    \
            )
  #define   MR_hp_alloc_atomic_msg(count, alloc_id, type) (                 \
                MR_offset_incr_hp_atomic_msg(MR_hp_word, 0, (count),        \
                    (alloc_id), (type)),                                    \
                MR_hp_word = (MR_Word) (MR_hp + (count)),                   \
                (void) 0                                                    \
            )
#else // !MR_CONSERVATIVE_GC

  #define   MR_hp_alloc(count)                                              \
            MR_offset_incr_hp(MR_hp_word, 0, (count))
  #define   MR_hp_alloc_atomic_msg(count, alloc_id, type)                   \
            MR_offset_incr_hp_atomic_msg(MR_hp_word, 0, (count),            \
                (alloc_id), (type))

#endif // MR_CONSERVATIVE_GC

////////////////////////////////////////////////////////////////////////////

// The fourth level of heap allocation macros. These implement the various
// cases of the incr_hp LLDS instruction, and look after recording term sizes.

#ifndef MR_RECORD_TERM_SIZES

#define     MR_incr_hp(dest, count)                                          \
            MR_offset_incr_hp((dest), 0, (count))
#define     MR_incr_hp_msg(dest, count, alloc_id, type)                      \
            MR_offset_incr_hp_msg((dest), 0, (count), (alloc_id), (type))
#define     MR_incr_hp_atomic(dest, count)                                   \
            MR_offset_incr_hp_atomic((dest), 0, (count))
#define     MR_incr_hp_atomic_msg(dest, count, alloc_id, type)               \
            MR_offset_incr_hp_atomic_msg((dest), 0, (count), (alloc_id),     \
                (type))

#endif

#define     MR_incr_hp_type(dest, typename)                                 \
            do {                                                            \
                MR_Word tmp;                                                \
                MR_tag_incr_hp(tmp, MR_mktag(0),                            \
                    (MR_bytes_to_words(sizeof(typename))));                 \
                (dest) = (typename *) tmp;                                  \
            } while (0)
#define     MR_incr_hp_type_msg(dest, typename, alloc_id, type)             \
            do {                                                            \
                MR_Word tmp;                                                \
                MR_tag_incr_hp_msg(tmp, MR_mktag(0),                        \
                    (MR_bytes_to_words(sizeof(typename))),                  \
                    (alloc_id), (type));                                    \
                (dest) = (typename *) tmp;                                  \
            } while (0)

// These are only used by the compiler in non-memory profiling grades,
// so do not have _msg equivalents. Avoid these in hand-written code.

#define     MR_alloc_heap(dest, count)                                      \
            MR_tag_offset_incr_hp((dest), MR_mktag(0), 0, (count))
#define     MR_alloc_heap_atomic(dest, count)                               \
            MR_tag_offset_incr_hp_atomic((dest), MR_mktag(0), 0, (count))
#define     MR_tag_alloc_heap(dest, tag, count)                             \
            MR_tag_offset_incr_hp((dest), MR_mktag(tag), 0, (count))
#define     MR_tag_alloc_heap_atomic(dest, tag, count)                      \
            MR_tag_offset_incr_hp_atomic((dest), MR_mktag(tag), 0, (count))

#ifdef MR_HIGHLEVEL_CODE

  // Term size profiling is not supported with MR_HIGHLEVEL_CODE.
  #define       MR_SIZE_SLOT_SIZE                           0
  #define       MR_cell_size(arity)                         0
  #define       MR_define_size_slot(ptag, new, size)        0
  #define       MR_copy_size_slot(nptag, new, optag, old)   0

#else // ! MR_HIGHLEVEL_CODE

  #ifdef  MR_RECORD_TERM_SIZES
    #define MR_SIZE_SLOT_SIZE 1
    #ifdef MR_RECORD_TERM_SIZES_AS_CELLS
        #define MR_cell_size(arity)                         1
    #else
        #define MR_cell_size(arity)                         arity
    #endif

    #define     MR_define_size_slot(ptag, new, size)                        \
                do {                                                        \
                    MR_field(ptag, new, -1) = size;                         \
                } while (0)
    #define     MR_copy_size_slot(nptag, new, optag, old)                   \
                do {                                                        \
                    MR_field(nptag, new, -1) = MR_field(optag, old, -1);    \
                } while (0)
  #else
    #define     MR_SIZE_SLOT_SIZE                             0
    #define     MR_cell_size(arity)                           0
    #define     MR_define_size_slot(ptag, new, size)          0
    #define     MR_copy_size_slot(nptag, new, optag, old)     0
  #endif

#endif // ! MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////

// Macros to implement structure reuse, conditional on whether the structure
// to reuse is really dynamically allocated. If not, fall back to allocating
// a new object on the heap.

#define     MR_reuse_or_alloc_heap(dest, reuse, fallback_alloc)             \
            MR_tag_reuse_or_alloc_heap((dest), 0, (reuse), (fallback_alloc))

#define     MR_reuse_or_alloc_heap_flag(dest, flag, reuse, fallback_alloc)  \
            MR_tag_reuse_or_alloc_heap((dest), 0, (flag), (reuse),          \
                (fallback_alloc))

#define     MR_tag_reuse_or_alloc_heap(dest, tag, reuse, fallback_alloc)    \
            do {                                                            \
                MR_bool dummy;                                              \
                MR_tag_reuse_or_alloc_heap_flag((dest), (tag), dummy,       \
                    (reuse), (fallback_alloc));                             \
                (void) dummy;                                               \
            } while (0)

#if defined(MR_BOEHM_GC) && !defined(MR_UNCONDITIONAL_STRUCTURE_REUSE)

  #define   MR_in_heap_range(addr)                                          \
            ((void *) (addr) >= GC_least_plausible_heap_addr &&             \
             (void *) (addr) < GC_greatest_plausible_heap_addr)             \

#else // ! MR_BOEHM_GC || MR_UNCONDITIONAL_STRUCTURE_REUSE

  // We don't have any way to check whether `addr' is dynamically allocated,
  // so just assume that it is. For this to be safe, `--static-ground-terms'
  // needs to be disabled.

  #define   MR_in_heap_range(addr)  (MR_TRUE)

#endif // ! MR_BOEHM_GC || MR_UNCONDITIONAL_STRUCTURE_REUSE

#define     MR_tag_reuse_or_alloc_heap_flag(dest, tag, flag, reuse_addr,    \
                fallback_alloc)                                             \
            do {                                                            \
                MR_Word tmp = (reuse_addr);                                 \
                if (MR_in_heap_range(tmp)) {                                \
                    (dest) = (MR_Word) MR_mkword((tag), tmp);               \
                    (flag) = MR_TRUE;                                       \
                } else {                                                    \
                    (fallback_alloc);                                       \
                    (flag) = MR_FALSE;                                      \
                }                                                           \
            } while (0)

#define     MR_assign_if_in_heap(dest, addr)                                \
            do {                                                            \
                MR_Word tmp = (addr);                                       \
                (dest) = MR_in_heap_range(tmp) ? tmp : (MR_Word) NULL;      \
            } while (0)

////////////////////////////////////////////////////////////////////////////

// Macros to box/unbox types declared with `pragma foreign_type'.

// void MR_MAYBE_BOX_FOREIGN_TYPE(type T, const T &value, MR_Box &box):
// Copy a value of type T from `value' to `box', boxing it if necessary
// (i.e. if type T won't fit in type MR_Box).

#define MR_MAYBE_BOX_FOREIGN_TYPE(T, value, box)                            \
        do {                                                                \
            MR_CHECK_EXPR_TYPE((value), T);                                 \
            MR_CHECK_EXPR_TYPE((box), MR_Box);                              \
            if (sizeof(T) == sizeof(MR_Box)) {                              \
                (box) = * (MR_Box *) &(value);                              \
            } else if (sizeof(T) > sizeof(MR_Box)) {                        \
                MR_Word     box_word;                                       \
                size_t size_in_words =                                      \
                    (sizeof(T) + sizeof(MR_Word) - 1) / sizeof(MR_Word);    \
                /*                                                          \
                ** XXX This assumes that nothing requires stricter          \
                ** alignment than MR_Float.                                 \
                */                                                          \
                MR_make_hp_float_aligned();                                 \
                /*                                                          \
                ** This assumes that we don't keep term sizes               \
                ** in grades that use boxes.                                \
                */                                                          \
                MR_offset_incr_hp_msg(box_word, 0, size_in_words,           \
                    MR_ALLOC_SITE_FOREIGN, NULL);                           \
                box = (MR_Box) box_word;                                    \
                MR_assign_structure(*(T *)(box), (value));                  \
                MR_profmem_record_allocation(size_in_words, NULL,           \
                    "foreign type: " MR_STRINGIFY(T));                      \
            } else {                                                        \
                /*                                                          \
                ** We can't take the address of `box' here,                 \
                ** since it might be a global register.                     \
                ** Hence we need to use a temporary copy.                   \
                */                                                          \
                MR_Box box_copy;                                            \
                if (sizeof(T) < sizeof(MR_Box)) {                           \
                    /*                                                      \
                    ** Make sure we don't leave any part of it uninitialized. \
                    */                                                      \
                    box_copy = 0;                                           \
                }                                                           \
                MR_memcpy(&box_copy, &(value), sizeof(T));                  \
                (box) = box_copy;                                           \
            }                                                               \
        } while (0)

// void MR_MAYBE_UNBOX_FOREIGN_TYPE(type T, MR_Box box, T &value):
// Copy a value of type T from `box' to `value', unboxing it if necessary.

#define MR_MAYBE_UNBOX_FOREIGN_TYPE(T, box, value)                          \
        do {                                                                \
            MR_CHECK_EXPR_TYPE((value), T);                                 \
            MR_CHECK_EXPR_TYPE((box), MR_Box);                              \
            if (sizeof(T) > sizeof(MR_Word)) {                              \
                MR_assign_structure((value), * (T *) (box));                \
            } else {                                                        \
                /*                                                          \
                ** We can't take the address of `box' here,                 \
                ** since it might be a global register.                     \
                ** Hence we need to use a temporary copy.                   \
                */                                                          \
                MR_Box box_copy = (box);                                    \
                if (sizeof(T) == sizeof(MR_Box)) {                          \
                    (value) = * (T *) &box_copy;                            \
                } else {                                                    \
                    MR_memcpy(&(value), &box_copy, sizeof(T));              \
                }                                                           \
            }                                                               \
        } while (0)

////////////////////////////////////////////////////////////////////////////

// The rest of this file defines macros designed to help hand-written C code
// create cells on the heap. These macros can be used directly, or indirectly
// via macros built on top of them. mercury_string.h and mercury_tags.h define
// some such macros.

#ifdef MR_HIGHLEVEL_CODE

// Note that this code is also duplicated in mercury.c.

MR_EXTERN_INLINE MR_Word MR_create1_func(MR_Word w1);
MR_EXTERN_INLINE MR_Word MR_create2_func(MR_Word w1, MR_Word w2);
MR_EXTERN_INLINE MR_Word MR_create3_func(MR_Word w1, MR_Word w2, MR_Word w3);

MR_EXTERN_INLINE MR_Word
MR_create1_func(MR_Word w1)
{
    MR_Word *p;

    p = (MR_Word *) MR_new_object(MR_Word, 1 * sizeof(MR_Word),
        NULL, "create1");
    p[0] = w1;
    return (MR_Word) p;
}

MR_EXTERN_INLINE MR_Word
MR_create2_func(MR_Word w1, MR_Word w2)
{
    MR_Word *p;

    p = (MR_Word *) MR_new_object(MR_Word, 2 * sizeof(MR_Word),
        NULL, "create2");
    p[0] = w1;
    p[1] = w2;
    return (MR_Word) p;
}

MR_EXTERN_INLINE MR_Word
MR_create3_func(MR_Word w1, MR_Word w2, MR_Word w3)
{
    MR_Word *p;

    p = (MR_Word *) MR_new_object(MR_Word, 3 * sizeof(MR_Word),
        NULL, "create3");
    p[0] = w1;
    p[1] = w2;
    p[2] = w3;
    return (MR_Word) p;
}

  #define   MR_create1(ti1, w1)                                             \
            MR_create1_func((w1))
  #define   MR_create2(ti1, w1, ti2, w2)                                    \
            MR_create2_func((w1), (w2))
  #define   MR_create3(ti1, w1, ti2, w2, ti3, w3)                           \
            MR_create3_func((w1), (w2), (w3))

  #define   MR_create1_msg(ti1, w1, alloc_id, type)                         \
            MR_create1((ti1), (w1))
  #define   MR_create2_msg(ti1, w1, ti2, w2, alloc_id, type)                \
            MR_create2((ti1), (w1), (ti2), (w2))
  #define   MR_create3_msg(ti1, w1, ti2, w2, ti3, w3, alloc_id, type)       \
            MR_create3((ti1), (w1), (ti2), (w2), (ti3), (w3))

#else // ! MR_HIGHLEVEL_CODE

  #ifdef  MR_RECORD_TERM_SIZES
    #define     MR_fill_create1_size(hp, ti1, w1)                           \
                (                                                           \
                    hp[-2] = MR_term_size(ti1, w1) + MR_cell_size(1)        \
                )
    #define     MR_fill_create2_size(hp, ti1, w1, ti2, w2)                  \
                (                                                           \
                    hp[-3] = MR_term_size(ti1, w1) + MR_term_size(ti2, w2)  \
                        + MR_cell_size(2)                                   \
                )
    #define     MR_fill_create3_size(hp, ti1, w1, ti2, w2, ti3, w3)         \
                (                                                           \
                    hp[-4] = MR_term_size(ti1, w1) + MR_term_size(ti2, w2)  \
                        + MR_term_size(ti3, w3) + MR_cell_size(3)           \
                )
  #else
    #define     MR_fill_create1_size(hp, ti1, w1)                     0
    #define     MR_fill_create2_size(hp, ti1, w1, ti2, w2)            0
    #define     MR_fill_create3_size(hp, ti1, w1, ti2, w2, ti3, w3)   0
  #endif

  #ifdef  MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    #define     MR_fill_create1_origin(hp, alloc_id)                        \
                (hp[-2] = (MR_Word) (alloc_id))
    #define     MR_fill_create2_origin(hp, alloc_id)                        \
                (hp[-3] = (MR_Word) (alloc_id))
    #define     MR_fill_create3_origin(hp, alloc_id)                        \
                (hp[-4] = (MR_Word) (alloc_id))
  #else
    #define     MR_fill_create1_origin(hp, alloc_id)     ((void) 0)
    #define     MR_fill_create2_origin(hp, alloc_id)     ((void) 0)
    #define     MR_fill_create3_origin(hp, alloc_id)     ((void) 0)
  #endif

// Note that gcc optimizes `hp += 2; return hp - 2;'
// to `tmp = hp; hp += 2; return tmp;', so we don't need to use
// gcc's expression statements in the code below.

// Used only by hand-written code not by the automatically generated code.
  #define   MR_create1(ti1, w1)                                             \
            (                                                               \
                MR_hp_alloc(MR_SIZE_SLOT_SIZE + 1),                         \
                MR_hp[-1] = (MR_Word) (w1),                                 \
                MR_fill_create1_size(MR_hp, ti1, w1),                       \
                MR_debugcr1(MR_hp),                                         \
                /* return */ (MR_Word) (MR_hp - 1)                          \
            )

// Used only by hand-written code not by the automatically generated code.
  #define   MR_create2(ti1, w1, ti2, w2)                                    \
            (                                                               \
                MR_hp_alloc(MR_SIZE_SLOT_SIZE + 2),                         \
                MR_hp[-2] = (MR_Word) (w1),                                 \
                MR_hp[-1] = (MR_Word) (w2),                                 \
                MR_fill_create2_size(MR_hp, ti1, w1, ti2, w2),              \
                MR_debugcr2(MR_hp),                                         \
                /* return */ (MR_Word) (MR_hp - 2)                          \
            )

// Used only by hand-written code not by the automatically generated code.
  #define   MR_create3(ti1, w1, ti2, w2, ti3, w3)                           \
            (                                                               \
                MR_hp_alloc(MR_SIZE_SLOT_SIZE + 3),                         \
                MR_hp[-3] = (MR_Word) (w1),                                 \
                MR_hp[-2] = (MR_Word) (w2),                                 \
                MR_hp[-1] = (MR_Word) (w3),                                 \
                MR_fill_create3_size(MR_hp, ti1, w1, ti2, w2, ti3, w3),     \
                MR_debugcr3(MR_hp),                                         \
                /* return */ (MR_Word) (MR_hp - 3)                          \
            )

// Used only by hand-written code not by the automatically generated code.
  #define   MR_create1_msg(ti1, w1, alloc_id, type)                         \
            (                                                               \
                MR_profmem_record_allocation(MR_SIZE_SLOT_SIZE + 1,         \
                    (alloc_id), (type)),                                    \
                MR_profmem_set_alloc_type((alloc_id), (type)),              \
                MR_hp_alloc(MR_SIZE_SLOT_SIZE + 1 + MR_profmem_attrib_word),\
                MR_hp[-1] = (MR_Word) (w1),                                 \
                MR_fill_create1_size(MR_hp, ti1, w1),                       \
                MR_fill_create1_origin(MR_hp, (alloc_id)),                  \
                MR_debugcr1(MR_hp),                                         \
                /* return */ (MR_Word) (MR_hp - 1)                          \
            )

// Used only by hand-written code not by the automatically generated code.
  #define   MR_create2_msg(ti1, w1, ti2, w2, alloc_id, type)                \
            (                                                               \
                MR_profmem_record_allocation(MR_SIZE_SLOT_SIZE + 2,         \
                    (alloc_id), (type)),                                    \
                MR_profmem_set_alloc_type((alloc_id), (type)),              \
                MR_hp_alloc(MR_SIZE_SLOT_SIZE + 2 + MR_profmem_attrib_word),\
                MR_hp[-2] = (MR_Word) (w1),                                 \
                MR_hp[-1] = (MR_Word) (w2),                                 \
                MR_fill_create2_size(MR_hp, ti1, w1, ti2, w2),              \
                MR_fill_create2_origin(MR_hp, (alloc_id)),                  \
                MR_debugcr2(MR_hp),                                         \
                /* return */ (MR_Word) (MR_hp - 2)                          \
            )

// Used only by hand-written code not by the automatically generated code.
  #define   MR_create3_msg(ti1, w1, ti2, w2, ti3, w3, alloc_id, type)       \
            (                                                               \
                MR_profmem_record_allocation(MR_SIZE_SLOT_SIZE + 3,         \
                    (alloc_id), (type)),                                    \
                MR_profmem_set_alloc_type((alloc_id), (type)),              \
                MR_hp_alloc(MR_SIZE_SLOT_SIZE + 3 + MR_profmem_attrib_word),\
                MR_hp[-3] = (MR_Word) (w1),                                 \
                MR_hp[-2] = (MR_Word) (w2),                                 \
                MR_hp[-1] = (MR_Word) (w3),                                 \
                MR_fill_create3_size(MR_hp, ti1, w1, ti2, w2, ti3, w3),     \
                MR_fill_create3_origin(MR_hp, (alloc_id)),                  \
                MR_debugcr3(MR_hp),                                         \
                /* return */ (MR_Word) (MR_hp - 3)                          \
            )

#endif // ! MR_HIGHLEVEL_CODE

// Intended for use in handwritten C code where the Mercury registers
// may have been clobbered due to C function calls (eg, on the SPARC due
// to sliding register windows).
// Remember to MR_save_transient_hp() before calls to such code, and
// MR_restore_transient_hp() after.
//
// There are intentionally no versions that do not specify an offset;
// this is to force anyone who wants to allocate cells on the saved heap
// to think about the implications of their code for term size profiling.

#define MR_offset_incr_saved_hp(dest, offset, count, alloc_id, type)        \
        do {                                                                \
            MR_restore_transient_hp();                                      \
            MR_offset_incr_hp_msg((dest), (offset), (count),                \
                (alloc_id), (type));                                        \
            MR_save_transient_hp();                                         \
        } while (0)

#define MR_offset_incr_saved_hp_atomic(dest, offset, count, alloc_id, type) \
        do {                                                                \
            MR_restore_transient_hp();                                      \
            MR_offset_incr_hp_atomic_msg((dest), (offset), (count),         \
                (alloc_id), (type));                                        \
            MR_save_transient_hp();                                         \
        } while (0)

#endif // not MERCURY_HEAP_H
