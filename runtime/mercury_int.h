// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2017 The Mercury team.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_int.h - integer type handling

#ifndef MERCURY_INT_H
#define MERCURY_INT_H

#include "mercury_conf.h"       // for MR_BOXED_INT64S, MR_CONSERVATIVE_GC
#include "mercury_types.h"      // for `MR_Word'
#include "mercury_std.h"        // for `MR_bool'

#define MR_INT64_WORDS          ((sizeof(int64_t) + sizeof(MR_Word) - 1) \
                                        / sizeof(MR_Word))
#define MR_UINT64_WORDS         ((sizeof(uint64_t) + sizeof(MR_Word) - 1) \
                                        / sizeof(MR_Word))
#if defined(MR_BOXED_INT64S)

   #define MR_word_to_int64(w)   (* (int64_t *) (w))
   #define MR_word_to_uint64(w)  (* (uint64_t *) (w))

   #if defined(MR_CONSERVATIVE_GC)

    #define MR_int64_to_word(i)                                             \
      (                                                                     \
        MR_hp_alloc_atomic_msg(MR_INT64_WORDS, MR_ALLOC_SITE_INT64, NULL),  \
        * (int64_t *) (void *) (MR_hp - MR_INT64_WORDS) = (i),              \
        /* return */ (MR_Word) (MR_hp - MR_INT64_WORDS)                     \
      )
    #define MR_make_hp_int64_aligned() ((void)0)

    #define MR_uint64_to_word(u)                                             \
      (                                                                      \
        MR_hp_alloc_atomic_msg(MR_UINT64_WORDS, MR_ALLOC_SITE_UINT64, NULL), \
        * (uint64_t *) (void *) (MR_hp - MR_UINT64_WORDS) = (u),             \
        /* return */ (MR_Word) (MR_hp - MR_UINT64_WORDS)                     \
      )
    #define MR_make_hp_uint64_aligned() ((void)0)

   #else // ! defined(MR_CONSERVATIVE_GC)
    // We need to ensure that what we allocated on the heap is properly
    // aligned for a 64-bit integer value, by rounding MR_hp up to the
    // nearest int64-aligned boundary.

    #define MR_make_hp_int64_aligned()                                      \
      ( (MR_Word) MR_hp & (sizeof(int64_t) - 1) ?                           \
            MR_hp_alloc_atomic_msg(1, MR_ALLOC_SITE_INT64, NULL)            \
      :                                                                     \
            (void)0                                                         \
      )
    #define MR_int64_to_word(i)                                             \
      (                                                                     \
        MR_make_hp_int64_aligned(),                                         \
        MR_hp_alloc_atomic_msg(MR_INT64_WORDS, MR_ALLOC_SITE_INT64, NULL),  \
        * (int64_t *) (void *)(MR_hp - MR_INT64_WORDS) = (i),               \
        /* return */ (MR_Word) (MR_hp - MR_INT64_WORDS)                     \
      )
    
    #define MR_make_hp_uint64_aligned()                                      \
      ( (MR_Word) MR_hp & (sizeof(uint64_t) - 1) ?                           \
            MR_hp_alloc_atomic_msg(1, MR_ALLOC_SITE_UINT64, NULL)            \
      :                                                                      \
            (void)0                                                          \
      )
    #define MR_uint64_to_word(u)                                             \
      (                                                                      \
        MR_make_hp_uint64_aligned(),                                         \
        MR_hp_alloc_atomic_msg(MR_UINT64_WORDS, MR_ALLOC_SITE_UINT64, NULL), \
        * (uint64_t *) (void *)(MR_hp - MR_UINT64_WORDS) = (u),              \
        /* return */ (MR_Word) (MR_hp - MR_UINT64_WORDS)                     \
      )

  #endif

#else // not MR_BOXED_INT64S

  // Unboxed int64s means we can assume sizeof(int64_t) <= sizeof(MR_Word)
  // and sizeof(uint64_t) <= sizeof(MR_Word).

  #define MR_make_hp_int64_aligned() ((void)0)
  #define MR_make_hp_uint64_aligned() ((void)0)

  #define MR_int64_to_word(i) ((MR_Word)(i))
  #define MR_uint64_to_word(u) ((MR_Word)(u))

  #define MR_word_to_int64(w) ((int64_t)(w))
  #define MR_word_to_uint64(w) ((uint64_t)(w))

#endif // not MR_BOXED_INT64S

extern MR_Integer MR_hash_int64(int64_t);
extern MR_Integer MR_hash_uint64(uint64_t);

#if defined(MR_GNUC) || defined(MR_CLANG)
  #define MR_uint16_reverse_bytes(U) __builtin_bswap16((U))
#elif defined(MR_MSVC)
  #define MR_uint16_reverse_bytes(U) _byteswap_ushort((U))
#else
  #define MR_uint16_reverse_bytes(U) (((U & 0xff00) >> 8) | \
                                      ((U & 0x00ff) << 8))
#endif

#if defined(MR_GNUC) || defined(MR_CLANG)
  #define MR_uint32_reverse_bytes(U) __builtin_bswap32((U))
#elif defined(MR_MSVC)
  #define MR_uint32_reverse_bytes(U) _byteswap_ulong((U))
#else
  #define MR_uint32_reverse_bytes(U) ((U & UINT32_C(0x000000ff)) << 24 | \
                                      (U & UINT32_C(0x0000ff00)) << 8  | \
                                      (U & UINT32_C(0x00ff0000)) >> 8  | \
                                      (U & UINT32_C(0xff000000)) >> 24 )
#endif

#if defined(MR_GNUC) || defined(MR_CLANG)
  #define MR_uint64_reverse_bytes(U) __builtin_bswap64((U))
#elif defined(MR_MSVC)
  #define MR_uint64_reverse_bytes(U) _byteswap_uint64((UU))
#else
  #define MR_uint64_reverse_bytes(U) \
        ((U & UINT64_C(0x00000000000000ff)) << 56  | \
         (U & UINT64_C(0x000000000000ff00)) << 40  | \
         (U & UINT64_C(0x0000000000ff0000)) << 24  | \
         (U & UINT64_C(0x00000000ff000000)) << 8   | \
         (U & UINT64_C(0x000000ff00000000)) >> 8   | \
         (U & UINT64_C(0x0000ff0000000000)) >> 24  | \
         (U & UINT64_C(0x00ff000000000000)) >> 40  | \
         (U & UINT64_C(0xff00000000000000)) >> 56)
#endif

#endif // not MERCURY_INT_H
