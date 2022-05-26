// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1999-2006, 2011 The University of Melbourne.
// Copyright (C) 2014, 2015-2016, 2018, 2022 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury.h - This file defines the macros, types, etc. that
// are used when generating high-level C code.
// (For the low-level C code, see mercury_imp.h.)

#ifndef MERCURY_H
#define MERCURY_H

// Everything in this file is specific to the high-level-code back-end
#ifdef MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
// Header files to include.

#include "mercury_conf.h"
#include "mercury_types.h"
#include "mercury_float.h"              // for the `MR_Float' type
#include "mercury_int.h"
#include "mercury_tags.h"
#include "mercury_grade.h"
#include "mercury_thread.h"             // for the MR_*_GLOBAL_LOCK() macros
#include "mercury_std.h"                // for the MR_CALL macro (and others)
#include "mercury_type_info.h"
#include "mercury_builtin_types.h"
#include "mercury_library_types.h"      // for MercuryFilePtr, for files
                                        // that use the type in io.m, whose
                                        // foreign_type is MercuryFilePtr XXX
#include "mercury_ho_call.h"            // for the `MR_Closure' type
#include "mercury_memory.h"             // for memory allocation routines
#include "mercury_type_tables.h"        // for MR_register_type_ctor_info
#include "mercury_misc.h"               // for MR_fatal_error()

#ifdef MR_CONSERVATIVE_GC
  #ifdef MR_BOEHM_GC
    #include "gc.h"
    #define GC_I_HIDE_POINTERS
    #ifdef MR_INLINE_ALLOC
      #include "gc_inline.h"
    #endif
  #endif
#else
  #include "mercury_regs.h"             // for MR_hp
  #include "mercury_engine.h"           // for MR_fake_reg (needed by MR_hp)
  #include "mercury_overflow.h"         // for MR_heap_overflow_check()
  #ifdef MR_NATIVE_GC
    #include "mercury_accurate_gc.h"    // for MR_garbage_collect()
    #include "mercury_layout_util.h"    // for MR_materialize_closure...()
  #endif
#endif

#if defined(MR_MPROF_PROFILE_CALLS) || defined(MR_MPROF_PROFILE_TIME)
  #include "mercury_prof.h"             // for MR_prof_call_profile
                                        // and MR_set_prof_current_proc
#endif

#ifdef MR_MPROF_PROFILE_MEMORY
  #include "mercury_heap_profile.h"     // for MR_record_allocation
#endif

#if defined(MR_MPROF_PROFILE_CALLS) || defined(MR_MPROF_PROFILE_MEMORY) || defined(MR_MPROF_PROFILE_TIME)
  #include "mercury_goto.h"             // for MR_init_entry
#endif

#include <setjmp.h> // for jmp_buf etc., which are used for commits
#include <string.h> // for strcmp(), which is used for =/2 on strings

////////////////////////////////////////////////////////////////////////////
// Type definitions.

// The jmp_buf type used by MR_builtin_setjmp()
// to save the stack context when implementing commits.

#if (MR_GNUC > 2 || (MR_GNUC == 2 && __GNUC_MINOR__ >= 8))
  // There is an issue with using __builtin_setjmp and __builtin_longjmp on
  // Linux/aarch64. When a thread commits using __builtin_longjmp, we get an
  // assertion failure or a segmentation fault. The problem appears to have
  // been fixed in some version between gcc 8.3.0 and gcc 10.2.
  #if defined(__aarch64__) && (MR_GNUC < 10)
    // Don't define MR_USE_GCC_BUILTIN_SETJMP_LONGJMP.
  #elif defined(MR_DARWIN_SETJMP_WORKAROUND)
    // Don't define MR_USE_GCC_BUILTIN_SETJMP_LONGJMP.
  #else
    #define MR_USE_GCC_BUILTIN_SETJMP_LONGJMP
  #endif
#endif

#ifdef MR_USE_GCC_BUILTIN_SETJMP_LONGJMP
  // The jump buffer for GCC `__builtin_setjmp' and `__builtin_longjmp'
  // functions is documented to be an array of five intptr_t's
  // (GCC manual section 6.5 Nonlocal gotos).
  typedef intptr_t MR_builtin_jmp_buf[5];
#else
  // Otherwise we use the standard jmp_buf type.
  typedef jmp_buf MR_builtin_jmp_buf;
#endif

// The chain of stack frames, used for accurate GC.
//
// Any changes to this struct may require changes to compiler/ml_elim_nested.m,
// which generates structs that whose initial members have to match the layout
// here, and which assumes that the `prev' is at offset zero.

struct MR_StackChain {
    struct MR_StackChain *prev;
    void (*trace)(void *this_frame);
};

////////////////////////////////////////////////////////////////////////////
// Declarations of constants and variables.

#ifdef MR_NATIVE_GC
  // This points to the start of the MR_StackChain frame list.
  //
  // XXX Using a global variable for this is not thread-safe.
  // We could use a GNU C global register variable to hold it instead,
  // but we don't have enough of those on x86s to allow us to spare one
  // for this purpose.

  #ifdef MR_THREAD_SAFE
    #error "Sorry, not supported: --gc accurate --thread-safe"
  #endif
  extern void *mercury__private_builtin__stack_chain;
#endif

// Declare the TypeCtorInfos of the library types that are not already declared
// in mercury_builtin_types.h

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    mercury__array__array__type_ctor_info_array_1);
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    mercury__univ__univ__type_ctor_info_univ_0);

// When generating code which passes an io.state or a store.store to a
// polymorphic procedure, or which does a higher-order call that passes
// one of these, then we need to generate a reference to a dummy variable.
// We use this variable for that purpose.

extern  MR_Word mercury__private_builtin__dummy_var;

////////////////////////////////////////////////////////////////////////////
// Macros and inline function definitions.

// These macros expand to the either the standard setjmp()/longjmp()
// or to the GNU __builtin_setjmp() and __builtin_longjmp().
// The GNU versions are the same as the standard versions,
// except that they are more efficient, and that they have two restrictions:
//
//  1. The second argument to __builtin_longjmp() must always be `1'.
//  2. The call to __builtin_longjmp() must not be in the same
//      function as the call to __builtin_setjmp().

#ifdef MR_USE_GCC_BUILTIN_SETJMP_LONGJMP
  #define MR_builtin_setjmp(buf)        __builtin_setjmp((buf))
  #define MR_builtin_longjmp(buf, val)  __builtin_longjmp((buf), (val))
#else
  #define MR_builtin_setjmp(buf)        setjmp((buf))
  #define MR_builtin_longjmp(buf, val)  longjmp((buf), (val))
#endif

// MR_new_object():
//  Allocates memory on the garbage-collected heap.

#ifdef MR_BOEHM_GC
  #ifdef MR_INLINE_ALLOC
    #ifndef MR_GNUC
      #error "MR_INLINE_ALLOC requires GNU C"
    #endif
    // This must be a macro, not an inline function, because GNU C's
    // `__builtin_constant_p' does not work inside inline functions.

    #define MR_GC_MALLOC_INLINE(bytes)                                      \
        ( __extension__ __builtin_constant_p(bytes) &&                      \
        (bytes) <= 16 * sizeof(MR_Word)                                     \
            ? ({                                                            \
                void *temp;                                                 \
                /* If size > 1 word, round up to multiple of 8 bytes. */    \
                MR_Word rounded_bytes =                                     \
                    ( (bytes) <= sizeof(MR_Word)                            \
                    ? sizeof(MR_Word)                                       \
                    : 8 * (((bytes) + 7) / 8)                               \
                    );                                                      \
                MR_Word num_words = rounded_bytes / sizeof(MR_Word);        \
                GC_MALLOC_WORDS(temp, num_words);                           \
                /* return */ temp;                                          \
          })                                                                \
        : GC_MALLOC(bytes)                                                  \
        )
    #define MR_new_object(type, size, alloc_id, name)                   \
        ((type *) MR_GC_MALLOC_INLINE(size))
    // Since the Boehm collector defined GC_MALLOC_WORDS but not
    // GC_MALLOC_WORDS_ATOMIC, we can define MR_new_object_atomic here
    // to call either MR_GC_MALLOC_ATOMIC or MR_GC_MALLOC_INLINE,
    // depending on whether we value atomicity or inline expansion more.
    // XXX The above is out-of-date: Boehm GC does now provide
    // GC_MALLOC_ATOMIC_WORDS.
    // XXX We don't provide MR_GC_MALLOC_ATOMIC.

    #define MR_new_object_atomic(type, size, alloc_id, name)            \
        ((type *) GC_MALLOC_ATOMIC(size))
  #else // !MR_INLINE_ALLOC

    #ifdef MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    #define MR_new_object(type, size, alloc_id, name)                   \
        ((type *) MR_new_object_func(size, alloc_id, name))
    #define MR_new_object_atomic(type, size, alloc_id, name)            \
        ((type *) MR_new_object_atomic_func(size, alloc_id, name))
    #else
    #define MR_new_object(type, size, alloc_id, name)                   \
        ((type *) GC_MALLOC(size))
    #define MR_new_object_atomic(type, size, alloc_id, name)            \
        ((type *) GC_MALLOC_ATOMIC(size))
    #endif
  #endif // !MR_INLINE_ALLOC

#else // !MR_BOEHM_GC

  #if !defined(MR_GNUC) && !defined(MR_CLANG)
    // We need GNU C's `({...})' expressions.
    // It's not worth worrying about compilers other than GCC or clang for
    // this obscure combination of options.

    #error "For C compilers other than GCC or clang, `--high-level-code' requires `--gc boehm'"
  #endif

  // XXX Note that currently we don't need to worry about alignment here,
  // other than word alignment, because floating point fields will be boxed
  // if they don't fit in a word. This would need to change if we ever start
  // using unboxed fields whose alignment requirement is greater than one word.

  #define MR_new_object(type, size, alloc_id, name)                     \
    ({                                                                  \
        size_t  MR_new_object_num_words;                                \
        MR_Word MR_new_object_ptr;                                      \
                                                                        \
        MR_new_object_num_words = MR_bytes_to_words(size);              \
        MR_incr_hp_msg(MR_new_object_ptr, MR_new_object_num_words,      \
            (alloc_id), (name));                                        \
        /* return */ (type *) MR_new_object_ptr;                        \
    })
  #define MR_new_object_atomic(type, size, alloc_id, name)              \
    MR_new_object(type, size, alloc_id, name)

#endif

////////////////////////////////////////////////////////////////////////////
// Code to box/unbox floats in high-level C grades.
// The low-level C grades only use MR_float_to_word and MR_word_to_float.
//
// This code is not in mercury_float.h because the function definition
// requires the declaration of MR_new_object_atomic.
//
// When sizeof(MR_Float) <= sizeof(MR_Box), the names "box" and "unbox" should
// be interpreted as casts to and from MR_Box, as we do not truly box the
// floats then.

#ifdef MR_HIGHLEVEL_CODE
  #ifdef MR_BOXED_FLOAT

    #if defined(MR_GNUC)
      #define MR_box_float(f)                                           \
      ({                                                                \
          MR_Float *MR_box_float_ptr;                                   \
                                                                        \
          MR_make_hp_float_aligned();                                   \
          MR_box_float_ptr = (MR_Float *) MR_new_object_atomic(MR_Float,\
              sizeof(MR_Float), MR_ALLOC_SITE_FLOAT, NULL);             \
          *MR_box_float_ptr = (f);                                      \
          /* return */ (MR_Box) MR_box_float_ptr;                       \
      })
    #else
      // Note that this code is also duplicated in mercury.c.
      MR_EXTERN_INLINE MR_Box MR_box_float(MR_Float f);

      MR_EXTERN_INLINE MR_Box
      MR_box_float(MR_Float f)
      {
          MR_Float *ptr;

          MR_make_hp_float_aligned();
          ptr = MR_new_object_atomic(MR_Float, sizeof(MR_Float),
              MR_ALLOC_SITE_FLOAT, NULL);
          *ptr = f;
          return (MR_Box) ptr;
      }
    #endif

    #define MR_unbox_float(ptr) (*(MR_Float *)ptr)

  #else  // ! MR_BOXED_FLOAT

    #define MR_box_float(F)     ((MR_Box) MR_float_to_word(F))
    #define MR_unbox_float(B)   MR_word_to_float((MR_Word)(B))

  #endif // ! MR_BOXED_FLOAT

#endif // MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
// Code to box/unbox 64-bit integers in high-level C grades.
// The low-level C grades only use MR_{int64,uint64)_to_word and
// MR_word_to_{int64,uint64}.
//
// This code is not in mercury_int.h because the function definition
// requires the declaration of MR_new_object_atomic.
//
// When sizeof(int64_t) <= sizeof(MR_Box), the names "box" and "unbox" should
// be interpreted as casts to and from MR_Box, as we do not truly box the
// 64-bit integers then.

#ifdef MR_HIGHLEVEL_CODE
  #if defined(MR_BOXED_INT64S)

    #if defined(MR_GNUC)
      #define MR_box_int64(i)                                             \
      ({                                                                  \
          int64_t  *MR_box_int64_ptr;                                     \
                                                                          \
          MR_make_hp_int64_aligned();                                     \
          MR_box_int64_ptr = (int64_t *) MR_new_object_atomic(int64_t,    \
              sizeof(int64_t), MR_ALLOC_SITE_INT64, NULL);                \
          *MR_box_int64_ptr = (i);                                        \
          /* return */ (MR_Box) MR_box_int64_ptr;                         \
      })
      #define MR_box_uint64(i)                                            \
      ({                                                                  \
          uint64_t  *MR_box_uint64_ptr;                                   \
                                                                          \
          MR_make_hp_uint64_aligned();                                    \
          MR_box_uint64_ptr = (uint64_t *) MR_new_object_atomic(uint64_t, \
              sizeof(uint64_t), MR_ALLOC_SITE_UINT64, NULL);              \
          *MR_box_uint64_ptr = (i);                                       \
          /* return */ (MR_Box) MR_box_uint64_ptr;                        \
      })
    #else
      // Note that this code is also duplicated in mercury.c.
      MR_EXTERN_INLINE MR_Box MR_box_int64(int64_t i);
      MR_EXTERN_INLINE MR_Box MR_box_uint64(uint64_t i);

      MR_EXTERN_INLINE MR_Box
      MR_box_int64(int64_t i)
      {
          int64_t *ptr;

          MR_make_hp_int64_aligned();
          ptr = MR_new_object_atomic(int64_t, sizeof(int64_t),
              MR_ALLOC_SITE_INT64, NULL);
          *ptr = i;
          return (MR_Box) ptr;
      }

      MR_EXTERN_INLINE MR_Box
      MR_box_uint64(uint64_t i)
      {
          uint64_t *ptr;

          MR_make_hp_uint64_aligned();
          ptr = MR_new_object_atomic(uint64_t, sizeof(uint64_t),
              MR_ALLOC_SITE_UINT64, NULL);
          *ptr = i;
          return (MR_Box) ptr;
      }
    #endif

    #define MR_unbox_int64(ptr)  (*(int64_t *) ptr)
    #define MR_unbox_uint64(ptr) (*(uint64_t *) ptr)

  #else  // not MR_BOXED_INT64S

    #define MR_box_int64(I)      ((MR_Box) MR_int64_to_word(I))
    #define MR_unbox_int64(B)    MR_word_to_int64((MR_Word) (B))
    #define MR_box_uint64(I)     ((MR_Box) MR_uint64_to_word(I))
    #define MR_unbox_uint64(B)   MR_word_to_uint64((MR_Word) (B))

  #endif // not MR_BOXED_INT64S
#endif // MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////

// MR_GC_check():
//  Check to see if we need to do a garbage collection, and if so, do it.

#define MR_GC_check()                                                   \
    do {                                                                \
        if ((char *) MR_hp >=                                           \
            MR_ENGINE(MR_eng_heap_zone)->MR_zone_gc_threshold)          \
        {                                                               \
            MR_save_registers();                                        \
            MR_garbage_collect();                                       \
            MR_restore_registers();                                     \
        }                                                               \
    } while (0)

////////////////////////////////////////////////////////////////////////////

// The #include of mercury_heap.h needs to come *after* the definition
// of MR_new_object(), because mercury_heap.h defines some inline
// functions that reference MR_new_object(). So does mercury_string.h.
// mercury_univ.h includes mercury_heap.h.

#include "mercury_heap.h"       // for MR_MAYBE_(UN)BOX_FOREIGN_TYPE()
#include "mercury_univ.h"
#include "mercury_string.h"     // for MR_nth_code_unit/MR_offset_streq()

#endif // MR_HIGHLEVEL_CODE

#endif // not MERCURY_H
