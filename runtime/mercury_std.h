// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-1995, 1997-2005, 2011-2012 The University of Melbourne.
// Copyright (C) 2014, 2016-2019, 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// std.h - "standard" [sic] definitions for C:
//  MR_bool, MR_TRUE, MR_FALSE, MR_min(), MR_max(), MR_streq(), etc.

#ifndef MERCURY_STD_H
#define MERCURY_STD_H

// We include mercury_regs.h to ensure we define any global register variables
// before any functions, even if the system libraries we include below it
// define any functions.

#include "mercury_regs.h"

#if defined(MR_HAVE_STDINT_H)
    #include <stdint.h>
#else
    #error "Mercury requires a system that provides stdint.h"
#endif

// On Windows we need to define _CRT_RAND_S *before* stdlib.h is included,
// otherwise the declaration for rand_s() will not be visible.
//
#if defined(MR_WIN32) && !defined(_CRT_RAND_S)
   #define _CRT_RAND_S
#endif
#include <stdlib.h> // for size_t

#include <assert.h> // for assert()
#include <errno.h>  // for EINTR
#include <ctype.h>  // for isalnum(), etc.

// The boolean type, MR_bool, with constants MR_TRUE and MR_FALSE.
//
// We use `int' rather than `char' for MR_bool, because GCC has problems
// optimizing tail calls for functions that return types smaller than `int'.
// In most cases, `int' is more efficient anyway.
// The only exception is that in some cases it is more important to optimize
// space rather than time; in those (rare) cases, you can use `MR_small_bool'
// instead of `MR_bool'.

typedef int         MR_bool;
typedef char        MR_small_bool;

// The values of MR_TRUE and MR_FALSE should correspond with the representation
// of the Mercury standard library type bool.bool, so that they can be used as
// values for bool arguments of exported Mercury procs.

#define MR_TRUE     1
#define MR_FALSE    0

#define MR_YES      MR_TRUE
#define MR_NO       MR_FALSE

#define MR_max(a, b)    ((a) > (b) ? (a) : (b))
#define MR_min(a, b)    ((a) < (b) ? (a) : (b))

// The ANSI C isalnum(), etc. macros require that the argument be cast to
// `unsigned char'; if you pass a signed char, the behaviour is undefined.
// Hence we define `MR_' versions of these that do the cast -- you should
// make sure to always use the `MR_' versions rather than the standard ones.

#define MR_isupper(c)           isupper((unsigned char) (c))
#define MR_islower(c)           islower((unsigned char) (c))
#define MR_isalpha(c)           isalpha((unsigned char) (c))
#define MR_isalnum(c)           isalnum((unsigned char) (c))
#define MR_isdigit(c)           isdigit((unsigned char) (c))
#define MR_isspace(c)           isspace((unsigned char) (c))
#define MR_isalnumunder(c)      (isalnum((unsigned char) (c)) || (c) == '_')

#define MR_streq(s1, s2)        (strcmp(s1, s2) == 0)
#define MR_strdiff(s1, s2)      (strcmp(s1, s2) != 0)
#define MR_strtest(s1, s2)      (strcmp(s1, s2))
#define MR_strneq(s1, s2, n)    (strncmp(s1, s2, n) == 0)
#define MR_strndiff(s1, s2, n)  (strncmp(s1, s2, n) != 0)
#define MR_strntest(s1, s2, n)  (strncmp(s1, s2, n))

#define MR_ungetchar(c)         ungetc(c, stdin)

// For speed, turn assertions off, unless low-level debugging is enabled.

#ifdef MR_DEBUG_THE_RUNTIME
  #define MR_assert(ASSERTION)  assert(ASSERTION)
#elif defined(MR_MSVC)
  // This marks an MR_assert(0) as an unreachable branch. This also silences
  // warnings about unused variables, and also can improve optimization.
  #define MR_assert(ASSERTION)  __assume(ASSERTION)
#else
  // Using sizeof ensures that the argument to MR_assert is valid, but does
  // not actually execute any code. This helps prevent bitrot when code is not
  // tested with assertions enabled.
  #define MR_assert(ASSERTION)  ((void)sizeof(ASSERTION))
#endif

////////////////////////////////////////////////////////////////////////////

#ifdef EINTR
  #define MR_is_eintr(x)    ((x) == EINTR)
#else
  #define MR_is_eintr(x)    MR_FALSE
#endif

////////////////////////////////////////////////////////////////////////////

// Macros for inlining.
//
// Inlining is treated differently by C++, C99, and GNU C.
// We also need to make it work for C89, which doesn't have
// any explicit support for inlining.
//
// To make a function inline, you should declare it as either
// `MR_INLINE', `MR_EXTERN_INLINE', or `MR_STATIC_INLINE'.
// You should not use `extern' or `static' in combination with these macros.
//
// `MR_STATIC_INLINE' should be used for functions that are defined and
// used only in a single translation unit (i.e. a single C source file).
//
// If the inline function is to be used from more than one translation unit,
// then the function definition (not just declaration) should go in
// a header file, and you should use either MR_INLINE or MR_EXTERN_INLINE;
// the difference between these two is explained below.
//
// MR_INLINE creates an inline definition of the function, and
// if needed it also creates an out-of-line definition of the function
// for the current translation unit, in case the function can't be inlined
// (e.g. because the function's address was taken, or because the
// file is compiled with the C compiler's optimizations turned off.)
// For C++, these definitions will be shared between different
// compilation units, but for C, each compilation unit that needs
// an out-of-line definition will gets its own definition.
// Generally that is not much of a problem, but if the C compiler
// doesn't optimize away such out-of-line definitions when they're
// not needed, this can get quite bad.
//
// MR_EXTERN_INLINE creates an inline definition of the function,
// but it does NOT guarantee to create an out-of-line definition,
// even if one might be needed. You need to explicitly provide
// an out-of-line definition for the function in one of the C files.
// This should be done using the MR_OUTLINE_DEFN(decl,body) macro,
// e.g. `MR_OUTLINE_DEFN(int foo(int x), { return x; })'.
//
// The advantage of MR_EXTERN_INLINE is that it is more code-space-efficient,
// especially in the case where you are compiling with C compiler optimizations
// turned off.
//
// It is OK to take the address of an inline function,
// but you should not assume that the address of a function declared
// MR_INLINE or MR_EXTERN_INLINE will be the same in all translation units.

#if defined(__cplusplus)
  // C++
  #define MR_STATIC_INLINE              static inline
  #define MR_INLINE                     inline
  #define MR_EXTERN_INLINE              inline
  #define MR_OUTLINE_DEFN(DECL,BODY)
#elif defined(MR_CLANG)
  #if __STDC_VERSION__ >= 199901
    // C99 style inlining.
    #define MR_STATIC_INLINE            static inline
    #define MR_INLINE                   static inline
    #define MR_EXTERN_INLINE            inline
    #define MR_OUTLINE_DEFN(DECL,BODY)  extern DECL;
  #else
   // C89: note that by default we use clang in C99 mode so this alternative
   // will only be used if user explicitly sets -std to use something prior
   // to C99.

   #define MR_STATIC_INLINE             static
   #define MR_INLINE                    static
   #define MR_EXTERN_INLINE             static
   #define MR_OUTLINE_DEFN(DECL,BODY)
  #endif
#elif defined(MR_GNUC)
  // GNU C: in (GNU) C99 or later mode GCC will use C99 style
  // inline functions; otherwise GNU style inline functions will
  // will be used.

  #if defined(__GNUC_STDC_INLINE__)
    // C99 style inlining.
    #define MR_STATIC_INLINE            static inline
    #define MR_INLINE                   static inline
    #define MR_EXTERN_INLINE            inline
    #define MR_OUTLINE_DEFN(DECL,BODY)  extern DECL;
  #else
    // GNU C90 style inlining.
    #define MR_STATIC_INLINE            static __inline__
    #define MR_INLINE                   static __inline__
    #define MR_EXTERN_INLINE            extern __inline__
    #define MR_OUTLINE_DEFN(DECL,BODY)  DECL BODY
  #endif // ! __GNUC_STDC_INLINE
#elif defined(MR_MSVC)
  #define MR_STATIC_INLINE              static __inline
  #define MR_INLINE                     static __inline
  #define MR_EXTERN_INLINE              extern __inline
  #define MR_OUTLINE_DEFN(DECL, BODY)
#elif __STDC_VERSION__ >= 199901
  // C99
  #define MR_STATIC_INLINE              static inline
  #define MR_INLINE                     static inline
  #define MR_EXTERN_INLINE              inline
  #define MR_OUTLINE_DEFN(DECL,BODY)    extern DECL;
#else
  // C89
  #define MR_STATIC_INLINE              static
  #define MR_INLINE                     static
  #define MR_EXTERN_INLINE              static
  #define MR_OUTLINE_DEFN(DECL,BODY)
#endif

////////////////////////////////////////////////////////////////////////////

// A macro for declaring functions that never return.

#if defined(MR_GNUC) || defined(MR_CLANG)
  #define MR_NO_RETURN(x) x __attribute__((noreturn))
#elif defined(MR_MSVC)
  #define MR_NO_RETURN(x) __declspec(noreturn) x
#else
  #define MR_NO_RETURN(x) x
#endif

////////////////////////////////////////////////////////////////////////////

// MR_CALL:
// A macro for specifying the calling convention to use for C functions
// generated by the MLDS back-end (and for builtins such as unification
// which must use the same calling convention). This can expand to whatever
// implementation-specific magic is required to tell the C compiler to use
// a different calling convention.
//
// If MR_USE_REGPARM was defined, and we were using gcc on x86,
// then we USED to use a non-standard but more efficient calling convention
// that passes parameters in registers. Otherwise we just used the default
// C calling convention.
//
// However, since 32-bit x86s are rare these days, and this non-standard
// calling convention is *slower* on x86/64 platforms than the standard one,
// we don't do this anymore.

#define MR_CALL

////////////////////////////////////////////////////////////////////////////

// C preprocessor tricks.

// Convert a macro to a string.
#define MR_STRINGIFY(x)     MR_STRINGIFY_2(x)
#define MR_STRINGIFY_2(x)   #x

// Paste some macros together.
#define MR_PASTE2(a,b)          MR_PASTE2_2(a,b)
#define MR_PASTE2_2(a,b)        a##b
#define MR_PASTE3(a,b,c)        MR_PASTE3_2(a,b,c)
#define MR_PASTE3_2(a,b,c)      a##b##c
#define MR_PASTE4(a,b,c,d)      MR_PASTE4_2(a,b,c,d)
#define MR_PASTE4_2(a,b,c,d)        a##b##c##d
#define MR_PASTE5(a,b,c,d,e)        MR_PASTE5_2(a,b,c,d,e)
#define MR_PASTE5_2(a,b,c,d,e)      a##b##c##d##e
#define MR_PASTE6(a,b,c,d,e,f)      MR_PASTE6_2(a,b,c,d,e,f)
#define MR_PASTE6_2(a,b,c,d,e,f)    a##b##c##d##e##f
#define MR_PASTE7(a,b,c,d,e,f,g)    MR_PASTE7_2(a,b,c,d,e,f,g)
#define MR_PASTE7_2(a,b,c,d,e,f,g)  a##b##c##d##e##f##g
#define MR_PASTE8(a,b,c,d,e,f,g,h)      MR_PASTE8_2(a,b,c,d,e,f,g,h)
#define MR_PASTE8_2(a,b,c,d,e,f,g,h)        a##b##c##d##e##f##g##h
#define MR_PASTE9(a,b,c,d,e,f,g,h,i)        MR_PASTE9_2(a,b,c,d,e,f,g,h,i)
#define MR_PASTE9_2(a,b,c,d,e,f,g,h,i)      a##b##c##d##e##f##g##h##i
#define MR_PASTE10(a,b,c,d,e,f,g,h,i,j)     MR_PASTE10_2(a,b,c,d,e,f,g,h,i,j)
#define MR_PASTE10_2(a,b,c,d,e,f,g,h,i,j)   a##b##c##d##e##f##g##h##i##j
#define MR_PASTE11(a,b,c,d,e,f,g,h,i,j,k)                               \
                MR_PASTE11_2(a,b,c,d,e,f,g,h,i,j,k)
#define MR_PASTE11_2(a,b,c,d,e,f,g,h,i,j,k)                             \
                a##b##c##d##e##f##g##h##i##j##k
#define MR_PASTE12(a,b,c,d,e,f,g,h,i,j,k,l)                             \
                MR_PASTE12_2(a,b,c,d,e,f,g,h,i,j,k,l)
#define MR_PASTE12_2(a,b,c,d,e,f,g,h,i,j,k,l)                           \
                a##b##c##d##e##f##g##h##i##j##k##l

// MR_CHECK_EXPR_TYPE(expr, type):
// This macro checks that the given expression has a type
// which is compatible with (assignable to) the specified type,
// forcing a compile error if it does not.
// It does not evaluate the expression.
// Note that the specified type must be a complete type,
// i.e. it must not be a pointer to a struct which has not been defined.
//
// This macro is useful for defining type-safe function-like macros.
//
// The implementation of this macro looks like it dereferences
// a null pointer, but because that code is inside sizeof(), it will
// not get executed; the compiler will instead just check that it is
// type-correct.

#define MR_CHECK_EXPR_TYPE(expr, type)                                  \
    ((void) sizeof(*(type *)NULL = (expr)))

// MR_STATIC_ASSERT(module, expr):
// Assert at compile time that the given expression is true.
// A named bit-field may not have zero width.

#define MR_STATIC_ASSERT(module, expr)                                  \
    struct MR_PASTE4(static_assert_, module, _line_, __LINE__)          \
    { unsigned int bf : !!(expr); }

////////////////////////////////////////////////////////////////////////////

#define MR_SORRY(msg) MR_fatal_error("Sorry, not yet implemented: " msg);

////////////////////////////////////////////////////////////////////////////

// MR_IF is for writing if-then-elses that must expand to expressions, not
// statements.

#define MR_IF(cond, val) ((cond) ? ((val), (void)0) : (void)0)

////////////////////////////////////////////////////////////////////////////

// This macro is needed by both mercury_float.h and mercury_int.h.

#if defined(MR_DEBUG_DWORD_ALIGNMENT) &&                                \
      (defined(MR_GNUC) || defined(MR_CLANG))
  #define MR_dword_ptr(ptr)                                             \
    ({                                                                  \
        MR_Word __addr = (MR_Word) (ptr);                               \
        assert((__addr % 8) == 0);                                      \
        /* return */ (void *) __addr;                                   \
    })
#else
  #define MR_dword_ptr(ptr)   ((void *) (ptr))
#endif

////////////////////////////////////////////////////////////////////////////

#endif // not MERCURY_STD_H
