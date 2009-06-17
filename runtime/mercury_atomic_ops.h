/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007, 2009 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_atomic.h - defines atomic operations.
*/

#ifndef MERCURY_ATOMIC_OPS_H
#define MERCURY_ATOMIC_OPS_H

#include "mercury_std.h"

/*---------------------------------------------------------------------------*/
#if defined(MR_LL_PARALLEL_CONJ)

/*
** If the value at addr is equal to old, assign new to addr and return true.
** Otherwise return false.
*/
MR_EXTERN_INLINE MR_bool
MR_compare_and_swap_word(volatile MR_Integer *addr, MR_Integer old,
        MR_Integer new_val);

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)

    /*
    ** gcc 4.1 and above have builtin atomic operations.
    */
    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            return __sync_bool_compare_and_swap(addr, old, new_val);        \
        } while (0)

#elif defined(__GNUC__) && defined(__x86_64__)

    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            char result;                                                    \
                                                                            \
            __asm__ __volatile__(                                           \
                "lock; cmpxchgq %3, %0; setz %1"                            \
                : "=m"(*addr), "=q"(result)                                 \
                : "m"(*addr), "r" (new_val), "a"(old)                       \
                : "memory"                                                  \
            );                                                              \
            return (int) result;                                            \
        } while (0)

#elif defined(__GNUC__) && defined(__i386__)

    /* Really 486 or better. */
    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            char result;                                                    \
                                                                            \
            __asm__ __volatile__(                                           \
                "lock; cmpxchgl %3, %0; setz %1"                            \
                : "=m"(*addr), "=q"(result)                                 \
                : "m"(*addr), "r" (new_val), "a"(old)                       \
                : "memory");                                                \
            return (int) result;                                            \
        } while (0)

#endif

#ifdef MR_COMPARE_AND_SWAP_WORD_BODY
    MR_EXTERN_INLINE MR_bool
    MR_compare_and_swap_word(volatile MR_Integer *addr, MR_Integer old,
            MR_Integer new_val) 
    {
        MR_COMPARE_AND_SWAP_WORD_BODY;
    }
#endif

/*---------------------------------------------------------------------------*/

/*
** Increment the word pointed at by the address.
*/
MR_EXTERN_INLINE void
MR_atomic_inc_int(volatile MR_Integer *addr);

#if defined(__GNUC__) && defined(__x86_64__)

    #define MR_ATOMIC_INC_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; incq %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#elif defined(__GNUC__) && defined(__i386__)

    /* Really 486 or better. */
    #define MR_ATOMIC_INC_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; incl %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)

    /*
    ** gcc doesn't seem to have an atomic operation for increment, it does have
    ** one for add though.  We prefer the hand-written increment operations
    ** above.
    */
    #define MR_ATOMIC_INC_WORD_BODY                                         \
        do {                                                                \
            __sync_add_and_fetch(addr, 1);                                  \
        } while (0)

#endif

#ifdef MR_ATOMIC_INC_WORD_BODY
    MR_EXTERN_INLINE void 
    MR_atomic_inc_int(volatile MR_Integer *addr)
    {
        MR_ATOMIC_INC_WORD_BODY;
    }
#endif

/*---------------------------------------------------------------------------*/

/*
** Decrement the word pointed at by the address.
*/
MR_EXTERN_INLINE void
MR_atomic_dec_int(volatile MR_Integer *addr);

#if defined(__GNUC__) && defined(__x86_64__)

    #define MR_ATOMIC_DEC_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; decq %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#elif defined(__GNUC__) && defined(__i386__)

    /* Really 486 or better. */
    #define MR_ATOMIC_DEC_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; decl %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)

    /*
    ** gcc doesn't seem to have an atomic operation for increment, it does have
    ** one for add though.  We prefer the hand-written increment operations
    ** above.
    */
    #define MR_ATOMIC_DEC_WORD_BODY                                         \
        do {                                                                \
            __sync_sub_and_fetch(addr, 1);                                  \
        } while (0)

#endif

#ifdef MR_ATOMIC_DEC_WORD_BODY
    MR_EXTERN_INLINE void 
    MR_atomic_dec_int(volatile MR_Integer *addr)
    {
        MR_ATOMIC_DEC_WORD_BODY;
    }
#endif

#endif /* MR_LL_PARALLEL_CONJ */
/*---------------------------------------------------------------------------*/

/*
** If we don't have definitions available for this compiler or architecture
** then we will get a link error in low-level .par grades.  No other grades
** currently require any atomic ops.
*/

#endif /* not MERCURY_ATOMIC_OPS_H */
