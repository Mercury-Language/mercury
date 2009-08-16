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

/*
** AMD say that __amd64__ is defined by the compiler for 64bit platforms,
** Intel say that __x86_64__ is the correct macro.  Really these refer to
** the same thing that is simply branded differently, we use __amd64__ below
** and define it if necessary ourselves.
*/
#if defined(__x86_64__) && !defined(__amd64__)
#define __amd64__
#endif

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

#elif defined(__GNUC__) && defined(__amd64__)

    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            char result;                                                    \
                                                                            \
            __asm__ __volatile__(                                           \
                "lock; cmpxchgq %3, %0; setz %1"                            \
                : "=m"(*addr), "=q"(result)                                 \
                : "m"(*addr), "r" (new_val), "a"(old)                       \
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
                );                                                          \
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

#if defined(__GNUC__) && defined(__amd64__)

    #define MR_ATOMIC_INC_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; incq %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#elif defined(__GNUC__) && defined(__i386__)

    /* Really 486 or better. */
    #define MR_ATOMIC_INC_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; incl %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#else

    /*
    ** Fall back to an atomic add 1 operation.
    **
    ** We could fall back to the built-in GCC instructions but they also fetch
    ** the value.  I believe this is more efficient.
    **  - pbone
    */
    #define MR_ATOMIC_INC_INT_BODY                                          \
        MR_atomic_add_int(addr, 1)                                          \

#endif

#ifdef MR_ATOMIC_INC_INT_BODY
    MR_EXTERN_INLINE void 
    MR_atomic_inc_int(volatile MR_Integer *addr)
    {
        MR_ATOMIC_INC_INT_BODY;
    }
#endif

/*---------------------------------------------------------------------------*/

/*
** Decrement the word pointed at by the address.
*/
MR_EXTERN_INLINE void
MR_atomic_dec_int(volatile MR_Integer *addr);

#if defined(__GNUC__) && defined(__amd64__)

    #define MR_ATOMIC_DEC_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; decq %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#elif defined(__GNUC__) && defined(__i386__)

    /* Really 486 or better. */
    #define MR_ATOMIC_DEC_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; decl %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)
#else
    /*
    ** Fall back to an atomic subtract 1 operation.
    */

    #define MR_ATOMIC_DEC_INT_BODY                                          \
        MR_atomic_sub_int(addr, 1)

#endif

#ifdef MR_ATOMIC_DEC_INT_BODY
    MR_EXTERN_INLINE void 
    MR_atomic_dec_int(volatile MR_Integer *addr)
    {
        MR_ATOMIC_DEC_INT_BODY;
    }
#endif

MR_EXTERN_INLINE void
MR_atomic_add_int(volatile MR_Integer *addr, MR_Integer addend);

#if defined(__GNUC__) && defined(__amd64__)

    #define MR_ATOMIC_ADD_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; addq %2, %0"                                         \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(addend)                                   \
                );                                                          \
        } while (0)
    
#elif defined(__GNUC__) && defined(__i386__)
    
    #define MR_ATOMIC_ADD_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; addl %2, %0;"                                        \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(addend)                                   \
                );                                                          \
        } while (0)

#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)

    #define MR_ATOMIC_ADD_INT_BODY                                          \
        do {                                                                \
            __sync_add_and_fetch(addr, addend);                             \
        } while (0)

#endif

#ifdef MR_ATOMIC_ADD_INT_BODY
    MR_EXTERN_INLINE void 
    MR_atomic_add_int(volatile MR_Integer *addr, MR_Integer addend)
    {
        MR_ATOMIC_ADD_INT_BODY;
    }
#endif

MR_EXTERN_INLINE void
MR_atomic_sub_int(volatile MR_Integer *addr, MR_Integer x);

#if defined(__GNUC__) && defined(__amd64__)

    #define MR_ATOMIC_SUB_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; subq %2, %0;"                                        \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(x)                                        \
                );                                                          \
        } while (0)
    
#elif defined(__GNUC__) && defined(__i386__)
    
    #define MR_ATOMIC_SUB_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; subl %2, %0;"                                        \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(x)                                        \
                );                                                          \
        } while (0)

#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1)

    #define MR_ATOMIC_SUB_INT_BODY                                          \
        do {                                                                \
            __sync_sub_and_fetch(addr, x);                                  \
        } while (0)

#endif

#ifdef MR_ATOMIC_SUB_INT_BODY
    MR_EXTERN_INLINE void
    MR_atomic_sub_int(volatile MR_Integer *addr, MR_Integer x)
    {
        MR_ATOMIC_SUB_INT_BODY;
    }
#endif

/*
 * Intel and AMD support a pause instruction that is roughly equivalent
 * to a no-op.  Intel recommend that it is used in spin-loops to improve
 * performance.  Without a pause instruction multiple simultaneous
 * read-requests will be in-flight for the synchronization variable from a
 * single thread.  Giving the pause instruction causes these to be executed
 * in sequence allowing the processor to handle the change in the
 * synchronization variable more easily.
 *
 * On some chips it may cause the spin-loop to use less power.
 *
 * This instruction was introduced with the Pentium 4 but is backwards
 * compatible, This works because the two byte instruction for PAUSE is
 * equivalent to the NOP instruction prefixed by REPE.  Therefore older
 * processors perform a no-op.
 *
 * This is not really an atomic instruction but we name it
 * MR_ATOMIC_PAUSE for consistency.
 *
 * References: Intel and AMD documentation for PAUSE, Intel optimisation
 * guide.
 */
#if defined(__GNUC__) && ( defined(__i386__) || defined(__amd64__) )

    #define MR_ATOMIC_PAUSE                                                 \
        do {                                                                \
            __asm__ __volatile__("pause");                                  \
        } while(0)

#else

    /* Fall back to a no-op */
    #define MR_ATOMIC_PAUSE                                                 \
        do {                                                                \
            ;                                                               \
        } while(0)

#endif

#endif /* MR_LL_PARALLEL_CONJ */

/*
** If we don't have definitions available for this compiler or architecture
** then we will get a link error in low-level .par grades.  No other grades
** currently require any atomic ops.
*/

/*---------------------------------------------------------------------------*/

#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)

typedef struct {
    MR_Unsigned         MR_stat_count_recorded;
    MR_Unsigned         MR_stat_count_not_recorded;
        /*
        ** The total number of times this event occurred is implicitly the
        ** sum of the recorded and not_recorded counts.
        */
    MR_int_least64_t    MR_stat_sum;
    MR_uint_least64_t   MR_stat_sum_squares;
        /*
        ** The sum of squares is used to calculate variance and standard
        ** deviation.
        */
} MR_Stats;

typedef struct {
    MR_uint_least64_t   MR_timer_time;
    MR_Unsigned         MR_timer_processor_id;
} MR_Timer;

/*
** Configure the profiling stats code.  On i386 and amd64 machines this uses
** CPUID to determine if the RDTSCP instruction is available and not prohibited
** by the OS.
*/
extern void
MR_configure_profiling_timers(void);

/*
** Start and initialize a timer structure.
*/
extern void
MR_profiling_start_timer(MR_Timer *timer);

/*
** Stop the timer and update stats with the results.
*/
extern void
MR_profiling_stop_timer(MR_Timer *timer, MR_Stats *stats);

#endif /* MR_THREAD_SAFE && MR_PROFILE_PARALLEL_EXECUTION */

/*---------------------------------------------------------------------------*/

#endif /* not MERCURY_ATOMIC_OPS_H */
