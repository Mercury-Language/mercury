// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_atomic.h - defines atomic operations and other primitives used by
// the parallel runtime.
//
// XXX we should have a version of this module that uses C11 atomics
// where possible.

#ifndef MERCURY_ATOMIC_OPS_H
#define MERCURY_ATOMIC_OPS_H

#if __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_ATOMICS__)
#include <stdatomic.h>
#else
#define _Atomic
#endif
#include "mercury_std.h"

////////////////////////////////////////////////////////////////////////////

// Use this to make some storage volatile only when using a threadsafe grade.

#ifdef MR_THREAD_SAFE
  #define MR_THREADSAFE_VOLATILE    volatile
#else
  #define MR_THREADSAFE_VOLATILE
#endif

#if defined(MR_THREAD_SAFE)

// Intel and AMD support a pause instruction that is roughly equivalent
// to a no-op. Intel recommend that it is used in spin-loops to improve
// performance. Without a pause instruction, multiple simultaneous
// read-requests will be in-flight for the synchronization variable from a
// single thread. Giving the pause instruction causes these to be executed
// in sequence, allowing the processor to handle the change in the
// synchronization variable more easily.
//
// On some chips it may cause the spin-loop to use less power.
//
// This instruction was introduced with the Pentium 4 but is backwards
// compatible, This works because the two byte instruction for PAUSE is
// equivalent to the NOP instruction prefixed by REPE. Therefore older
// processors perform a no-op.
//
// This is not really an atomic instruction but we name it MR_ATOMIC_PAUSE
// for consistency.
//
// References: Intel and AMD documentation for PAUSE, Intel optimisation guide.

#if ( defined(MR_CLANG) || defined(MR_GNUC) ) &&                        \
    ( defined(__i386__) || defined(__x86_64__) ) &&                     \
    !defined(MR_DO_NOT_USE_CPU_RELAX)

    #define MR_ATOMIC_PAUSE                                                 \
        do {                                                                \
            __asm__ __volatile__("pause");                                  \
        } while (0)

#else

    // Fall back to a no-op
    #define MR_ATOMIC_PAUSE                                                 \
        do {                                                                \
            ;                                                               \
        } while (0)

#endif

////////////////////////////////////////////////////////////////////////////

// Declarations for inline atomic operations.
//
// These operations work on machine word-sized values, this is distinct from
// C's idea of 'int' and 'unsigned int'. MR_Integer and MR_Unsigned are
// supposed to be machine word sized so these functions should only be used
// with values of these types.

// If the value at addr is equal to old, assign new to addr and return true.
// Otherwise return false.

MR_EXTERN_INLINE MR_bool    MR_compare_and_swap_int(
                                volatile MR_Integer *addr,
                                MR_Integer old, MR_Integer new_val);
MR_EXTERN_INLINE MR_bool    MR_compare_and_swap_uint(
                                volatile MR_Unsigned *addr,
                                MR_Unsigned old, MR_Unsigned new_val);

// Atomically add to an integer in memory and retrieve the result. In other
// words an atomic pre-increment operation.

MR_EXTERN_INLINE MR_Integer MR_atomic_add_and_fetch_int(
                                volatile MR_Integer *addr, MR_Integer addend);
MR_EXTERN_INLINE MR_Unsigned MR_atomic_add_and_fetch_uint(
                                volatile MR_Unsigned *addr, MR_Unsigned addend);

// Atomically add the second argument to the memory pointed to by the first
// argument.

MR_EXTERN_INLINE void       MR_atomic_add_int(volatile MR_Integer *addr,
                                MR_Integer addend);
MR_EXTERN_INLINE void       MR_atomic_add_uint(volatile MR_Unsigned *addr,
                                MR_Unsigned addend);

// Atomically subtract the second argument from the memory pointed to by the
// first argument.

MR_EXTERN_INLINE void       MR_atomic_sub_int(volatile _Atomic MR_Integer *addr,
                                MR_Integer x);

// Increment the word pointed at by the address.

MR_EXTERN_INLINE void       MR_atomic_inc_int(volatile MR_Integer *addr);
MR_EXTERN_INLINE void       MR_atomic_inc_uint(volatile MR_Unsigned *addr);

// Decrement the word pointed at by the address.

MR_EXTERN_INLINE void       MR_atomic_dec_int(volatile MR_Integer *addr);

// Decrement the integer pointed at by the address and return true iff it is
// zero after the decrement.

MR_EXTERN_INLINE MR_bool    MR_atomic_dec_and_is_zero_int(
                                volatile MR_Integer *addr);
MR_EXTERN_INLINE MR_bool    MR_atomic_dec_and_is_zero_uint(
                                volatile MR_Unsigned *addr);

// For information about GCC's builtins for atomic operations see:
// http://gcc.gnu.org/onlinedocs/gcc-4.2.4/gcc/Atomic-Builtins.html

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

#if __STD_VERSION__ >= 201112L && !defined(__STD_NO_ATOMICS__)

    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            return atomic_compare_exchange_strong(addr, old, new_val);      \
        } while (0)

#elif (defined(MR_CLANG) || (MR_GNUC > 4 || (MR_GNUC == 4 && __GNUC_MINOR__ >= 1))) && \
    !defined(MR_AVOID_COMPILER_INTRINSICS)

    // gcc 4.1 and above have builtin atomic operations.

    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            return __sync_bool_compare_and_swap(addr, old, new_val);        \
        } while (0)

#elif defined(MR_GNUC) && defined(__x86_64__)

    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            char result;                                                    \
                                                                            \
            __asm__ __volatile__(                                           \
                "lock; cmpxchgq %4, %0; setz %1"                            \
                : "=m"(*addr), "=q"(result), "=a"(old)                      \
                : "m"(*addr), "r" (new_val), "a"(old)                       \
            );                                                              \
            return (MR_bool) result;                                        \
        } while (0)

#elif defined(MR_GNUC) && defined(__i386__)

    // Really 486 or better.
    #define MR_COMPARE_AND_SWAP_WORD_BODY                                   \
        do {                                                                \
            char result;                                                    \
                                                                            \
            __asm__ __volatile__(                                           \
                "lock; cmpxchgl %4, %0; setz %1"                            \
                : "=m"(*addr), "=q"(result), "=a"(old)                      \
                : "m"(*addr), "r" (new_val), "a"(old)                       \
                );                                                          \
            return (MR_bool) result;                                        \
        } while (0)

#endif

#ifdef MR_COMPARE_AND_SWAP_WORD_BODY
    MR_EXTERN_INLINE MR_bool
    MR_compare_and_swap_int(volatile MR_Integer *addr, MR_Integer old,
            MR_Integer new_val)
    {
        MR_COMPARE_AND_SWAP_WORD_BODY;
    }

    MR_EXTERN_INLINE MR_bool
    MR_compare_and_swap_uint(volatile MR_Unsigned *addr, MR_Unsigned old,
            MR_Unsigned new_val)
    {
        MR_COMPARE_AND_SWAP_WORD_BODY;
    }
#endif

////////////////////////////////////////////////////////////////////////////

#if __STD_VERSION__ >= 201112L && !defined(__STD_NO_ATOMICS__)

    #define MR_ATOMIC_ADD_AND_FETCH_WORD_BODY                               \
        do {                                                                \
            return atomic_fetch_add(addr, addend);                          \
        } while (0)

    #define MR_ATOMIC_ADD_AND_FETCH_INT_BODY MR_ATOMIC_ADD_AND_FETCH_WORD_BODY
    #define MR_ATOMIC_ADD_AND_FETCH_UINT_BODY MR_ATOMIC_ADD_AND_FETCH_WORD_BODY

#elif (MR_GNUC > 4 || (MR_GNUC == 4 && __GNUC_MINOR__ >= 1)) &&           \
    !defined(MR_AVOID_COMPILER_INTRINSICS)

    #define MR_ATOMIC_ADD_AND_FETCH_WORD_BODY                               \
        do {                                                                \
            return __sync_add_and_fetch(addr, addend);                      \
        } while (0)

    #define MR_ATOMIC_ADD_AND_FETCH_INT_BODY MR_ATOMIC_ADD_AND_FETCH_WORD_BODY
    #define MR_ATOMIC_ADD_AND_FETCH_UINT_BODY MR_ATOMIC_ADD_AND_FETCH_WORD_BODY

#elif defined(MR_COMPARE_AND_SWAP_WORD_BODY)
    // If there is no GCC builtin for this then it can be implemented in terms
    // of compare and swap, assuming that that has been implemented in
    // assembler for this architecture.
    //
    // XXX: There is an add and exchange (xadd) instruction on x86, this is
    // better than the CAS loop below.

    #define MR_ATOMIC_ADD_AND_FETCH_INT_BODY                                \
        do {                                                                \
            MR_Integer temp;                                                \
            temp = *addr;                                                   \
            while (!MR_compare_and_swap_int(addr, temp, temp+addend)) {     \
                MR_ATOMIC_PAUSE;                                            \
                temp = *addr;                                               \
            }                                                               \
            return temp+addend;                                             \
        } while (0)

    #define MR_ATOMIC_ADD_AND_FETCH_UINT_BODY                               \
        do {                                                                \
            MR_Unsigned temp;                                               \
            temp = *addr;                                                   \
            while (!MR_compare_and_swap_uint(addr, temp, temp+addend)) {    \
                MR_ATOMIC_PAUSE;                                            \
                temp = *addr;                                               \
            }                                                               \
            return temp+addend;                                             \
        } while (0)

#endif

#ifdef MR_ATOMIC_ADD_AND_FETCH_INT_BODY
    MR_EXTERN_INLINE MR_Integer
    MR_atomic_add_and_fetch_int(volatile MR_Integer *addr, MR_Integer addend)
    {
        MR_ATOMIC_ADD_AND_FETCH_INT_BODY;
    }
#endif

#ifdef MR_ATOMIC_ADD_AND_FETCH_UINT_BODY
    MR_EXTERN_INLINE MR_Unsigned
    MR_atomic_add_and_fetch_uint(volatile MR_Unsigned *addr, MR_Unsigned addend)
    {
        MR_ATOMIC_ADD_AND_FETCH_UINT_BODY;
    }
#endif

////////////////////////////////////////////////////////////////////////////

#if (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__x86_64__) &&   \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

    #define MR_ATOMIC_ADD_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; addq %2, %0"                                         \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(addend)                                   \
                );                                                          \
        } while (0)

    #define MR_ATOMIC_ADD_INT_BODY MR_ATOMIC_ADD_WORD_BODY
    #define MR_ATOMIC_ADD_UINT_BODY MR_ATOMIC_ADD_WORD_BODY

#elif (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__i386__)

    #define MR_ATOMIC_ADD_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; addl %2, %0;"                                        \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(addend)                                   \
                );                                                          \
        } while (0)

    #define MR_ATOMIC_ADD_INT_BODY MR_ATOMIC_ADD_WORD_BODY
    #define MR_ATOMIC_ADD_UINT_BODY MR_ATOMIC_ADD_WORD_BODY

#elif defined(MR_ATOMIC_ADD_AND_FETCH_INT_BODY)

    #define MR_ATOMIC_ADD_INT_BODY                                          \
        do {                                                                \
            MR_atomic_add_and_fetch_int(addr, addend);                      \
        } while (0)

    #define MR_ATOMIC_ADD_UINT_BODY                                         \
        do {                                                                \
            MR_atomic_add_and_fetch_uint(addr, addend);                     \
        } while (0)

#endif

#ifdef MR_ATOMIC_ADD_INT_BODY
    MR_EXTERN_INLINE void
    MR_atomic_add_int(volatile MR_Integer *addr, MR_Integer addend)
    {
        MR_ATOMIC_ADD_INT_BODY;
    }
#endif

#ifdef MR_ATOMIC_ADD_UINT_BODY
    MR_EXTERN_INLINE void
    MR_atomic_add_uint(volatile MR_Unsigned *addr, MR_Unsigned addend)
    {
        MR_ATOMIC_ADD_UINT_BODY;
    }
#endif

////////////////////////////////////////////////////////////////////////////

#if __STDC_VERSION__ >= 201112l && !defined(__STDC_NO_ATOMICS__)

    #define MR_ATOMIC_SUB_INT_BODY                                          \
        do {                                                                \
            atomic_fetch_sub(addr, x);                                      \
        } while (0)

#elif (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__x86_64__) &&   \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

    #define MR_ATOMIC_SUB_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; subq %2, %0;"                                        \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(x)                                        \
                );                                                          \
        } while (0)

#elif (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__i386__)

    #define MR_ATOMIC_SUB_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; subl %2, %0;"                                        \
                : "=m"(*addr)                                               \
                : "m"(*addr), "r"(x)                                        \
                );                                                          \
        } while (0)

#elif MR_GNUC > 4 || (MR_GNUC == 4 && __GNUC_MINOR__ >= 1)

    #define MR_ATOMIC_SUB_INT_BODY                                          \
        do {                                                                \
            __sync_sub_and_fetch(addr, x);                                  \
        } while (0)

#endif

#ifdef MR_ATOMIC_SUB_INT_BODY
    MR_EXTERN_INLINE void
    MR_atomic_sub_int(volatile _Atomic MR_Integer *addr, MR_Integer x)
    {
        MR_ATOMIC_SUB_INT_BODY;
    }
#endif

////////////////////////////////////////////////////////////////////////////

#if (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__x86_64__) &&   \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

    #define MR_ATOMIC_INC_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; incq %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

    #define MR_ATOMIC_INC_INT_BODY MR_ATOMIC_INC_WORD_BODY
    #define MR_ATOMIC_INC_UINT_BODY MR_ATOMIC_INC_WORD_BODY

#elif (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__i386__) &&   \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

    // Really 486 or better.
    #define MR_ATOMIC_INC_WORD_BODY                                         \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; incl %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

    #define MR_ATOMIC_INC_INT_BODY MR_ATOMIC_INC_WORD_BODY
    #define MR_ATOMIC_INC_UINT_BODY MR_ATOMIC_INC_WORD_BODY

#else

    // Fall back to an atomic add 1 operation.
    //
    // We could fall back to the built-in GCC instructions but they also fetch
    // the value. I believe this is more efficient. pbone

    #define MR_ATOMIC_INC_INT_BODY                                          \
        MR_atomic_add_int(addr, 1)
    #define MR_ATOMIC_INC_UINT_BODY                                         \
        MR_atomic_add_uint(addr, 1)

#endif

#ifdef MR_ATOMIC_INC_INT_BODY
    MR_EXTERN_INLINE void
    MR_atomic_inc_int(volatile MR_Integer *addr)
    {
        MR_ATOMIC_INC_INT_BODY;
    }
#endif

#ifdef MR_ATOMIC_INC_UINT_BODY
    MR_EXTERN_INLINE void
    MR_atomic_inc_uint(volatile MR_Unsigned *addr)
    {
        MR_ATOMIC_INC_UINT_BODY;
    }
#endif

////////////////////////////////////////////////////////////////////////////

#if (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__x86_64__) &&   \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

    #define MR_ATOMIC_DEC_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; decq %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)

#elif (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__i386__) &&   \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

    // Really 486 or better.
    #define MR_ATOMIC_DEC_INT_BODY                                          \
        do {                                                                \
            __asm__ __volatile__(                                           \
                "lock; decl %0;"                                            \
                : "=m"(*addr)                                               \
                : "m"(*addr)                                                \
                );                                                          \
        } while (0)
#else
    // Fall back to an atomic subtract 1 operation.

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

////////////////////////////////////////////////////////////////////////////

// Note that on x86(_64) we have to use the sub instruction rather than the
// dec instruction because we need it to set the CPU flags.

#if (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__x86_64__) &&   \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

// This could be trivially implemented using the __sync_sub_and_fetch compiler
// intrinsic. However on some platforms this could use a compare and exchange
// loop. We can avoid this because we don't need to retrieve the result of the
// subtraction.

    #define MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY                             \
        do {                                                                \
            char is_zero;                                                   \
            __asm__(                                                        \
                "lock; subq $1, %0; setz %1"                                \
                : "=m"(*addr), "=q"(is_zero)                                \
                : "m"(*addr)                                                \
                );                                                          \
            return (MR_bool)is_zero;                                        \
        } while (0)

    #define MR_ATOMIC_DEC_AND_IS_ZERO_INT_BODY                          \
        MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY
    #define MR_ATOMIC_DEC_AND_IS_ZERO_UINT_BODY                         \
        MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY

#elif (defined(MR_CLANG) || defined(MR_GNUC)) && defined(__i386__)

    #define MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY                              \
        do {                                                                 \
            char is_zero;                                                    \
            __asm__(                                                         \
                "lock; subl $1, %0; setz %1"                                 \
                : "=m"(*addr), "=q"(is_zero)                                 \
                : "m"(*addr)                                                 \
                );                                                           \
            return (MR_bool)is_zero;                                         \
        } while (0)

    #define MR_ATOMIC_DEC_AND_IS_ZERO_INT_BODY                          \
        MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY
    #define MR_ATOMIC_DEC_AND_IS_ZERO_UINT_BODY                         \
        MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY

#elif MR_GNUC > 4 || (MR_GNUC == 4 && __GNUC_MINOR__ >= 1)

    #define MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY                             \
        do {                                                                \
            return (__sync_sub_and_fetch(addr, 1) == 0);                    \
        } while (0)

    #define MR_ATOMIC_DEC_AND_IS_ZERO_INT_BODY                          \
        MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY
    #define MR_ATOMIC_DEC_AND_IS_ZERO_UINT_BODY                         \
        MR_ATOMIC_DEC_AND_IS_ZERO_WORD_BODY

#endif

#ifdef MR_ATOMIC_DEC_AND_IS_ZERO_INT_BODY
    MR_EXTERN_INLINE MR_bool
    MR_atomic_dec_and_is_zero_int(volatile MR_Integer *addr)
    {
        MR_ATOMIC_DEC_AND_IS_ZERO_INT_BODY;
    }
#endif

#ifdef MR_ATOMIC_DEC_AND_IS_ZERO_UINT_BODY
    MR_EXTERN_INLINE MR_bool
    MR_atomic_dec_and_is_zero_uint(volatile MR_Unsigned *addr)
    {
        MR_ATOMIC_DEC_AND_IS_ZERO_UINT_BODY;
    }
#endif

#endif // MR_THREAD_SAFE

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

#ifdef MR_THREAD_SAFE

// Memory fence operations.

#if ( defined(MR_CLANG) || defined(MR_GNUC) ) &&                        \
    ( defined(__i386__) || defined(__x86_64__) ) &&                     \
    !defined(MR_AVOID_HANDWRITTEN_ASSEMBLER)

    // Guarantees that any stores executed before this fence are
    // globally visible before those after this fence.

    #define MR_CPU_SFENCE                                                   \
        do {                                                                \
            __asm__ __volatile__("sfence");                                 \
        } while (0)

    // Guarantees that any loads executed before this fence are complete
    // before any loads after this fence.

    #define MR_CPU_LFENCE                                                   \
        do {                                                                \
            __asm__ __volatile__("lfence");                                 \
        } while (0)

    // A combination of the above.

    #define MR_CPU_MFENCE                                                   \
        do {                                                                \
            __asm__ __volatile__("mfence");                                 \
        } while (0)

#elif defined(MR_CLANG) || MR_GNUC > 4 || (MR_GNUC == 4 && __GNUC_MINOR__ >= 1)

    // Our memory fences are better than GCC's. GCC only implements a full
    // fence.

    #define MR_CPU_MFENCE                                                   \
        do {                                                                \
            __sync_synchronize();                                           \
        } while (0)
    #define MR_CPU_SFENCE MR_CPU_MFENCE
    #define MR_CPU_LFENCE MR_CPU_MFENCE

#else

    // Do not break this string literal; doing so produces an unreadable
    // mess with clang.
    #error "Please implement memory fence operations for this compiler/architecture"

#endif

#endif // MR_THREAD_SAFE

////////////////////////////////////////////////////////////////////////////

#ifdef MR_LL_PARALLEL_CONJ

// Roll our own cheap user-space mutual exclusion locks. Blocking without
// spinning is not supported. Storage for these locks should be volatile.
//
// I expect these to be faster than pthread mutexes when threads are pinned
// and critical sections are short.

typedef MR_Unsigned MR_Us_Lock;

#define MR_US_LOCK_INITIAL_VALUE (0)

#define MR_US_TRY_LOCK(x)                                                   \
    MR_compare_and_swap_uint(x, 0, 1)

#define MR_US_SPIN_LOCK(x)                                                  \
    do {                                                                    \
        while (!MR_compare_and_swap_uint(x, 0, 1)) {                        \
            MR_ATOMIC_PAUSE;                                                \
        }                                                                   \
    } while (0)

#define MR_US_UNLOCK(x)                                                     \
    do {                                                                    \
        MR_CPU_MFENCE;                                                      \
        *x = 0;                                                             \
    } while (0)

// Similar support for condition variables. Again, make sure that storage for
// these is declared as volatile.
//
// XXX: These are not atomic, A waiting thread will not see a change until
// sometime after the signaling thread has signaled the condition. The same
// race can occur when clearing a condition. Order of memory operations is not
// guaranteed either.

typedef MR_Unsigned MR_Us_Cond;

#define MR_US_COND_CLEAR(x)                                                 \
    do {                                                                    \
        MR_CPU_MFENCE;                                                      \
        *x = 0;                                                             \
    } while (0)

#define MR_US_COND_SET(x)                                                   \
    do {                                                                    \
        MR_CPU_MFENCE;                                                      \
        *x = 1;                                                             \
        MR_CPU_MFENCE;                                                      \
    } while (0)

#define MR_US_SPIN_COND(x)                                                  \
    do {                                                                    \
        while (!(*x)) {                                                     \
            MR_ATOMIC_PAUSE;                                                \
        }                                                                   \
        MR_CPU_MFENCE;                                                      \
    } while (0)

#endif // MR_LL_PARALLEL_CONJ

// If we don't have definitions available for this compiler or architecture
// then we will get a link error in low-level .par grades. No other grades
// currently require any atomic ops.

////////////////////////////////////////////////////////////////////////////

#if defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)

// Declarations for profiling the parallel runtime.

typedef struct {
    // The total number of times this event occurred is implicitly the sum of
    // the recorded and not_recorded counts.

    volatile MR_Unsigned    MR_stat_count_recorded;
    volatile MR_Unsigned    MR_stat_count_not_recorded;

    // Atomic instructions are used to update these fields, and these fields
    // must be 64 bit to contain the valid ranges of values. However a 32 bit
    // machine cannot (usually) do atomic operations on 64 bit data. Therefore
    // if we have fewer than 64 bits we protect these two fields with a lock.
    //
    // The sum of squares is used to calculate variance and standard deviation.

  #if MR_LOW_TAG_BIGS >= 3
    volatile MR_Integer     MR_stat_sum;
    volatile MR_Unsigned    MR_stat_sum_squares;
  #else
    MR_Us_Lock              MR_stat_sums_lock;
    MR_int_least64_t        MR_stat_sum;
    MR_uint_least64_t       MR_stat_sum_squares;
  #endif
} MR_Stats;

typedef struct {
    MR_uint_least64_t   MR_timer_time;
    MR_Unsigned         MR_timer_processor_id;
} MR_Timer;

// The number of CPU clock cycles per second, ie a 1GHz CPU will have a value
// of 10^9, zero if unknown.
// This value is only available after MR_do_cpu_feature_detection() has been
// called.

extern MR_uint_least64_t MR_cpu_cycles_per_sec;

// Do CPU feature detection, this is necessary for profiling parallel code
// execution and the threadscope code.
// On i386 and x86_64 machines this uses CPUID to determine if the RDTSCP
// instruction is available and not prohibited by the OS.
// This function is idempotent.

extern void     MR_do_cpu_feature_detection(void);

// Start and initialize a timer structure.

extern void     MR_profiling_start_timer(MR_Timer *timer);

// Stop the timer and update stats with the results.

extern void     MR_profiling_stop_timer(MR_Timer *timer, MR_Stats *stats);

// The TSC works and MR_cpu_cycles_per_sec is nonzero.

extern MR_bool  MR_tsc_is_sensible(void);

// Read the CPU's TSC. This is currently only implemented for i386 and x86-64
// systems. It returns 0 when support is not available.

extern MR_uint_least64_t    MR_read_cpu_tsc(void);

#endif // MR_PROFILE_PARALLEL_EXECUTION_SUPPORT

////////////////////////////////////////////////////////////////////////////

#endif // not MERCURY_ATOMIC_OPS_H
