/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007, 2009 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_atomic_ops.c
*/

#include "mercury_imp.h"
#include "mercury_atomic_ops.h"

/*---------------------------------------------------------------------------*/

#if defined(MR_LL_PARALLEL_CONJ)

/*
** Definitions for the atomic functions declared `extern inline'.
*/

MR_OUTLINE_DEFN(
    MR_bool
    MR_compare_and_swap_word(volatile MR_Integer *addr, MR_Integer old,
        MR_Integer new_val)
,
    {
        MR_COMPARE_AND_SWAP_WORD_BODY;
    }
)

MR_OUTLINE_DEFN(
    void 
    MR_atomic_inc_int(volatile MR_Integer *addr)
,
    {
        MR_ATOMIC_INC_INT_BODY;
    }
)

MR_OUTLINE_DEFN(
    void 
    MR_atomic_dec_int(volatile MR_Integer *addr)
,
    {
        MR_ATOMIC_DEC_INT_BODY;
    }
)

MR_OUTLINE_DEFN(
    void
    MR_atomic_add_int(volatile MR_Integer *addr, MR_Integer addend)
,
    {
        MR_ATOMIC_ADD_INT_BODY;
    }
)

MR_OUTLINE_DEFN(
    void
    MR_atomic_sub_int(volatile MR_Integer *addr, MR_Integer x)
,
    {
        MR_ATOMIC_SUB_INT_BODY;
    }
)

#endif /* MR_LL_PARALLEL_CONJ */

/*---------------------------------------------------------------------------*/

#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)

/*
** Profiling of the parallel runtime.
*/

#if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))
static MR_bool  MR_rdtscp_is_available = MR_FALSE;
static MR_bool  MR_rdtsc_is_available = MR_FALSE;
#endif

#if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))

/* Set this to 1 to enable some printfs below */
#define MR_DEBUG_CPU_FEATURE_DETECTION 0 

/*
** cpuid, rdtscp and rdtsc are i386/x86_64 instructions.
*/
static __inline__ void
MR_cpuid(MR_Unsigned code, MR_Unsigned sub_code,
    MR_Unsigned *a, MR_Unsigned *b, MR_Unsigned *c, MR_Unsigned *d);

static __inline__ void
MR_rdtscp(MR_uint_least64_t *tsc, MR_Unsigned *processor_id);

static __inline__ void
MR_rdtsc(MR_uint_least64_t *tsc);

#endif /* __GNUC__ && (__i386__ || __x86_64__) */

extern void 
MR_configure_profiling_timers(void) {
#if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))
    MR_Unsigned     a, b, c, d;
    MR_Unsigned     eflags, old_eflags;

    /* 
    ** Check for the CPUID instruction.  CPUID is supported if we can flip bit
    ** 21 in the CPU's EFLAGS register.  The assembly below is written in a
    ** subset of i386 and x86_64 assembly.  To read and write EFLAGS we have
    ** to go via the C stack.
    */
    __asm__ ("pushf; pop %0"
            :"=r"(eflags));
    old_eflags = eflags;
    /* Flip bit 21 */
    eflags ^= (1 << 21);
    __asm__ ("push %0; popf; pushf; pop %0;"
            :"=r"(eflags)
            :"0"(eflags));

    /*
    ** Test to see if our change held.  We don't restore eflags, a change to
    ** the ID bit has no effect.
    */
    if (eflags == old_eflags)
    {
#if MR_DEBUG_CPU_FEATURE_DETECTION
        fprintf(stderr, "This CPU doesn't support the CPUID instruction.\n",
            eflags, old_eflags);
#endif
        return;
    }

    /*
    ** CPUID 0 gives the maximum basic CPUID page in EAX.  Basic pages go up to
    ** but not including 0x40000000.
    */
    MR_cpuid(0, 0, &a, &b, &c, &d);
    if (a < 1)
        return;

    /* CPUID 1 gives type, family, model and stepping information in EAX. */
    MR_cpuid(1, 0, &a, &b, &c, &d);
    
    /* Bit 4 in EDX is high if RDTSC is available */
    if (d & (1 << 4))
        MR_rdtsc_is_available = MR_TRUE;

    /*
     * BTW: Intel can't count:
     *
     * http://www.pagetable.com/?p=18
     * http://www.codinghorror.com/blog/archives/000364.html
     *
     * 486 (1989): family 4
     * Pentium (1993): family 5
     * Pentium Pro (1995): family 6, models 0 and 1
     * Pentium 2 (1997): family 6, models 3, 5 and 6
     * Pentium 3 (2000): family 6, models 7, 8, 10, 11
     * Itanium (2001): family 7
     * Pentium 4 (2000): family 15/0
     * Itanium 2 (2002): family 15/1 and 15/2
     * Pentium D: family 15/4
     * Pentium M (2003): family 6, models 9 and 13
     * Core (2006): family 6, model 14
     * Core 2 (2006): family 6, model 15
     * i7: family 6, model 26
     * Atom: family 6, model 28
     *
     * This list is incomplete, it doesn't cover AMD or any other brand of x86
     * processor, and it probably doesn't cover all post-pentium Intel
     * processors.
     */

    /* bits 8-11 (first bit (LSB) is bit 0) */
    MR_Unsigned extended_family, basic_family, family,
        extended_model, model;
    basic_family = (a & 0x00000F00) >> 8;
    if (basic_family == 0x0F) {
        /* bits 20-27 */
        extended_family = (a & 0x0FF00000) >> 20;
        family = basic_family + extended_family;
    } else {
        family = basic_family;
    }
    /* 
    ** I'm not using the model value but I'll leave the code here incase we
    ** have a reason to use it in the future.
    */
    /* bits 4-7 */
    model = (a & 0x000000F0) >> 4;
    if ((basic_family == 0x0F) || (basic_family == 0x06))
    {
        /* bits 16-19 */
        extended_model = (a & 0x000F0000) >> 16;
        model += (extended_model << 4);
    }
#if MR_DEBUG_CPU_FEATURE_DETECTION
    fprintf(stderr, "This is family %d and model %d\n", family, model);
#endif 

    /* Now check for P3 or higher since they have the extended pages */
    if (family < 6) {
        /* This is a 486 or Pentium */
        return;
    }
    /*
    ** I could bail out here if this was a pentium 3, but there's a more
    ** reliable check for extended CPUID support below that should work on AMD
    ** chips as well, if I knew all the model numbers for all family 6
    ** processors and knew if they honoured extended CPUID.
    */

    /*
    ** Extended CPUID 0x80000000.
    ** 
    ** EAX contains the maximum extended CPUID node.
    */
    MR_cpuid(0x80000000, 0, &a, &b, &c, &d);
    if ((a & 0x80000000) == 0) {
        /* 
        ** Extended CPUID is not supported.
        ** Note that this check is still not as reliable as I'd like.  If it
        ** succeeds I'm not confident that the processor definitely implements
        ** extended CPUID.
        */
        return;
    }
#if MR_DEBUG_CPU_FEATURE_DETECTION
    fprintf(stderr, "Maximum extended CPUID node: 0x%x\n", a);
#endif
    if (a < 0x80000001)
        return;

    /*
    ** Extended CPUID 0x80000001
    **
    ** If EDX bit 27 is set the RDTSCP instruction is available.
    */
    MR_cpuid(0x80000001, 0, &a, &b, &c, &d);
#if MR_DEBUG_CPU_FEATURE_DETECTION
    fprintf(stderr, "CPUID 0x80000001 EDX: 0x%x\n", d);
#endif
    if (!(d & (1 << 27)))
        return;
   
    /*
    ** Support for RDTSCP appears to be present
    */
#if MR_DEBUG_CPU_FEATURE_DETECTION
    fprintf(stderr, "RDTSCP is available\n");
#endif
    MR_rdtscp_is_available = MR_TRUE;

#endif /* __GNUC__ && (__i386__ || __x86_64__) */
}

extern void
MR_profiling_start_timer(MR_Timer *timer) {
#if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))
    /*
    ** If we don't have enough data to fill in all the fields of this structure
    ** we leave them alone, we won't check them later without checking
    ** MR_rdtsc{p}_is_available first.
    */
    if (MR_rdtscp_is_available == MR_TRUE)
    {
        MR_rdtscp(&(timer->MR_timer_time), &(timer->MR_timer_processor_id));
    }
    else if (MR_rdtsc_is_available == MR_TRUE)
    {
        MR_rdtsc(&(timer->MR_timer_time));
    }
#endif
}

extern void
MR_profiling_stop_timer(MR_Timer *timer, MR_Stats *stats) {
#if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))
    MR_Timer            now;
    MR_int_least64_t    duration;
    MR_uint_least64_t   duration_squared;

    if (MR_rdtscp_is_available == MR_TRUE)
    {
        MR_rdtscp(&(now.MR_timer_time), &(now.MR_timer_processor_id));
        if (timer->MR_timer_processor_id == now.MR_timer_processor_id)
        {
            duration = now.MR_timer_time - timer->MR_timer_time;
            duration_squared = duration * duration;
            MR_atomic_inc_int(&(stats->MR_stat_count_recorded));
            MR_atomic_add_int(&(stats->MR_stat_sum), duration);
            MR_atomic_add_int(&(stats->MR_stat_sum_squares), duration_squared);
        } else {
            MR_atomic_inc_int(&(stats->MR_stat_count_not_recorded));
        }
    }
    else if (MR_rdtsc_is_available == MR_TRUE)
    {
        MR_rdtsc(&(now.MR_timer_time));
        duration = now.MR_timer_time - timer->MR_timer_time;
        duration_squared = duration * duration;
        MR_atomic_inc_int(&(stats->MR_stat_count_recorded));
        MR_atomic_add_int(&(stats->MR_stat_sum), duration);
        MR_atomic_add_int(&(stats->MR_stat_sum_squares), duration_squared);
    }
#elif /* not __GNUC__ && (__i386__ || __x86_64__) */
    /* No TSC support on this architecture or with this C compiler */
    MR_atomic_inc_int(&(stats->MR_stat_count_recorded));
#endif /* not __GNUC__ && (__i386__ || __x86_64__) */
}

/*
** It's convenient that this instruction is the same on both i386 and x86_64
*/
#if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))

static __inline__ void 
MR_cpuid(MR_Unsigned code, MR_Unsigned sub_code,
        MR_Unsigned *a, MR_Unsigned *b, MR_Unsigned *c, MR_Unsigned *d) {
    __asm__("cpuid"
        : "=a"(*a), "=b"(*b), "=c"(*c), "=d"(*d)
        : "0"(code), "2"(sub_code));
}

static __inline__ void
MR_rdtscp(MR_uint_least64_t *tsc, MR_Unsigned *processor_id) {
    MR_uint_least64_t tsc_high;

    /*
    ** On 64bit systems the high 32 bits of RAX and RDX are 0 filled by
    ** rdtsc{p}
    */
    __asm__("rdtscp"
           : "=a"(*tsc), "=d"(tsc_high), "=c"(*processor_id));

    tsc_high = tsc_high << 32;
    *tsc |= tsc_high; 
}

static __inline__ void
MR_rdtsc(MR_uint_least64_t *tsc) {
    MR_uint_least64_t tsc_high;

    __asm__("rdtsc"
           : "=a"(*tsc), "=d"(tsc_high));

    tsc_high = tsc_high << 32;
    *tsc |= tsc_high; 
}

#endif /* __GNUC__ && (__i386__ || __x86_64__) */

#endif /* MR_THREAD_SAFE && MR_PROFILE_PARALLEL_EXECUTION_SUPPORT */

