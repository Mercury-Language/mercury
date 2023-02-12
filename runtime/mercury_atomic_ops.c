// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_atomic_ops.c

#include "mercury_imp.h"
#include "mercury_atomic_ops.h"

////////////////////////////////////////////////////////////////////////////

#if defined(MR_THREAD_SAFE)

// Definitions for the atomic functions declared `extern inline'.

MR_OUTLINE_DEFN(
    MR_bool
    MR_compare_and_swap_int(volatile MR_Integer *addr, MR_Integer old,
        MR_Integer new_val)
,
    {
        MR_COMPARE_AND_SWAP_WORD_BODY;
    }
)

MR_OUTLINE_DEFN(
    MR_bool
    MR_compare_and_swap_uint(volatile MR_Unsigned *addr, MR_Unsigned old,
        MR_Unsigned new_val)
,
    {
        MR_COMPARE_AND_SWAP_WORD_BODY;
    }
)

MR_OUTLINE_DEFN(
    MR_Integer
    MR_atomic_add_and_fetch_int(volatile MR_Integer *addr, MR_Integer addend)
,
    {
        MR_ATOMIC_ADD_AND_FETCH_INT_BODY;
    }
)

MR_OUTLINE_DEFN(
    MR_Unsigned
    MR_atomic_add_and_fetch_uint(volatile MR_Unsigned *addr, MR_Unsigned addend)
,
    {
        MR_ATOMIC_ADD_AND_FETCH_UINT_BODY;
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
    MR_atomic_add_uint(volatile MR_Unsigned *addr, MR_Unsigned addend)
,
    {
        MR_ATOMIC_ADD_UINT_BODY;
    }
)

MR_OUTLINE_DEFN(
    void
    MR_atomic_sub_int(volatile _Atomic MR_Integer *addr, MR_Integer x)
,
    {
        MR_ATOMIC_SUB_INT_BODY;
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
    MR_atomic_inc_uint(volatile MR_Unsigned *addr)
,
    {
        MR_ATOMIC_INC_UINT_BODY;
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
    MR_bool
    MR_atomic_dec_and_is_zero_int(volatile MR_Integer *addr)
,
    {
        MR_ATOMIC_DEC_AND_IS_ZERO_INT_BODY;
    }
)

MR_OUTLINE_DEFN(
    MR_bool
    MR_atomic_dec_and_is_zero_uint(volatile MR_Unsigned *addr)
,
    {
        MR_ATOMIC_DEC_AND_IS_ZERO_UINT_BODY;
    }
)

#endif // MR_THREAD_SAFE

////////////////////////////////////////////////////////////////////////////

#if defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)

// Profiling of the parallel runtime.

#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))
// True if the RDTSCP and RDTSC instructions are available respectively.

static MR_bool  MR_rdtscp_is_available = MR_FALSE;
static MR_bool  MR_rdtsc_is_available = MR_FALSE;
#endif

MR_uint_least64_t MR_cpu_cycles_per_sec = 0;

#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))

// Set this to 1 to enable some printfs below
#define MR_DEBUG_CPU_FEATURE_DETECTION 0

// cpuid, rdtscp and rdtsc are i386/x86_64 instructions.

static __inline__ void  MR_cpuid(MR_Unsigned code, MR_Unsigned sub_code,
                            MR_Unsigned *a, MR_Unsigned *b,
                            MR_Unsigned *c, MR_Unsigned *d);

static __inline__ void  MR_rdtscp(MR_uint_least64_t *tsc,
                            MR_Unsigned *processor_id);

static __inline__ void  MR_rdtsc(MR_uint_least64_t *tsc);

// Return zero if parsing failed, otherwise return the number of cycles per
// second.

static MR_uint_least64_t parse_freq_from_x86_brand_string(char *string);

#endif // MR_GNUC && (__i386__ || __x86_64__)

void
MR_do_cpu_feature_detection(void)
{
#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))
    MR_Unsigned     a, b, c, d;
    MR_Unsigned     eflags, old_eflags;
    MR_Unsigned     maximum_extended_page;
    MR_Unsigned     extended_family, basic_family, family;
    MR_Unsigned     extended_model, model;

    // Check for the CPUID instruction. CPUID is supported if we can flip bit
    // 21 in the CPU's EFLAGS register. The assembly below is written in a
    // subset of i386 and x86_64 assembly. To read and write EFLAGS we have
    // to go via the C stack.

    __asm__ ("pushf; pop %0"
            :"=r"(eflags));
    old_eflags = eflags;
    // Flip bit 21
    eflags ^= (1 << 21);
    __asm__ ("push %0; popf; pushf; pop %0;"
            :"=r"(eflags)
            :"0"(eflags));

    // Test to see if our change held. We don't restore eflags, a change to
    // the ID bit has no effect.

    if (eflags == old_eflags) {
#if MR_DEBUG_CPU_FEATURE_DETECTION
        fprintf(stderr, "This CPU doesn't support the CPUID instruction.\n",
            eflags, old_eflags);
#endif
        return;
    }

    // CPUID 0 gives the maximum basic CPUID page in EAX. Basic pages go up to
    // but not including 0x40000000.

    MR_cpuid(0, 0, &a, &b, &c, &d);
    if (a < 1) {
        return;
    }

    // CPUID 1 gives type, family, model and stepping information in EAX.
    MR_cpuid(1, 0, &a, &b, &c, &d);

    // Bit 4 in EDX is high if RDTSC is available
    if (d & (1 << 4)) {
        MR_rdtsc_is_available = MR_TRUE;
    }

    // BTW: Intel can't count:
    //
    // http://www.pagetable.com/?p=18
    // http://www.codinghorror.com/blog/archives/000364.html
    //
    // 486 (1989): family 4
    // Pentium (1993): family 5
    // Pentium Pro (1995): family 6, models 0 and 1
    // Pentium 2 (1997): family 6, models 3, 5 and 6
    // Pentium 3 (2000): family 6, models 7, 8, 10, 11
    // Itanium (2001): family 7
    // Pentium 4 (2000): family 15/0
    // Itanium 2 (2002): family 15/1 and 15/2
    // Pentium D: family 15/4
    // Pentium M (2003): family 6, models 9 and 13
    // Core (2006): family 6, model 14
    // Core 2 (2006): family 6, model 15
    // i7: family 6, model 26
    // Atom: family 6, model 28
    //
    // This list is incomplete, it doesn't cover AMD or any other brand of x86
    // processor, and it probably doesn't cover all post-pentium Intel
    // processors.

    // bits 8-11 (first bit (LSB) is bit 0)
    basic_family = (a & 0x00000F00) >> 8;
    if (basic_family == 0x0F) {
        // bits 20-27
        extended_family = (a & 0x0FF00000) >> 20;
        family = basic_family + extended_family;
    } else {
        family = basic_family;
    }

    // I'm not using the model value but I'll leave the code here in case we
    // have a reason to use it in the future.

    // bits 4-7
    model = (a & 0x000000F0) >> 4;
    if ((basic_family == 0x0F) || (basic_family == 0x06)) {
        // bits 16-19
        extended_model = (a & 0x000F0000) >> 16;
        model += (extended_model << 4);
    }
#if MR_DEBUG_CPU_FEATURE_DETECTION
    fprintf(stderr, "This is family %d and model %d\n", family, model);
#endif

    // Now check for P3 or higher since they have the extended pages.
    if (family < 6) {
        // This is a 486 or Pentium.
        return;
    }
    // I could bail out here if this was a pentium 3, but there is a more
    // reliable check for extended CPUID support below that should work on AMD
    // chips as well, if I knew all the model numbers for all family 6
    // processors and knew if they honoured extended CPUID.

    // Extended CPUID 0x80000000.
    //
    // EAX contains the maximum extended CPUID node.

    MR_cpuid(0x80000000, 0, &a, &b, &c, &d);
    if ((a & 0x80000000) == 0) {
        // Extended CPUID is not supported.
        // Note that this check is still not as reliable as I'd like. If it
        // succeeds I'm not confident that the processor definitely implements
        // extended CPUID.

        return;
    }
    maximum_extended_page = a;
#if MR_DEBUG_CPU_FEATURE_DETECTION
    fprintf(stderr, "Maximum extended CPUID page: 0x%x\n", maximum_extended_page);
#endif

    // Extended CPUID 0x80000001
    //
    // If EDX bit 27 is set the RDTSCP instruction is available.

    if (maximum_extended_page >= 0x80000001) {
        MR_cpuid(0x80000001, 0, &a, &b, &c, &d);
#if MR_DEBUG_CPU_FEATURE_DETECTION
        fprintf(stderr, "CPUID 0x80000001 EDX: 0x%x\n", d);
#endif
        if ((d & (1 << 27))) {
            // This processor supports RDTSCP.

#if MR_DEBUG_CPU_FEATURE_DETECTION
            fprintf(stderr, "RDTSCP is available\n");
#endif
            MR_rdtscp_is_available = MR_TRUE;
        }
    }

    if (maximum_extended_page >= 0x80000004) {
        // 3 CPUID pages, 4 return registers each, containing 4 bytes each,
        // plus a null byte. Intel says they include their own null byte, but
        // for the cost of a single byte I feel safer using our own.

#define CPUID_BRAND_STRING_SIZE (3*4*4 + 1)
        char buff[CPUID_BRAND_STRING_SIZE];
        unsigned int page;
        unsigned int byte;
        unsigned int shift;

        // This processor supports the brand string from which we can
        // try to extract the clock speed. This algorithm is described
        // in the Intel Instruction Set Reference, Volume 2B, Chapter 3,
        // Pages 207-208, In particular the flow chart in figure 3-10.
        // This does not work on AMD processors since they don't include
        // the clock speed in the brand string.

        for (page = 0; page < 3; page++) {
            MR_cpuid(page + 0x80000002, 0, &a, &b, &c, &d);
#if MR_DEBUG_CPU_FEATURE_DETECTION
            fprintf(stderr, "CPUID page: 0x%.8x, eax: 0x%.8x, ebx: 0x%.8x, ecx: 0x%.8x, edx: 0x%.8x\n",
                page + 0x80000002, a, b, c, d);
#endif
            for (byte = 0; byte < 4; byte++) {
                shift = byte * 8;
                buff[page*4*4 + 0 + byte] = (char)(0xFF & (a >> shift));
                buff[page*4*4 + 4 + byte] = (char)(0xFF & (b >> shift));
                buff[page*4*4 + 8 + byte] = (char)(0xFF & (c >> shift));
                buff[page*4*4 + 12 + byte] = (char)(0xFF & (d >> shift));
            }
        }
        // Add a null byte.
        buff[CPUID_BRAND_STRING_SIZE - 1] = 0;
#if MR_DEBUG_CPU_FEATURE_DETECTION
        fprintf(stderr, "CPUID Brand string: %s\n", buff);
#endif

        MR_cpu_cycles_per_sec = parse_freq_from_x86_brand_string(buff);
#if MR_DEBUG_CPU_FEATURE_DETECTION
        if (MR_cpu_cycles_per_sec == 0) {
            fprintf(stderr, "Failed to detect cycles per second "
                "you can probably blame AMD for this.\n");
        } else {
            fprintf(stderr, "Cycles per second: %ld\n", MR_cpu_cycles_per_sec);
        }
#endif
    }
#endif // MR_GNUC && (__i386__ || __x86_64__)
}

#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))
static MR_uint_least64_t
parse_freq_from_x86_brand_string(char *string)
{
    unsigned int brand_string_len;
    unsigned int i;
    double       multiplier;
    int          freq_index = -1;

    brand_string_len = strlen(string);

    // There will be at least five characters if we can parse this, three
    // for the '?Hz' suffix, at least one for the units, plus a space at
    // the beginning of the number.

    if (!(brand_string_len > 5))
        return 0;

    if (!((string[brand_string_len - 1] == 'z') &&
          (string[brand_string_len - 2] == 'H'))) {
        return 0;
    }

    switch (string[brand_string_len - 3]) {
        case 'M':
            multiplier = 1000000.0;
            break;
        case 'G':
            multiplier = 1000000000.0;
            break;
        case 'T':
            // Yes, this is defined in the specification, Intel have some
            // strong ambitions regarding Moore's law. :-)
            // We include it here to conform with the standard.

            multiplier = 1000000000000.0;
            break;
        default:
            return 0;
    }

    // Search for the beginning of the digits.
    for (i = brand_string_len - 4; i >= 0; i--) {
        if (string[i] == ' ') {
            freq_index = i+1;
            break;
        }
    }
    if (freq_index == -1) {
        // We didn't find the beginning of the frequency.
        return 0;
    }

    // If strtod fails it returns zero, so if we fail to parse a number here,
    // we will return zero, which our caller understands as a parsing failure.

    return (MR_uint_least64_t)(strtod(&string[freq_index], NULL) * multiplier);
}
#endif // MR_GNUC && (__i386__ || __x86_64__)

void
MR_profiling_start_timer(MR_Timer *timer)
{
#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))
    // If we don't have enough data to fill in all the fields of this structure
    // we leave them alone, we won't check them later without checking
    // MR_rdtsc{p}_is_available first.

    if (MR_rdtscp_is_available) {
        MR_rdtscp(&(timer->MR_timer_time), &(timer->MR_timer_processor_id));
    } else if (MR_rdtsc_is_available) {
        MR_rdtsc(&(timer->MR_timer_time));
    }
#endif
}

void
MR_profiling_stop_timer(MR_Timer *timer, MR_Stats *stats)
{
#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))
    MR_Timer            now;
    MR_int_least64_t    duration;
    MR_uint_least64_t   duration_squared;

    if (MR_rdtscp_is_available) {
        MR_rdtscp(&(now.MR_timer_time), &(now.MR_timer_processor_id));
        if (timer->MR_timer_processor_id == now.MR_timer_processor_id) {
            duration = now.MR_timer_time - timer->MR_timer_time;
            duration_squared = duration * duration;
            MR_atomic_inc_uint(&(stats->MR_stat_count_recorded));
  #if MR_LOW_TAG_BITS >= 3
            MR_atomic_add_int(&(stats->MR_stat_sum), duration);
            MR_atomic_add_uint(&(stats->MR_stat_sum_squares), duration_squared);
  #else
            MR_US_SPIN_LOCK(&(stats->MR_stat_sums_lock));
            stats->MR_stat_sum += duration;
            stats->MR_stat_sum_squares += duration_squared;
            MR_US_UNLOCK(&(stats->MR_stat_sums_lock));
  #endif
        } else {
            MR_atomic_inc_uint(&(stats->MR_stat_count_not_recorded));
        }
    } else if (MR_rdtsc_is_available) {
        MR_rdtsc(&(now.MR_timer_time));
        duration = now.MR_timer_time - timer->MR_timer_time;
        duration_squared = duration * duration;
        MR_atomic_inc_uint(&(stats->MR_stat_count_recorded));
  #if MR_LOW_TAG_BITS >= 3
        MR_atomic_add_int(&(stats->MR_stat_sum), duration);
        MR_atomic_add_uint(&(stats->MR_stat_sum_squares), duration_squared);
  #else
        MR_US_SPIN_LOCK(&(stats->MR_stat_sums_lock));
        stats->MR_stat_sum += duration;
        stats->MR_stat_sum_squares += duration_squared;
        MR_US_UNLOCK(&(stats->MR_stat_sums_lock));
  #endif
    }
#else // not MR_GNUC && (__i386__ || __x86_64__)
    // No TSC support on this architecture or with this C compiler.
    MR_atomic_inc_int(&(stats->MR_stat_count_recorded));
#endif // not MR_GNUC && (__i386__ || __x86_64__)
}

// The TSC works and MR_cpu_cycles_per_sec is nonzero.

extern MR_bool
MR_tsc_is_sensible(void)
{
#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))
    return ((MR_rdtscp_is_available || MR_rdtsc_is_available) &&
        (MR_cpu_cycles_per_sec != 0));
#else
    return MR_FALSE;
#endif
}

MR_uint_least64_t
MR_read_cpu_tsc(void)
{
#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))
    MR_uint_least64_t   tsc;

    if (MR_rdtsc_is_available) {
        MR_rdtsc(&tsc);
    } else {
        tsc = 0;
    }
    return tsc;
#else // not MR_GNUC && (__i386__ || __x86_64__)
    return 0;
#endif // not MR_GNUC && (__i386__ || __x86_64__)
}

// It is convenient that this instruction is the same on both i386 and x86_64.

#if defined(MR_GNUC) && (defined(__i386__) || defined(__x86_64__))

static __inline__ void
MR_cpuid(MR_Unsigned code, MR_Unsigned sub_code,
    MR_Unsigned *a, MR_Unsigned *b, MR_Unsigned *c, MR_Unsigned *d)
{
#ifdef __x86_64__
    __asm__("cpuid"
        : "=a"(*a), "=b"(*b), "=c"(*c), "=d"(*d)
        : "0"(code), "2"(sub_code));
#elif defined(__i386__)
    // i386 is more register staved, in particular we can't use ebx in
    // position independent code. And we can't move ebx into another
    // general purpose register, between register pinning, PIC, the
    // stack and frame pointers and the other registers used by CPUID
    // there are literally no general purpose registers left on i386.

    __asm__("pushl %%ebx;    \
             cpuid;          \
             movl %%ebx, %1; \
             popl %%ebx;"
        : "=a"(*a), "=m"(*b), "=c"(*c), "=d"(*d)
        : "0"(code), "2"(sub_code)
        : "memory");
#endif
}

static __inline__ void
MR_rdtscp(MR_uint_least64_t *tsc, MR_Unsigned *processor_id)
{
    MR_Unsigned tsc_low;
    MR_Unsigned tsc_high;

    // On 64bit systems the high 32 bits of RAX and RDX are 0 filled by
    // rdtsc{p}.

    __asm__("rdtscp"
           : "=a"(tsc_low), "=d"(tsc_high), "=c"(*processor_id));

    *tsc = tsc_high;
    *tsc = *tsc << 32;
    *tsc |= tsc_low;
}

static __inline__ void
MR_rdtsc(MR_uint_least64_t *tsc)
{
    MR_Unsigned tsc_low;
    MR_Unsigned tsc_high;

    __asm__("rdtsc"
           : "=a"(tsc_low), "=d"(tsc_high));

    *tsc = tsc_high;
    *tsc = *tsc << 32;
    *tsc |= tsc_low;
}

#endif // MR_GNUC && (__i386__ || __x86_64__)

#endif // MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
