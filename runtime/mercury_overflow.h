// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-1998,2000-2001, 2005-2006 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_overflow.h - definitions for overflow checks

#ifndef MERCURY_OVERFLOW_H
#define MERCURY_OVERFLOW_H

#include "mercury_types.h"
#include "mercury_memory_zones.h"

// If maxfr is not in any of the zones of the nondet stack, abort the program,
// with the abort message including the message in `error', and (if not NULL)
// the location in `where'.
//
// If maxfr *is* in one of the zones of the nondet stack, then update
// its MR_zone_max field if maxfr exceeds it, since the MR_zone_max field
// should always hold the highest address used in the zone so far.

#if !defined(MR_HIGHLEVEL_CODE)
extern  void    MR_nondetstack_inclusion_check(MR_Word *maxfr,
                    const char *error, const char *where);
#endif

typedef enum {
    MR_OVERFLOW_ZONE_DETSTACK,
    MR_OVERFLOW_ZONE_NONDETSTACK,
    MR_OVERFLOW_ZONE_HEAP,
    MR_OVERFLOW_ZONE_OTHER
} MR_OverflowZone;

// Output a message about a zone error to standard error and abort.
// This function is for fatal zone underflow and overflow errors
// in the Mercury runtime.

MR_NO_RETURN(extern void                                                \
MR_fatal_zone_error(MR_OverflowZone ptr_kind,
    const char *ptr_name, const void *ptr,
    const char *zone_name,
    const MR_MemoryZone *zone, const MR_MemoryZones *zones,
    const char *error, const char *where));

#ifndef MR_CHECK_FOR_OVERFLOW

#define MR_heap_overflow_check()                ((void) 0)
#define MR_detstack_overflow_check()            ((void) 0)
#define MR_detstack_overflow_check_msg(x)       ((void) 0)
#define MR_detstack_underflow_check()           ((void) 0)
#define MR_detstack_underflow_check_msg(x)      ((void) 0)
#define MR_nondetstack_overflow_check()         ((void) 0)
#define MR_nondetstack_overflow_check_msg(x)    ((void) 0)
#define MR_nondetstack_underflow_check()        ((void) 0)
#define MR_nondetstack_underflow_check_msg(x)   ((void) 0)

#else // MR_CHECK_FOR_OVERFLOW

#include "mercury_regs.h"
#include "mercury_misc.h"   // for MR_fatal_error()

#define MR_heap_overflow_check()                                             \
    (                                                                        \
        MR_IF (MR_hp >= MR_ENGINE(MR_eng_heap_zone)->MR_zone_top,(           \
            MR_fatal_error("heap overflow")                                  \
        )),                                                                  \
        MR_IF (MR_hp > MR_ENGINE(MR_eng_heap_zone)->MR_zone_max,(            \
            MR_ENGINE(MR_eng_heap_zone)->MR_zone_max = MR_hp                 \
        )),                                                                  \
        (void)0                                                              \
    )

#define MR_detstack_overflow_check()                                         \
    (                                                                        \
        MR_IF (MR_sp >= MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_top,(     \
            MR_fatal_error("stack overflow")                                 \
        )),                                                                  \
        MR_IF (MR_sp > MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_max,(      \
            MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_max = MR_sp           \
        )),                                                                  \
        (void)0                                                              \
    )

#define MR_detstack_overflow_check_msg(where)                                \
    (                                                                        \
        MR_IF (MR_sp >= MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_top,(     \
            MR_fatal_error("stack overflow: " where)                         \
        )),                                                                  \
        MR_IF (MR_sp > MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_max,(      \
            MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_max = MR_sp           \
        )),                                                                  \
        (void)0                                                              \
    )

#define MR_detstack_underflow_check()                                        \
    (                                                                        \
        MR_IF (MR_sp < MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_min,(      \
            MR_fatal_error("stack underflow")                                \
        )),                                                                  \
        (void)0                                                              \
    )

#define MR_detstack_underflow_check_msg(where)                               \
    (                                                                        \
        MR_IF (MR_sp < MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_min,(      \
            MR_fatal_error("stack underflow: " where)                        \
        )),                                                                  \
        (void)0                                                              \
    )

// Checking for overflow and underflow on the nondet stack is not as simple
// as on the det stack. Since it is OK for MR_maxfr to be in a segment that
// is NOT currently the top nondet stack segment (see the comment on
// MR_new_nondetstack_segment to see why), we have check whether MR_maxfr
// is in *any* of the segments that currently belong to the nondet stack.
//
// MR_nondetstack_inclusion_check will set the MR_zone_max field of a zone
// if MR_maxfr exceeds it. It will test for this even during underflow checks,
// since testing whether the current check is for underflow would probably
// take just as long.

#define MR_nondetstack_overflow_check()                                      \
    MR_nondetstack_inclusion_check(MR_maxfr, "nondetstack overflow", NULL)

#define MR_nondetstack_overflow_check_msg(where)                             \
    MR_nondetstack_inclusion_check(MR_maxfr, "nondetstack overflow", where)

#define MR_nondetstack_underflow_check()                                     \
    MR_nondetstack_inclusion_check(MR_maxfr, "nondetstack underflow", NULL)

#define MR_nondetstack_underflow_check_msg(where)                            \
    MR_nondetstack_inclusion_check(MR_maxfr, "nondetstack underflow", where)

#endif // MR_CHECK_FOR_OVERFLOW

#endif // not MERCURY_OVERFLOW_H
