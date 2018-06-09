// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2014, 2016, 2018 The Mercury team
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_imp.h"
#include "mercury_overflow.h"
#include "mercury_memory_zones.h"   // for MR_MemoryZone
#include "mercury_debug.h"          // for MR_print_zone() etc
#include "mercury_misc.h"           // for MR_fatal_error()

#if !defined(MR_HIGHLEVEL_CODE)
void
MR_nondetstack_inclusion_check(MR_Word *maxfr,
    const char *error, const char *where)
{
    MR_MemoryZone   *cur_zone;
    MR_MemoryZones  *prev_zones;

    cur_zone = MR_CONTEXT(MR_ctxt_nondetstack_zone);
    prev_zones = MR_CONTEXT(MR_ctxt_prev_nondetstack_zones);
    while (MR_TRUE) {
        if (MR_in_zone(maxfr, cur_zone)) {
            if (maxfr > cur_zone->MR_zone_max) {
                cur_zone->MR_zone_max = maxfr;
            }

            return;
        }

        if (prev_zones == NULL) {
            MR_fatal_zone_error(MR_OVERFLOW_ZONE_NONDETSTACK,
                "MR_maxfr", maxfr, "nondetstack_zone",
                MR_CONTEXT(MR_ctxt_nondetstack_zone),
                MR_CONTEXT(MR_ctxt_prev_nondetstack_zones),
                error, where);
        }

        cur_zone = prev_zones->MR_zones_head;
        prev_zones = prev_zones->MR_zones_tail;
    }
}
#endif // !MR_HIGHLEVEL_CODE

void
MR_fatal_zone_error(MR_OverflowZone ptr_kind,
    const char *ptr_name, const void *ptr,
    const char *zone_name,
    const MR_MemoryZone *zone, const MR_MemoryZones *zones,
    const char *error, const char *where)
{
#ifdef MR_LOWLEVEL_DEBUG
    fprintf(stderr, "fatal zone error\n%s: ", ptr_name);
    switch (ptr_kind) {

        case MR_OVERFLOW_ZONE_DETSTACK:
            MR_print_detstackptr(stderr, ptr);
            break;

        case MR_OVERFLOW_ZONE_NONDETSTACK:
            MR_print_nondetstackptr(stderr, ptr);
            break;

        case MR_OVERFLOW_ZONE_HEAP:
            MR_print_heapptr(stderr, ptr);
            break;

        case MR_OVERFLOW_ZONE_OTHER:
        default:
            fprintf(stderr, "%p", ptr);
            break;
    }

    fprintf(stderr, "\n%s: ", zone_name);
    MR_print_zone(stderr, zone);
    MR_print_zones(stderr, zones);

    if (where != NULL) {
        fprintf(stderr, "error occurred in %s\n", where);
    }
#endif

    MR_fatal_error(error);
}
