// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2000, 2006-2008, 2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trail.c - code for the Mercury trail.
//
// The trail is used to record values that need to be restored on backtracking.

#include "mercury_imp.h"
#include "mercury_trail.h"
#include "mercury_memory.h"
#include "mercury_memory_handlers.h"
#include "mercury_misc.h"

#ifdef MR_USE_TRAIL

#if !defined(MR_THREAD_SAFE)
MR_MemoryZone   *MR_trail_zone;
MR_TrailEntry   *MR_trail_ptr_var;

  #if !defined(MR_USE_FIXED_SIZE_TRAIL)
    MR_MemoryZones *MR_prev_trail_zones;
  #endif

MR_Unsigned     MR_ticket_counter_var = 1;
MR_Unsigned     MR_ticket_high_water_var = 1;
#endif

#if !defined(MR_USE_FIXED_SIZE_TRAIL)
static void
MR_pop_trail_segment(void);
#endif

static void
MR_reset_trail_zone(void);

void
MR_untrail_to(MR_TrailEntry *old_trail_ptr, MR_untrail_reason reason)
{
    MR_TrailEntry   *tr_ptr;
    MR_TrailEntry   *tr_base;

    // Not needed, since MR_trail_ptr is never a real reg:
    // MR_restore_transient_registers();
    tr_ptr = MR_trail_ptr;

    switch (reason) {
    case MR_solve:
    case MR_commit:

        // Just handle the function trail entries.
        tr_base = MR_TRAIL_BASE;
        while (tr_ptr != old_trail_ptr) {
            tr_ptr--;
            if (MR_get_trail_entry_kind(tr_ptr) == MR_func_entry) {
                (*MR_get_trail_entry_untrail_func(tr_ptr))(
                    MR_get_trail_entry_datum(tr_ptr), reason);
            }

            // When we are using trail segments it is possible that
            // `old_trail_ptr' is not a location on the current trail segment.
            // We need to walk backwards through all the previous segments
            // (invoking function trail entries as we go) until we find it.

            #if !defined(MR_USE_FIXED_SIZE_TRAIL)
                if (tr_ptr == tr_base
                    && tr_ptr != old_trail_ptr)
                {
                    MR_MemoryZones  *prev_zones;
                    MR_MemoryZone   *zone;

                    prev_zones = MR_PREV_TRAIL_ZONES;
                    zone = prev_zones->MR_zones_head;
                    tr_ptr = (MR_TrailEntry *) zone->MR_zone_end;

                    while (tr_ptr != old_trail_ptr) {
                        tr_ptr--;
                        if (MR_get_trail_entry_kind(tr_ptr) == MR_func_entry) {
                            (*MR_get_trail_entry_untrail_func(tr_ptr))(
                                MR_get_trail_entry_datum(tr_ptr), reason);
                        }

                        if (tr_ptr == (MR_TrailEntry *) zone->MR_zone_min
                            && tr_ptr != old_trail_ptr)
                        {
                            prev_zones = prev_zones->MR_zones_tail;
                            zone = prev_zones->MR_zones_head;
                            tr_ptr = (MR_TrailEntry *) zone->MR_zone_end;
                        }
                    }
                    break;
                }
            #endif
        }
        // NB. We do _not_ reset the trail pointer here. Doing so would be
        // unsafe, for `mdi' modes, because we may still need to restore
        // the value if/when we backtrack to a choicepoint prior to the one
        // we are cutting away.

        break;

    case MR_undo:
    case MR_exception:
    case MR_retry:
        // Handle both function and value trail entries.
        tr_base = MR_TRAIL_BASE;
        while (tr_ptr != old_trail_ptr) {
            tr_ptr--;
            if (MR_get_trail_entry_kind(tr_ptr) == MR_func_entry) {
                (*MR_get_trail_entry_untrail_func(tr_ptr))(
                    MR_get_trail_entry_datum(tr_ptr), reason);
            } else {
                *MR_get_trail_entry_address(tr_ptr) =
                    MR_get_trail_entry_value(tr_ptr);
            }
            #if !defined(MR_USE_FIXED_SIZE_TRAIL)
                if (tr_ptr == tr_base
                    && tr_ptr != old_trail_ptr)
                {
                    MR_pop_trail_segment();
                    tr_ptr = MR_trail_ptr;
                    tr_base = MR_TRAIL_BASE;
                }
            #endif
        }

        MR_trail_ptr = tr_ptr;
        // Not needed, since MR_trail_ptr is never a real reg:
        // MR_save_transient_registers();
        break;

    default:
        MR_fatal_error("unknown MR_untrail_reason");
    }
}

////////////////////////////////////////////////////////////////////////////

MR_Unsigned
MR_num_trail_entries(void)
{
    MR_Unsigned     n_entries = 0;

#if !defined(MR_USE_FIXED_SIZE_TRAIL)
    MR_MemoryZones  *list;
    MR_MemoryZone   *zone;

    list = MR_PREV_TRAIL_ZONES;
    while (list != NULL) {
        zone = list->MR_zones_head;
        n_entries += (MR_TrailEntry *) zone->MR_zone_end
            - (MR_TrailEntry *) zone->MR_zone_min;
        list = list->MR_zones_tail;
    }
#endif // ! MR_USE_FIXED_SIZE_TRAIL

    n_entries += MR_trail_ptr - MR_TRAIL_BASE;

    return n_entries;
}

////////////////////////////////////////////////////////////////////////////

void
MR_reset_trail(void)
{
    #if !defined(MR_USE_FIXED_SIZE_TRAIL)
        while (MR_PREV_TRAIL_ZONES != NULL) {
            MR_reset_trail_zone();
            MR_pop_trail_segment();
        }
    #endif

    MR_reset_trail_zone();

    #if defined(MR_CONSERVATIVE_GC)
        MR_clear_zone_for_GC(MR_TRAIL_ZONE, MR_trail_ptr);
    #endif

    MR_ticket_counter = 1;
    MR_ticket_high_water = 1;
}

static void
MR_reset_trail_zone(void) {

    MR_TrailEntry   *tr_ptr;
    MR_TrailEntry   *tr_base;

    tr_ptr = MR_trail_ptr;
    tr_base = MR_TRAIL_BASE;

    while (tr_ptr != tr_base) {
        tr_ptr--;
        if (MR_get_trail_entry_kind(tr_ptr) == MR_func_entry) {
            (*MR_get_trail_entry_untrail_func(tr_ptr))(
                MR_get_trail_entry_datum(tr_ptr), MR_gc);
        }
    }
    MR_trail_ptr = tr_base;
}

////////////////////////////////////////////////////////////////////////////

#if !defined(MR_USE_FIXED_SIZE_TRAIL)
void
MR_new_trail_segment(void)
{

    MR_MemoryZones  *list;
    MR_MemoryZone   *new_zone;
    MR_TrailEntry   *old_trail_ptr;

    old_trail_ptr = MR_trail_ptr;

    // We perform explicit overflow checks so redzones just waste space.

    new_zone = MR_create_or_reuse_zone("trail_segment", MR_trail_size, 0,
        0, MR_default_handler);

    list = MR_GC_malloc_uncollectable_attrib(sizeof(MR_MemoryZones),
        MR_ALLOC_SITE_RUNTIME);

#if defined(MR_DEBUG_TRAIL_SEGMENTS)
    printf("create new trail segment: old zone: %p, old trail_ptr %p\n",
        MR_TRAIL_ZONE, MR_trail_ptr);
#endif

    list->MR_zones_head = MR_TRAIL_ZONE;
    list->MR_zones_tail = MR_PREV_TRAIL_ZONES;
    MR_PREV_TRAIL_ZONES = list;
    MR_TRAIL_ZONE = new_zone;
    MR_trail_ptr = (MR_TrailEntry *) MR_TRAIL_ZONE->MR_zone_min;

#if defined(MR_DEBUG_TRAIL_SEGMENTS)
    printf("create new trail segment: new zone: %p, new trail_ptr %p\n",
        MR_TRAIL_ZONE, MR_trail_ptr);
#endif
}

static void
MR_pop_trail_segment(void)
{

    MR_MemoryZones  *list;

#if defined(MR_DEBUG_TRAIL_SEGMENTS)
    printf("restore old trail segment: old zone %p, old trail_ptr %p\n",
        MR_TRAIL_ZONE, MR_trail_ptr);
#endif

    MR_release_zone(MR_TRAIL_ZONE);

    list = MR_PREV_TRAIL_ZONES;
    MR_TRAIL_ZONE = list->MR_zones_head;
    MR_PREV_TRAIL_ZONES = list->MR_zones_tail;
    MR_trail_ptr = (MR_TrailEntry *) MR_TRAIL_ZONE->MR_zone_end;
    MR_GC_free(list);

#if defined(MR_DEBUG_TRAIL_SEGMENTS)
    printf("restore old trail segment: new zone %p, new trail_ptr %p\n",
        MR_TRAIL_ZONE, MR_trail_ptr);
#endif
}

MR_Unsigned
MR_num_trail_segments(void)
{
    MR_Unsigned     n_segments = 1;
    MR_MemoryZones  *list;

    list = MR_PREV_TRAIL_ZONES;
    while (list != NULL) {
        n_segments++;
        list = list->MR_zones_tail;
    }

    return n_segments;
}

#endif // ! MR_USE_FIXED_SIZE_TRAIL

#endif // MR_USE_TRAIL

////////////////////////////////////////////////////////////////////////////
