/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1997-2000, 2006-2008 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trail.c - code for the Mercury trail.
**
** The trail is used to record values that need to be restored on backtracking.
*/

#include "mercury_imp.h"

#include "mercury_trail.h"

#include "mercury_memory.h"
#include "mercury_misc.h"

#ifdef MR_USE_TRAIL

/*
** The following macros are used to access (parts of) the trail zone in a
** grade independent manner.
** 
** MR_TRAIL_ZONE expands to the address of the trail zone for the current
** thread.
**
** MR_TRAIL_BASE expands to the address of the base of the trail for the
** current thread, i.e. the initial value to which MR_trail_ptr_var is set.
**
*/
#if defined(MR_THREAD_SAFE)

    #define MR_TRAIL_ZONE (MR_CONTEXT(MR_ctxt_trail_zone))

    #define MR_TRAIL_BASE \
        ((MR_TrailEntry *) (MR_CONTEXT(MR_ctxt_trail_zone)->MR_zone_min))
#else
    #define MR_TRAIL_ZONE   MR_trail_zone
    #define MR_TRAIL_BASE   ((MR_TrailEntry *) (MR_trail_zone->MR_zone_min))
#endif

#if !defined(MR_THREAD_SAFE)
MR_MemoryZone   *MR_trail_zone;
MR_TrailEntry   *MR_trail_ptr_var;
MR_Unsigned     MR_ticket_counter_var = 1;
MR_Unsigned     MR_ticket_high_water_var = 1;
#endif

void
MR_untrail_to(MR_TrailEntry *old_trail_ptr, MR_untrail_reason reason)
{
    MR_TrailEntry *tr_ptr;
    /* not needed, since MR_trail_ptr is never a real reg: */
    /* MR_restore_transient_registers(); */
    tr_ptr = MR_trail_ptr;

    switch (reason) {
    case MR_solve:
    case MR_commit:
        /* Just handle the function trail entries */
        while (tr_ptr != old_trail_ptr) {
            tr_ptr--;
            if (MR_get_trail_entry_kind(tr_ptr) == MR_func_entry) {
                (*MR_get_trail_entry_untrail_func(tr_ptr))(
                    MR_get_trail_entry_datum(tr_ptr), reason);
            }
        }
        /*
        ** NB. We do _not_ reset the trail pointer here. Doing so would be
        ** unsafe, for `mdi' modes, because we may still need to restore
        ** the value if/when we backtrack to a choicepoint prior to the one
        ** we're cutting away.
        */
        break;

    case MR_undo:
    case MR_exception:
    case MR_retry:
        /* Handle both function and value trail entries */
        while (tr_ptr != old_trail_ptr) {
            tr_ptr--;
            if (MR_get_trail_entry_kind(tr_ptr) == MR_func_entry) {
                (*MR_get_trail_entry_untrail_func(tr_ptr))(
                    MR_get_trail_entry_datum(tr_ptr), reason);
            } else {
                *MR_get_trail_entry_address(tr_ptr) =
                    MR_get_trail_entry_value(tr_ptr);
            }
        }

        MR_trail_ptr = tr_ptr;
        /* Not needed, since MR_trail_ptr is never a real reg: */
        /* MR_save_transient_registers(); */
        break;

    default:
        MR_fatal_error("unknown MR_untrail_reason");
    }
}

/*---------------------------------------------------------------------------*/


MR_Unsigned
MR_num_trail_entries(void)
{
    return MR_trail_ptr - MR_TRAIL_BASE;
}

/*---------------------------------------------------------------------------*/

void
MR_reset_trail(void)
{
    MR_TrailEntry *tr_ptr;

    tr_ptr = MR_trail_ptr;

    while (tr_ptr != MR_TRAIL_BASE) {
        tr_ptr--;
        if (MR_get_trail_entry_kind(tr_ptr) == MR_func_entry) {
            (*MR_get_trail_entry_untrail_func(tr_ptr))(
                MR_get_trail_entry_datum(tr_ptr), MR_gc);
        }
    }

    #if defined(MR_CONSERVATIVE_GC)
        MR_clear_zone_for_GC(MR_TRAIL_ZONE, MR_trail_ptr);
    #endif

    MR_ticket_counter = 1;
    MR_ticket_high_water = 1;
}

#endif /* MR_USE_TRAIL */
/*---------------------------------------------------------------------------*/
