/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trail.c - code for the Mercury trail.
**
** The trail is used to record values that need to be
** restored on backtracking.
*/

#include "imp.h"

#include "mercury_trail.h"

#include "memory.h"
#include "misc.h"

#ifdef MR_USE_TRAIL

MemoryZone	*MR_trail_zone;
MR_TrailEntry	*MR_trail_ptr_var;
Unsigned	MR_ticket_counter_var;

void
MR_untrail_to(MR_TrailEntry *old_trail_ptr, MR_untrail_reason reason)
{
    switch (reason) {
	case MR_commit:
	    /* Just handle the function trail entries */
	    while (MR_trail_ptr != old_trail_ptr) {
	    	MR_trail_ptr--;
	    	if (MR_get_trail_entry_kind(MR_trail_ptr) == MR_func_entry) {
		    (*MR_get_trail_entry_untrail_func(MR_trail_ptr))(
				MR_get_trail_entry_datum(MR_trail_ptr),
				reason);
		}
	    }
	    break;

	case MR_undo:
	case MR_exception:
	    /* Handle both function and value trail entries */
	    while (MR_trail_ptr != old_trail_ptr) {
		MR_trail_ptr--;
	    	if (MR_get_trail_entry_kind(MR_trail_ptr) == MR_func_entry) {
		    (*MR_get_trail_entry_untrail_func(MR_trail_ptr))(
				MR_get_trail_entry_datum(MR_trail_ptr),
				reason);
		} else {
		    *MR_get_trail_entry_address(MR_trail_ptr) =
				MR_get_trail_entry_value(MR_trail_ptr);
		}
	    }
	    break;
	
	default:
	    fatal_error("unknown MR_untrail_reason");
    }
}

#endif

/*---------------------------------------------------------------------------*/
