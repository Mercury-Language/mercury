/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains code to manage spy points for both
** the internal and external debuggers.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace_base.h"
#include "mercury_trace.h"
#include "mercury_trace_spy.h"
#include "mercury_array_macros.h"

typedef struct {
	const MR_Stack_Layout_Entry	*spy_proc;
	MR_Spy_Point			*spy_points;
} MR_Spied_Proc;

static	MR_Spied_Proc	*MR_spied_procs;
static	int		MR_spied_proc_next = 0;
static	int		MR_spied_proc_max = 0;

#define	INIT_SPY_TABLE_SIZE	10

/*
** Return the index of the entry in MR_spied_procs whose spy_proc field
** is entry, or a negative number if absent.
*/
static	int	MR_search_spy_table_for_proc(const MR_Stack_Layout_Entry
			*entry);

static int
MR_search_spy_table_for_proc(const MR_Stack_Layout_Entry *entry)
{
	int	slot;
	bool	found;

	MR_bsearch(MR_spied_proc_next, slot, found,
		(Unsigned) MR_spied_procs[slot].spy_proc - (Unsigned) entry);
	if (found) {
		return slot;
	} else {
		return -1;
	}
}

bool
MR_event_matches_spy_point(const MR_Stack_Layout_Label *layout,
	MR_Trace_Port port, MR_Spy_Action *action_ptr)
{
	int				slot;
	bool				enabled;
	MR_Spy_Point			*point;
	MR_Spy_Action			action;

	slot = MR_search_spy_table_for_proc(layout->MR_sll_entry);
	if (slot < 0) {
		return FALSE;
	}

	enabled = FALSE;
	action = MR_SPY_PRINT;
	for (point = MR_spied_procs[slot].spy_points; point != NULL;
			point = point->spy_next) {
		if (! point->spy_enabled) {
			continue;
		}

		switch (point->spy_when) {

			case MR_SPY_ALL:
				enabled = TRUE;
				action = max(action, point->spy_action);
				break;

			case MR_SPY_ENTRY:
				if (MR_port_is_entry(port)) {
					enabled = TRUE;
					action = max(action, point->spy_action);
				} else {
					continue;
				}

				break;

			case MR_SPY_INTERFACE:
				if (MR_port_is_interface(port)) {
					enabled = TRUE;
					action = max(action, point->spy_action);
				} else {
					continue;
				}

				break;

			case MR_SPY_SPECIFIC:
				if (layout == point->spy_label) {
					enabled = TRUE;
					action = max(action, point->spy_action);
				} else {
					continue;
				}

				break;

			default:
				fatal_error("bad spy point when in "
						"MR_event_matches_spy_point");
		}
	}

	if (enabled) {
		*action_ptr = action;
		return TRUE;
	} else {
		return FALSE;
	}
}

MR_Spy_Point *
MR_add_spy_point(MR_Spy_When when, MR_Spy_Action action,
	const MR_Stack_Layout_Entry *entry, const MR_Stack_Layout_Label *label)
{
	MR_Spy_Point	*point;
	int		slot;
	bool		found;

	slot = MR_search_spy_table_for_proc(entry);
	if (slot < 0) {
		MR_ensure_room_for_next(MR_spied_proc, MR_Spied_Proc,
			INIT_SPY_TABLE_SIZE);
		MR_prepare_insert_into_sorted(MR_spied_procs,
			MR_spied_proc_next, slot,
			(Unsigned) entry -
			(Unsigned) MR_spied_procs[slot].spy_proc);
		MR_spied_procs[slot].spy_proc = entry;
		MR_spied_procs[slot].spy_points = NULL;
	}

	/* Insert the spy point at the head of the list for the proc. */
	point = checked_malloc(sizeof(MR_Spy_Point));
	point->spy_when    = when;
	point->spy_enabled = TRUE;
	point->spy_action  = action;
	point->spy_proc    = entry;
	point->spy_label   = label;
	point->spy_next    = MR_spied_procs[slot].spy_points;
	MR_spied_procs[slot].spy_points = point;

	return point;
}
