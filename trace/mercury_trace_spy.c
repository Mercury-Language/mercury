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

/*
** The table of spy points, with one entry per existing (or deleted but
** not yet reused) spy point, with counters saying which is the next
** free slot and how many slots are allocated.
*/

MR_Spy_Point    	**MR_spy_points;
int			MR_spy_point_next = 0;
int			MR_spy_point_max  = 0;

/* The initial size of the spy points table. */
#define	MR_INIT_SPY_POINTS	10

/*
** The table of spied on procedures, with one entry per procedure that
** has ever been spied on, giving the possibly empty list of spy points
** (enabled or disabled but not deleted) that refer to that procedure,
** with counters saying which is the next free slot and how many slots
** are allocated.
*/

typedef struct {
	const MR_Stack_Layout_Entry	*spy_proc;
	MR_Spy_Point			*spy_points;
} MR_Spied_Proc;

static	MR_Spied_Proc	*MR_spied_procs;
static	int		MR_spied_proc_next = 0;
static	int		MR_spied_proc_max = 0;

/* The initial size of the spied procs table. */
#define	MR_INIT_SPIED_PROCS	10

/**************************************************************************/

static	int	MR_compare_addr(const void *address1, const void *address2);
static	int	MR_search_spy_table_for_proc(const MR_Stack_Layout_Entry
			*entry);

/*
** Compare two addresses, and return an integer which is <0, 0, or >0
** depending on whether the first address is less than, equal to, or
** greater than the second.  Suitable for use with MR_bsearch() and
** MR_prepare_insert_into_sorted().
*/

static int
MR_compare_addr(const void *address1, const void *address2)
{
	/*
	** Note that we can't just compare the pointers, because
	** because on a segmented architecture, that might
	** only compare the segments, not the offsets (ANSI C
	** doesn't require pointer comparisons to work unless
	** the pointers point into the same array, which is not
	** necessarily going to be the case here).
	** So instead we need to cast the pointers to integers
	** and compare the integers.
	*/
	Unsigned num1 = (Unsigned) address1;
	Unsigned num2 = (Unsigned) address2;
	return (num1 > num2 ? 1 : num1 == num2 ? 0 : -1);
}

/*
** Return the index of the entry in MR_spied_procs whose spy_proc field
** is entry, or a negative number if absent.
*/

static int
MR_search_spy_table_for_proc(const MR_Stack_Layout_Entry *entry)
{
	int	slot;
	bool	found;

	MR_bsearch(MR_spied_proc_next, slot, found,
		MR_compare_addr(MR_spied_procs[slot].spy_proc, entry));
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

int
MR_add_spy_point(MR_Spy_When when, MR_Spy_Action action,
	const MR_Stack_Layout_Entry *entry, const MR_Stack_Layout_Label *label)
{
	MR_Spy_Point	*point;
	int		point_slot;
	int		proc_slot;
	int		i;
	bool		found;

	proc_slot = MR_search_spy_table_for_proc(entry);
	if (proc_slot < 0) {
		MR_ensure_room_for_next(MR_spied_proc, MR_Spied_Proc,
			MR_INIT_SPIED_PROCS);
		MR_prepare_insert_into_sorted(MR_spied_procs,
			MR_spied_proc_next, proc_slot,
			MR_compare_addr(MR_spied_procs[proc_slot].spy_proc,
				entry));
		MR_spied_procs[proc_slot].spy_proc = entry;
		MR_spied_procs[proc_slot].spy_points = NULL;
	}

	/* Insert the spy point at the head of the list for the proc. */
	point = MR_NEW(MR_Spy_Point);
	point->spy_when    = when;
	point->spy_exists  = TRUE;
	point->spy_enabled = TRUE;
	point->spy_action  = action;
	point->spy_proc    = entry;
	point->spy_label   = label;
	point->spy_next    = MR_spied_procs[proc_slot].spy_points;
	MR_spied_procs[proc_slot].spy_points = point;

	for (i = 0; i < MR_spy_point_next; i++) {
		if (! MR_spy_points[i]->spy_exists) {
			MR_spy_points[i] = point;
			return i;
		}
	}

	MR_ensure_room_for_next(MR_spy_point, MR_Spy_Point *,
		MR_INIT_SPY_POINTS);
	point_slot = MR_spy_point_next;
	MR_spy_points[point_slot] = point;
	MR_spy_point_next++;

	return point_slot;
}

void
MR_delete_spy_point(int point_table_slot)
{
	MR_Spy_Point	*point;
	MR_Spy_Point	**cur_addr;
	MR_Spy_Point	*cur;
	int		proc_table_slot;

	point = MR_spy_points[point_table_slot];

	/* this effectively remove the point from the spypoint table */
	point->spy_exists = FALSE;

	/* now remove the point from the spied proc table list for its proc */
	proc_table_slot = MR_search_spy_table_for_proc(point->spy_proc);
	if (proc_table_slot < 0) {
		fatal_error("deleted spy point was not indexed by proc addr");
	}

	cur_addr = &MR_spied_procs[proc_table_slot].spy_points;
	cur = MR_spied_procs[proc_table_slot].spy_points;
	while (cur != NULL && cur != point) {
		cur_addr = &cur->spy_next;
		cur = cur->spy_next;
	}

	if (cur == NULL) {
		fatal_error("deleted spy point was not on proc index list");
	}

	*cur_addr = point->spy_next;
}
