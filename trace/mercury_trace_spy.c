/*
** Copyright (C) 1998-2001 The University of Melbourne.
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
#include "mercury_array_macros.h"
#include "mercury_trace_base.h"

#include "mercury_trace.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_tables.h"

#include <stdlib.h>

const char		*MR_spy_when_names[] =
{
	"all",
	"interface",
	"entry",
	"specific",
	"linenumber",
};

/*
** The table of spy points, with one entry per existing (or deleted but
** not yet reused) spy point, with counters saying which is the next
** free slot and how many slots are allocated.
*/

MR_Spy_Point    	**MR_spy_points;
int			MR_spy_point_next = 0;
int			MR_spy_point_max  = 0;

int			MR_most_recent_spy_point = -1;

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
	const MR_Proc_Layout		*spy_proc;
	MR_Spy_Point			*spy_points;
} MR_Spied_Proc;

static	MR_Spied_Proc	*MR_spied_procs;
static	int		MR_spied_proc_next = 0;
static	int		MR_spied_proc_max = 0;

/* The initial size of the spied procs table. */
#define	MR_INIT_SPIED_PROCS	10

/*
** The table of spied on return labels, with one entry per return label
** that is currently being spied on, ordered on label layout structure address,
** and with counters saying which is the next free slot and how many slots
** are allocated.
*/

typedef struct {
	const MR_Label_Layout	*spy_label;
	int			spy_point_num;
} MR_Spied_Label;

static	MR_Spied_Label	*MR_spied_labels;
static	int		MR_spied_label_next = 0;
static	int		MR_spied_label_max = 0;

/* The initial size of the spied labels table. */
#define	MR_INIT_SPIED_LABELS	10

/**************************************************************************/

static	int	MR_compare_addr(const void *address1, const void *address2);
static	int	MR_search_spy_table_for_proc(const MR_Proc_Layout *entry);
static	int	MR_search_spy_table_for_label(const MR_Label_Layout *label);
static	void	MR_add_line_spy_point_callback(const MR_Label_Layout *label,
			int spy_point_num);
static	int	MR_compare_spied_labels(const void *, const void *);
static	void	MR_update_enabled_action(MR_Spy_Point *point,
			MR_Trace_Port port, MR_Spy_Action *action_ptr,
			MR_bool *enabled_ptr);
static	const char *MR_ignore_when_to_string(MR_Spy_Ignore_When ignore_when);

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
	MR_Unsigned num1 = (MR_Unsigned) address1;
	MR_Unsigned num2 = (MR_Unsigned) address2;
	return (num1 > num2 ? 1 : num1 == num2 ? 0 : -1);
}

/*
** Return the index of the entry in MR_spied_procs whose spy_proc field
** is entry, or a negative number if absent.
*/

static int
MR_search_spy_table_for_proc(const MR_Proc_Layout *entry)
{
	int	slot;
	MR_bool	found;

	MR_bsearch(MR_spied_proc_next, slot, found,
		MR_compare_addr(MR_spied_procs[slot].spy_proc, entry));
	if (found) {
		return slot;
	} else {
		return -1;
	}
}

/*
** Return the index of the entry in MR_spied_labels whose spy_label field
** is label, or a negative number if absent.
*/

static int
MR_search_spy_table_for_label(const MR_Label_Layout *label)
{
	int	slot;
	MR_bool	found;

	MR_bsearch(MR_spied_label_next, slot, found,
		MR_compare_addr(MR_spied_labels[slot].spy_label, label));
	if (found) {
		return slot;
	} else {
		return -1;
	}
}

MR_bool
MR_event_matches_spy_point(const MR_Label_Layout *layout,
	MR_Trace_Port port, MR_Spy_Action *action_ptr)
{
	int			slot;
	MR_bool			enabled;
	MR_Spy_Point		*point;
	MR_Spy_Action		action;
	const MR_Label_Layout	*parent;
	const char		*problem;
	MR_Word			*base_sp;
	MR_Word			*base_curfr;

	enabled = MR_FALSE;
	action = MR_SPY_PRINT;

	if (MR_spied_label_next > 0) {
		slot = MR_search_spy_table_for_label(layout);
		if (slot >= 0) {
			point = MR_spy_points[
				MR_spied_labels[slot].spy_point_num];
			if (point->spy_when != MR_SPY_LINENO) {
				MR_fatal_error("non-lineno spy point in "
					"spied labels array");
			}

			MR_update_enabled_action(point, port,
				&action, &enabled);
		}

		if (MR_port_is_interface(port)) {
			MR_restore_transient_registers();
			base_sp = MR_sp;
			base_curfr = MR_curfr;
			parent = MR_find_nth_ancestor(layout, 1,
				&base_sp, &base_curfr, &problem);
			if (parent != NULL && 0 <=
				(slot = MR_search_spy_table_for_label(parent)))
			{
				point = MR_spy_points[MR_spied_labels[slot].
					spy_point_num];
				if (point->spy_when != MR_SPY_LINENO) {
					MR_fatal_error("non-lineno "
						"spy point in "
						"spied labels array");
				}

				MR_update_enabled_action(point, port,
					&action, &enabled);
			}
		}
	}

	slot = MR_search_spy_table_for_proc(layout->MR_sll_entry);
	if (slot >= 0) {
		for (point = MR_spied_procs[slot].spy_points; point != NULL;
				point = point->spy_next)
		{
			switch (point->spy_when) {

				case MR_SPY_ALL:
					MR_update_enabled_action(point,
						port, &action,
						&enabled);
				break;

				case MR_SPY_ENTRY:
					if (MR_port_is_entry(port)) {
						MR_update_enabled_action(point,
							port, &action,
							&enabled);
					} else {
						continue;
					}

					break;

				case MR_SPY_INTERFACE:
					if (MR_port_is_interface(port)) {
						MR_update_enabled_action(point,
							port, &action,
							&enabled);
					} else {
						continue;
					}

					break;

				case MR_SPY_SPECIFIC:
					if (layout == point->spy_label) {
						MR_update_enabled_action(point,
							port, &action,
							&enabled);
					} else {
						continue;
					}

					break;

				case MR_SPY_LINENO:
					MR_fatal_error("lineno spy point in "
						"spied procs array");

				default:
					MR_fatal_error("bad spy point when in "
						"MR_event_matches_spy_point");
			}
		}
	}

	if (enabled) {
		*action_ptr = action;
		return MR_TRUE;
	} else {
		return MR_FALSE;
	}
}

static void
MR_update_enabled_action(MR_Spy_Point *point, MR_Trace_Port port,
	MR_Spy_Action *action_ptr, MR_bool *enabled_ptr)
{
	if (point->spy_enabled) {
		if (point->spy_ignore_count == 0) {
			*enabled_ptr = MR_TRUE;
			*action_ptr = MR_max(*action_ptr, point->spy_action);
		}

		if (point->spy_ignore_count > 0) {
			switch (point->spy_ignore_when) {
			case MR_SPY_DONT_IGNORE:
				break;

			case MR_SPY_IGNORE_ENTRY:
				if (port == MR_PORT_CALL) {
					--point->spy_ignore_count;
				}
				break;

			case MR_SPY_IGNORE_INTERFACE:
				if (MR_port_is_interface(port)) {
					--point->spy_ignore_count;
				}
				break;

			default:
				MR_fatal_error("MR_update_enabled_action: "
					"invalid ignore_when");
			}
		}
	}
}

static const char *incompatible =
	"Ignore count is not compatible with break point specification";

int
MR_add_proc_spy_point(MR_Spy_When when, MR_Spy_Action action,
	MR_Spy_Ignore_When ignore_when, int ignore_count,
	const MR_Proc_Layout *entry, const MR_Label_Layout *label,
	const char **problem)
{
	MR_Spy_Point	*point;
	int		point_slot;
	int		proc_slot;
	int		i;

	*problem = NULL;

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
	point->spy_exists  = MR_TRUE;
	point->spy_enabled = MR_TRUE;
	point->spy_action  = action;
	point->spy_ignore_when  = ignore_when;
	point->spy_ignore_count = ignore_count;
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

	MR_most_recent_spy_point = point_slot;
	return point_slot;
}

/* 1024 characters should be big enough ... */
#define		MR_ERROR_MSG_BUF_SIZE	1024
static char	MR_error_msg_buf[MR_ERROR_MSG_BUF_SIZE];

int
MR_add_line_spy_point(MR_Spy_Action action, MR_Spy_Ignore_When ignore_when,
	int ignore_count, const char *orig_filename, int linenumber,
	const char **problem)
{
	MR_Spy_Point	*point;
	int		point_slot;
	int		old_size, new_size;
	char 		*filename;

	*problem = NULL;
	if (ignore_when != MR_SPY_DONT_IGNORE) {
		*problem = incompatible;
		return -1;
	}

	/*
	** The original filename string may have come from a buffer
	** or other volatile storage.
	*/

	filename = MR_copy_string(orig_filename);

	point_slot = MR_spy_point_next;

	old_size = MR_spied_label_next;
	MR_process_file_line_layouts(filename, linenumber,
		MR_add_line_spy_point_callback, point_slot);
	new_size = MR_spied_label_next;

	if (new_size == old_size) {
		/* there were no matching labels */
#ifdef	MR_HAVE_SNPRINTF
		snprintf(MR_error_msg_buf, MR_ERROR_MSG_BUF_SIZE,
			"there is no event at %s:%d",
			filename, linenumber);
#else
		/* not absolutely safe, but the risk of overflow is minimal */
		sprintf(MR_error_msg_buf,
			"there is no event at %s:%d",
			filename, linenumber);
		if (strlen(MR_error_msg_buf) >= MR_ERROR_MSG_BUF_SIZE) {
			MR_fatal_error("MR_add_line_spy_point: buf overflow");
		}
#endif
		*problem = MR_error_msg_buf;
		return -1;
	}

	/*
	** The matching labels were added at the end of the spied label table.
	** We must make the table sorted again.
	*/

	qsort(MR_spied_labels, MR_spied_label_next, sizeof(MR_Spied_Label), 
		MR_compare_spied_labels);

	point = MR_NEW(MR_Spy_Point);
	point->spy_when       = MR_SPY_LINENO;
	point->spy_exists     = MR_TRUE;
	point->spy_enabled    = MR_TRUE;
	point->spy_action     = action;
	point->spy_ignore_when  = ignore_when;
	point->spy_ignore_count = ignore_count;
	point->spy_filename   = filename;
	point->spy_linenumber = linenumber;

	MR_ensure_room_for_next(MR_spy_point, MR_Spy_Point *,
		MR_INIT_SPY_POINTS);
	MR_spy_points[point_slot] = point;
	MR_spy_point_next++;

	MR_most_recent_spy_point = point_slot;
	return point_slot;
}

static void
MR_add_line_spy_point_callback(const MR_Label_Layout *label, int spy_point_num)
{
	int	spied_label_slot;

	MR_ensure_room_for_next(MR_spied_label, MR_Spied_Label,
		MR_INIT_SPIED_LABELS);
	spied_label_slot = MR_spied_label_next;
	MR_spied_labels[spied_label_slot].spy_label = label;
	MR_spied_labels[spied_label_slot].spy_point_num = spy_point_num;
	MR_spied_label_next++;
}

static int
MR_compare_spied_labels(const void *l1, const void *l2)
{
	const MR_Spied_Label	*label1, *label2;

	label1 = (const MR_Spied_Label *) l1;
	label2 = (const MR_Spied_Label *) l2;

	return (int) ((MR_Integer) label1->spy_label
		- (MR_Integer) label2->spy_label);
}

const char *
MR_ignore_spy_point(int point_slot, MR_Spy_Ignore_When ignore_when,
	int ignore_count)
{
	switch (MR_spy_points[point_slot]->spy_when) {
		case MR_SPY_ENTRY:
		case MR_SPY_INTERFACE:
		case MR_SPY_ALL:
			break;

		case MR_SPY_SPECIFIC:
		case MR_SPY_LINENO:
			return incompatible;
			break;

		default:
			MR_fatal_error("MR_add_proc_spy_point: bad when");
			break;
	}

	MR_spy_points[point_slot]->spy_ignore_when  = ignore_when;
	MR_spy_points[point_slot]->spy_ignore_count = ignore_count;
	return NULL;
}

void
MR_delete_spy_point(int point_table_slot)
{
	MR_Spy_Point	*point;
	MR_Spy_Point	**cur_addr;
	MR_Spy_Point	*cur;
	int		proc_table_slot;
	int		i;
	int		label_slot;

	point = MR_spy_points[point_table_slot];

	if (MR_most_recent_spy_point == point_table_slot) {
		MR_most_recent_spy_point = -1;
	}

	if (point->spy_when == MR_SPY_LINENO) {
		/* Release the storage acquired by MR_copy_string. */
		MR_free(point->spy_filename);

		/*
		** Remove the spy point from the spied label table list.
		*/

		label_slot = 0;
		for (i = 0; i < MR_spied_label_next; i++) {
			if (MR_spied_labels[i].spy_point_num !=
				point_table_slot)
			{
				MR_spied_labels[label_slot].spy_label =
					MR_spied_labels[i].spy_label;
				MR_spied_labels[label_slot].spy_point_num =
					MR_spied_labels[i].spy_point_num;
				label_slot++;
			}
		}

		MR_spied_label_next = label_slot;
	} else {
		/*
		** Remove the spy point from the spied proc table list
		** for its proc.
		*/

		proc_table_slot = MR_search_spy_table_for_proc(point->spy_proc);
		if (proc_table_slot < 0) {
			MR_fatal_error("deleted spy point "
				"was not indexed by proc addr");
		}

		cur_addr = &MR_spied_procs[proc_table_slot].spy_points;
		cur = MR_spied_procs[proc_table_slot].spy_points;
		while (cur != NULL && cur != point) {
			cur_addr = &cur->spy_next;
			cur = cur->spy_next;
		}

		if (cur == NULL) {
			MR_fatal_error("deleted spy point "
				"was not on proc index list");
		}

		*cur_addr = point->spy_next;
	}
}

void
MR_print_spy_point(FILE *fp, int spy_point_num)
{
	MR_Spy_Point	*point;

	point = MR_spy_points[spy_point_num];
	fprintf(fp, "%2d: %1s %-5s %9s ",
		spy_point_num,
		point->spy_exists ?
			(point->spy_enabled ? "+" : "-") :
			(point->spy_enabled ? "E" : "D"),
		MR_spy_action_string(point->spy_action),
		MR_spy_when_names[point->spy_when]);
	if (point->spy_when == MR_SPY_LINENO) {
		fprintf(fp, "%s:%d",
			point->spy_filename, point->spy_linenumber);
	} else {
		MR_print_proc_id(fp, point->spy_proc);
	}

	if (point->spy_ignore_count > 1) {
		fprintf(fp, "\n%12s(ignore next %d %s events)\n",
			"", point->spy_ignore_count,
			MR_ignore_when_to_string(point->spy_ignore_when));
	} else if (point->spy_ignore_count > 0) {
		fprintf(fp, "\n%12s(ignore next %s event)\n",
			"", MR_ignore_when_to_string(point->spy_ignore_when));
	} else {
		fprintf(fp, "\n");
	}
}

static const char *
MR_ignore_when_to_string(MR_Spy_Ignore_When ignore_when)
{
	switch (ignore_when) {
		case MR_SPY_IGNORE_ENTRY:
			return "call";

		case MR_SPY_IGNORE_INTERFACE:
			return "interface";

		default:
			MR_fatal_error("MR_ignore_when_to_string: invalid ignore_when");
	}
}

MR_bool
MR_save_spy_points(FILE *fp, FILE *err_fp)
{
	MR_Spy_Point	*point;
	int		i;

	for (i = 0; i < MR_spy_point_next; i++) {
		if (! MR_spy_points[i]->spy_exists) {
			continue;
		}

		point = MR_spy_points[i];

		switch (point->spy_action) {
			case MR_SPY_STOP:
				fprintf(fp, "break ");
				break;

			case MR_SPY_PRINT:
				fprintf(fp, "break -P ");
				break;

			default:
				fprintf(err_fp, "internal error: "
					"unknown spy action\n");
				return MR_TRUE;
		}

		if (point->spy_ignore_count > 0) {
			switch (point->spy_ignore_when) {
				case MR_SPY_IGNORE_INTERFACE:
					fprintf(fp, " -I%d",
						point->spy_ignore_count);
					break;

				case MR_SPY_IGNORE_ENTRY:
					fprintf(fp, " -E%d",
						point->spy_ignore_count);
					break;

				default:
					MR_fatal_error("MR_save_spy_points: "
						"invalid ignore_when");
			}
		}

		switch (point->spy_when) {
			case MR_SPY_LINENO:
				fprintf(fp, "%s:%d\n",
					point->spy_filename,
					point->spy_linenumber);
				break;

			case MR_SPY_ALL:
				fprintf(fp, "-a ");
				MR_print_proc_spec(fp, point->spy_proc);
				fprintf(fp, "\n");
				break;

			case MR_SPY_INTERFACE:
				MR_print_proc_spec(fp, point->spy_proc);
				fprintf(fp, "\n");
				break;

			case MR_SPY_ENTRY:
				fprintf(fp, "-e ");
				MR_print_proc_spec(fp, point->spy_proc);
				fprintf(fp, "\n");
				break;

			case MR_SPY_SPECIFIC:
				fprintf(err_fp, "mdb: cannot save "
					"breakpoint on specific "
					"internal label\n");
				break;

			default:
				fprintf(err_fp, "mdb: internal error: "
					"unknown spy when\n");
				return MR_TRUE;
		}

		if (!point->spy_enabled) {
			fprintf(fp, "disable\n");
		}
	}

	return MR_FALSE;
}
