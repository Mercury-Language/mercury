/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the declarations of the types and functions that
** the internal and external debuggers can use to manipulate spy points.
**
** Main author: Zoltan Somogyi.
*/

#ifndef	MERCURY_TRACE_SPY_H
#define	MERCURY_TRACE_SPY_H

typedef enum {
	MR_SPY_PRINT, MR_SPY_STOP
} MR_Spy_Action;

#define	MR_spy_action_string(a)		((a == MR_SPY_STOP) ? "stop" :      \
					(a == MR_SPY_PRINT) ? "print" :     \
					"unknown spy action")

typedef enum {
	MR_SPY_ALL,
	MR_SPY_INTERFACE,
	MR_SPY_ENTRY,
	MR_SPY_SPECIFIC,
	MR_SPY_LINENO
} MR_Spy_When;

extern	const char	*MR_spy_when_names[];

typedef struct MR_Spy_Point_Struct MR_Spy_Point;

struct MR_Spy_Point_Struct {
	bool				spy_exists;	/* FALSE if deleted */
	bool				spy_enabled;
	MR_Spy_When			spy_when;
	MR_Spy_Action			spy_action;
	const MR_Stack_Layout_Entry	*spy_proc;      /* if not LINENO */
	const MR_Stack_Layout_Label	*spy_label;	/* if SPECIFIC */
	const char			*spy_filename;  /* if LINENO */
	int				spy_linenumber; /* if LINENO */
	MR_Spy_Point			*spy_next;	/* if not LINENO */
};

/*
** The table of spy points, with counters saying which is the next free slot
** and how many slots are allocated.
*/

extern	MR_Spy_Point    **MR_spy_points;
extern	int		MR_spy_point_next;
extern	int		MR_spy_point_max;

/*
** Check whether the event described by the given label layout and port
** matches any spy points. If yes, return TRUE and set *action to say what
** action should be executed for the spy point.
*/

extern	bool		MR_event_matches_spy_point(const MR_Stack_Layout_Label
				*layout, MR_Trace_Port port,
				MR_Spy_Action *action);

/*
** Add a new spy point on a procedure (as opposed to on a line number)
** to the table.
*/

extern	int		MR_add_proc_spy_point(MR_Spy_When when,
				MR_Spy_Action action,
				const MR_Stack_Layout_Entry *entry,
				const MR_Stack_Layout_Label *label);

/*
** Add a new spy point on a line number (as opposed to on a procedure)
** to the table.
*/

extern	int		MR_add_line_spy_point(MR_Spy_Action action,
				const char *filename, int linenumber);

/*
** Delete a spy point from the table.
*/

extern	void		MR_delete_spy_point(int point_table_slot);

#endif	/* not MERCURY_TRACE_SPY_H */
