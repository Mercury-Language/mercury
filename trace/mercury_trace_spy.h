/*
** Copyright (C) 1998 The University of Melbourne.
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
	MR_SPY_ALL, MR_SPY_INTERFACE, MR_SPY_ENTRY, MR_SPY_SPECIFIC
} MR_Spy_When;

#define	MR_spy_when_string(w)		((w == MR_SPY_ALL) ? "all" :          \
					(w == MR_SPY_INTERFACE) ? "interface":\
					(w == MR_SPY_ENTRY) ? "entry" : \
					(w == MR_SPY_SPECIFIC) ? "specific" : \
					"unknown spy when")

typedef struct MR_Spy_Point_Struct MR_Spy_Point;

struct MR_Spy_Point_Struct {
	MR_Spy_When			spy_when;
	bool				spy_enabled;
	MR_Spy_Action			spy_action;
	const MR_Stack_Layout_Entry	*spy_proc;
	const MR_Stack_Layout_Label	*spy_label; /* if MR_SPY_SPECIFIC */
	MR_Spy_Point			*spy_next;
};

/*
** Check whether the event described by the given label layout and port
** matches any spy points. If yes, return TRUE and set *action to say what
** action should be executed for the spy point.
*/

extern	bool		MR_event_matches_spy_point(const MR_Stack_Layout_Label
				*layout, MR_Trace_Port port,
				MR_Spy_Action *action);

/*
** Add a new spy point to the table.
*/

extern	MR_Spy_Point	*MR_add_spy_point(MR_Spy_When when,
				MR_Spy_Action action,
				const MR_Stack_Layout_Entry *entry,
				const MR_Stack_Layout_Label *label);

#endif	/* not MERCURY_TRACE_SPY_H */
