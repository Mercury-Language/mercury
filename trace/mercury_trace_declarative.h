/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"

#ifndef MERCURY_TRACE_DECLARATIVE_H
#define MERCURY_TRACE_DECLARATIVE_H

/*
** This file defines the MR_Edt_Node data type, which stores nodes
** of an Evaluation Dependency Tree (EDT), used for declarative
** debugging.  It also defines an interface to the back end of the
** declarative debugger from the internal debugger.
*/

/*
** Each node in an EDT has a tag to denote its type.  At the moment
** the only type of analysis is wrong answer analysis, so the tag
** is just used to distinguish between implicitly and explicitly
** represented nodes.
**
** Implicit nodes are similar to explicit nodes, but they do not
** store their children.  The children can be created by re-executing
** the events in the stored range and collecting a new EDT.
*/

typedef enum {
	MR_EDT_WRONG_ANSWER_EXPLICIT,
	MR_EDT_WRONG_ANSWER_IMPLICIT
} MR_Edt_Node_Type;

/*
** Wrong answer analysis is currently the only type of analysis available.
** Consequently, the EDT nodes only contain enough information to support
** this type of analysis.
*/

typedef struct MR_Edt_Node_Struct MR_Edt_Node;

struct MR_Edt_Node_Struct {
		/*
		** Type of EDT node.
		*/
	MR_Edt_Node_Type		MR_edt_node_tag;
		/*
		** The layout of the EXIT port.
		*/
	const MR_Stack_Layout_Label	*MR_edt_node_layout;
		/*
		** The arguments.
		*/
	Word				*MR_edt_node_arg_values;
	Word				*MR_edt_node_arg_types;
		/*
		** This goal path gives the location, in the calling
		** procedure, of the call that this proof is for.
		*/
	const char			*MR_edt_node_path;
		/*
		** The event numbers of the CALL and EXIT events for
		** this proof.
		*/
	Unsigned			MR_edt_node_start_event;
	Unsigned			MR_edt_node_end_event;
		/*
		** The sequence number of the CALL and EXIT events.
		*/
	Unsigned			MR_edt_node_seqno;
		/*
		** The rightmost child of this node, or NULL if there
		** are no children.
		*/
	MR_Edt_Node			*MR_edt_node_children;
		/*
		** The next sibling to the left of this node, or NULL
		** if this is the leftmost.
		*/
	MR_Edt_Node			*MR_edt_node_sibling;
};

/*
** When in declarative debugging mode, the internal debugger calls
** MR_trace_decl_wrong_answer for each event.  
*/

extern	Code	*MR_trace_decl_wrong_answer(MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info);

/*
** The internal (interactive) debugger calls this function to enter
** declarative debugging mode.  It returns TRUE if successful, and
** FALSE if there was some problem that prevented this mode from
** being entered.
*/

extern	bool	MR_trace_start_wrong_answer(MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info,
			MR_Event_Details *event_details, Code **jumpaddr);

#endif	/* MERCURY_TRACE_DECLARATIVE_H */
