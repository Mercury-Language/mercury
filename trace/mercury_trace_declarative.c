/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** Main author: Mark Brown
**
** This file implements the back end of the declarative debugger.  The
** back end is an extension to the internal debugger which collects
** related trace events and builds them into an execution tree.  Once
** built, the tree is passed to the front end where it can be analysed
** to find bugs.  The front end is implemented in
** browse/declarative_debugger.m.
**
** The interface between the front and back ends is via the
** execution_tree/2 typeclass, which is documented in
** browse/declarative_debugger.m.  It would be possible to replace
** the front end or the back end with an alternative implementation
** which also conforms to the typeclass constraints.  For example:
** 	- An alternative back end could generate the same tree
** 	  structure in a different way, such as via program
** 	  transformation.
** 	- An alternative front end could graphically display the
** 	  generated trees as part of a visualization tool rather
** 	  than analyzing them for bugs.
*/

#include "mercury_imp.h"
#include "mercury_trace_declarative.h"

#ifdef MR_USE_DECLARATIVE_DEBUGGER

#include "mercury_trace.h"
#include "mercury_trace_browse.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"
#include "mercury_layout_util.h"
#include "mercury_deep_copy.h"
#include "mercury_string.h"
#include "declarative_debugger.h"
#include "declarative_execution.h"
#include "std_util.h"

#include <errno.h>

/*
** We only build the execution tree to a certain depth.  The following
** macro gives the default depth limit (relative to the starting depth).
*/

#define MR_EDT_DEPTH_STEP_SIZE		8

/*
** The declarative debugger back end is controlled by the
** settings of the following variables.  They are set in
** MR_trace_start_decl_debug when the back end is started.  They
** are used by MR_trace_decl_debug to decide what action to
** take for a particular trace event.  Events that are outside
** the given depth range are ignored.  Events that are beyond the
** given last event cause the internal debugger to be switched
** back into interactive mode.
*/

static	Unsigned	MR_edt_min_depth;
static	Unsigned	MR_edt_max_depth;
static	Unsigned	MR_edt_last_event;

/*
** This is used as the abstract map from node identifiers to nodes
** in the data structure passed to the front end.  It should be
** incremented each time the data structure is destructively
** updated, before being passed to Mercury code again.
*/

static	Unsigned	MR_trace_node_store;

/*
** The front end state is stored here in between calls to it.
*/

static	Word		MR_trace_front_end_state = (Word) NULL;

/*
** MR_trace_current_node always contains the last node allocated,
** or NULL if the collection has just started.
*/

static	MR_Trace_Node	MR_trace_current_node;

/*
** When in test mode, MR_trace_store_file points to an open file to
** which the store should be written when built.
*/

static	FILE		*MR_trace_store_file;

static	void
MR_trace_decl_call(MR_Event_Info *event_info);

static	void
MR_trace_decl_exit(MR_Event_Info *event_info);

static	void
MR_trace_decl_redo(MR_Event_Info *event_info);

static	void
MR_trace_decl_fail(MR_Event_Info *event_info);

static	void
MR_trace_decl_switch(MR_Event_Info *event_info);

static	void
MR_trace_decl_disj(MR_Event_Info *event_info);

static	void
MR_trace_decl_cond(MR_Event_Info *event_info);

static	void
MR_trace_decl_then_else(MR_Event_Info *event_info);

static	void
MR_trace_decl_enter_neg(MR_Event_Info *event_info);

static	void
MR_trace_decl_leave_neg(MR_Event_Info *event_info);

static	MR_Trace_Node
MR_trace_decl_get_slot(const MR_Stack_Layout_Entry *entry, Word *saved_regs);

static	void
MR_trace_decl_set_slot(const MR_Stack_Layout_Entry *entry, Word *saved_regs,
		MR_Trace_Node node);

static	bool
MR_trace_matching_cond(const char *path, MR_Trace_Node node);

static	bool
MR_trace_matching_neg(const char *path, MR_Trace_Node node);

static	bool
MR_trace_matching_disj(const char *path, MR_Trace_Node node);

static	bool
MR_trace_same_construct(const char *p1, const char *p2);

static	bool
MR_trace_single_component(const char *path);

static	Word
MR_decl_make_atom(const MR_Stack_Layout_Label *layout, Word *saved_regs);

static	ConstString
MR_decl_atom_name(const MR_Stack_Layout_Entry *entry);

static	Word
MR_decl_atom_args(const MR_Stack_Layout_Label *layout, Word *saved_regs);

static	void
MR_decl_diagnosis(MR_Trace_Node root);

static	void
MR_decl_diagnosis_test(MR_Trace_Node root);

static	String
MR_trace_node_path(MR_Trace_Node node);

static	MR_Trace_Node
MR_trace_scan_backwards(MR_Trace_Node node);

Code *
MR_trace_decl_debug(MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info)
{
	MR_Stack_Layout_Entry 	*entry;
	Unsigned		depth;

	entry = event_info->MR_event_sll->MR_sll_entry;
	depth = event_info->MR_call_depth;

	if (event_info->MR_event_number > MR_edt_last_event) {
		/* This shouldn't ever be reached. */
		fprintf(MR_mdb_err, "Warning: missed final event.\n");
		fprintf(MR_mdb_err, "event %d\nlast event %d\n",
				event_info->MR_event_number,
				MR_edt_last_event);
		MR_trace_decl_mode = MR_TRACE_INTERACTIVE;
		return MR_trace_event_internal(cmd, TRUE, event_info);
	}

	if (!MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
		/* XXX this should be handled better. */
		fatal_error("layout has no execution tracing");
	}

	if (depth > MR_edt_max_depth ||
		depth < MR_edt_min_depth ||
		entry->MR_sle_maybe_decl_debug < 1 ) {
		/*
		** We ignore any event for a procedure that does not have
		** slots reserved for declarative debugging.  Such
		** procedures are assumed to be correct.  We also filter
		** out events with a depth outside the range given by
		** MR_edt_{min,max}_depth.  These events are either
		** irrelevant, or else implicitly represented in the
		** structure being built.  See comment in
		** trace/mercury_trace_declarative.h.
		*/
		return NULL;
	}

	MR_trace_enabled = FALSE;

	switch (event_info->MR_trace_port) {
		case MR_PORT_CALL:
			MR_trace_decl_call(event_info);
			break;
		case MR_PORT_EXIT:
			MR_trace_decl_exit(event_info);
			break;
		case MR_PORT_REDO:
			MR_trace_decl_redo(event_info);
			break;
		case MR_PORT_FAIL:
			MR_trace_decl_fail(event_info);
			break;
		case MR_PORT_DISJ:
			MR_trace_decl_disj(event_info);
			break;
		case MR_PORT_SWITCH:
			MR_trace_decl_switch(event_info);
			break;
		case MR_PORT_COND:
			MR_trace_decl_cond(event_info);
			break;
		case MR_PORT_THEN:
		case MR_PORT_ELSE:
			MR_trace_decl_then_else(event_info);
			break;
		case MR_PORT_NEG_ENTER:
			MR_trace_decl_enter_neg(event_info);
			break;
		case MR_PORT_NEG_SUCCESS:
		case MR_PORT_NEG_FAILURE:
			MR_trace_decl_leave_neg(event_info);
			break;
		case MR_PORT_PRAGMA_FIRST:
		case MR_PORT_PRAGMA_LATER:
			break;
		case MR_PORT_EXCEPTION:
			fatal_error("MR_trace_decl_debug: "
				"exceptions are not handled (yet)");
		default:
			fatal_error("MR_trace_decl_debug: unknown port");
	}
	
	if (MR_trace_event_number == MR_edt_last_event) {
		switch (MR_trace_decl_mode) {
			case MR_TRACE_DECL_DEBUG:
				/* Call the front end */
				MR_decl_diagnosis(MR_trace_current_node);
				break;

			case MR_TRACE_DECL_DEBUG_TEST:
				MR_decl_diagnosis_test(MR_trace_current_node);
				break;

			default:
				fatal_error("MR_trace_decl_debug: "
						"unexpected mode");
		}

		/*
		** XXX we should return to the CALL event of the buggy
		** node, if one was found.
		*/
		MR_trace_decl_mode = MR_TRACE_INTERACTIVE;
		return MR_trace_event_internal(cmd, TRUE, event_info);
	}

	MR_trace_enabled = TRUE;

	return NULL;
}

static	void
MR_trace_decl_call(MR_Event_Info *event_info)
{
	MR_Trace_Node			node;
	Word				atom;
	const MR_Stack_Layout_Label	*layout = event_info->MR_event_sll;

	atom = MR_decl_make_atom(layout, event_info->MR_saved_regs);
	MR_TRACE_CALL_MERCURY(
		node = (MR_Trace_Node) MR_DD_construct_call_node(
					(Word) MR_trace_current_node, atom);
	);
	MR_trace_decl_set_slot(layout->MR_sll_entry,
					event_info->MR_saved_regs, node);

	MR_trace_current_node = node;
}
	
static	void
MR_trace_decl_exit(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;
	MR_Trace_Node		call;
	Word			atom;

	atom = MR_decl_make_atom(event_info->MR_event_sll,
				event_info->MR_saved_regs);
	call = MR_trace_decl_get_slot(event_info->MR_event_sll->MR_sll_entry,
				event_info->MR_saved_regs);
	MR_TRACE_CALL_MERCURY(
		node = (MR_Trace_Node) MR_DD_construct_exit_node(
					(Word) MR_trace_current_node, 
					(Word) call,
					MR_trace_call_node_answer(call),
					atom);
	);
	MR_trace_call_node_answer(call) = (Word) node;

	MR_trace_current_node = node;
}

static	void
MR_trace_decl_redo(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;
	MR_Trace_Node		call;

	call = MR_trace_decl_get_slot(event_info->MR_event_sll->MR_sll_entry,
				event_info->MR_saved_regs);
	MR_TRACE_CALL_MERCURY(
		node = (MR_Trace_Node) MR_DD_construct_redo_node(
					(Word) MR_trace_current_node,
					MR_trace_call_node_answer(call));
	);
	MR_trace_call_node_answer(call) = (Word) node;

	MR_trace_current_node = node;
}

static	void
MR_trace_decl_fail(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;
	MR_Trace_Node		call;

	call = MR_trace_decl_get_slot(event_info->MR_event_sll->MR_sll_entry,
				event_info->MR_saved_regs);
	MR_TRACE_CALL_MERCURY(
		node = (MR_Trace_Node) MR_DD_construct_fail_node(
						(Word) MR_trace_current_node,
						(Word) call);
	);

	MR_trace_current_node = node;
}

static	void
MR_trace_decl_cond(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;

	MR_TRACE_CALL_MERCURY(
		node = (MR_Trace_Node) MR_DD_construct_cond_node(
					(Word) MR_trace_current_node,
					(String) event_info->MR_event_path);
	);
	MR_trace_current_node = node;
}

static	void
MR_trace_decl_then_else(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;
	MR_Trace_Node		prev;

	prev = MR_trace_current_node;

	/*
	** Search through previous nodes for a matching COND event.
	*/
	while (prev != (MR_Trace_Node) NULL)
	{
		if (MR_trace_matching_cond(event_info->MR_event_path, prev))
		{
			break;
		}
		prev = MR_trace_scan_backwards(prev);
	}
	if (prev == (MR_Trace_Node) NULL) {
		fatal_error("MR_trace_decl_then_else: no matching COND");
	}
	
	switch (event_info->MR_trace_port) {
		case MR_PORT_THEN:
			MR_trace_cond_node_status(prev) =
					MR_TRACE_STATUS_SUCCEEDED;
			MR_TRACE_CALL_MERCURY(
				node = (MR_Trace_Node)
					MR_DD_construct_then_node(
						(Word) MR_trace_current_node,
						(Word) prev);
			);
			break;
		case MR_PORT_ELSE:
			MR_trace_cond_node_status(prev) =
					MR_TRACE_STATUS_FAILED;
			MR_TRACE_CALL_MERCURY(
				node = (MR_Trace_Node)
					MR_DD_construct_else_node(
						(Word) MR_trace_current_node,
						(Word) prev);
			);
			break;
		default:
			fatal_error("MR_trace_decl_then_else: invalid node");
			break;
	}

	MR_trace_current_node = node;
}

static	void
MR_trace_decl_enter_neg(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;

	MR_TRACE_CALL_MERCURY(
		node = (MR_Trace_Node) MR_DD_construct_neg_node(
					(Word) MR_trace_current_node,
					(String) event_info->MR_event_path);
	);
	MR_trace_current_node = node;
}

static	void
MR_trace_decl_leave_neg(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;
	MR_Trace_Node		prev;

	prev = MR_trace_current_node;

	/*
	** Search through previous nodes for a matching NEGE event.
	*/
	while (prev != (MR_Trace_Node) NULL)
	{
		if (MR_trace_matching_neg(event_info->MR_event_path, prev))
		{
			break;
		}
		prev = MR_trace_scan_backwards(prev);
	}
	if (prev == (MR_Trace_Node) NULL) {
		fatal_error("MR_trace_decl_leave_neg: no matching NEGE");
	}
	
	switch (event_info->MR_trace_port) {
		case MR_PORT_NEG_SUCCESS:
			MR_trace_neg_node_status(prev) =
					MR_TRACE_STATUS_SUCCEEDED;
			MR_TRACE_CALL_MERCURY(
				node = (MR_Trace_Node)
					MR_DD_construct_neg_succ_node(
						(Word) MR_trace_current_node,
						(Word) prev);
			);
			break;
		case MR_PORT_NEG_FAILURE:
			MR_trace_neg_node_status(prev) =
					MR_TRACE_STATUS_FAILED;
			MR_TRACE_CALL_MERCURY(
				node = (MR_Trace_Node)
					MR_DD_construct_neg_fail_node(
						(Word) MR_trace_current_node,
						(Word) prev);
			);
			break;
		default:
			fatal_error("MR_trace_decl_leave_neg: invalid node");
			break;
	}

	MR_trace_current_node = node;
}

static	void
MR_trace_decl_switch(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;

	MR_TRACE_CALL_MERCURY(
		node = (MR_Trace_Node) MR_DD_construct_first_disj_node(
					(Word) MR_trace_current_node,
					(String) event_info->MR_event_path,
					(Word) TRUE);
	);
	MR_trace_current_node = node;
}

static	void
MR_trace_decl_disj(MR_Event_Info *event_info)
{
	MR_Trace_Node		node;
	MR_Trace_Node		prev;
	MR_Trace_Node		back;

	prev = MR_trace_current_node;

	/*
	** Search through previous nodes for a matching DISJ event.
	*/
	while (prev != (MR_Trace_Node) NULL)
	{
		if (MR_trace_matching_disj(event_info->MR_event_path, prev))
		{
			break;
		}
		prev = MR_trace_scan_backwards(prev);
	}

	if (prev == (MR_Trace_Node) NULL) {
		/*
		** This is a first_disj.
		*/
		MR_TRACE_CALL_MERCURY(
			node = (MR_Trace_Node) MR_DD_construct_first_disj_node(
					(Word) MR_trace_current_node,
					(String) event_info->MR_event_path,
					(Word) FALSE);
		);
	} else {
		/*
		** This is a later_disj.
		*/
		back = MR_trace_scan_backwards(prev);
		MR_TRACE_CALL_MERCURY(
			node = (MR_Trace_Node) MR_DD_construct_later_disj_node(
					(Word) MR_trace_current_node,
					(Word) back,
					(String) event_info->MR_event_path);
		);
	}

	MR_trace_current_node = node;
}

static	MR_Trace_Node
MR_trace_decl_get_slot(const MR_Stack_Layout_Entry *entry, Word *saved_regs)
{
	int			decl_slot;
	Word			*saved_sp;
	Word			*saved_curfr;
	MR_Trace_Node		node;
	
	decl_slot = entry->MR_sle_maybe_decl_debug;
	
	if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
		saved_sp = (Word *) MR_saved_sp(saved_regs);
		node = (MR_Trace_Node) MR_based_stackvar(saved_sp, decl_slot);
	} else {
		saved_curfr = (Word *) MR_saved_curfr(saved_regs);
		node = (MR_Trace_Node) MR_based_framevar(saved_curfr,
							decl_slot);
	}
	
	return node;
}

static	void
MR_trace_decl_set_slot(const MR_Stack_Layout_Entry *entry,
		Word *saved_regs, MR_Trace_Node node)
{
	int			decl_slot;
	Word			*saved_sp;
	Word			*saved_curfr;
	
	decl_slot = entry->MR_sle_maybe_decl_debug;
	
	if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
		saved_sp = (Word *) MR_saved_sp(saved_regs);
		MR_based_stackvar(saved_sp, decl_slot) = (Word) node;
	} else {
		saved_curfr = (Word *) MR_saved_curfr(saved_regs);
		MR_based_framevar(saved_curfr, decl_slot) = (Word) node;
	}
}

static	bool
MR_trace_matching_cond(const char *path, MR_Trace_Node node)
{
	MR_Trace_Port		port;
	const char		*node_path;

	MR_TRACE_CALL_MERCURY(
		port = (MR_Trace_Port) MR_DD_trace_node_port(node);
	);
	if (port != MR_PORT_COND)
	{
		return FALSE;
	}
	node_path = MR_trace_node_path(node);

	return MR_trace_same_construct(path, node_path);
}

static	bool
MR_trace_matching_neg(const char *path, MR_Trace_Node node)
{
	MR_Trace_Port		port;
	const char		*node_path;

	MR_TRACE_CALL_MERCURY(
		port = (MR_Trace_Port) MR_DD_trace_node_port(node);
	);
	if (port != MR_PORT_NEG_ENTER) {
		return FALSE;
	}
	node_path = MR_trace_node_path(node);

	return MR_trace_same_construct(path, node_path);
}

static	bool
MR_trace_matching_disj(const char *path, MR_Trace_Node node)
{
	MR_Trace_Port		port;
	const char		*node_path;

	MR_TRACE_CALL_MERCURY(
		port = (MR_Trace_Port) MR_DD_trace_node_port(node);
	);
	if (port == MR_PORT_DISJ || port == MR_PORT_SWITCH) {
		node_path = MR_trace_node_path(node);
		return MR_trace_same_construct(path, node_path);
	} else {
		return FALSE;
	}
}

static	bool
MR_trace_same_construct(const char *p1, const char *p2)
{
	/*
	** Checks if the two arguments represent goals in the same
	** construct.  If both strings are identical up to the last
	** component, return TRUE, otherwise return FALSE.
	** If the arguments point to identical strings, return TRUE.
	*/
	while (*p1 == *p2) {
		if (*p1 == '\0' && *p2 == '\0') {
			return TRUE;	/* They are identical. */
		}
		if (*p1 == '\0' || *p2 == '\0') {
			return FALSE;	/* Different number of elements. */
		}

		p1++;
		p2++;
	}

	/*
	** If there is exactly one component left in each string, then
	** the goal paths match, otherwise they don't.
	*/
	return MR_trace_single_component(p1) && MR_trace_single_component(p2);
}

static	bool
MR_trace_single_component(const char *path)
{
	while (*path != ';') {
		if (*path == '\0') {
			return FALSE;
		}
		path++;
	}
	path++;
	return (*path == '\0');
}

static	Word
MR_decl_make_atom(const MR_Stack_Layout_Label *layout, Word *saved_regs)
{
	ConstString		name;
	Word			args;
	Word			atom;

	name = MR_decl_atom_name(layout->MR_sll_entry);
	args = MR_decl_atom_args(layout, saved_regs);

	MR_TRACE_USE_HP(
		MR_trace_atom(atom, name, args);
	);

	return atom;
}

static	ConstString
MR_decl_atom_name(const MR_Stack_Layout_Entry *entry)
{
	ConstString		name;

	if (MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)) {
		if (MR_ENTRY_LAYOUT_COMPILER_GENERATED(entry)) {
			MR_TRACE_USE_HP(
				make_aligned_string(name, "<<internal>>");
			);
		} else {
			name = entry->MR_sle_proc_id.MR_proc_user.MR_user_name;
		}
	} else {
		MR_TRACE_USE_HP(
			make_aligned_string(name, "<<unknown>>");
		);
	}

	return name;
}

static	Word
MR_decl_atom_args(const MR_Stack_Layout_Label *layout, Word *saved_regs)
{
	int				i;
	Word				arglist;
	Word				tail;
	Word				head;
	const MR_Stack_Layout_Vars	*vars;
	int				arg_count;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	Word				arg_type;
	Word				arg_value;

	MR_TRACE_USE_HP(
		arglist = MR_list_empty();
	);

	vars = &layout->MR_sll_var_info;
	if (!MR_has_valid_var_count(vars)) {
		fprintf(MR_mdb_err, "mdb: no info about live variables.\n");
	}

	if (!MR_has_valid_var_info(vars)) {
		/* there are no live variables */

		return arglist;
	}

	arg_count = MR_all_desc_var_count(vars);
	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	type_params = MR_materialize_typeinfos_base(vars, saved_regs, 
			base_sp, base_curfr);

	MR_TRACE_USE_HP(
		for (i = arg_count - 1; i >= 0; i--) {
			MR_get_type_and_value_base(vars, i, saved_regs,
					base_sp, base_curfr, type_params,
					&arg_type, &arg_value);

			tail = arglist;
			tag_incr_hp(head, MR_mktag(0), 2);
			MR_field(MR_mktag(0), head, UNIV_OFFSET_FOR_TYPEINFO) =
					arg_type;
			MR_field(MR_mktag(0), head, UNIV_OFFSET_FOR_DATA) =
					arg_value;
			arglist = MR_list_cons(head, tail);
		}
	);

	return arglist;
}

bool
MR_trace_start_decl_debug(const char *outfile, MR_Trace_Cmd_Info *cmd,
		MR_Event_Info *event_info, MR_Event_Details *event_details,
		Code **jumpaddr)
{
	MR_Stack_Layout_Entry 	*entry;
	int			decl_slot;
	const char		*message;
	FILE			*out;

	entry = event_info->MR_event_sll->MR_sll_entry;
	if (!MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
		return FALSE;
	}

	decl_slot = entry->MR_sle_maybe_decl_debug;
	if (decl_slot < 1) {
		/* No slots are reserved for declarative debugging */
		return FALSE;
	}

	message = MR_trace_retry(event_info, event_details, jumpaddr);
	if (message != NULL) {
		return FALSE;
	}

	if (outfile == (const char *) NULL) {
		/* Normal debugging mode */
		MR_trace_decl_mode = MR_TRACE_DECL_DEBUG;
	} else {
		/* Test mode */
		out = fopen(outfile, "w");
		if (out == NULL) {
			fflush(MR_mdb_out);
			fprintf(MR_mdb_err, "mdb: cannot open file `%s' "
					"for output: %s.\n",
					outfile, strerror(errno));
			return FALSE;
		} else {
			MR_trace_decl_mode = MR_TRACE_DECL_DEBUG_TEST;
			MR_trace_store_file = out;
		}
	}

	MR_edt_last_event = event_info->MR_event_number;
	MR_edt_min_depth = event_info->MR_call_depth;
	MR_edt_max_depth = event_info->MR_call_depth + MR_EDT_DEPTH_STEP_SIZE;
	MR_trace_node_store = 0;
	MR_trace_current_node = (MR_Trace_Node) NULL;

	if (MR_trace_front_end_state == (Word) NULL) {
		MR_TRACE_CALL_MERCURY(
			MR_DD_diagnoser_state_init(&MR_trace_front_end_state);
		);
	}

	cmd->MR_trace_cmd = MR_CMD_GOTO;
	cmd->MR_trace_stop_event = MR_trace_event_number + 1;
	cmd->MR_trace_strict = FALSE;
	cmd->MR_trace_print_level = MR_PRINT_LEVEL_ALL;

	return TRUE;
}

static	void
MR_decl_diagnosis(MR_Trace_Node root)
{
	MercuryFile		mdb_in, mdb_out;
	Word			response;

	mdb_in.file = MR_mdb_in;
	mdb_in.line_number = 1;
	mdb_out.file = MR_mdb_out;
	mdb_out.line_number = 1;

	MR_TRACE_CALL_MERCURY(
		MR_DD_decl_diagnosis((Word) &mdb_in, (Word) &mdb_out,
				MR_trace_node_store, root, &response,
				MR_trace_front_end_state,
				&MR_trace_front_end_state
			);
	);
}

static	void
MR_decl_diagnosis_test(MR_Trace_Node root)
{
	MercuryFile		stream;

	stream.file = MR_trace_store_file;
	stream.line_number = 1;

	MR_TRACE_CALL_MERCURY(
		MR_DD_save_trace((Word) &stream, MR_trace_node_store, root);
	);

	fclose(MR_trace_store_file);
}

static	String
MR_trace_node_path(MR_Trace_Node node)
{
	String			path;

	MR_trace_node_store++;
	MR_TRACE_CALL_MERCURY(
		path = MR_DD_trace_node_path(MR_trace_node_store, (Word) node);
	);
	return path;
}

static	MR_Trace_Node
MR_trace_scan_backwards(MR_Trace_Node node)
{
	MR_Trace_Node		prev;

	MR_trace_node_store++;
	MR_TRACE_CALL_MERCURY(
		prev = (MR_Trace_Node) MR_DD_scan_backwards(
						MR_trace_node_store, node);
	);
	return prev;
}

#endif /* defined(MR_USE_DECLARATIVE_DEBUGGER) */
