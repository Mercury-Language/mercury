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
** related trace events and builds them into an Evaluation Dependency
** Tree (EDT).  Once built, the EDT is passed to the front end where it can
** be analysed to find bugs.  The front end is implemented in
** browse/declarative_debugger.m.
**
** The interface between the front and back ends is via the
** evaluation_tree typeclass, which is documented in
** browse/declarative_debugger.m.  It would be possible to replace
** the front end or the back end with an alternative implementation
** which also conforms to the typeclass constraints.  For example:
** 	- An alternative back end could generate the same EDT
** 	  structure in a different way, such as via program
** 	  transformation.
** 	- An alternative front end could graphically display the
** 	  generated EDTs as part of a visualization tool rather
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
#include "std_util.h"
#include <stdio.h>

/*
** We only build the execution tree to a certain depth.  The following
** macro gives the default depth limit (relative to the starting depth).
*/

#define MR_EDT_DEPTH_STEP_SIZE		8

/*
** The declarative debugger back end is controlled by the
** settings of the following variables.  They are set in
** MR_trace_start_wrong_answer when the back end is started.  They
** are used by MR_trace_decl_wrong_answer to decide what action to
** take for a particular trace event.  Events that are outside
** the given depth range are ignored.  Events that are beyond the
** given last event cause the internal debugger to be switched
** back into interactive mode.
*/

static	Unsigned	MR_edt_min_depth;
static	Unsigned	MR_edt_max_depth;
static	Unsigned	MR_edt_last_event;

/*
** MR_edt_parent points to the the parent edt_node of a procedure
** that is being called.  When a CALL event occurs, this value is
** saved in a dedicated stackvar (or framevar).  It is then set to
** point to the new edt_node for that procedure, ready to be used
** by CALL events at the next depth down.
*/

static	MR_Edt_Node	*MR_edt_parent;

static void
MR_trace_decl_wrong_answer_call(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot);

static void
MR_trace_decl_wrong_answer_exit(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot);

static void
MR_trace_decl_wrong_answer_redo(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot);

static void
MR_trace_decl_wrong_answer_fail(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot);

static void
MR_trace_decl_update_path(MR_Event_Info *event_info, int decl_slot);

static void
MR_trace_decl_save_args(const MR_Stack_Layout_Label *layout, Word *saved_regs,
		MR_Edt_Node *edt_node);

static	void
MR_analyse_edt(MR_Edt_Node *root);

static	MR_Edt_Node *
MR_edt_node_construct(const MR_Stack_Layout_Label *layout,
		MR_Edt_Node_Type node_tag, int start_event);

static	ConstString
MR_edt_root_node_name(const MR_Stack_Layout_Entry *entry);

static	Word
MR_edt_root_node_args(const MR_Edt_Node *edt);

Code *
MR_trace_decl_wrong_answer(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info)
{
	int			decl_slot;
	MR_Stack_Layout_Entry 	*entry;
	MR_Edt_Node		*edt_node;
	Unsigned		depth;

	entry = event_info->MR_event_sll->MR_sll_entry;
	depth = event_info->MR_call_depth;

	if (event_info->MR_event_number > MR_edt_last_event) {
		/* This shouldn't ever be reached. */
		fprintf(MR_mdb_err, "Warning: missed final event.\n");
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
	decl_slot = entry->MR_sle_maybe_decl_debug;

	switch (event_info->MR_trace_port) {
		case MR_PORT_CALL:
			MR_trace_decl_wrong_answer_call(cmd, event_info,
					decl_slot);
			break;
		case MR_PORT_EXIT:
			MR_trace_decl_wrong_answer_exit(cmd, event_info,
					decl_slot);
			break;
		case MR_PORT_REDO:
			MR_trace_decl_wrong_answer_redo(cmd, event_info,
					decl_slot);
			break;
		case MR_PORT_FAIL:
			MR_trace_decl_wrong_answer_fail(cmd, event_info,
					decl_slot);
			break;
		case MR_PORT_THEN:
		case MR_PORT_ELSE:
		case MR_PORT_DISJ:
		case MR_PORT_SWITCH:
			MR_trace_decl_update_path(event_info, decl_slot);
		case MR_PORT_PRAGMA_FIRST:
		case MR_PORT_PRAGMA_LATER:
			break;
		default:
			fatal_error("Unknown port type");
	}
	
	if (MR_trace_event_number == MR_edt_last_event) {
		/* Call the front end */
		MR_analyse_edt(MR_edt_parent->MR_edt_node_children);

		MR_trace_decl_mode = MR_TRACE_INTERACTIVE;
		return MR_trace_event_internal(cmd, TRUE, event_info);
	}

	MR_trace_enabled = TRUE;

	return NULL;
}

static void
MR_trace_decl_wrong_answer_call(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot)
{
	MR_Edt_Node		*edt_node;
	MR_Edt_Node_Type	node_tag;
	MR_Stack_Layout_Entry 	*entry;
	Word			*saved_regs;
	
	entry = event_info->MR_event_sll->MR_sll_entry;
	saved_regs = event_info->MR_saved_regs;

	if (event_info->MR_call_depth < MR_edt_max_depth) {
		node_tag = MR_EDT_WRONG_ANSWER_EXPLICIT;
	} else {
		/*
		** At this point depth == MR_edt_max_depth.
		*/

		node_tag = MR_EDT_WRONG_ANSWER_IMPLICIT;
	}

	edt_node = MR_edt_node_construct(event_info->MR_event_sll, node_tag,
			event_info->MR_event_number);

	if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
		MR_based_stackvar(MR_saved_sp(saved_regs), decl_slot) =
				(Word) edt_node;
		MR_based_stackvar(MR_saved_sp(saved_regs), decl_slot + 1) =
				(Word) MR_edt_parent;
	} else {
		MR_based_framevar(MR_saved_curfr(saved_regs), decl_slot) =
				(Word) edt_node;
		MR_based_framevar(MR_saved_curfr(saved_regs), decl_slot + 1) =
				(Word) MR_edt_parent;
	}
	/*
	** The children of edt_node will refer to the
	** global variable MR_edt_parent to locate their
	** parent.
	*/
	MR_edt_parent = edt_node;
}

static void
MR_trace_decl_wrong_answer_exit(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot)
{
	MR_Edt_Node			*edt_node;
	const MR_Stack_Layout_Label 	*layout;
	Word				*saved_regs;
	
	layout = event_info->MR_event_sll;
	saved_regs = event_info->MR_saved_regs;

	if (MR_DETISM_DET_STACK(layout->MR_sll_entry->MR_sle_detism)) {
		edt_node = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot);
		MR_edt_parent = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot + 1);
	} else {
		edt_node = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot);
		MR_edt_parent = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot + 1);
	}

	edt_node->MR_edt_node_layout = layout;
	edt_node->MR_edt_node_end_event = event_info->MR_event_number;
	edt_node->MR_edt_node_seqno = event_info->MR_call_seqno;

	MR_trace_decl_save_args(layout, saved_regs, edt_node);

	/*
	** Attach this node to the child list of its parent.
	*/
	edt_node->MR_edt_node_sibling = MR_edt_parent->MR_edt_node_children;
	MR_edt_parent->MR_edt_node_children = edt_node;
}

static void
MR_trace_decl_wrong_answer_redo(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot)
{
	MR_Edt_Node		*edt_node;
	MR_Stack_Layout_Entry 	*entry;
	Word			*saved_regs;
	
	entry = event_info->MR_event_sll->MR_sll_entry;
	saved_regs = event_info->MR_saved_regs;

	/*
	** Re-use the node that was allocated at the CALL event.
	*/
	if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
		edt_node = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot);
		MR_edt_parent = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot + 1);
	} else {
		edt_node = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot);
		MR_edt_parent = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot + 1);
	}

	/*
	** Remove the nodes that we have bactracked over.  Since we have
	** a REDO event for this goal, we must have had an EXIT event
	** earlier, therefore the current node is known to be attached
	** to the current parent.
	**
	** XXX need to deallocate properly.
	*/
	if (MR_edt_parent != NULL ) {
		MR_edt_parent->MR_edt_node_children =
				edt_node->MR_edt_node_sibling;
	}

	MR_edt_parent = edt_node;
}

static void
MR_trace_decl_wrong_answer_fail(MR_Trace_Cmd_Info *cmd, 
		MR_Event_Info *event_info, int decl_slot)
{
	MR_Edt_Node		*edt_node;
	MR_Stack_Layout_Entry 	*entry;
	Word			*saved_regs;
	
	entry = event_info->MR_event_sll->MR_sll_entry;
	saved_regs = event_info->MR_saved_regs;

	if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
		edt_node = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot);
		MR_edt_parent = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot + 1);
	} else {
		edt_node = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot);
		MR_edt_parent = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot + 1);
	}
	MR_free(edt_node);
}

/*
** MR_trace_decl_update_path adds to the record of the execution path
** taken for the current EDT parent node.
**
** XXX It currently doesn't do anything useful.  When implemented properly
** it will add a new type of node to the current parent to indicate the
** path taken.
*/
static void
MR_trace_decl_update_path(MR_Event_Info *event_info, int decl_slot)
{
	MR_Edt_Node			*edt_node;
	const MR_Stack_Layout_Label	*layout;
	Word				*saved_regs;

	layout = event_info->MR_event_sll;
	saved_regs = event_info->MR_saved_regs;

	if (MR_DETISM_DET_STACK(layout->MR_sll_entry->MR_sle_detism)) {
		edt_node = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot);
	} else {
		edt_node = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot);
	}
	edt_node->MR_edt_node_path = event_info->MR_event_path;
}

static void
MR_trace_decl_save_args(const MR_Stack_Layout_Label *layout, Word *saved_regs,
		MR_Edt_Node *edt_node)
{
	Word				*arg_values;
	Word				*arg_types;
	int				arg_count;
	const MR_Stack_Layout_Vars	*vars;
	Word				*base_sp;
	Word				*base_curfr;
	Word				*type_params;
	Word				typeinfo_type;
	int				i;

	vars = &layout->MR_sll_var_info;
	if (!MR_has_valid_var_count(vars)) {
		fprintf(MR_mdb_err, "mdb: no info about live variables.\n");
	}

	if (!MR_has_valid_var_info(vars)) {
		/* there are no live variables */
		edt_node->MR_edt_node_arg_values = NULL;
		edt_node->MR_edt_node_arg_types = NULL;
		return;
	}

	arg_count = MR_all_desc_var_count(vars);
	arg_values = MR_NEW_ARRAY(Word, arg_count);
	arg_types = MR_NEW_ARRAY(Word, arg_count);

	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	type_params = MR_materialize_typeinfos_base(vars, saved_regs, 
			base_sp, base_curfr);

	MR_TRACE_CALL_MERCURY(
		ML_get_type_info_for_type_info(&typeinfo_type);
	);

	for (i = 0; i < arg_count; i++) {
		Word	arg_type;

		MR_get_type_and_value_base(vars, i, saved_regs, base_sp,
				base_curfr, type_params, &arg_type,
				&arg_values[i]);

		arg_types[i] = MR_make_permanent(arg_type,
					(Word *) typeinfo_type);

#ifdef MR_DEBUG_DD_BACK_END
		fprintf(MR_mdb_out, "\t");
		fflush(MR_mdb_out);
		MR_trace_print(arg_types[i], arg_values[i]);
#endif
	}

	edt_node->MR_edt_node_arg_values = arg_values;
	edt_node->MR_edt_node_arg_types = arg_types;
}

bool
MR_trace_start_wrong_answer(MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
		MR_Event_Details *event_details, Code **jumpaddr)
{
	MR_Stack_Layout_Entry 	*entry;
	int			decl_slot;
	const char		*message;

	entry = event_info->MR_event_sll->MR_sll_entry;

	if (!MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
		return FALSE;
	}

	decl_slot = entry->MR_sle_maybe_decl_debug;
	if (decl_slot < 1) {
		/* No slots are reserved for declarative debugging */
		return FALSE;
	}

	MR_trace_decl_mode = MR_TRACE_WRONG_ANSWER;
	MR_edt_parent = MR_edt_node_construct(NULL, 
				MR_EDT_WRONG_ANSWER_EXPLICIT, 0);
	MR_edt_last_event = MR_trace_event_number;
	MR_edt_min_depth = event_info->MR_call_depth;
	MR_edt_max_depth = event_info->MR_call_depth + MR_EDT_DEPTH_STEP_SIZE;

	message = MR_trace_retry(event_info, event_details, jumpaddr);

	if (message != NULL) {
		return FALSE;
	}

	cmd->MR_trace_cmd = MR_CMD_GOTO;
	cmd->MR_trace_stop_event = MR_trace_event_number + 1;
	cmd->MR_trace_strict = FALSE;
	cmd->MR_trace_print_level = MR_PRINT_LEVEL_ALL;

	return TRUE;
}

static	MR_Edt_Node *
MR_edt_node_construct(const MR_Stack_Layout_Label *layout,
		MR_Edt_Node_Type node_tag, int start_event)
{
	MR_Edt_Node 	*edt_node;

	edt_node = MR_NEW(MR_Edt_Node);
	edt_node->MR_edt_node_tag = node_tag;
	edt_node->MR_edt_node_layout = layout;
	edt_node->MR_edt_node_path = NULL;
	edt_node->MR_edt_node_start_event = start_event;
	edt_node->MR_edt_node_children = NULL;
	edt_node->MR_edt_node_sibling = NULL;

	return edt_node;
}

static void
MR_analyse_edt(MR_Edt_Node *root)
{
	MercuryFile	mdb_in, mdb_out;

	mdb_in.file = MR_mdb_in;
	mdb_in.line_number = 1;
	mdb_out.file = MR_mdb_out;
	mdb_out.line_number = 1;

	MR_TRACE_CALL_MERCURY(
		ML_DD_analyse_edt((Word) root,
				(Word) &mdb_in,
				(Word) &mdb_out
			);
	);
}

extern void
MR_edt_root_node(Word EDT, Word *Node)
{
	MR_Edt_Node		*edt;
	MR_Stack_Layout_Entry	*entry;
	ConstString		name;
	Word			args;

	edt = (MR_Edt_Node *) EDT;
	entry = edt->MR_edt_node_layout->MR_sll_entry;
	
	switch (edt->MR_edt_node_tag) {
		case MR_EDT_WRONG_ANSWER_EXPLICIT:
		case MR_EDT_WRONG_ANSWER_IMPLICIT:
			name = MR_edt_root_node_name(entry);
			args = MR_edt_root_node_args(edt);
			MR_TRACE_USE_HP(
				incr_hp(*Node, 2);
			);
			MR_field(MR_mktag(0), *Node, 0) = (Word) name;
			MR_field(MR_mktag(0), *Node, 1) = args;
			break;
		default:
			fatal_error("MR_edt_root_node: unknown tag");
	}
}

static ConstString
MR_edt_root_node_name(const MR_Stack_Layout_Entry *entry)
{
	ConstString	name;

	if (MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)) {
		if (MR_ENTRY_LAYOUT_COMPILER_GENERATED(entry)) {
			MR_TRACE_USE_HP(
				make_aligned_string(name, "(internal)");
			);
		} else {
			name = entry->MR_sle_proc_id.MR_proc_user.MR_user_name;
		}
	} else {
		MR_TRACE_USE_HP(
			make_aligned_string(name, "(unknown)");
		);
	}

	return name;
}

static Word
MR_edt_root_node_args(const MR_Edt_Node *edt)
{
	int				i;
	int				argc;
	Word				arglist;
	Word				tail;
	Word				head;
	const MR_Stack_Layout_Vars	*vars;

	vars = &edt->MR_edt_node_layout->MR_sll_var_info;

	if (MR_has_valid_var_info(vars)) {
		argc = MR_all_desc_var_count(vars);
	} else {
		argc = 0;
	}

	MR_TRACE_USE_HP(
		arglist = MR_list_empty();
		for (i = argc - 1; i >= 0; i--) {
			tail = arglist;
			incr_hp(head, 2);
			MR_field(MR_mktag(0), head, UNIV_OFFSET_FOR_TYPEINFO) =
				edt->MR_edt_node_arg_types[i];
			MR_field(MR_mktag(0), head, UNIV_OFFSET_FOR_DATA) =
				edt->MR_edt_node_arg_values[i];
			arglist = MR_list_cons(head, tail);
		}
	);

	return arglist;
}

#endif
