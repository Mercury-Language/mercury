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
** be analysed to find bugs.
**
** In this incarnation, the front end just dumps the EDT to stdout where
** the user can do manual analysis.
*/

#include "mercury_imp.h"

#ifdef MR_USE_DECLARATIVE_DEBUGGER

#include "mercury_dummy.h"
#include "mercury_trace.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_declarative.h"
#include "mercury_layout_util.h"
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

static	int		MR_edt_min_depth;
static	int		MR_edt_max_depth;
static	int		MR_edt_last_event;

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
MR_trace_decl_update_path(const MR_Stack_Layout_Label *layout, 
		Word *saved_regs, const char *path, int decl_slot);

static void
MR_trace_decl_save_args(const MR_Stack_Layout_Label *layout, Word *saved_regs,
		MR_Edt_Node *edt_node);

static 	void
MR_edt_print(MR_Edt_Node *root, int level);

static 	void
MR_edt_print_node(MR_Edt_Node *node, int level);

static	MR_Edt_Node *
MR_edt_node_construct(const MR_Stack_Layout_Label *layout,
		MR_Edt_Node_Type node_tag, int start_event);

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

	if (MR_trace_event_number > MR_edt_last_event) {
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
		** We can just ignore any event with a depth outside the
		** range given by MR_edt_{min,max}_depth, or which
		** does not have slots reserved for declarative
		** debugging.
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
			MR_trace_decl_update_path(event_info->MR_event_sll,
					event_info->MR_saved_regs, 
					event_info->MR_event_path, decl_slot);
		case MR_PORT_PRAGMA_FIRST:
		case MR_PORT_PRAGMA_LATER:
			break;
		default:
			fatal_error("Unknown port type");
	}
	
	if (MR_trace_event_number == MR_edt_last_event) {
		/* Call the front end */
		MR_edt_print(MR_edt_parent->MR_edt_node_children, 0);

		MR_trace_decl_mode = MR_TRACE_INTERACTIVE;
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
			MR_trace_event_number);

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
	edt_node->MR_edt_node_end_event = MR_trace_event_number;
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
	deallocate_memory(edt_node);
}

/*
** MR_trace_decl_update_path adds to the record of the execution path
** taken for the current edt_node.
**
** XXX It currently just overwrites all but the last path seen.  If
** the goal is a conjunction of two disjunctions, only the path 
** through the second disjunction is remembered.
*/
static void
MR_trace_decl_update_path(const MR_Stack_Layout_Label *layout, 
		Word *saved_regs, const char *path, int decl_slot)
{
	MR_Edt_Node	*edt_node;

	if (MR_DETISM_DET_STACK(layout->MR_sll_entry->MR_sle_detism)) {
		edt_node = (MR_Edt_Node *) MR_based_stackvar(
				MR_saved_sp(saved_regs), decl_slot);
	} else {
		edt_node = (MR_Edt_Node *) MR_based_framevar(
				MR_saved_curfr(saved_regs), decl_slot);
	}
	edt_node->MR_edt_node_path = path;
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
	const MR_Stack_Layout_Var	*var;
	MR_Live_Lval			locn;
	int				i;
	bool				succeeded;
	Word				*pseudo_type_info;
	Word				type_info;

	arg_count = layout->MR_sll_var_count;
	if (arg_count < 0) {
		printf("mdb: no info about live variables.\n");
		edt_node->MR_edt_node_arg_values = NULL;
		edt_node->MR_edt_node_arg_types = NULL;
		return;
	}
	arg_values = allocate_array(Word, arg_count);
	arg_types = allocate_array(Word, arg_count);
	vars = &layout->MR_sll_var_info;
	base_sp = MR_saved_sp(saved_regs);
	base_curfr = MR_saved_curfr(saved_regs);
	type_params = MR_materialize_typeinfos_base(vars, saved_regs, 
			base_sp, base_curfr);
	for (i = 0; i < arg_count; i++) {
		var = &vars->MR_slvs_pairs[i];
		MR_get_type_and_value_base(var, saved_regs, base_sp,
				base_curfr, type_params, &arg_types[i],
				&arg_values[i]);
		locn = var->MR_slv_locn;

#ifdef 0
		printf("var %d: lval type = %d, "
				"lval number = %d, value = ",
				i,
				MR_LIVE_LVAL_TYPE(locn),
				MR_LIVE_LVAL_NUMBER(locn)
		);
		MR_write_variable(arg_types[i], arg_values[i]);
		printf("\n");
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

	MR_trace_retry(event_info, event_details, jumpaddr);

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

	edt_node = allocate_object(MR_Edt_Node);
	edt_node->MR_edt_node_tag = node_tag;
	edt_node->MR_edt_node_layout = layout;
	edt_node->MR_edt_node_path = NULL;
	edt_node->MR_edt_node_start_event = start_event;
	edt_node->MR_edt_node_children = NULL;
	edt_node->MR_edt_node_sibling = NULL;

	return edt_node;
}

/*
** The following functions do a fairly unpretty print of the EDT.  They
** currently form the front end of the declarative debugger.  In the
** future the front end will be in a separate module to the back, and
** will be much more detailed.  These functions will probably disappear.
*/

static void
MR_edt_print(MR_Edt_Node *root, int level)
{
	int i;

	if (root->MR_edt_node_sibling != NULL) {
		MR_edt_print(root->MR_edt_node_sibling, level);
	}

	MR_edt_print_node(root, level);

	if (root->MR_edt_node_tag == MR_EDT_WRONG_ANSWER_IMPLICIT) {
		for (i = 0; i < level + 1; i++) {
			printf("    ");
		}
		printf("/* implicit */\n");
	}

	if (root->MR_edt_node_children != NULL) {
		MR_edt_print(root->MR_edt_node_children, level + 1);
	}
}

static void
MR_edt_print_node(MR_Edt_Node *node, int level)
{
	int i;

	for (i = 0; i < level; i++) {
		printf("    ");
	}
	printf("(");
	MR_write_variable(node->MR_edt_node_arg_types[0],
			node->MR_edt_node_arg_values[0]);
	for (i = 1; i < node->MR_edt_node_layout->MR_sll_var_count; i++) {
		printf(", ");
		MR_write_variable(node->MR_edt_node_arg_types[i],
				node->MR_edt_node_arg_values[i]);
	}
	printf(") ");
	if (node->MR_edt_node_path != NULL) {
		printf("%s ", node->MR_edt_node_path);
	}
	MR_print_proc_id_for_debugger(stdout, 
			node->MR_edt_node_layout->MR_sll_entry);
}

#endif	/* MR_USE_DECLARATIVE_DEBUGGER */
