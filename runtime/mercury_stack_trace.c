/*
** Copyright (C) 1998-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_stack_trace.c - implements stack traces.
**
** Main authors: Tyson Dowd (trd), Zoltan Somogyi (zs).
*/

#include "mercury_imp.h"
#include "mercury_stack_trace.h"
#include "mercury_debug.h"
#include <stdio.h>

static	void	MR_dump_stack_record_init(bool include_trace_data,
			bool include_contexts);
static	void	MR_dump_stack_record_frame(FILE *fp,
			const MR_Stack_Layout_Label *label_layout,
			MR_Word *base_sp, MR_Word *base_curfr, 
			MR_Print_Stack_Record print_stack_record);
static	void	MR_dump_stack_record_flush(FILE *fp, 
			MR_Print_Stack_Record print_stack_record);

static	void	MR_print_proc_id_internal(FILE *fp,
			const MR_Stack_Layout_Entry *entry, bool spec);

static	void	MR_maybe_print_context(FILE *fp,
			const char *filename, int lineno);
static	void	MR_maybe_print_parent_context(FILE *fp, bool print_parent,
			bool verbose, const char *filename, int lineno);

/* see comments in mercury_stack_trace.h */
MR_Code	*MR_stack_trace_bottom;
MR_Word	*MR_nondet_stack_trace_bottom;

void
MR_dump_stack(MR_Code *success_pointer, MR_Word *det_stack_pointer,
	MR_Word *current_frame, bool include_trace_data)
{
#ifndef	MR_STACK_TRACE
	fprintf(stderr, "Stack dump not available in this grade.\n");
#else

	const MR_Internal		*label;
	const MR_Stack_Layout_Label	*layout;
	const char			*result;

	fprintf(stderr, "Stack dump follows:\n");

	MR_do_init_modules();
	label = MR_lookup_internal_by_addr(success_pointer);
	if (label == NULL) {
		fprintf(stderr, "internal label not found\n");
	} else {
		layout = label->i_layout;
		result = MR_dump_stack_from_layout(stderr, layout,
			det_stack_pointer, current_frame, include_trace_data,
			TRUE, &MR_dump_stack_record_print);

		if (result != NULL) {
			fprintf(stderr, "%s\n", result);
		}
	}
#endif
}

const char *
MR_dump_stack_from_layout(FILE *fp, const MR_Stack_Layout_Label *label_layout,
	MR_Word *det_stack_pointer, MR_Word *current_frame,
	bool include_trace_data, bool include_contexts,
	MR_Print_Stack_Record print_stack_record)
{
	MR_Stack_Walk_Step_Result	result;
	const MR_Stack_Layout_Entry	*entry_layout;
	const MR_Stack_Layout_Label	*cur_label_layout;
	const MR_Stack_Layout_Label	*prev_label_layout;
	const char			*problem;
	MR_Word				*stack_trace_sp;
	MR_Word				*stack_trace_curfr;
	MR_Word				*old_trace_sp;
	MR_Word				*old_trace_curfr;

	MR_do_init_modules();
	MR_dump_stack_record_init(include_trace_data, include_contexts);

	stack_trace_sp = det_stack_pointer;
	stack_trace_curfr = current_frame;

	cur_label_layout = label_layout;

	do {
		entry_layout = cur_label_layout->MR_sll_entry;
		prev_label_layout = cur_label_layout;

		old_trace_sp    = stack_trace_sp;
		old_trace_curfr = stack_trace_curfr;

		result = MR_stack_walk_step(entry_layout, &cur_label_layout,
				&stack_trace_sp, &stack_trace_curfr, &problem);
		if (result == STEP_ERROR_BEFORE) {
			MR_dump_stack_record_flush(fp, print_stack_record);
			return problem;
		} else if (result == STEP_ERROR_AFTER) {
			MR_dump_stack_record_frame(fp, prev_label_layout,
				old_trace_sp, old_trace_curfr, 
				print_stack_record);

			MR_dump_stack_record_flush(fp, print_stack_record);
			return problem;
		} else {
			MR_dump_stack_record_frame(fp, prev_label_layout,
				old_trace_sp, old_trace_curfr, 
				print_stack_record);
		}
	} while (cur_label_layout != NULL);

	MR_dump_stack_record_flush(fp, print_stack_record);
	return NULL;
}

const MR_Stack_Layout_Label *
MR_find_nth_ancestor(const MR_Stack_Layout_Label *label_layout,
	int ancestor_level, MR_Word **stack_trace_sp, MR_Word **stack_trace_curfr,
	const char **problem)
{
	MR_Stack_Walk_Step_Result	result;
	const MR_Stack_Layout_Label	*return_label_layout;
	int				i;

	if (ancestor_level < 0) {
		*problem = "no such stack frame";
		return NULL;
	}

	MR_do_init_modules();
	*problem = NULL;
	for (i = 0; i < ancestor_level && label_layout != NULL; i++) {
		result = MR_stack_walk_step(label_layout->MR_sll_entry,
				&return_label_layout,
				stack_trace_sp, stack_trace_curfr, problem);

		if (result != STEP_OK) {
			/* *problem has already been filled in */
			return NULL;
		}

		label_layout = return_label_layout;
	}

	if (label_layout == NULL && *problem == NULL) {
		*problem = "not that many ancestors";
	}

	return label_layout;
}

MR_Stack_Walk_Step_Result
MR_stack_walk_step(const MR_Stack_Layout_Entry *entry_layout,
	const MR_Stack_Layout_Label **return_label_layout,
	MR_Word **stack_trace_sp_ptr, MR_Word **stack_trace_curfr_ptr,
	const char **problem_ptr)
{
	MR_Internal		*label;
	MR_Long_Lval		location;
	MR_Long_Lval_Type	type;
	int			number;
	int			determinism;
	MR_Code			*success;

	*return_label_layout = NULL;

	determinism = entry_layout->MR_sle_detism;
	if (determinism < 0) {
		/*
		** This means we have reached some handwritten code that has
		** no further information about the stack frame.
		*/

		*problem_ptr = "reached procedure with no stack trace info";
		return STEP_ERROR_BEFORE;
	}

	if (MR_DETISM_DET_STACK(determinism)) {
		location = entry_layout->MR_sle_succip_locn;
		type = MR_LONG_LVAL_TYPE(location);
		number = MR_LONG_LVAL_NUMBER(location);

		if (type != MR_LONG_LVAL_TYPE_STACKVAR) {
			*problem_ptr = "can only handle stackvars";
			return STEP_ERROR_AFTER;
		}

		success = (MR_Code *) MR_based_stackvar(*stack_trace_sp_ptr,
					number);
		*stack_trace_sp_ptr = *stack_trace_sp_ptr -
			entry_layout->MR_sle_stack_slots;
	} else {
		success = MR_succip_slot(*stack_trace_curfr_ptr);
		*stack_trace_curfr_ptr = MR_succfr_slot(*stack_trace_curfr_ptr);
	}

	if (success == MR_stack_trace_bottom) {
		return STEP_OK;
	}

	label = MR_lookup_internal_by_addr(success);
	if (label == NULL) {
		*problem_ptr = "reached unknown label";
		return STEP_ERROR_AFTER;
	}

	if (label->i_layout == NULL) {
		*problem_ptr = "reached label with no stack layout info";
		return STEP_ERROR_AFTER;
	}

	*return_label_layout = label->i_layout;
	return STEP_OK;
}

void
MR_dump_nondet_stack_from_layout(FILE *fp, MR_Word *base_maxfr)
{
	int	frame_size;

	MR_do_init_modules();

	/*
	** The comparison operator in the condition of the while loop
	** should be >= if you want the trace to include the bottom frame
	** created by mercury_wrapper.c (whose redoip/redofr field can be
	** hijacked by other code), and > if you don't want the bottom
	** frame to be included.
	*/

	while (base_maxfr >= MR_nondet_stack_trace_bottom) {
		frame_size = base_maxfr - MR_prevfr_slot(base_maxfr);
		if (frame_size == MR_NONDET_TEMP_SIZE) {
			MR_print_nondstackptr(fp, base_maxfr);
			fprintf(fp, ": temp\n");
			fprintf(fp, " redoip: ");
			MR_printlabel(fp, MR_redoip_slot(base_maxfr));
			fprintf(fp, " redofr: ");
			MR_print_nondstackptr(fp, MR_redofr_slot(base_maxfr));
			fprintf(fp, " \n");
		} else if (frame_size == MR_DET_TEMP_SIZE) {
			MR_print_nondstackptr(fp, base_maxfr);
			fprintf(fp, ": temp\n");
			fprintf(fp, " redoip: ");
			MR_printlabel(fp, MR_redoip_slot(base_maxfr));
			fprintf(fp, " redofr: ");
			MR_print_nondstackptr(fp, MR_redofr_slot(base_maxfr));
			fprintf(fp, " \n");
			fprintf(fp, " detfr: ");
			MR_print_detstackptr(fp, MR_detfr_slot(base_maxfr));
			fprintf(fp, " \n");
		} else {
			MR_print_nondstackptr(fp, base_maxfr);
			fprintf(fp, ": ordinary, %d words\n",
				frame_size);
			fprintf(fp, " redoip: ");
			MR_printlabel(fp, MR_redoip_slot(base_maxfr));
			fprintf(fp, " redofr: ");
			MR_print_nondstackptr(fp, MR_redofr_slot(base_maxfr));
			fprintf(fp, " \n");
			fprintf(fp, " succip: ");
			MR_printlabel(fp, MR_succip_slot(base_maxfr));
			fprintf(fp, " succfr: ");
			MR_print_nondstackptr(fp, MR_succfr_slot(base_maxfr));
			fprintf(fp, " \n");
		}

		base_maxfr = MR_prevfr_slot(base_maxfr);
	}
}

static	const MR_Stack_Layout_Entry	*prev_entry_layout;
static	int				prev_entry_layout_count;
static	int				prev_entry_start_level;
static	MR_Word				*prev_entry_base_sp;
static	MR_Word				*prev_entry_base_curfr;
static	const char			*prev_entry_filename;
static	int				prev_entry_linenumber;
static	const char			*prev_entry_goal_path;
static	bool				prev_entry_context_mismatch;
static	int				current_level;
static	bool				trace_data_enabled;
static	bool				contexts_enabled;

static void
MR_dump_stack_record_init(bool include_trace_data, bool include_contexts)
{
	prev_entry_layout = NULL;
	prev_entry_layout_count = 0;
	prev_entry_start_level = 0;
	current_level = 0;
	contexts_enabled = include_contexts;
	trace_data_enabled = include_trace_data;
}

static void
MR_dump_stack_record_frame(FILE *fp, const MR_Stack_Layout_Label *label_layout,
	MR_Word *base_sp, MR_Word *base_curfr,
	MR_Print_Stack_Record print_stack_record)
{
	const MR_Stack_Layout_Entry	*entry_layout;
	const char			*filename;
	int				linenumber;
	bool				must_flush;

	entry_layout = label_layout->MR_sll_entry;
	if (! MR_find_context(label_layout, &filename, &linenumber)
		|| ! contexts_enabled)
	{
		filename = "";
		linenumber = 0;
	}

	must_flush = 
		/*
		** We cannot merge two calls if they are to different
		** procedures.
		*/

		(entry_layout != prev_entry_layout) ||

		/*
		** We cannot merge two calls even to the same procedure
		** if we are printing trace data, since this will differ
		** between the calls.
		**
		** Note that it is not possible for two calls to the same
		** procedure to differ on whether the procedure has trace
		** layout data or not.
		*/

		trace_data_enabled;

	if (must_flush) {
		MR_dump_stack_record_flush(fp, print_stack_record);

		prev_entry_layout = entry_layout;
		prev_entry_layout_count = 1;
		prev_entry_start_level = current_level;
		prev_entry_base_sp = base_sp;
		prev_entry_base_curfr = base_curfr;
		prev_entry_filename = filename;
		prev_entry_linenumber = linenumber;
		prev_entry_goal_path = MR_label_goal_path(label_layout);
		prev_entry_context_mismatch = FALSE;
	} else {
		prev_entry_layout_count++;
		if (prev_entry_filename != filename
			|| prev_entry_linenumber != linenumber)
		{
			prev_entry_context_mismatch = TRUE;
		}
	}

	current_level++;
}

static void
MR_dump_stack_record_flush(FILE *fp, MR_Print_Stack_Record print_stack_record)
{
	if (prev_entry_layout != NULL) {
		print_stack_record(fp, prev_entry_layout,
			prev_entry_layout_count, prev_entry_start_level,
			prev_entry_base_sp, prev_entry_base_curfr,
			prev_entry_filename, prev_entry_linenumber,
			prev_entry_goal_path, prev_entry_context_mismatch);
	}
}

void
MR_dump_stack_record_print(FILE *fp, const MR_Stack_Layout_Entry *entry_layout,
	int count, int start_level, MR_Word *base_sp, MR_Word *base_curfr,
	const char *filename, int linenumber, const char *goal_path,
	bool context_mismatch)
{
	fprintf(fp, "%4d ", start_level);

	if (count > 1) {
		fprintf(fp, " %3d* ", count);
	} else if (! trace_data_enabled) {
		fprintf(fp, "%5s ", "");
	} else {
		/*
		** If we are printing trace data, we need all the horizonal
		** room we can get, and there will not be any repeated lines,
		** so we don't reserve space for the repeat counts.
		*/
	}

	MR_maybe_print_call_trace_info(fp, trace_data_enabled, entry_layout,
		base_sp, base_curfr);
	MR_print_proc_id(fp, entry_layout);
	if (strdiff(filename, "") && linenumber > 0) {
		fprintf(fp, " (%s:%d%s)", filename, linenumber,
			context_mismatch ? " and others" : "");
	}

	if (trace_data_enabled) {
		if (strdiff(goal_path, "")) {
			fprintf(fp, " %s", goal_path);
		} else {
			fprintf(fp, " (empty)");
		}
	}

	fprintf(fp, "\n");
}

bool
MR_find_context(const MR_Stack_Layout_Label *label, const char **fileptr,
	int *lineptr)
{
	const MR_Stack_Layout_Entry	*proc;
	const MR_Module_Layout		*module;
	const MR_Module_File_Layout	*file_layout;
	int				i, j;

	proc = label->MR_sll_entry;
	if (! MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(proc)) {
		return FALSE;
	}

	module = proc->MR_sle_module_layout;
	for (i = 0; i < module->MR_ml_filename_count; i++) {
		file_layout = module->MR_ml_module_file_layout[i];
		for (j = 0; j < file_layout->MR_mfl_label_count; j++) {
			if (file_layout->MR_mfl_label_layout[j] == label) {
				*fileptr = file_layout->MR_mfl_filename;
				*lineptr = file_layout->MR_mfl_label_lineno[j];
				return TRUE;
			}
		}
	}

	return FALSE;
}

void
MR_maybe_print_call_trace_info(FILE *fp, bool include_trace_data,
	const MR_Stack_Layout_Entry *entry,
	MR_Word *base_sp, MR_Word *base_curfr)
{
	if (include_trace_data) {
		MR_print_call_trace_info(fp, entry, base_sp, base_curfr);
	}
}

/*
** Note that MR_print_call_trace_info is more permissive than its documentation
** in the header file.
*/

void
MR_print_call_trace_info(FILE *fp, const MR_Stack_Layout_Entry *entry,
	MR_Word *base_sp, MR_Word *base_curfr)
{
	bool	print_details;

	if (base_sp == NULL || base_curfr == NULL) {
		return;
	}

	if (MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
		MR_Integer maybe_from_full =
			entry->MR_sle_maybe_from_full;
		if (maybe_from_full > 0) {
			/*
			** For procedures compiled with shallow
			** tracing, the details will be valid only
			** if the value of MR_from_full saved in
			** the appropriate stack slot was TRUE.
			*/
			if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
				print_details = MR_based_stackvar(
					base_sp, maybe_from_full);
			} else {
				print_details = MR_based_framevar(
					base_curfr, maybe_from_full);
			}
		} else {
			/*
			** For procedures compiled with full tracing,
			** we can always print out the details.
			*/
			print_details = TRUE;
		}
	} else {
		print_details = FALSE;
	}

	if (print_details) {
		if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
			fprintf(fp, "%7lu %7lu %4lu ",
				(unsigned long)
				MR_event_num_stackvar(base_sp) + 1,
				(unsigned long)
				MR_call_num_stackvar(base_sp),
				(unsigned long)
				MR_call_depth_stackvar(base_sp));
		} else {
			fprintf(fp, "%7lu %7lu %4lu ",
				(unsigned long)
				MR_event_num_framevar(base_curfr) + 1,
				(unsigned long)
				MR_call_num_framevar(base_curfr),
				(unsigned long)
				MR_call_depth_framevar(base_curfr));
		}
	} else {
		/* ensure that the remaining columns line up */
		fprintf(fp, "%21s", "");
	}
}

void
MR_print_proc_id(FILE *fp, const MR_Stack_Layout_Entry *entry)
{
	MR_print_proc_id_internal(fp, entry, FALSE);
}

void
MR_print_proc_spec(FILE *fp, const MR_Stack_Layout_Entry *entry)
{
	MR_print_proc_id_internal(fp, entry, TRUE);
}

static void
MR_print_proc_id_internal(FILE *fp, const MR_Stack_Layout_Entry *entry,
	bool spec)
{
	if (! MR_ENTRY_LAYOUT_HAS_PROC_ID(entry)) {
		MR_fatal_error("cannot print procedure id without layout");
	}

	if (MR_ENTRY_LAYOUT_COMPILER_GENERATED(entry)) {
		if (spec) {
			MR_fatal_error("cannot generate specifications "
				"for compiler generated procedures");
		}

		fprintf(fp, "%s for %s:%s/%ld-%ld",
			entry->MR_sle_comp.MR_comp_pred_name,
			entry->MR_sle_comp.MR_comp_type_module,
			entry->MR_sle_comp.MR_comp_type_name,
			(long) entry->MR_sle_comp.MR_comp_arity,
			(long) entry->MR_sle_comp.MR_comp_mode);

		if (strcmp(entry->MR_sle_comp.MR_comp_type_module,
				entry->MR_sle_comp.MR_comp_def_module) != 0)
		{
			fprintf(fp, " {%s}",
				entry->MR_sle_comp.MR_comp_def_module);
		}
	} else {
		if (entry->MR_sle_user.MR_user_pred_or_func == MR_PREDICATE) {
			fprintf(fp, "pred");
		} else if (entry->MR_sle_user.MR_user_pred_or_func ==
				MR_FUNCTION)
		{
			fprintf(fp, "func");
		} else {
			MR_fatal_error("procedure is not pred or func");
		}

		if (spec) {
			fprintf(fp, "*");
		} else {
			fprintf(fp, " ");
		}

		fprintf(fp, "%s:%s/%ld-%ld",
			entry->MR_sle_user.MR_user_decl_module,
			entry->MR_sle_user.MR_user_name,
			(long) entry->MR_sle_user.MR_user_arity,
			(long) entry->MR_sle_user.MR_user_mode);

		if (!spec && strcmp(entry->MR_sle_user.MR_user_decl_module,
				entry->MR_sle_user.MR_user_def_module) != 0)
		{
			fprintf(fp, " {%s}",
				entry->MR_sle_user.MR_user_def_module);
		}
	}

	if (! spec) {
		fprintf(fp, " (%s)", MR_detism_names[entry->MR_sle_detism]);
	}
}

void
MR_print_proc_id_trace_and_context(FILE *fp, bool include_trace_data,
	MR_Context_Position pos, const MR_Stack_Layout_Entry *entry,
	MR_Word *base_sp, MR_Word *base_curfr,
	const char *path, const char *filename, int lineno, bool print_parent, 
	const char *parent_filename, int parent_lineno, int indent)
{

	switch (pos) {
		case MR_CONTEXT_NOWHERE:
			fprintf(fp, " ");
			MR_maybe_print_call_trace_info(fp, include_trace_data,
				entry, base_sp, base_curfr);
			MR_print_proc_id(fp, entry);
			if (strlen(path) > 0) {
				fprintf(fp, " %s", path);
			}
			fprintf(fp, "\n");
			break;

		case MR_CONTEXT_BEFORE:
			MR_maybe_print_context(fp, filename, lineno);
			MR_maybe_print_parent_context(fp, print_parent,
				FALSE, parent_filename, parent_lineno);
			fprintf(fp, " ");
			MR_maybe_print_call_trace_info(fp, include_trace_data,
				entry, base_sp, base_curfr);
			MR_print_proc_id(fp, entry);
			if (strlen(path) > 0) {
				fprintf(fp, " %s", path);
			}
			fprintf(fp, "\n");
			break;

		case MR_CONTEXT_AFTER:
			fprintf(fp, " ");
			MR_maybe_print_call_trace_info(fp, include_trace_data,
				entry, base_sp, base_curfr);
			MR_print_proc_id(fp, entry);
			if (strlen(path) > 0) {
				fprintf(fp, " %s", path);
			}
			MR_maybe_print_context(fp, filename, lineno);
			MR_maybe_print_parent_context(fp, print_parent,
				FALSE, parent_filename, parent_lineno);
			fprintf(fp, "\n");
			break;

		case MR_CONTEXT_PREVLINE:
			MR_maybe_print_context(fp, filename, lineno);
			MR_maybe_print_parent_context(fp, print_parent,
				TRUE, parent_filename, parent_lineno);
			fprintf(fp, "\n%*s ", indent, "");
			MR_maybe_print_call_trace_info(fp, include_trace_data,
				entry, base_sp, base_curfr);
			MR_print_proc_id(fp, entry);
			if (strlen(path) > 0) {
				fprintf(fp, " %s", path);
			}
			fprintf(fp, "\n");
			break;

		case MR_CONTEXT_NEXTLINE:
			fprintf(fp, " ");
			MR_maybe_print_call_trace_info(fp, include_trace_data,
				entry, base_sp, base_curfr);
			MR_print_proc_id(fp, entry);
			if (strlen(path) > 0) {
				fprintf(fp, " %s", path);
			}
			fprintf(fp, "\n%*s", indent, "");
			MR_maybe_print_context(fp, filename, lineno);
			MR_maybe_print_parent_context(fp, print_parent,
				TRUE, parent_filename, parent_lineno);
			fprintf(fp, "\n");
			break;

		default:
			MR_fatal_error("invalid MR_Context_Position");
	}
}

static void
MR_maybe_print_context(FILE *fp, const char *filename, int lineno)
{
	if (strdiff(filename, "") && lineno != 0) {
		fprintf(fp, " %s:%d", filename, lineno);
	}
}

static void
MR_maybe_print_parent_context(FILE *fp, bool print_parent, bool verbose,
	const char *filename, int lineno)
{
	if (print_parent && strdiff(filename, "") && lineno != 0) {
		if (verbose) {
			fprintf(fp, " (from %s:%d)", filename, lineno);
		} else {
			fprintf(fp, " (%s:%d)", filename, lineno);
		}
	}
}

/*
** The different Mercury determinisms are internally represented by integers. 
** This array gives the correspondance with the internal representation and 
** the names that are usually used to denote determinisms.
*/

const char * MR_detism_names[] = {
	"failure",	/* 0 */
	"",		/* 1 */
	"semidet",	/* 2 */
	"nondet",	/* 3 */
	"erroneous",	/* 4 */
	"",		/* 5 */
	"det",		/* 6 */
	"multi",	/* 7 */
	"",		/* 8 */
	"",		/* 9 */
	"cc_nondet",	/* 10 */
	"",		/* 11 */
	"",		/* 12 */
	"",		/* 13 */
	"cc_multi"	/* 14 */
};
