/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_stack_trace.c - implements stack traces.
**
** Main author: Tyson Dowd (trd).
*/

#include "mercury_imp.h"
#include "mercury_stack_trace.h"
#include <stdio.h>


static const char * detism_names[] = {
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

static	void	MR_dump_stack_record_init(void);
static	void	MR_dump_stack_record_frame(FILE *fp,
			const MR_Stack_Layout_Entry *);
static	void	MR_dump_stack_record_flush(FILE *fp);
static	void	MR_dump_stack_record_print(FILE *fp,
			const MR_Stack_Layout_Entry *, int, int);

void
MR_dump_stack(Code *success_pointer, Word *det_stack_pointer,
		Word *current_frame)
{
#ifndef MR_STACK_TRACE
	fprintf(stderr, "Stack dump not available in this grade.\n");
#else
	const MR_Internal		*label;
	const MR_Stack_Layout_Label	*layout;
	const MR_Stack_Layout_Entry	*entry_layout;
	const char			*result;

	fprintf(stderr, "Stack dump follows:\n");

	label = MR_lookup_internal_by_addr(success_pointer);
	if (label == NULL) {
		fprintf(stderr, "internal label not found\n");
	} else {
		layout = label->i_layout;
		entry_layout = layout->MR_sll_entry;
		result = MR_dump_stack_from_layout(stderr, entry_layout,
			det_stack_pointer, current_frame);

		if (result != NULL) {
			fprintf(stderr, "%s\n", result);
		}
	}
#endif
}

const char *
MR_dump_stack_from_layout(FILE *fp, const MR_Stack_Layout_Entry *entry_layout,
	Word *det_stack_pointer, Word *current_frame)
{
	MR_Stack_Walk_Step_Result	result;
	const MR_Stack_Layout_Label	*return_label_layout;
	const char			*problem;
	Word				*stack_trace_sp;
	Word				*stack_trace_curfr;

	MR_dump_stack_record_init();

	stack_trace_sp = det_stack_pointer;
	stack_trace_curfr = current_frame;

	do {
		result = MR_stack_walk_step(entry_layout, &return_label_layout,
				&stack_trace_sp, &stack_trace_curfr, &problem);
		if (result == STEP_ERROR_BEFORE) {
			MR_dump_stack_record_flush(fp);
			return problem;
		} else if (result == STEP_ERROR_AFTER) {
			MR_dump_stack_record_frame(fp, entry_layout);
			MR_dump_stack_record_flush(fp);
			return problem;
		} else {
			MR_dump_stack_record_frame(fp, entry_layout);
		}

		if (return_label_layout == NULL) {
			break;
		}

		entry_layout = return_label_layout->MR_sll_entry;
	} while (TRUE); 

	MR_dump_stack_record_flush(fp);
	return NULL;
}

const MR_Stack_Layout_Label *
MR_find_nth_ancestor(const MR_Stack_Layout_Label *label_layout,
	int ancestor_level, Word **stack_trace_sp, Word **stack_trace_curfr,
	const char **problem)
{
	MR_Stack_Walk_Step_Result	result;
	const MR_Stack_Layout_Label	*return_label_layout;
	int				i;

	*problem = NULL;
	for (i = 0; i < ancestor_level && label_layout != NULL; i++) {
		(void) MR_stack_walk_step(label_layout->MR_sll_entry,
				&return_label_layout,
				stack_trace_sp, stack_trace_curfr, problem);
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
	Word **stack_trace_sp_ptr, Word **stack_trace_curfr_ptr,
	const char **problem_ptr)
{
	MR_Internal		*label;
	MR_Live_Lval		location;
	MR_Lval_Type		type;
	int			number;
	int			determinism;
	Code			*success;

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
		type = MR_LIVE_LVAL_TYPE(location);
		number = MR_LIVE_LVAL_NUMBER(location);

		if (type != MR_LVAL_TYPE_STACKVAR) {
			*problem_ptr = "can only handle stackvars";
			return STEP_ERROR_AFTER;
		}

		success = (Code *) field(0, *stack_trace_sp_ptr, -number);
		*stack_trace_sp_ptr = *stack_trace_sp_ptr -
			entry_layout->MR_sle_stack_slots;
	} else {
		success = bt_succip(*stack_trace_curfr_ptr);
		*stack_trace_curfr_ptr = bt_succfr(*stack_trace_curfr_ptr);
	}

	if (success == MR_stack_trace_bottom) {
		return STEP_OK;
	}

	label = MR_lookup_internal_by_addr(success);
	if (label == NULL) {
		*problem_ptr = "reached label with no stack trace info";
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
MR_dump_nondet_stack_from_layout(FILE *fp, Word *base_maxfr)
{
	/*
	** Change the >= below to > if you don't want the trace to include
	** the bottom frame created by mercury_wrapper.c (whose redoip/redofr
	** field can be hijacked by other code).
	*/

	while (base_maxfr >= MR_nondet_stack_trace_bottom) {
		if ((base_maxfr - bt_prevfr(base_maxfr)) < NONDET_FIXED_SIZE) {
			fprintf(fp, "%p: temp\n", base_maxfr);
			fprintf(fp, " redoip: ");
			printlabel(bt_redoip(base_maxfr));
			fprintf(fp, " redofr: %p\n", bt_redofr(base_maxfr));
		} else {
			fprintf(fp, "%p: ordinary\n", base_maxfr);
			fprintf(fp, " redoip: ");
			printlabel(bt_redoip(base_maxfr));
			fprintf(fp, " redofr: %p\n", bt_redofr(base_maxfr));
			fprintf(fp, " succip: ");
			printlabel(bt_succip(base_maxfr));
			fprintf(fp, " succfr: %p\n", bt_succfr(base_maxfr));
		}

		base_maxfr = bt_prevfr(base_maxfr);
	}
}

static	const MR_Stack_Layout_Entry	*prev_entry_layout;
static	int				prev_entry_layout_count;
static	int				prev_entry_start_level;
static	int				current_level;

static void
MR_dump_stack_record_init(void)
{
	prev_entry_layout = NULL;
	prev_entry_layout_count = 0;
	prev_entry_start_level = 0;
	current_level = 0;
}

static void
MR_dump_stack_record_frame(FILE *fp, const MR_Stack_Layout_Entry *entry_layout)
{
	if (entry_layout == prev_entry_layout) {
		prev_entry_layout_count++;
	} else {
		MR_dump_stack_record_flush(fp);
		prev_entry_layout = entry_layout;
		prev_entry_layout_count = 1;
		prev_entry_start_level = current_level;
	}

	current_level++;
}

static void
MR_dump_stack_record_flush(FILE *fp)
{
	if (prev_entry_layout != NULL) {
		MR_dump_stack_record_print(fp, prev_entry_layout,
			prev_entry_layout_count, prev_entry_start_level);
	}
}

static void
MR_dump_stack_record_print(FILE *fp, const MR_Stack_Layout_Entry *entry_layout,
	int count, int start_level)
{
	fprintf(fp, "%9d ", start_level);

	if (count > 1) {
		fprintf(fp, " %3d*", count);
	} else {
		fprintf(fp, "%5s", "");
	}

	fprintf(fp, " %s:%s/%ld-%ld (%s)\n",
		entry_layout->MR_sle_def_module,
		entry_layout->MR_sle_name,
		(long) entry_layout->MR_sle_arity,
		(long) entry_layout->MR_sle_mode,
		detism_names[entry_layout->MR_sle_detism]);
}
