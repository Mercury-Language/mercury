/*
** Copyright (C) 1998-1999 The University of Melbourne.
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


static	void	MR_dump_stack_record_init(void);
static	void	MR_dump_stack_record_frame(FILE *fp,
			const MR_Stack_Layout_Entry *,
			Word *base_sp, Word *base_curfr, 
			void *print_stack_record(
				FILE *, const MR_Stack_Layout_Entry *, 
				int, int, Word *, Word *));
static	void	MR_dump_stack_record_flush(FILE *fp, 
			void *print_stack_record(
				FILE *, const MR_Stack_Layout_Entry *, 
				int, int, Word *, Word *));

void
MR_dump_stack(Code *success_pointer, Word *det_stack_pointer,
	Word *current_frame, bool include_trace_data)
{
#ifndef MR_STACK_TRACE
	fprintf(stderr, "Stack dump not available in this grade.\n");
#else
	const MR_Internal		*label;
	const MR_Stack_Layout_Label	*layout;
	const MR_Stack_Layout_Entry	*entry_layout;
	const char			*result;

	fprintf(stderr, "Stack dump follows:\n");

	do_init_modules();
	label = MR_lookup_internal_by_addr(success_pointer);
	if (label == NULL) {
		fprintf(stderr, "internal label not found\n");
	} else {
		layout = label->i_layout;
		entry_layout = layout->MR_sll_entry;
		result = MR_dump_stack_from_layout(stderr, entry_layout,
			det_stack_pointer, current_frame, include_trace_data);

		if (result != NULL) {
			fprintf(stderr, "%s\n", result);
		}
	}
#endif
}

const char *
MR_dump_stack_from_layout(FILE *fp, const MR_Stack_Layout_Entry *entry_layout,
	Word *det_stack_pointer, Word *current_frame, bool include_trace_data,
	void *print_stack_record(FILE *, const MR_Stack_Layout_Entry *, 
	int, int, Word *, Word *))
{
	MR_Stack_Walk_Step_Result	result;
	const MR_Stack_Layout_Label	*return_label_layout;
	const char			*problem;
	Word				*stack_trace_sp;
	Word				*stack_trace_curfr;
	Word				*old_trace_sp;
	Word				*old_trace_curfr;

	do_init_modules();
	MR_dump_stack_record_init();

	stack_trace_sp = det_stack_pointer;
	stack_trace_curfr = current_frame;

	do {
		old_trace_sp    = stack_trace_sp;
		old_trace_curfr = stack_trace_curfr;

		result = MR_stack_walk_step(entry_layout, &return_label_layout,
				&stack_trace_sp, &stack_trace_curfr, &problem);
		if (result == STEP_ERROR_BEFORE) {
			MR_dump_stack_record_flush(fp, 
				print_stack_record);
			return problem;
		} else if (result == STEP_ERROR_AFTER) {
			if (include_trace_data) {
				MR_dump_stack_record_frame(fp, entry_layout,
					old_trace_sp, old_trace_curfr, 
					print_stack_record);
			} else {
				MR_dump_stack_record_frame(fp, entry_layout,
					NULL, NULL, print_stack_record);
			}

			MR_dump_stack_record_flush(fp, 
				print_stack_record);
			return problem;
		} else {
			if (include_trace_data) {
				MR_dump_stack_record_frame(fp, entry_layout,
					old_trace_sp, old_trace_curfr, 
					print_stack_record);
			} else {
				MR_dump_stack_record_frame(fp, entry_layout,
					NULL, NULL, print_stack_record);
			}
		}

		if (return_label_layout == NULL) {
			break;
		}

		entry_layout = return_label_layout->MR_sll_entry;
	} while (TRUE); 

	MR_dump_stack_record_flush(fp, print_stack_record);
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

	if (ancestor_level < 0) {
		*problem = "no such stack frame";
		return NULL;
	}

	do_init_modules();
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

		success = (Code *) MR_based_stackvar(*stack_trace_sp_ptr,
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
MR_dump_nondet_stack_from_layout(FILE *fp, Word *base_maxfr)
{
	int	frame_size;

	do_init_modules();

	/*
	** Change the >= below to > if you don't want the trace to include
	** the bottom frame created by mercury_wrapper.c (whose redoip/redofr
	** field can be hijacked by other code).
	*/

	while (base_maxfr >= MR_nondet_stack_trace_bottom) {
		frame_size = base_maxfr - MR_prevfr_slot(base_maxfr);
		if (frame_size == MR_NONDET_TEMP_SIZE) {
			fprintf(fp, "%p: nondet temp, %d words\n",
				base_maxfr, frame_size);
			fprintf(fp, " redoip: ");
			printlabel(MR_redoip_slot(base_maxfr));
			fprintf(fp, " redofr: %p\n",
				MR_redofr_slot(base_maxfr));
		} else if (frame_size == MR_DET_TEMP_SIZE) {
			fprintf(fp, "%p: nondet temp, %d words\n",
				base_maxfr, frame_size);
			fprintf(fp, " redoip: ");
			printlabel(MR_redoip_slot(base_maxfr));
			fprintf(fp, " redofr: %p\n",
				MR_redofr_slot(base_maxfr));
			fprintf(fp, " detfr:  %p\n",
				MR_detfr_slot(base_maxfr));
		} else {
			fprintf(fp, "%p: ordinary, %d words\n",
				base_maxfr, frame_size);
			fprintf(fp, " redoip: ");
			printlabel(MR_redoip_slot(base_maxfr));
			fprintf(fp, " redofr: %p\n",
				MR_redofr_slot(base_maxfr));
			fprintf(fp, " succip: ");
			printlabel(MR_succip_slot(base_maxfr));
			fprintf(fp, " succfr: %p\n",
				MR_succfr_slot(base_maxfr));
		}

		base_maxfr = MR_prevfr_slot(base_maxfr);
	}
}

static	const MR_Stack_Layout_Entry	*prev_entry_layout;
static	int				prev_entry_layout_count;
static	int				prev_entry_start_level;
static	Word				*prev_entry_base_sp;
static	Word				*prev_entry_base_curfr;
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
MR_dump_stack_record_frame(FILE *fp, const MR_Stack_Layout_Entry *entry_layout,
	Word *base_sp, Word *base_curfr, void *print_stack_record(
		FILE *, const MR_Stack_Layout_Entry *, int, int, Word *, Word *))
{
	bool	must_flush;

	must_flush = 
		/*
		** We cannot merge two calls if they are to different
		** procedures.
		*/

		(entry_layout != prev_entry_layout) ||

		/*
		** We cannot merge two calls even to the same procedure.
		** if we are printing trace data, since this will differ
		** between the calls.
		**
		** Note that it is not possible for two calls to the same
		** procedure to differ on whether the procedure has trace
		** layout data or not; this is why we don't have to test
		** prev_entry_base_sp and prev_entry_base_curfr.
		*/

		((base_sp != NULL) || (base_curfr != NULL));

	if (must_flush) {
		MR_dump_stack_record_flush(fp, print_stack_record);

		prev_entry_layout = entry_layout;
		prev_entry_layout_count = 1;
		prev_entry_start_level = current_level;
		prev_entry_base_sp = base_sp;
		prev_entry_base_curfr = base_curfr;
	} else {
		prev_entry_layout_count++;
	}

	current_level++;
}

static void
MR_dump_stack_record_flush(FILE *fp, void *print_stack_record(
	FILE *, const MR_Stack_Layout_Entry *, int, int, Word *, Word *))
{
	if (prev_entry_layout != NULL) {
		print_stack_record(fp, prev_entry_layout,
			prev_entry_layout_count, prev_entry_start_level,
			prev_entry_base_sp, prev_entry_base_curfr);
	}
}

