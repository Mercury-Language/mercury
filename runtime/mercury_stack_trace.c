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
static	void	MR_dump_stack_record_frame(FILE *fp, MR_Stack_Layout_Entry *);
static	void	MR_dump_stack_record_flush(FILE *fp);
static	void	MR_dump_stack_record_print(FILE *fp, MR_Stack_Layout_Entry *,
			int);

void
MR_dump_stack(Code *success_pointer, Word *det_stack_pointer,
		Word *current_frame)
{
#ifndef MR_STACK_TRACE
	fprintf(stderr, "Stack dump not available in this grade.\n");
#else
	Label			*label;
	MR_Stack_Layout_Label	*layout;
	MR_Stack_Layout_Entry	*entry_layout;
	char			*result;

	fprintf(stderr, "Stack dump follows:\n");

	label = lookup_label_addr(success_pointer);
	if (label == NULL) {
		fprintf(stderr, "internal label not found\n");
	} else {
		layout = (MR_Stack_Layout_Label *) label->e_layout;
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
MR_dump_stack_from_layout(FILE *fp, MR_Stack_Layout_Entry *entry_layout,
		Word *det_stack_pointer, Word *current_frame)
{
	Label			*label;
	MR_Live_Lval		location;
	MR_Stack_Layout_Label	*layout;
	MR_Lval_Type		type;
	Code			*success_pointer;
	int			number, determinism;

	MR_dump_stack_record_init();

	do {
		location = entry_layout->MR_sle_succip_locn;
		type = MR_LIVE_LVAL_TYPE(location);
		number = MR_LIVE_LVAL_NUMBER(location);

		determinism = entry_layout->MR_sle_detism;

		/*
		** A negative determinism means handwritten code has
		** been reached.  Usually this means we have reached
		** "global_success", so we should stop dumping the stack.
		**
		** Otherwise it means we have reached some handwritten
		** code that has no further information about the stack
		** frame.  In this case, we also stop dumping the stack.
		*/

		if (determinism < 0) {
			MR_dump_stack_record_flush(fp);
			return "reached procedure with no stack trace info";
		}

		MR_dump_stack_record_frame(fp, entry_layout);
		if (MR_DETISM_DET_STACK(determinism)) {
			if (type == MR_LVAL_TYPE_STACKVAR) {
				success_pointer = (Code *) field(0,
					det_stack_pointer, -number);
			} else {
				MR_dump_stack_record_flush(fp);
				return "can only handle stackvars";
			}
			det_stack_pointer = det_stack_pointer -
				entry_layout->MR_sle_stack_slots;
		} else {
			success_pointer = bt_succip(current_frame);
			current_frame = bt_succfr(current_frame);
		}

		if (success_pointer == MR_stack_trace_bottom) {
			MR_dump_stack_record_flush(fp);
			return NULL;
		}

		label = lookup_label_addr(success_pointer);
		if (label == NULL) {
			MR_dump_stack_record_flush(fp);
			return "reached label with no stack trace info";
		}

		layout = (MR_Stack_Layout_Label *) label->e_layout;
		if (layout == NULL) {
			MR_dump_stack_record_flush(fp);
			return "reached label with no stack layout info";
		}

		entry_layout = layout->MR_sll_entry;
	} while (TRUE);

	/*NOTREACHED*/
	return "internal error in MR_dump_stack_from_layout";
}

static MR_Stack_Layout_Entry	*prev_entry_layout;
static int			prev_entry_layout_count;

static void
MR_dump_stack_record_init(void)
{
	prev_entry_layout = NULL;
	prev_entry_layout_count = 0;
}

static void
MR_dump_stack_record_frame(FILE *fp, MR_Stack_Layout_Entry *entry_layout)
{
	if (entry_layout == prev_entry_layout) {
		prev_entry_layout_count++;
	} else {
		MR_dump_stack_record_flush(fp);
		prev_entry_layout = entry_layout;
		prev_entry_layout_count = 1;
	}
}

static void
MR_dump_stack_record_flush(FILE *fp)
{
	if (prev_entry_layout != NULL) {
		MR_dump_stack_record_print(fp, prev_entry_layout,
			prev_entry_layout_count);
	}
}

static void
MR_dump_stack_record_print(FILE *fp, MR_Stack_Layout_Entry *entry_layout,
	int count)
{
	fprintf(fp, "\t%s:%s/%ld (mode %ld, %s)",
		entry_layout->MR_sle_def_module,
		entry_layout->MR_sle_name,
		(long) entry_layout->MR_sle_arity,
		(long) entry_layout->MR_sle_mode,
		detism_names[entry_layout->MR_sle_detism]);

	if (count > 1) {
		fprintf(fp, " * %d\n", count);
	} else {
		putc('\n', fp);
	}
}
