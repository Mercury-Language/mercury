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

void
MR_dump_stack(Code *success_pointer, Word *det_stack_pointer,
		Word *current_frame)
{
	Label			*label;
	MR_Live_Lval		location;
	MR_Stack_Layout_Label	*layout;
	MR_Stack_Layout_Entry	*entry_layout;
	MR_Lval_Type		type;
	int			number, determinism;

#ifndef MR_STACK_TRACE
	fprintf(stderr, "Stack dump not available in this grade.\n");
#else
	fprintf(stderr, "Stack dump follows:\n");

	do {
		label = lookup_label_addr(success_pointer);
		if (label == NULL) {
			fatal_error("internal label not found");
		}

		layout = (MR_Stack_Layout_Label *) label->e_layout;
		entry_layout = layout->MR_sll_entry;
		
		label = lookup_label_addr(
			entry_layout->MR_sle_code_addr);
		if (label == NULL) {
			fatal_error("entry label not found");
		}

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
			break;
		}
		if (MR_DETISM_DET_STACK(determinism)) {
			fprintf(stderr, "\t%s\n", label->e_name);
			if (type == MR_LVAL_TYPE_STACKVAR) {
				success_pointer = (Code *) field(0, 
					det_stack_pointer, -number);
			} else {
				fatal_error("can only handle stackvars");
			}
			det_stack_pointer = det_stack_pointer - 
				entry_layout->MR_sle_stack_slots;
		} else {
			fprintf(stderr, "\t%s\n", label->e_name);
			success_pointer = bt_succip(current_frame);
			current_frame = bt_succfr(current_frame);
		}
	} while (TRUE);
#endif /* MR_STACK_TRACE */
}
