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
MR_dump_stack(Code *success_pointer, Word *det_stack_pointer)
{
	Label *label;
	MR_Live_Lval location;
	MR_stack_layout_label *layout;
	MR_stack_layout_entry *entry_layout;
	MR_Lval_Type type;
	int number, determinism;


#ifndef MR_STACK_TRACE
	fprintf(stderr, "Stack dump not available in this grade.\n");
#else
	fprintf(stderr, "Stack dump follows (determinisitic stack only):\n");

	do {
		label = lookup_label_addr(success_pointer);
		if (label == NULL) {
			fatal_error("internal label not found");
		}

		layout = (MR_stack_layout_label *) label->e_layout;
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

		if (MR_DETISM_DET_CODE_MODEL(determinism)) {
			fprintf(stderr, "\t%s\n", label->e_name);
			if (type == MR_LVAL_TYPE_STACKVAR) {
				success_pointer = (Code *) field(0, 
					det_stack_pointer, -number);
			} else {
				fatal_error("can only handle stackvars");
			}
			det_stack_pointer = det_stack_pointer - 
				entry_layout->MR_sle_stack_slots;
		}
	} while (MR_DETISM_DET_CODE_MODEL(determinism));
#endif /* MR_STACK_TRACE */
}

