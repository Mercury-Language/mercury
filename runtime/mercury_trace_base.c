/*
INIT mercury_sys_init_trace
ENDINIT
*/
/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_base.c implements the interface between the main part
** of the runtime system (mainly mercury_wrapper.c) and the part of the
** tracing subsystem that has to be present even if no module in the program
** is compiled with execution tracing.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace_base.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include "mercury_misc.h"
#include <stdio.h>
#include <unistd.h>		/* for the write system call */

/*
** Do we want to use the debugger within this process, or do want to use
** the Opium-style trace analyzer debugger implemented by an external process.
** This variable is set in mercury_wrapper.c and never modified afterwards.
*/

MR_Trace_Type	MR_trace_handler = MR_TRACE_INTERNAL;

/*
** Compiler generated tracing code will check whether MR_trace_enabled is true,
** before calling MR_trace. For now, and until we implement interface tracing,
** MR_trace_enabled should keep the same value throughout the execution of
** the entire program after being set in mercury_wrapper.c. There is one
** exception to this: the Mercury routines called as part of the functionality
** of the tracer itself (e.g. the term browser) should always be executed
** with MR_trace_enabled set to FALSE.
*/

bool		MR_trace_enabled = FALSE;

/*
** MR_trace_call_seqno counts distinct calls. The prologue of every
** procedure assigns the current value of this counter as the sequence number
** of that invocation and increments the counter. This is the only way that
** MR_trace_call_seqno is modified.
**
** MR_trace_call_depth records the current depth of the call tree. The prologue
** of every procedure assigns the current value of this variable plus one
** as the depth of that invocation. Just before making a call, the caller
** will set MR_trace_call_depth to its own remembered depth value. 
** These are the only ways in which MR_trace_call_depth is modified.
**
** Although neither MR_trace_call_seqno nor MR_trace_call_depth are used
** directly in this module, the seqno and depth arguments of MR_trace
** always derive their values from the saved values of these two global
** variables.
*/

Unsigned	MR_trace_call_seqno = 0;
Unsigned	MR_trace_call_depth = 0;

/*
** MR_trace_event_number is a simple counter of events. This is used in
** two places: here, for display to the user and for skipping a given number
** of events, and when printing an abort message, so that the programmer
** can zero in on the source of the problem more quickly.
*/

Unsigned	MR_trace_event_number = 0;

/*
** MR_trace_from_full is a boolean that is set before every call;
** it states whether the caller is being fully traced, or only interface
** traced. If the called code is interface traced, it will generate
** call, exit and fail trace events only if MR_trace_from_full is true.
** (It will never generate internal events.) If the called code is fully
** traced, it will always generate all trace events, external and internal,
** regardless of the setting of this variable on entry.
**
** The initial value is set to TRUE to allow the programmer to gain
** control in the debugger when main/2 is called.
*/

Bool		MR_trace_from_full = 1;

#ifdef	MR_TRACE_HISTOGRAM

int		*MR_trace_histogram_all = NULL;
int		*MR_trace_histogram_exp = NULL;
int		MR_trace_histogram_max  = 0;
int		MR_trace_histogram_hwm  = 0;

#define	MR_TRACE_HISTOGRAM_FILENAME	".mercury_histogram"

#endif

Code *
MR_trace(const MR_Stack_Layout_Label *layout, MR_Trace_Port port,
	const char * path, int max_mr_num)
{
	bool		maybe_from_full;
	Unsigned	seqno;
	Unsigned	depth;

	if (! MR_trace_enabled) {
		return NULL;
	}

	/* in case MR_sp or MR_curfr is transient */
	restore_transient_registers();

	maybe_from_full = layout->MR_sll_entry->MR_sle_maybe_from_full;
	if (MR_DETISM_DET_STACK(layout->MR_sll_entry->MR_sle_detism)) {
		if (maybe_from_full > 0 && ! MR_stackvar(maybe_from_full)) {
			return NULL;
		}

		seqno = (Unsigned) MR_call_num_stackvar(MR_sp);
		depth = (Unsigned) MR_call_depth_stackvar(MR_sp);
	} else {
		if (maybe_from_full > 0 && ! MR_framevar(maybe_from_full)) {
			return NULL;
		}

		seqno = (Unsigned) MR_call_num_framevar(MR_curfr);
		depth = (Unsigned) MR_call_depth_framevar(MR_curfr);
	}

	return (*MR_trace_func_ptr)(layout, port, seqno, depth,
			path, max_mr_num);
}

Code *
MR_trace_fake(const MR_Stack_Layout_Label *layout, MR_Trace_Port port,
	Word seqno, Word depth, const char * path, int max_mr_num)
{
	fatal_error("This executable is not set up for debugging.\n"
		"Rebuild the <main>_init.c file, "
		"and give the -t flag to c2init when you do so.\n"
		"If you are using mmake, you can do this by including "
		"-t in C2INIT_FLAGS.\n"
		"For further details, please see the Debugging chapter"
		"of the Mercury User's Guide.\n");
	/*NOTREACHED*/
	return NULL;
}

void
MR_trace_init(void)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL)
		MR_trace_init_external(); /* should be in this module */
#endif
}

void
MR_trace_final(void)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL)
		MR_trace_final_external(); /* should be in this module */
#endif
}

void
MR_trace_start(bool enabled)
{
	MR_trace_event_number = 0;
	MR_trace_call_seqno = 0;
	MR_trace_call_depth = 0;
	MR_trace_from_full = TRUE;
	MR_trace_enabled = enabled;
}

void
MR_trace_end(void)
{
	MR_trace_enabled = FALSE;
}

void
MR_trace_report(FILE *fp)
{
	if (MR_trace_event_number > 0) {
		/*
		** This means that the executable was compiled with tracing,
		** which implies that the user wants trace info on abort.
		*/

		fprintf(fp, "Last trace event was event #%ld.\n",
			(long) MR_trace_event_number);

#ifdef	MR_TRACE_HISTOGRAM
		{
			FILE	*hfp;

			hfp = fopen(MR_TRACE_HISTOGRAM_FILENAME, "w");
			if (hfp != NULL) {
				MR_trace_print_histogram(hfp, "All-inclusive",
					MR_trace_histogram_all,
					MR_trace_histogram_hwm);
				if (fclose(hfp) == 0) {
					fprintf(fp, "Event histogram put into "
						"file `%s'.\n",
						MR_TRACE_HISTOGRAM_FILENAME);
				} else {
					fprintf(fp, "Cannot put event "
						"histogram into `%s': %s."
						MR_TRACE_HISTOGRAM_FILENAME,
						strerror(errno));
				}
			} else {
				fprintf(fp, "Cannot open `%s': %s.\n"
					MR_TRACE_HISTOGRAM_FILENAME,
					strerror(errno));
			}
		}
#endif	/* MR_TRACE_HISTOGRAM */
	}
}

void
MR_trace_report_raw(int fd)
{
	char	buf[80];	/* that ought to be more than long enough */

	if (MR_trace_event_number > 0) {
		/*
		** This means that the executable was compiled with tracing,
		** which implies that the user wants trace info on abort.
		*/

		sprintf(buf, "Last trace event was event #%ld.\n",
			(long) MR_trace_event_number);
		write(fd, buf, strlen(buf));
	}
}

#ifdef	MR_TRACE_HISTOGRAM

void
MR_trace_print_histogram(FILE *fp, const char *which, int *histogram, int max)
{
	int	i;

	fprintf(fp, "%s histogram\n", which);
	for (i = 1; i <= max; i++) {
		fprintf(fp, "depth %4d: %10d", i, histogram[i]);
		if (i + 1 <= max && histogram[i] != 0) {
			fprintf(fp, ", branching factor %7.2f\n",
				(float) histogram[i+1] / (float) histogram[i]);
		} else {
			fprintf(fp, "\n");
		}
	}
}

#endif	/* MR_TRACE_HISTOGRAM */

Define_extern_entry(MR_do_trace_redo_fail);

BEGIN_MODULE(MR_trace_labels_module)
	init_entry_ai(MR_do_trace_redo_fail);
BEGIN_CODE

Define_entry(MR_do_trace_redo_fail);
#if 0
	/* For use in case this ever needs to be debugged again. */
	printf("MR_curfr = %p\n", MR_curfr);
	printf("MR_redofr_slot(MR_curfr) = %p\n", MR_redofr_slot(MR_curfr));
	printf("&MR_redo_layout_framevar(MR_redofr_slot(MR_curfr) = %p\n",
		&MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
	printf("MR_redo_layout_framevar(MR_redofr_slot(MR_curfr) = %p\n",
		MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)));
#endif
	MR_trace((const MR_Stack_Layout_Label *)
		MR_redo_layout_framevar(MR_redofr_slot(MR_curfr)),
		MR_PORT_REDO, "", 0);
	fail();

END_MODULE

void mercury_sys_init_trace(void); /* suppress gcc warning */
void mercury_sys_init_trace(void) {
	MR_trace_labels_module();
}
