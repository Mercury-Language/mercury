/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace.c - implements the tracing subsystem.
**
** For the general basis of trace analysis systems, see the paper
** "Opium: An extendable trace analyser for Prolog" by Mireille Ducasse,
** available from http://www.irisa.fr/lande/ducasse.
*/

#include "mercury_imp.h"
#include "mercury_trace.h"
#include <stdio.h>

int	MR_trace_call_seqno = 0;
int	MR_trace_call_depth = 0;

void
MR_trace(MR_trace_port port, MR_trace_code_model model, int seqno, int depth,
	const char *modulename, const char *predname, int arity, int modenum)
{
	int	i;

	fprintf(stderr, "%4d %2d ", seqno, depth);

	for (i = 0; i < depth; i++)
	{
		putc(' ', stderr);
	}

	switch (port)
	{
		case MR_PORT_CALL:
			fprintf(stderr, "CALL ");
			break;

		case MR_PORT_EXIT:
			fprintf(stderr, "EXIT ");
			break;

		case MR_PORT_FAIL:
			fprintf(stderr, "FAIL ");
			break;

		default:
			fatal_error("MR_trace called with inappropriate port");
	}

	switch (model)
	{
		case MR_MODEL_DET:
			fprintf(stderr, "DET  ");
			break;

		case MR_MODEL_SEMI:
			fprintf(stderr, "SEMI ");
			break;

		case MR_MODEL_NON:
			fprintf(stderr, "NON  ");
			break;
	}

	fprintf(stderr, "%s:%s/%d-%d\n", modulename, predname, arity, modenum);
}

void
MR_trace_path(MR_trace_port port, MR_trace_code_model model,
	int seqno, int depth,
	const char *modulename, const char *predname, int arity, int modenum,
	const char *path)
{
	int	i;

	fprintf(stderr, "%4d %2d ", seqno, depth);

	for (i = 0; i < depth; i++)
	{
		putc(' ', stderr);
	}

	switch (port)
	{
		case MR_PORT_THEN:
			fprintf(stderr, "THEN ");
			break;

		case MR_PORT_ELSE:
			fprintf(stderr, "ELSE ");
			break;

		case MR_PORT_DISJ:
			fprintf(stderr, "DISJ ");
			break;

		case MR_PORT_SWITCH:
			fprintf(stderr, "SWTC ");
			break;

		default:
			fatal_error("MR_trace_path called with inappropriate port");
	}

	switch (model)
	{
		case MR_MODEL_DET:
			fprintf(stderr, "DET  ");
			break;

		case MR_MODEL_SEMI:
			fprintf(stderr, "SEMI ");
			break;

		case MR_MODEL_NON:
			fprintf(stderr, "NON  ");
			break;
	}

	fprintf(stderr, "%s:%s/%d-%d %s\n",
		modulename, predname, arity, modenum, path);
}
