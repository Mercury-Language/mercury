%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: time.m.
% Main author: zs.
% Stability: medium to high.

% This module contains predicates that deal with the CPU time requirements
% of (various parts of) the program.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module time.

:- interface.

% Declaratively, `report_stats' is the same as `true'.
% It has the side-effect of reporting some memory and time usage statistics
% to stdout. (Technically, every Mercury implementation must offer
% a mode of invocation which disables this side-effect.)

:- pred report_stats is det.

:- implementation.

:- pragma(c_header_code, "

#include ""timing.h""

").

:- pragma(c_code, report_stats, "
	int	time_at_prev_stat;

	time_at_prev_stat = time_at_last_stat;
	time_at_last_stat = get_run_time();

	fprintf(stderr, 
		""[Time: +%.3fs, %.3fs, D Stack: %.3fk, ND Stack: %.3fk, "",
		(time_at_last_stat - time_at_prev_stat) / 1000.0,
		(time_at_last_stat - time_at_start) / 1000.0,
		((char *) sp - (char *) detstackmin) / 1024.0,
		((char *) maxfr - (char *) nondstackmin) / 1024.0
	);

#ifdef CONSERVATIVE_GC
	fprintf(stderr, 
		""#GCs: %lu,\\n""
		""Heap used since last GC: %.3fk, Total used: %.3fk]\\n"",
		(unsigned long) GC_gc_no,
		GC_get_bytes_since_gc() / 1024.0,
		GC_get_heap_size() / 1024.0
	);
#else
	fprintf(stderr, 
		""Heap: %.3fk]\\n"",
		((char *) hp - (char *) heapmin) / 1024.0
	);
#endif
").

:- pred benchmark_det(pred(T1, T2), T1, T2, int, int).
:- mode benchmark_det(pred(in, out) is det, in, out, in, out) is det.

:- pred benchmark_nondet(pred(T1, T2), T1, int, int, int).
:- mode benchmark_nondet(pred(in, out) is nondet, in, out, in, out) is det.

:- external(benchmark_det/5).
:- external(benchmark_nondet/5).

:- pragma(c_code, "

/*
INIT mercury_time_init_benchmark
ENDINIT
*/

/*
** :- pred benchmark_nondet(pred(T1, T2), T1, int, int, int).
** :- mode benchmark_nondet(pred(in, out) is nondet, in, out, in, out) is det.
**
** :- pred benchmark_det(pred(T1, T2), T1, int, int, int).
** :- mode benchmark_det(pred(in, out) is det, in, out, in, out) is det.
**
** Polymorphism will add two extra input parameters, type_infos for T1 and T2,
** which we don't use. With both the simple and compact argument passing
** conventions, these will be in r1 and r2, while the closure will be in r3,
** and the input data in r4. The repetition count will be in r6 for simple
** and r5 for compact.
**
** The first output is a count of solutions for benchmark_nondet and the
** actual solution for benchmark_det; the second output for both is the
** time taken in milliseconds. The outputs go into r5 and r7 for the simple
** convention and and r1 and r2 for the compact convention.
*/

#ifdef	COMPACT_ARGS
#define	rep_count	r5
#define	count_output	r1
#define	soln_output	r1
#define	time_output	r2
#else
#define	rep_count	r6
#define	count_output	r5
#define	soln_output	r5
#define	time_output	r7
#endif

Define_extern_entry(mercury__time__benchmark_nondet_5_0);
Declare_label(mercury__time__benchmark_nondet_5_0_i1);
Declare_label(mercury__time__benchmark_nondet_5_0_i2);

BEGIN_MODULE(benchmark_nondet_module)
	init_entry(mercury__time__benchmark_nondet_5_0);
	init_label(mercury__time__benchmark_nondet_5_0_i1);
	init_label(mercury__time__benchmark_nondet_5_0_i2);
BEGIN_CODE

Define_entry(mercury__time__benchmark_nondet_5_0);

	/*
	** Create a nondet stack frame. The contents of the slots:
	**
	** framevar(0): the closure to be called.
	** framevar(1): the input for the closure.
	** framevar(2): the number of iterations left to be done.
	** framevar(3): the number of solutions found so far.
	** framevar(4): the time at entry to the first iteration.
	**
	** We must make that the closure is called at least once,
	** otherwise the count we return isn't valid.
	*/

	mkframe(""benchmark_nondet"", 5,
		LABEL(mercury__time__benchmark_nondet_5_0_i2));

	framevar(0) = r3;
	framevar(1) = r4;

	if (rep_count <= 0)
		framevar(2) = 1;
	else
		framevar(2) = rep_count;

	framevar(3) = 0;
	framevar(4) = get_run_time();

	/* call the higher-order pred closure that we were passed in r3 */
	r1 = r3;
	r2 = (Word) 1;	/* the higher-order call has 1 extra input argument  */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	/* r4 already has the extra input argument */
	{ 
		Declare_entry(do_call_nondet_closure);
		call(ENTRY(do_call_nondet_closure),
			LABEL(mercury__time__benchmark_nondet_5_0_i1),
			LABEL(mercury__time__benchmark_nondet_5_0));
	}

Define_label(mercury__time__benchmark_nondet_5_0_i1);
	/* we found a solution */
	framevar(3) = framevar(3) + 1;
	redo();

Define_label(mercury__time__benchmark_nondet_5_0_i2);
	/* no more solutions for this iteration, so mark it completed */
	framevar(2) = framevar(2) - 1;
	/* are there any other iterations? */
	if (framevar(2) > 0)
	{
		/* yes, so reset the solution counter */
		/* and then set up the call just like last time */
		framevar(3) = 0;
		r1 = framevar(0);
		r2 = (Word) 1;
		r3 = (Word) 1;
		r4 = framevar(1);
		{
			Declare_entry(do_call_nondet_closure);
			call(ENTRY(do_call_nondet_closure),
				LABEL(mercury__time__benchmark_nondet_5_0_i1),
				LABEL(mercury__time__benchmark_nondet_5_0));
		}
	}

	/* no more iterations */
	count_output = framevar(3);
	time_output = get_run_time() - framevar(4);
	succeed_discard();
END_MODULE

Define_extern_entry(mercury__time__benchmark_det_5_0);
Declare_label(mercury__time__benchmark_det_5_0_i1);

BEGIN_MODULE(benchmark_det_module)
	init_entry(mercury__time__benchmark_det_5_0);
	init_label(mercury__time__benchmark_det_5_0_i1);
BEGIN_CODE

Define_entry(mercury__time__benchmark_det_5_0);

	/*
	** Create a det stack frame. The contents of the slots:
	**
	** detstackvar(1): the closure to be called.
	** detstackvar(2): the input for the closure.
	** detstackvar(3): the number of iterations left to be done.
	** detstackvar(4): the time at entry to the first iteration.
	** detstackvar(5): the return address.
	**
	** We must make that the closure is called at least once,
	** otherwise the count we return isn't valid.
	*/

	incr_sp(5);
	detstackvar(5) = (Word) succip;

	detstackvar(1) = r3;
	detstackvar(2) = r4;

	if (rep_count <= 0)
		detstackvar(3) = 1;
	else
		detstackvar(3) = rep_count;

	detstackvar(4) = get_run_time();

	/* call the higher-order pred closure that we were passed in r3 */
	r1 = r3;
	r2 = (Word) 1;	/* the higher-order call has 1 extra input argument  */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	/* r4 already has the extra input argument */
	{ 
		Declare_entry(do_call_det_closure);
		call(ENTRY(do_call_det_closure),
			LABEL(mercury__time__benchmark_det_5_0_i1),
			LABEL(mercury__time__benchmark_det_5_0));
	}

Define_label(mercury__time__benchmark_det_5_0_i1);
	/* mark current iteration completed */
	detstackvar(3) = detstackvar(3) - 1;
	/* are there any other iterations? */
	if (detstackvar(3) > 0)
	{
		/* yes, so set up the call just like last time */
		r1 = detstackvar(1);
		r2 = (Word) 1;
		r3 = (Word) 1;
		r4 = detstackvar(2);
		{ 
			Declare_entry(do_call_det_closure);
			call(ENTRY(do_call_det_closure),
				LABEL(mercury__time__benchmark_det_5_0_i1),
				LABEL(mercury__time__benchmark_det_5_0));
		}
	}

	/* no more iterations */
	soln_output = r1; /* the closure *always* returns its output in r1 */
	time_output = get_run_time() - detstackvar(4);
	succip = (Word *) detstackvar(5);
	decr_sp(5);
	proceed();
END_MODULE

void mercury_time_init_benchmark(void); /* suppress gcc warning */
void mercury_time_init_benchmark(void) {
	benchmark_nondet_module();
	benchmark_det_module();
}

").
