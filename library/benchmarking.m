%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: benchmarking.m.
% Main author: zs.
% Stability: medium to high.

% This module contains predicates that deal with the CPU time requirements
% of (various parts of) the program.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module benchmarking.

:- interface.

% Declaratively, `report_stats' is the same as `true'.
% It has the side-effect of reporting some memory and time usage statistics
% to stdout. (Technically, every Mercury implementation must offer
% a mode of invocation which disables this side-effect.)

:- pred report_stats is det.

% benchmark_det(Pred, In, Out, Repeats, Time) is for benchmarking the
% det predicate Pred. We call Pred with the input In and the output Out,
% and return Out so that the caller can check the correctness of the
% benchmarked predicate. Since most systems do not have good facilities
% for measuring small times, the Repeats parameter allows the caller to
% specify how many times Pred should be called inside the timed interval.
% The number of milliseconds required to execute Pred with input In this
% many times is returned as Time.

:- pred benchmark_det(pred(T1, T2), T1, T2, int, int).
:- mode benchmark_det(pred(in, out) is det, in, out, in, out) is cc_multi.

% benchmark_nondet(Pred, In, Count, Repeats, Time) is for benchmarking
% the nondet predicate Pred. benchmark_nondet is similar to benchmark_det,
% but it returns only a count of the solutions, rather than solutions
% themselves.  The number of milliseconds required to generate
% all solutions of Pred with input In Repeats times is returned as Time.

:- pred benchmark_nondet(pred(T1, T2), T1, int, int, int).
:- mode benchmark_nondet(pred(in, out) is nondet, in, out, in, out)
	is cc_multi.

:- implementation.

:- pragma c_header_code("

#include <stdio.h>
#include ""mercury_timing.h""

").

:- pragma c_code(report_stats, will_not_call_mercury, "
	int	time_at_prev_stat;

	time_at_prev_stat = time_at_last_stat;
	time_at_last_stat = MR_get_user_cpu_miliseconds();

	fprintf(stderr, 
		""[Time: +%.3fs, %.3fs, D Stack: %.3fk, ND Stack: %.3fk, "",
		(time_at_last_stat - time_at_prev_stat) / 1000.0,
		(time_at_last_stat - time_at_start) / 1000.0,
		((char *) sp - (char *) detstack_zone->min) / 1024.0,
		((char *) maxfr - (char *) nondetstack_zone->min) / 1024.0
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
		((char *) hp - (char *) heap_zone->min) / 1024.0
	);
#endif
").

:- external(benchmark_det/5).
:- external(benchmark_nondet/5).

:- pragma c_code("

/*
INIT mercury_benchmarking_init_benchmark
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

Define_extern_entry(mercury__benchmarking__benchmark_nondet_5_0);
Declare_label(mercury__benchmarking__benchmark_nondet_5_0_i1);
Declare_label(mercury__benchmarking__benchmark_nondet_5_0_i2);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__benchmarking__benchmark_nondet_5_0);
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__benchmarking__benchmark_nondet_5_0, 1);
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__benchmarking__benchmark_nondet_5_0, 2);

BEGIN_MODULE(benchmark_nondet_module)
	init_entry(mercury__benchmarking__benchmark_nondet_5_0);
	init_label(mercury__benchmarking__benchmark_nondet_5_0_i1);
	init_label(mercury__benchmarking__benchmark_nondet_5_0_i2);
BEGIN_CODE

Define_entry(mercury__benchmarking__benchmark_nondet_5_0);

	/*
	** Create a nondet stack frame. The contents of the slots:
	**
	** framevar(0): the closure to be called.
	** framevar(1): the input for the closure.
	** framevar(2): the number of iterations left to be done.
	** framevar(3): the number of solutions found so far.
	** framevar(4): the time at entry to the first iteration.
	** framevar(5): the saved heap pointer
	**
	** We must make that the closure is called at least once,
	** otherwise the count we return isn't valid.
	*/

	mkframe(""benchmark_nondet"", 6,
		LABEL(mercury__benchmarking__benchmark_nondet_5_0_i2));

	framevar(0) = r3;
	framevar(1) = r4;

	if (rep_count <= 0)
		framevar(2) = 1;
	else
		framevar(2) = rep_count;

	framevar(3) = 0;
	mark_hp(framevar(5));
	framevar(4) = MR_get_user_cpu_miliseconds();

	/* call the higher-order pred closure that we were passed in r3 */
	r1 = r3;
	r2 = (Word) 1;	/* the higher-order call has 1 extra input argument  */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	/* r4 already has the extra input argument */
	{ 
		Declare_entry(do_call_nondet_closure);
		call(ENTRY(do_call_nondet_closure),
			LABEL(mercury__benchmarking__benchmark_nondet_5_0_i1),
			LABEL(mercury__benchmarking__benchmark_nondet_5_0));
	}

Define_label(mercury__benchmarking__benchmark_nondet_5_0_i1);
	/* we found a solution */
	framevar(3) = framevar(3) + 1;
	redo();

Define_label(mercury__benchmarking__benchmark_nondet_5_0_i2);
	/* no more solutions for this iteration, so mark it completed */
	framevar(2) = framevar(2) - 1;
	/* we can now reclaim memory by resetting the heap pointer */
	restore_hp(framevar(5));
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
				LABEL(mercury__benchmarking__benchmark_nondet_5_0_i1),
				LABEL(mercury__benchmarking__benchmark_nondet_5_0));
		}
	}

	/* no more iterations */
	count_output = framevar(3);
	time_output = MR_get_user_cpu_miliseconds() - framevar(4);
	succeed_discard();
END_MODULE

Define_extern_entry(mercury__benchmarking__benchmark_det_5_0);
Declare_label(mercury__benchmarking__benchmark_det_5_0_i1);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__benchmarking__benchmark_det_5_0);
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__benchmarking__benchmark_det_5_0, 1);

BEGIN_MODULE(benchmark_det_module)
	init_entry(mercury__benchmarking__benchmark_det_5_0);
	init_label(mercury__benchmarking__benchmark_det_5_0_i1);
BEGIN_CODE

Define_entry(mercury__benchmarking__benchmark_det_5_0);

	/*
	** Create a det stack frame. The contents of the slots:
	**
	** detstackvar(1): the closure to be called.
	** detstackvar(2): the input for the closure.
	** detstackvar(3): the number of iterations left to be done.
	** detstackvar(4): the time at entry to the first iteration.
	** detstackvar(5): the saved heap pointer
	** detstackvar(6): the return address.
	**
	** We must make that the closure is called at least once,
	** otherwise the count we return isn't valid.
	*/

	incr_sp(6);
	detstackvar(6) = (Word) succip;
	mark_hp(detstackvar(5));

	detstackvar(1) = r3;
	detstackvar(2) = r4;

	if (rep_count <= 0)
		detstackvar(3) = 1;
	else
		detstackvar(3) = rep_count;

	detstackvar(4) = MR_get_user_cpu_miliseconds();

	/* call the higher-order pred closure that we were passed in r3 */
	r1 = r3;
	r2 = (Word) 1;	/* the higher-order call has 1 extra input argument  */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	/* r4 already has the extra input argument */
	{ 
		Declare_entry(do_call_det_closure);
		call(ENTRY(do_call_det_closure),
			LABEL(mercury__benchmarking__benchmark_det_5_0_i1),
			LABEL(mercury__benchmarking__benchmark_det_5_0));
	}

Define_label(mercury__benchmarking__benchmark_det_5_0_i1);
	/* mark current iteration completed */
	detstackvar(3) = detstackvar(3) - 1;
	/* are there any other iterations? */
	if (detstackvar(3) > 0)
	{
		/* yes, so set up the call just like last time */
		restore_hp(detstackvar(5));
		r1 = detstackvar(1);
		r2 = (Word) 1;
		r3 = (Word) 1;
		r4 = detstackvar(2);
		{ 
			Declare_entry(do_call_det_closure);
			call(ENTRY(do_call_det_closure),
				LABEL(mercury__benchmarking__benchmark_det_5_0_i1),
				LABEL(mercury__benchmarking__benchmark_det_5_0));
		}
	}

	/* no more iterations */
	soln_output = r1; /* the closure *always* returns its output in r1 */
	time_output = MR_get_user_cpu_miliseconds() - detstackvar(4);
	succip = (Word *) detstackvar(6);
	decr_sp(6);
	proceed();
END_MODULE

void mercury_benchmarking_init_benchmark(void); /* suppress gcc warning */
void mercury_benchmarking_init_benchmark(void) {
	benchmark_nondet_module();
	benchmark_det_module();
}

").
