%---------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
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

% `report_stats' is a non-logical procedure intended for use in profiling
% the performance of a program.
% It has the side-effect of reporting some memory and time usage statistics
% about the time period since the last call to report_stats to stderr.

:- impure pred report_stats is det.

% `report_full_memory_stats' is a non-logical procedure intended for use
% in profiling the memory usage of a program.  It has the side-effect of
% reporting a full memory profile to stderr.

:- impure pred report_full_memory_stats is det.

% benchmark_det(Pred, In, Out, Repeats, Time) is for benchmarking the
% det predicate Pred. We call Pred with the input In and the output Out,
% and return Out so that the caller can check the correctness of the
% benchmarked predicate. Since most systems do not have good facilities
% for measuring small times, the Repeats parameter allows the caller to
% specify how many times Pred should be called inside the timed interval.
% The number of milliseconds required to execute Pred with input In this
% many times is returned as Time.
%
% benchmark_func(Func, In, Out, Repeats, Time) does for functions exactly
% what benchmark_det does for predicates.

:- pred benchmark_det(pred(T1, T2), T1, T2, int, int).
:- mode benchmark_det(pred(in, out) is det, in, out, in, out) is cc_multi.
:- mode benchmark_det(pred(in, out) is cc_multi, in, out, in, out) is cc_multi.

:- pred benchmark_func(func(T1) = T2, T1, T2, int, int).
:- mode benchmark_func(func(in) = out is det, in, out, in, out) is cc_multi.

% benchmark_nondet(Pred, In, Count, Repeats, Time) is for benchmarking
% the nondet predicate Pred. benchmark_nondet is similar to benchmark_det,
% but it returns only a count of the solutions, rather than solutions
% themselves.  The number of milliseconds required to generate
% all solutions of Pred with input In Repeats times is returned as Time.

:- pred benchmark_nondet(pred(T1, T2), T1, int, int, int).
:- mode benchmark_nondet(pred(in, out) is nondet, in, out, in, out)
	is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, std_util.

:- pragma foreign_decl("C", "

#include ""mercury_timing.h""
#include ""mercury_heap.h""

extern void ML_report_stats(void);

extern void ML_report_full_memory_stats(void);

"). % end pragma foreign_decl

:- pragma foreign_proc("C", report_stats,
	[will_not_call_mercury, promise_pure],
"
	ML_report_stats();
").

:- pragma foreign_proc("C", report_full_memory_stats,
	[will_not_call_mercury, promise_pure],
"
#ifdef	MR_MPROF_PROFILE_MEMORY
	ML_report_full_memory_stats();
#endif
").

:- pragma foreign_proc("MC++", report_stats,
	[will_not_call_mercury, promise_pure],
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- pragma foreign_proc("MC++", report_full_memory_stats,
	[will_not_call_mercury, promise_pure],
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "

#include <stdio.h>
#include <stdlib.h>
#include ""mercury_prof_mem.h""
#include ""mercury_heap_profile.h""
#include ""mercury_wrapper.h""		/* for MR_time_at_last_stat */

#ifdef MR_MPROF_PROFILE_MEMORY

  #define MEMORY_PROFILE_SIZE	10	/* Profile the top 10 entries */

  #define MAX_REPORT_LINES	10	/* Display the top 10 entries */

  /* local types */

  typedef struct ML_memprof_float_counter
  {
	double		cells_at_period_end;
	double		words_at_period_end;
	double		cells_since_period_start;
	double		words_since_period_start;
  } ML_memprof_float_counter;

  typedef struct	ML_memprof_report_entry
  {
	const char			*name;
	ML_memprof_float_counter	counter;
  } ML_memprof_report_entry;

  /* static variables */

  static ML_memprof_float_counter	ML_overall_counter;

  /* local function declarations */

  static void ML_update_counter(MR_memprof_counter *counter,
				ML_memprof_float_counter *float_counter);

  static int  ML_insert_into_table(const ML_memprof_report_entry *new_entry,
				ML_memprof_report_entry *table,
				int table_size, int next_slot);

  static int  ML_memory_profile_top_table(MR_memprof_record *node,
				ML_memprof_report_entry *table,
				int size, int next_slot);

  static int  ML_memory_profile_fill_table(MR_memprof_record *node,
				ML_memprof_report_entry *table, int next_slot);

  static void ML_memory_profile_report(const ML_memprof_report_entry *,
				int num_entries, MR_bool complete);

  static int  ML_memory_profile_compare_final(const void *, const void *);

#endif /* MR_MPROF_PROFILE_MEMORY */

void
ML_report_stats(void)
{
	int			time_at_prev_stat;
#if !defined(MR_HIGHLEVEL_CODE) || !defined(MR_CONSERVATIVE_GC)
	MercuryEngine		*eng;
#endif
#ifdef MR_MPROF_PROFILE_MEMORY
	int			num_table_entries;
	ML_memprof_report_entry	table[MEMORY_PROFILE_SIZE];
#endif
  
	/*
	** Print timing and stack usage information
	*/

	time_at_prev_stat = MR_time_at_last_stat;
	MR_time_at_last_stat = MR_get_user_cpu_miliseconds();

#if !defined(MR_HIGHLEVEL_CODE) || !defined(MR_CONSERVATIVE_GC)
	eng = MR_get_engine();
#endif

	fprintf(stderr, 
		""[Time: +%.3fs, %.3fs,"",
		(MR_time_at_last_stat - time_at_prev_stat) / 1000.0,
		(MR_time_at_last_stat - MR_time_at_start) / 1000.0
	);

#ifndef MR_HIGHLEVEL_CODE
	fprintf(stderr, "" D Stack: %.3fk, ND Stack: %.3fk,"",
		((char *) MR_sp - (char *)
			eng->MR_eng_context.MR_ctxt_detstack_zone->min)
				/ 1024.0,
		((char *) MR_maxfr - (char *)
			eng->MR_eng_context.MR_ctxt_nondetstack_zone->min)
				/ 1024.0
	);
#endif

#ifdef MR_CONSERVATIVE_GC
	{ char local_var;
	  fprintf(stderr, "" C Stack: %.3fk,"",
		labs(&local_var - (char *) GC_stackbottom) / 1024.0);
	}
#endif

#ifdef MR_USE_TRAIL
	fprintf(stderr,
		"" Trail: %.3fk,"",
		((char *) MR_trail_ptr - (char *)
			MR_trail_zone->min) / 1024.0
	);
#endif

	/*
	** Print heap usage information.
	*/

#ifdef MR_CONSERVATIVE_GC
	fprintf(stderr, 
		""\\n#GCs: %lu, ""
		""Heap used since last GC: %.3fk, Total used: %.3fk"",
		(unsigned long) GC_gc_no,
		GC_get_bytes_since_gc() / 1024.0,
		GC_get_heap_size() / 1024.0
	);
#else
	fprintf(stderr, 
		""\\nHeap: %.3fk"",
		((char *) MR_hp - (char *) eng->MR_eng_heap_zone->min) / 1024.0
	);
#endif

#ifdef	MR_MPROF_PROFILE_MEMORY

	/*
	** Update the overall counter (this needs to be done first,
	** so that the percentages come out right).
	*/
	ML_update_counter(&MR_memprof_overall, &ML_overall_counter);

	/*
	** Print out the per-procedure memory profile (top N entries)
	*/
	num_table_entries = ML_memory_profile_top_table(MR_memprof_procs.root,
		table, MEMORY_PROFILE_SIZE, 0);
	fprintf(stderr, ""\\nMemory profile by procedure\\n"");
	ML_memory_profile_report(table, num_table_entries, MR_FALSE);

	/*
	** Print out the per-type memory profile (top N entries)
	*/
	num_table_entries = ML_memory_profile_top_table(MR_memprof_types.root,
		table, MEMORY_PROFILE_SIZE, 0);
	fprintf(stderr, ""\\nMemory profile by type\\n"");
	ML_memory_profile_report(table, num_table_entries, MR_FALSE);

	/*
	** Print out the overall memory usage.
	*/
	fprintf(stderr, 
		""Overall memory usage:""
		""+%8.8g %8.8g cells, +%8.8g %8.8g words\\n"",
		ML_overall_counter.cells_since_period_start,
		ML_overall_counter.cells_at_period_end,
		ML_overall_counter.words_since_period_start,
		ML_overall_counter.words_at_period_end
	);

#endif /* MR_MPROF_PROFILE_MEMORY */

	fprintf(stderr, ""]\\n"");
}

#ifdef MR_MPROF_PROFILE_MEMORY

void
ML_report_full_memory_stats(void)
{
	int			num_table_entries;
	int			table_size;
	ML_memprof_report_entry	*table;

	/*
	** Update the overall counter (this needs to be done first,
	** so that the percentages come out right).
	*/
	ML_update_counter(&MR_memprof_overall, &ML_overall_counter);

	/*
	** Allocate space for the table
	*/
	if (MR_memprof_procs.num_entries > MR_memprof_types.num_entries) {
		table_size = MR_memprof_procs.num_entries;
	} else {
		table_size = MR_memprof_types.num_entries;
	}
	table = MR_GC_NEW_ARRAY(ML_memprof_report_entry, table_size);

	/*
	** Print the by-procedure memory profile
	*/
	num_table_entries = ML_memory_profile_fill_table(MR_memprof_procs.root,
		table, 0);
	qsort(table, MR_memprof_procs.num_entries,
		sizeof(ML_memprof_report_entry),
		ML_memory_profile_compare_final);
	fprintf(stderr, ""\\nMemory profile by procedure\\n"");
	fprintf(stderr, ""%14s %14s  %s\\n"",
		""Cells"", ""Words"", ""Procedure label"");
	ML_memory_profile_report(table, num_table_entries, MR_TRUE);

	/*
	** Print the by-type memory profile
	*/
	num_table_entries = ML_memory_profile_fill_table(MR_memprof_types.root,
		table, 0);
	qsort(table, MR_memprof_types.num_entries,
		sizeof(ML_memprof_report_entry),
		ML_memory_profile_compare_final);
	fprintf(stderr, ""\\nMemory profile by type\\n"");
	fprintf(stderr, ""%14s %14s  %s\\n"",
		""Cells"", ""Words"", ""Procedure label"");
	ML_memory_profile_report(table, num_table_entries, MR_TRUE);

	/*
	** Deallocate space for the table
	*/
	MR_GC_free(table);

	/*
	** Print the overall memory usage
	*/
	fprintf(stderr, 
		""\\nOverall memory usage: %8.8g cells, %8.8g words\\n"",
		ML_overall_counter.cells_at_period_end,
		ML_overall_counter.words_at_period_end
	);
}

/*
** ML_update_counter(counter, float_counter):
**	Copy the data for a period from `counter' into
**	`float_counter' (changing the format slightly as we go),
**	and update `counter' to reflect the start of a new period.
*/
static void
ML_update_counter(MR_memprof_counter *counter,
	ML_memprof_float_counter *float_counter)
{
	MR_add_two_dwords(counter->cells_at_period_start, 
		counter->cells_since_period_start);
	MR_add_two_dwords(counter->words_at_period_start, 
		counter->words_since_period_start);

	MR_convert_dword_to_double(counter->cells_since_period_start,
		float_counter->cells_since_period_start);
	MR_convert_dword_to_double(counter->words_since_period_start,
		float_counter->words_since_period_start);

	/* since the 'at start' numbers have already been incremented, */
	/* they now refer to the start of the *next* period */
	MR_convert_dword_to_double(counter->cells_at_period_start,
		float_counter->cells_at_period_end);
	MR_convert_dword_to_double(counter->words_at_period_start,
		float_counter->words_at_period_end);

	MR_zero_dword(counter->cells_since_period_start);
	MR_zero_dword(counter->words_since_period_start);
}

/*
** Insert an entry into the table of the top `table_size' entries.
** Entries are ranked according to their words_since_period_start.
** (This is an arbitrary choice; we might equally well choose
** to order them by cells_since_period_start. I prefer words (zs).)
** Entries that are not in the top `table_size' are discarded.
*/
static int
ML_insert_into_table(const ML_memprof_report_entry *new_entry,
	ML_memprof_report_entry *table, int table_size, int next_slot)
{
	int slot;

	/* ignore entries whose counts are zero (allowing for rounding) */
	if (new_entry->counter.words_since_period_start < 1.0) {
		return next_slot;
	}

	/*
	** Find the slot where this entry should be inserted.
	** Start at the end and work backwards until we find
	** the start of the table or until we find a table
	** entry which ranks higher that the new entry.
	*/
	slot = next_slot;
	while (slot > 0 && table[slot - 1].counter.words_since_period_start <
				new_entry->counter.words_since_period_start)
	{
		slot--;
	}

	/*
	** If this entry fits in the table, then
	** shuffle the displaced entries to the right,
	** insert the new entry in the table, and increment next_slot
	** (unless it is already at the end of the table).
	*/
	if (slot < table_size) {
#if 0
/*
** The following code is disabled because it causes gcc (2.7.2) internal
** errors (``fixed or forbidden register spilled'') on x86 machines when
** using gcc global register variables.
*/
		int i;
		for (i = table_size - 1; i > slot; i--) {
			table[i] = table[i - 1];
		}
		table[slot] = *new_entry;
#else
		memmove(&table[slot + 1], &table[slot],
			(table_size - slot - 1) * sizeof(*table));
		memcpy(&table[slot], new_entry, sizeof(*table));
#endif

		if (next_slot < table_size) {
			next_slot++;
		}
	}

	return next_slot;
}

/*
** ML_memory_profile_top_table(node, table, table_size, next_slot):
**	Insert the entries for `node' and its children into `table',
**	which is big enough to hold the top `table_size' entries.
**	`next_slot' specifies the number of entries currently
**	in the table.  Returns the new value of `next_slot'.
*/
static int
ML_memory_profile_top_table(MR_memprof_record *node,
	ML_memprof_report_entry *table, int table_size, int next_slot)
{
	ML_memprof_report_entry new_entry;

	if (node != NULL) {
		next_slot = ML_memory_profile_top_table(node->left,
					table, table_size, next_slot);

		new_entry.name = node->name;
		ML_update_counter(&node->counter, &new_entry.counter);
		next_slot = ML_insert_into_table(&new_entry,
					table, table_size, next_slot);

		next_slot = ML_memory_profile_top_table(node->right,
					table, table_size, next_slot);
	}
	return next_slot;
}

/*
** ML_memory_profile_fill_table(node, table, next_slot):
**	Insert the entries for `node' and its children into `table',
**	which the caller guarantees is big enough to hold them all.
**	`next_slot' specifies the number of entries currently
**	in the table.  Returns the new value of `next_slot'.
*/
static int
ML_memory_profile_fill_table(MR_memprof_record *node,
	ML_memprof_report_entry *table, int next_slot)
{
	if (node != NULL) {
		next_slot = ML_memory_profile_fill_table(node->left,
					table, next_slot);

		table[next_slot].name = node->name;
		ML_update_counter(&node->counter, &table[next_slot].counter);
		next_slot++;

		next_slot = ML_memory_profile_fill_table(node->right,
					table, next_slot);
	}
	return next_slot;
}

/*
** ML_memory_profile_report(table, num_entries, complete):
**	Print out a profiling report for the specified table.
*/
static void
ML_memory_profile_report(const ML_memprof_report_entry *table, int num_entries,
	MR_bool complete)
{
	int		i;
	const char	*name;

	if (complete) {
		if (ML_overall_counter.cells_at_period_end < 1.0
		||  ML_overall_counter.words_at_period_end < 1.0) {
			fprintf(stderr, ""no allocations to report\\n"");
			return;
		}
	} else {
		if (ML_overall_counter.cells_since_period_start < 1.0
		||  ML_overall_counter.words_since_period_start < 1.0) {
			fprintf(stderr, ""no allocations to report\\n"");
			return;
		}
	}

	if (num_entries > MAX_REPORT_LINES && !complete) {
		num_entries = MAX_REPORT_LINES;
	}

	for (i = 0; i < num_entries; i++) {
		if (complete) {
			fprintf(stderr,
				""%8.8g/%4.1f%% %8.8g/%4.1f%%  %s\\n"",
				table[i].counter.cells_at_period_end,
				100 * table[i].counter.cells_at_period_end /
					ML_overall_counter.cells_at_period_end,
				table[i].counter.words_at_period_end,
				100 * table[i].counter.words_at_period_end /
					ML_overall_counter.words_at_period_end,
				table[i].name
			);
		} else {
			fprintf(stderr,
				""%8.8g/%4.1f%% %8.8g/%4.1f%%  %s\\n"",
				table[i].counter.cells_since_period_start,
				100 *
				   table[i].counter.cells_since_period_start /
				   ML_overall_counter.cells_since_period_start,
				table[i].counter.words_since_period_start,
				100 *
				   table[i].counter.words_since_period_start /
				   ML_overall_counter.words_since_period_start,
				table[i].name
			);
		}
	}
}

/*
** Comparison routine used for qsort().
** Compares two ML_memprof_report_entry structures.
*/
static int 
ML_memory_profile_compare_final(const void *i1, const void *i2)
{
	const ML_memprof_report_entry *e1 = 
		(const ML_memprof_report_entry *) i1;
	const ML_memprof_report_entry *e2 =
		(const ML_memprof_report_entry *) i2;

	if (e1->counter.words_at_period_end < e2->counter.words_at_period_end)
	{
		return 1;
	} else if
	  (e1->counter.words_at_period_end > e2->counter.words_at_period_end)
	{
		return -1;
	} else {
		return strcmp(e1->name, e2->name);
	}
}

#endif /* MR_MPROF_PROFILE_MEMORY */
").

%-----------------------------------------------------------------------------%

:- pragma promise_pure(benchmark_det/5).
benchmark_det(Pred, In, Out, Repeats, Time) :-
	impure get_user_cpu_miliseconds(StartTime),
	impure benchmark_det_loop(Pred, In, Out, Repeats),
	impure get_user_cpu_miliseconds(EndTime),
	Time0 = EndTime - StartTime,
	cc_multi_equal(Time0, Time).

:- impure pred benchmark_det_loop(pred(T1, T2), T1, T2, int).
:- mode benchmark_det_loop(pred(in, out) is det, in, out, in) is det.
:- mode benchmark_det_loop(pred(in, out) is cc_multi, in, out, in) is cc_multi.

benchmark_det_loop(Pred, In, Out, Repeats) :-
	% The call to do_nothing/1 here is to make sure the compiler
	% doesn't optimize away the call to `Pred'.
	Pred(In, Out0),
	impure do_nothing(Out0),
	( Repeats > 1 ->
		impure benchmark_det_loop(Pred, In, Out, Repeats - 1)
	;
		Out = Out0
	).

:- pragma promise_pure(benchmark_func/5).
benchmark_func(Func, In, Out, Repeats, Time) :-
	impure get_user_cpu_miliseconds(StartTime),
	impure benchmark_func_loop(Func, In, Out, Repeats),
	impure get_user_cpu_miliseconds(EndTime),
	Time0 = EndTime - StartTime,
	cc_multi_equal(Time0, Time).

:- impure pred benchmark_func_loop(func(T1) = T2, T1, T2, int).
:- mode benchmark_func_loop(func(in) = out is det, in, out, in) is det.

benchmark_func_loop(Func, In, Out, Repeats) :-
	% The call to do_nothing/1 here is to make sure the compiler
	% doesn't optimize away the call to `Func'.
	Out0 = Func(In),
	impure do_nothing(Out0),
	( Repeats > 1 ->
		impure benchmark_func_loop(Func, In, Out, Repeats - 1)
	;
		Out = Out0
	).

:- pragma promise_pure(benchmark_nondet/5).
benchmark_nondet(Pred, In, Count, Repeats, Time) :-
	impure get_user_cpu_miliseconds(StartTime),
	impure benchmark_nondet_loop(Pred, In, Count, Repeats),
	impure get_user_cpu_miliseconds(EndTime),
	Time0 = EndTime - StartTime,
	cc_multi_equal(Time0, Time).

:- impure pred benchmark_nondet_loop(pred(T1, T2), T1, int, int).
:- mode benchmark_nondet_loop(pred(in, out) is nondet, in, out, in) is det.

benchmark_nondet_loop(Pred, In, Count, Repeats) :-
	impure new_int_reference(0, SolutionCounter),
	(
		impure repeat(Repeats),
		impure update_ref(SolutionCounter, 0),
		Pred(In, Out0),
		impure do_nothing(Out0),
		impure incr_ref(SolutionCounter),
		fail
	;
		true
	),
	semipure ref_value(SolutionCounter, Count).

:- impure pred repeat(int::in) is nondet.
repeat(N) :-
	N > 0,
	( true ; impure repeat(N - 1) ).

:- impure pred get_user_cpu_miliseconds(int::out) is det.
:- pragma foreign_proc("C",
	get_user_cpu_miliseconds(Time::out), [will_not_call_mercury],
"
	Time = MR_get_user_cpu_miliseconds();
").
:- pragma foreign_proc("MC++",
	get_user_cpu_miliseconds(_Time::out), [will_not_call_mercury],
"
	// This won't return the elapsed time since program start,
	// as it begins timing after the first call.
	// For computing time differences it should be fine.
	// Time = (int) (1000 * System::Diagnostics::Counter::GetElapsed());
	// XXX Can't seem to get this to work -- perhaps Diagnositcs isn't
	// yet available in Beta 1 of the .NET frameworks.

	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

/*
** To prevent the C compiler from optimizing the benchmark code
** away, we assign the benchmark output to a volatile global variable.
*/

:- pragma foreign_decl("C", "
	extern volatile MR_Word ML_benchmarking_dummy_word;
").
:- pragma foreign_code("C", "
	volatile MR_Word ML_benchmarking_dummy_word;
").

:- impure pred do_nothing(T::in) is det.
:- pragma foreign_proc("C", 
	do_nothing(X::in), [will_not_call_mercury, thread_safe], "
	ML_benchmarking_dummy_word = (MR_Word) X;
").
/*
** To prevent the MC++ compiler from optimizing the benchmark code
** away, we assign the benchmark output to a volatile static variable.
** XXX at least, we should do this but it doesn't seem to work.
*/
:- pragma foreign_proc("MC++", 
	do_nothing(X::in), [will_not_call_mercury, thread_safe],
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
/*	static volatile MR_Word ML_benchmarking_dummy_word;
	ML_benchmarking_dummy_word = (MR_Word) X;
*/
").

%-----------------------------------------------------------------------------%

%  Impure integer references.
%  This type is implemented in C.
:- type int_reference ---> int_reference(c_pointer).

%  Create a new int_reference given a term for it to reference.
:- impure pred new_int_reference(int::in, int_reference::out) is det.
:- pragma inline(new_int_reference/2).
:- pragma foreign_proc("C",
	new_int_reference(X::in, Ref::out), [will_not_call_mercury],
"
	MR_incr_hp(Ref, 1);
	* (MR_Integer *) Ref = X;
").
:- pragma foreign_proc("MC++",
	new_int_reference(_X::in, _Ref::out), [will_not_call_mercury], 
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").


:- impure pred incr_ref(int_reference::in) is det.
incr_ref(Ref) :-
	semipure ref_value(Ref, X),
	impure update_ref(Ref, X + 1).

:- semipure pred ref_value(int_reference::in, int::out) is det.
:- pragma inline(ref_value/2).
:- pragma foreign_proc("C", ref_value(Ref::in, X::out),
		[will_not_call_mercury, promise_semipure],
"
	X = * (MR_Integer *) Ref;
").
:- pragma foreign_proc("MC++", ref_value(_Ref::in, _X::out),
		[will_not_call_mercury, promise_semipure],
"
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

:- impure pred update_ref(int_reference::in, T::in) is det.
:- pragma inline(update_ref/2).
:- pragma foreign_proc("C",
	update_ref(Ref::in, X::in), [will_not_call_mercury], "
	* (MR_Integer *) Ref = X;
").
:- pragma foreign_proc("MC++",
	update_ref(_Ref::in, _X::in), [will_not_call_mercury], "
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

%-----------------------------------------------------------------------------%
