%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
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
% about the time period since the last call to report_stats to stdout.

:- impure pred report_stats is det.

% `report_full_memory_stats' is a non-logical procedure intended for use
% in profiling the memory usage of a program.  It has the side-effect of
% reporting a full memory profile to stdout.

:- impure pred report_full_memory_stats is det.

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

#include ""mercury_timing.h""

extern void ML_report_stats(void);

extern void ML_report_full_memory_stats(void);

"). % end pragma c_header_code

:- pragma c_code(report_stats, will_not_call_mercury,
"
	ML_report_stats();
").

:- pragma c_code(report_full_memory_stats, will_not_call_mercury,
"
#ifdef	PROFILE_MEMORY
	ML_report_full_memory_stats();
#endif
").

%-----------------------------------------------------------------------------%

:- pragma c_code("

#include <stdio.h>
#include <stdlib.h>
#include ""mercury_prof_mem.h""
#include ""mercury_heap_profile.h""

#ifdef PROFILE_MEMORY

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
				int num_entries, bool complete);

  static int  ML_memory_profile_compare_final(const void *, const void *);

#endif /* PROFILE_MEMORY */

void
ML_report_stats(void)
{
	int			time_at_prev_stat;
	MercuryEngine		*eng;
#ifdef PROFILE_MEMORY
	int			num_table_entries;
	ML_memprof_report_entry	table[MEMORY_PROFILE_SIZE];
#endif
  
	/*
	** Print timing and stack usage information
	*/

	time_at_prev_stat = time_at_last_stat;
	time_at_last_stat = MR_get_user_cpu_miliseconds();

	eng = MR_get_engine();

	fprintf(stderr, 
		""[Time: +%.3fs, %.3fs, D Stack: %.3fk, ND Stack: %.3fk, "",
		(time_at_last_stat - time_at_prev_stat) / 1000.0,
		(time_at_last_stat - time_at_start) / 1000.0,
		((char *) sp - (char *)
			eng->context.detstack_zone->min) / 1024.0,
		((char *) maxfr - (char *)
			eng->context.nondetstack_zone->min) / 1024.0
	);

	/*
	** Print heap usage information.
	*/

#ifdef CONSERVATIVE_GC
	fprintf(stderr, 
		""#GCs: %lu,\\n""
		""Heap used since last GC: %.3fk, Total used: %.3fk"",
		(unsigned long) GC_gc_no,
		GC_get_bytes_since_gc() / 1024.0,
		GC_get_heap_size() / 1024.0
	);
#else
	fprintf(stderr, 
		""Heap: %.3fk]\\n"",
		((char *) hp - (char *) eng->heap_zone->min) / 1024.0
	);
#endif

#ifdef	PROFILE_MEMORY

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
	ML_memory_profile_report(table, num_table_entries, FALSE);

	/*
	** Print out the per-type memory profile (top N entries)
	*/
	num_table_entries = ML_memory_profile_top_table(MR_memprof_types.root,
		table, MEMORY_PROFILE_SIZE, 0);
	fprintf(stderr, ""\\nMemory profile by type\\n"");
	ML_memory_profile_report(table, num_table_entries, FALSE);

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

#endif /* PROFILE_MEMORY */

	fprintf(stderr, ""]\\n"");
}

#ifdef PROFILE_MEMORY

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
	table = make_many(ML_memprof_report_entry, table_size);

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
	ML_memory_profile_report(table, num_table_entries, TRUE);

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
	ML_memory_profile_report(table, num_table_entries, TRUE);

	/*
	** Deallocate space for the table
	*/
	oldmem(table);

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
	bool complete)
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

#endif /* PROFILE_MEMORY */
").

%-----------------------------------------------------------------------------%

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

Declare_entry(do_call_nondet_closure);
Declare_entry(do_call_det_closure);

BEGIN_MODULE(benchmark_nondet_module)
	init_entry_sl(mercury__benchmarking__benchmark_nondet_5_0);
	init_label_sl(mercury__benchmarking__benchmark_nondet_5_0_i1);
	init_label_sl(mercury__benchmarking__benchmark_nondet_5_0_i2);
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
	** framevar(6): the saved trail pointer (if trailing enabled)
	**
	** We must make that the closure is called at least once,
	** otherwise the count we return isn't valid.
	*/

#ifdef MR_USE_TRAIL
  #define NONDET_STACK_SLOTS 7
#else
  #define NONDET_STACK_SLOTS 6
#endif

	mkframe(""benchmark_nondet"", NONDET_STACK_SLOTS,
		LABEL(mercury__benchmarking__benchmark_nondet_5_0_i2));

	framevar(0) = r3;
	framevar(1) = r4;

	if (rep_count <= 0) {
		framevar(2) = 1;
	} else {
		framevar(2) = rep_count;
	}

	framevar(3) = 0;
	mark_hp(framevar(5));
#ifdef MR_USE_TRAIL
	framevar(6) = MR_trail_ptr;
#endif
	framevar(4) = MR_get_user_cpu_miliseconds();

	/* call the higher-order pred closure that we were passed in r3 */
	r1 = r3;
	r2 = (Word) 1;	/* the higher-order call has 1 extra input argument  */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	/* r4 already has the extra input argument */
	call(ENTRY(do_call_nondet_closure),
		LABEL(mercury__benchmarking__benchmark_nondet_5_0_i1),
		LABEL(mercury__benchmarking__benchmark_nondet_5_0));

Define_label(mercury__benchmarking__benchmark_nondet_5_0_i1);
	update_prof_current_proc(
		LABEL(mercury__benchmarking__benchmark_nondet_5_0));

	/* we found a solution */
	framevar(3) = framevar(3) + 1;
	redo();

Define_label(mercury__benchmarking__benchmark_nondet_5_0_i2);
	update_prof_current_proc(
		LABEL(mercury__benchmarking__benchmark_nondet_5_0));

	/* no more solutions for this iteration, so mark it completed */
	framevar(2) = framevar(2) - 1;

	/* we can now reclaim memory by resetting the heap pointer */
	restore_hp(framevar(5));
#ifdef MR_USE_TRAIL
	/* ... and the trail pointer */
	MR_trail_ptr = framevar(6);
#endif

	/* are there any other iterations? */
	if (framevar(2) > 0) {
		/* yes, so reset the solution counter */
		/* and then set up the call just like last time */
		framevar(3) = 0;
		r1 = framevar(0);
		r2 = (Word) 1;
		r3 = (Word) 1;
		r4 = framevar(1);
		call(ENTRY(do_call_nondet_closure),
			LABEL(mercury__benchmarking__benchmark_nondet_5_0_i1),
			LABEL(mercury__benchmarking__benchmark_nondet_5_0));
	}

	/* no more iterations */
	count_output = framevar(3);
	time_output = MR_get_user_cpu_miliseconds() - framevar(4);
	succeed_discard();

#undef NONDET_STACK_SLOTS

END_MODULE

Define_extern_entry(mercury__benchmarking__benchmark_det_5_0);
Declare_label(mercury__benchmarking__benchmark_det_5_0_i1);
MR_MAKE_STACK_LAYOUT_ENTRY(mercury__benchmarking__benchmark_det_5_0);
MR_MAKE_STACK_LAYOUT_INTERNAL(mercury__benchmarking__benchmark_det_5_0, 1);

BEGIN_MODULE(benchmark_det_module)
	init_entry_sl(mercury__benchmarking__benchmark_det_5_0);
	init_label_sl(mercury__benchmarking__benchmark_det_5_0_i1);
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
	** detstackvar(7): the saved trail pointer (if trailing enabled)
	**
	** We must make that the closure is called at least once,
	** otherwise the count we return isn't valid.
	*/

#ifdef MR_USE_TRAIL
  #define DET_STACK_SLOTS 7
#else
  #define DET_STACK_SLOTS 6
#endif

	incr_sp(DET_STACK_SLOTS);
#ifdef MR_USE_TRAIL
	detstackvar(7) = MR_trail_ptr;
#endif
	detstackvar(6) = (Word) succip;
	mark_hp(detstackvar(5));

	detstackvar(1) = r3;
	detstackvar(2) = r4;

	if (rep_count <= 0) {
		detstackvar(3) = 1;
	} else {
		detstackvar(3) = rep_count;
	}

	detstackvar(4) = MR_get_user_cpu_miliseconds();

	/* call the higher-order pred closure that we were passed in r3 */
	r1 = r3;
	r2 = (Word) 1;	/* the higher-order call has 1 extra input argument  */
	r3 = (Word) 1;	/* the higher-order call has 1 extra output argument */
	/* r4 already has the extra input argument */
	call(ENTRY(do_call_det_closure),
		LABEL(mercury__benchmarking__benchmark_det_5_0_i1),
		LABEL(mercury__benchmarking__benchmark_det_5_0));

Define_label(mercury__benchmarking__benchmark_det_5_0_i1);
	update_prof_current_proc(
		LABEL(mercury__benchmarking__benchmark_det_5_0));

	/* mark current iteration completed */
	detstackvar(3) = detstackvar(3) - 1;

	/* are there any other iterations? */
	if (detstackvar(3) > 0) {
		/* yes, so set up the call just like last time */
#ifdef MR_USE_TRAIL
		/* Restore the trail... */
		MR_untrail_to(detstackvar(7), MR_undo);
		MR_trail_ptr = detstackvar(7);
#endif
		restore_hp(detstackvar(5));
		r1 = detstackvar(1);
		r2 = (Word) 1;
		r3 = (Word) 1;
		r4 = detstackvar(2);
		call(ENTRY(do_call_det_closure),
			LABEL(mercury__benchmarking__benchmark_det_5_0_i1),
			LABEL(mercury__benchmarking__benchmark_det_5_0));
	}

	/* no more iterations */
	soln_output = r1; /* the closure *always* returns its output in r1 */
	time_output = MR_get_user_cpu_miliseconds() - detstackvar(4);
	succip = (Word *) detstackvar(6);
	decr_sp(DET_STACK_SLOTS);
	proceed();

#undef DET_STACK_SLOTS

END_MODULE

void mercury_benchmarking_init_benchmark(void); /* suppress gcc warning */
void mercury_benchmarking_init_benchmark(void) {
	benchmark_nondet_module();
	benchmark_det_module();
}

").

%-----------------------------------------------------------------------------%
