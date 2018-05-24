%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: benchmarking.m.
% Main author: zs.
% Stability: medium.
%
% This module contains predicates that deal with the CPU time requirements
% of (various parts of) the program.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module benchmarking.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

    % `report_stats' is a non-logical procedure intended for use in profiling
    % the performance of a program. It has the side-effect of reporting
    % some memory and time usage statistics about the time period since
    % the last call to report_stats to stderr.
    %
    % Note: in Java, this reports usage of the calling thread. You will get
    % nonsensical results if the previous call to `report_stats' was from a
    % different thread.
    %
    % Note: in Erlang, the benchmark_* procedures will change the apparent time
    % of the last call to report_stats.
    %
:- impure pred report_stats is det.

    % `report_full_memory_stats' is a non-logical procedure intended for use
    % in profiling the memory usage of a program. It has the side-effect
    % of reporting a full memory profile to stderr.
    %
:- impure pred report_full_memory_stats is det.

    % report_memory_attribution(Label, Collect, !IO) is a procedure intended
    % for use in profiling the memory usage by a program. It is supported in
    % `memprof.gc' grades only, in other grades it is a no-op. It reports a
    % summary of the objects on the heap to a data file. See ``Using mprof -s
    % for profiling memory retention'' in the Mercury User's Guide. The label
    % is for your reference. If Collect is yes it has the effect of forcing a
    % garbage collection before building the report.
    %
:- pred report_memory_attribution(string::in, bool::in, io::di, io::uo) is det.
:- impure pred report_memory_attribution(string::in, bool::in) is det.

    % report_memory_attribution(Label, !IO) is the same as
    % report_memory_attribution/4 above, except that it always forces a
    % collection (in 'memprof.gc' grades).
    %
:- pred report_memory_attribution(string::in, io::di, io::uo) is det.
:- impure pred report_memory_attribution(string::in) is det.

    % benchmark_det(Pred, In, Out, Repeats, Time) is for benchmarking the det
    % predicate Pred. We call Pred with the input In and the output Out, and
    % return Out so that the caller can check the correctness of the
    % benchmarked predicate. Since most systems do not have good facilities
    % for measuring small times, the Repeats parameter allows the caller
    % to specify how many times Pred should be called inside the timed
    % interval. The number of milliseconds required to execute Pred with input
    % In this many times is returned as Time.
    %
    % benchmark_func(Func, In, Out, Repeats, Time) does for functions
    % exactly what benchmark_det does for predicates.
    %
:- pred benchmark_det(pred(T1, T2), T1, T2, int, int).
:- mode benchmark_det(pred(in, out) is det, in, out, in, out) is cc_multi.
:- mode benchmark_det(pred(in, out) is cc_multi, in, out, in, out) is cc_multi.

:- pred benchmark_func(func(T1) = T2, T1, T2, int, int).
:- mode benchmark_func(func(in) = out is det, in, out, in, out) is cc_multi.

:- pred benchmark_det_io(pred(T1, T2, T3, T3), T1, T2, T3, T3, int, int).
:- mode benchmark_det_io(pred(in, out, di, uo) is det, in, out, di, uo,
    in, out) is cc_multi.

    % benchmark_nondet(Pred, In, Count, Repeats, Time) is for benchmarking
    % the nondet predicate Pred. benchmark_nondet is similar to benchmark_det,
    % but it returns only a count of the solutions, rather than solutions
    % themselves. The number of milliseconds required to generate all
    % solutions of Pred with input In Repeats times is returned as Time.
    %
:- pred benchmark_nondet(pred(T1, T2), T1, int, int, int).
:- mode benchmark_nondet(pred(in, out) is nondet, in, out, in, out)
    is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Turn off or on the collection of all profiling statistics.
    %
:- pred turn_off_profiling(io::di, io::uo) is det.
:- pred turn_on_profiling(io::di, io::uo) is det.

:- impure pred turn_off_profiling is det.
:- impure pred turn_on_profiling is det.

    % Turn off or on the collection of call graph profiling statistics.
    %
:- pred turn_off_call_profiling(io::di, io::uo) is det.
:- pred turn_on_call_profiling(io::di, io::uo) is det.

:- impure pred turn_off_call_profiling is det.
:- impure pred turn_on_call_profiling is det.

    % Turn off or on the collection of time spent in each procedure
    % profiling statistics.
    %
:- pred turn_off_time_profiling(io::di, io::uo) is det.
:- pred turn_on_time_profiling(io::di, io::uo) is det.

:- impure pred turn_off_time_profiling is det.
:- impure pred turn_on_time_profiling is det.

    % Turn off or on the collection of memory allocated in each procedure
    % profiling statistics.
    %
:- pred turn_off_heap_profiling(io::di, io::uo) is det.
:- pred turn_on_heap_profiling(io::di, io::uo) is det.

:- impure pred turn_off_heap_profiling is det.
:- impure pred turn_on_heap_profiling is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % write_out_trace_counts(FileName, MaybeErrorMsg, !IO):
    %
    % Write out the trace counts accumulated so far in this program's execution
    % to FileName. If successful, set MaybeErrorMsg to "no". If unsuccessful,
    % e.g. because the program wasn't compiled with debugging enabled or
    % because trace counting isn't turned on, then set MaybeErrorMsg to a "yes"
    % wrapper around an error message.
    %
:- pred write_out_trace_counts(string::in, maybe(string)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Place a log message in the threadscope event stream. The event will be
    % logged as being generated by the current Mercury Engine. This is a no-op
    % when threadscope is not available.
    %
:- pred log_threadscope_message(string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module mutvar.
:- import_module string.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

#include ""mercury_timing.h""
#include ""mercury_heap.h""

extern void ML_report_stats(void);

extern void ML_report_full_memory_stats(void);

").

:- pragma foreign_proc("C",
    report_stats,
    [will_not_call_mercury],
"
    ML_report_stats();
").

:- pragma foreign_proc("C",
    report_full_memory_stats,
    [will_not_call_mercury],
"
#ifdef  MR_MPROF_PROFILE_MEMORY
    ML_report_full_memory_stats();
#endif
").

:- pragma foreign_proc("C#",
    report_stats,
    [may_call_mercury, terminates],
"
    ML_report_stats();
").

:- pragma foreign_proc("C#",
    report_full_memory_stats,
    [will_not_call_mercury],
"
    ML_report_full_memory_stats();
").

:- pragma foreign_proc("Java",
    report_stats,
    [may_call_mercury, terminates],
"
    ML_report_stats();
").

:- pragma foreign_proc("Java",
    report_full_memory_stats,
    [will_not_call_mercury],
"
    ML_report_full_memory_stats();
").

:- pragma foreign_proc("Erlang",
    report_stats,
    [may_call_mercury, terminates],
"
    'ML_report_stats'()
").

%---------------------------------------------------------------------------%

report_memory_attribution(Label, Collect) :-
    trace [io(!IO)] (
        report_memory_attribution(Label, Collect, !IO)
    ),
    impure impure_true.

:- pragma foreign_proc("C",
    report_memory_attribution(Label::in, Collect::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    MR_bool run_collect = (Collect) ? MR_TRUE : MR_FALSE;

#ifdef  MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
    MR_report_memory_attribution(Label, run_collect);
#else
    (void) Label;
#endif
").

report_memory_attribution(_, _, !IO).

report_memory_attribution(Label, !IO) :-
    report_memory_attribution(Label, yes, !IO).

report_memory_attribution(Label) :-
    impure report_memory_attribution(Label, yes).

%---------------------------------------------------------------------------%

:- pragma foreign_code("C", "

#include <stdio.h>
#include <stdlib.h>
#include ""mercury_prof_mem.h""
#include ""mercury_heap_profile.h""
#include ""mercury_wrapper.h""      /* for MR_user_time_at_last_stat */

#ifdef MR_MPROF_PROFILE_MEMORY

  #define MEMORY_PROFILE_SIZE   10  /* Profile the top 10 entries */

  #define MAX_REPORT_LINES  10  /* Display the top 10 entries */

  /* local types */

  typedef struct ML_memprof_float_counter
  {
    double      cells_at_period_end;
    double      words_at_period_end;
    double      cells_since_period_start;
    double      words_since_period_start;
  } ML_memprof_float_counter;

  typedef struct    ML_memprof_report_entry
  {
    const char                  *name;
    ML_memprof_float_counter    counter;
  } ML_memprof_report_entry;

  /* static variables */

  static ML_memprof_float_counter   ML_overall_counter;

  /* local function declarations */

  static    void    ML_update_counter(MR_memprof_counter *counter,
                        ML_memprof_float_counter *float_counter);

  static    int     ML_insert_into_table(const ML_memprof_report_entry
                        *new_entry, ML_memprof_report_entry *table,
                        int table_size, int next_slot);

  static    int     ML_memory_profile_top_table(MR_memprof_record *node,
                        ML_memprof_report_entry *table,
                        int size, int next_slot);

  static    int     ML_memory_profile_fill_table(MR_memprof_record *node,
                        ML_memprof_report_entry *table, int next_slot);

  static    void    ML_memory_profile_report(const ML_memprof_report_entry *,
                        int num_entries, MR_bool complete);

  static    int     ML_memory_profile_compare_final(const void *i1,
                        const void *i2);

#endif /* MR_MPROF_PROFILE_MEMORY */

void
ML_report_stats(void)
{
    int                 user_time_at_prev_stat;
    int                 real_time_at_prev_stat;
#if !defined(MR_HIGHLEVEL_CODE) || !defined(MR_CONSERVATIVE_GC)
    MercuryEngine       *eng;
#endif
#ifdef MR_MPROF_PROFILE_MEMORY
    int                 num_table_entries;
    ML_memprof_report_entry table[MEMORY_PROFILE_SIZE];
#endif

    /*
    ** Print timing and stack usage information
    */

    user_time_at_prev_stat = MR_user_time_at_last_stat;
    MR_user_time_at_last_stat = MR_get_user_cpu_milliseconds();

    real_time_at_prev_stat = MR_real_time_at_last_stat;
    MR_real_time_at_last_stat = MR_get_real_milliseconds();

#if !defined(MR_HIGHLEVEL_CODE) || !defined(MR_CONSERVATIVE_GC)
    eng = MR_get_engine();
#endif

    fprintf(stderr, ""[User time: +%.3fs, %.3fs,"",
        (MR_user_time_at_last_stat - user_time_at_prev_stat) / 1000.0,
        (MR_user_time_at_last_stat - MR_user_time_at_start) / 1000.0
    );

    fprintf(stderr, "" Real time: +%.3fs, %.3fs,"",
        (MR_real_time_at_last_stat - real_time_at_prev_stat) / 1000.0,
        (MR_real_time_at_last_stat - MR_real_time_at_start) / 1000.0
    );

#ifndef MR_HIGHLEVEL_CODE
    fprintf(stderr, "" D Stack: %.3fk, ND Stack: %.3fk,"",
        ((char *) MR_sp - (char *)
            eng->MR_eng_context.MR_ctxt_detstack_zone->MR_zone_min) / 1024.0,
        ((char *) MR_maxfr - (char *)
            eng->MR_eng_context.MR_ctxt_nondetstack_zone->MR_zone_min) / 1024.0
    );
#endif

#ifdef MR_BOEHM_GC
    {
        char local_var;
        struct GC_stack_base base;

        if (GC_SUCCESS == GC_get_stack_base(&base)) {
            fprintf(stderr, "" C Stack: %.3fk,"",
                labs(&local_var - (char *)base.mem_base) / 1024.0);
        } else {
            fprintf(stderr, "" Cannot locate C stack base."");
        }
    }
#endif

#ifdef MR_USE_TRAIL
    #ifdef MR_THREAD_SAFE
        fprintf(stderr, "", Trail: %.3fk,"",
            ((char *) MR_trail_ptr -
            (char *) MR_CONTEXT(MR_ctxt_trail_zone)->MR_zone_min) / 1024.0
        );
    #else
        fprintf(stderr, "" Trail: %.3fk,"",
            ((char *) MR_trail_ptr -
            (char *) MR_trail_zone->MR_zone_min) / 1024.0
        );
   #endif /* !MR_THREAD_SAFE */
#endif /* !MR_USE_TRAIL */

    /*
    ** Print heap usage information.
    */

#ifdef MR_CONSERVATIVE_GC
  #ifdef MR_BOEHM_GC
    fprintf(stderr, ""\\n#GCs: %lu, "",
        (unsigned long) GC_get_gc_no());
    if (GC_mercury_calc_gc_time) {
        /* convert from unsigned long milliseconds to float seconds */
        fprintf(stderr, ""total GC time: %.2fs, "",
            (float) GC_total_gc_time / (float) 1000);
    }
    fprintf(stderr, ""Heap used since last GC: %.3fk, Total used: %.3fk"",
        GC_get_bytes_since_gc() / 1024.0,
        GC_get_heap_size() / 1024.0
    );
  #endif
#else /* !MR_CONSERVATIVE_GC */
    fprintf(stderr, ""\\nHeap: %.3fk"",
        ((char *) MR_hp - (char *) eng->MR_eng_heap_zone->MR_zone_min) / 1024.0
    );
#endif /* !MR_CONSERVATIVE_GC */

#ifdef  MR_MPROF_PROFILE_MEMORY

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
    fprintf(stderr, ""Overall memory usage:""
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
    int                     num_table_entries;
    int                     table_size;
    ML_memprof_report_entry *table;

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
    qsort(table, MR_memprof_procs.num_entries, sizeof(ML_memprof_report_entry),
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
    qsort(table, MR_memprof_types.num_entries, sizeof(ML_memprof_report_entry),
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
    fprintf(stderr, ""\\nOverall memory usage: %8.8g cells, %8.8g words\\n"",
        ML_overall_counter.cells_at_period_end,
        ML_overall_counter.words_at_period_end
    );
}

/*
** ML_update_counter(counter, float_counter):
**
** Copy the data for a period from `counter' into
** `float_counter' (changing the format slightly as we go),
** and update `counter' to reflect the start of a new period.
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
    while (slot > 0 && table[slot - 1].counter.words_since_period_start
        < new_entry->counter.words_since_period_start)
    {
        slot--;
    }

    /*
    ** If this entry fits in the table, then shuffle the displaced entries
    ** to the right, insert the new entry in the table, and increment next_slot
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
        MR_memcpy(&table[slot], new_entry, sizeof(*table));
#endif

        if (next_slot < table_size) {
            next_slot++;
        }
    }

    return next_slot;
}

/*
** ML_memory_profile_top_table(node, table, table_size, next_slot):
**
** Insert the entries for `node' and its children into `table', which is
** big enough to hold the top `table_size' entries. `next_slot' specifies
** the number of entries currently in the table. Returns the new value
** of `next_slot'.
*/
static int
ML_memory_profile_top_table(MR_memprof_record *node,
    ML_memprof_report_entry *table, int table_size, int next_slot)
{
    ML_memprof_report_entry new_entry;

    if (node != NULL) {
        next_slot = ML_memory_profile_top_table(node->left,
            table, table_size, next_slot);

        if (node->type_name != NULL) {
            new_entry.name = node->type_name;
        } else {
            new_entry.name = MR_lookup_entry_or_internal(node->proc);
        }
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
** Insert the entries for `node' and its children into `table', which the
** caller guarantees is big enough to hold them all. `next_slot' specifies
** the number of entries currently in the table. Returns the new value
** of `next_slot'.
*/

static int
ML_memory_profile_fill_table(MR_memprof_record *node,
    ML_memprof_report_entry *table, int next_slot)
{
    if (node != NULL) {
        next_slot = ML_memory_profile_fill_table(node->left,
            table, next_slot);

        if (node->type_name != NULL) {
            table[next_slot].name = node->type_name;
        } else {
            table[next_slot].name = MR_lookup_entry_or_internal(node->proc);
        }
        ML_update_counter(&node->counter, &table[next_slot].counter);
        next_slot++;

        next_slot = ML_memory_profile_fill_table(node->right,
            table, next_slot);
    }
    return next_slot;
}

/*
** ML_memory_profile_report(table, num_entries, complete):
**
** Print out a profiling report for the specified table.
*/

static void
ML_memory_profile_report(const ML_memprof_report_entry *table, int num_entries,
    MR_bool complete)
{
    int     i;
    const char  *name;

    if (complete) {
        if (ML_overall_counter.cells_at_period_end < 1.0
        ||  ML_overall_counter.words_at_period_end < 1.0)
        {
            fprintf(stderr, ""no allocations to report\\n"");
            return;
        }
    } else {
        if (ML_overall_counter.cells_since_period_start < 1.0
        ||  ML_overall_counter.words_since_period_start < 1.0)
        {
            fprintf(stderr, ""no allocations to report\\n"");
            return;
        }
    }

    if (num_entries > MAX_REPORT_LINES && !complete) {
        num_entries = MAX_REPORT_LINES;
    }

    for (i = 0; i < num_entries; i++) {
        if (complete) {
            fprintf(stderr, ""%8.8g/%4.1f%% %8.8g/%4.1f%%  %s\\n"",
                table[i].counter.cells_at_period_end,
                100 * table[i].counter.cells_at_period_end /
                    ML_overall_counter.cells_at_period_end,
                table[i].counter.words_at_period_end,
                100 * table[i].counter.words_at_period_end /
                    ML_overall_counter.words_at_period_end,
                table[i].name
            );
        } else {
            fprintf(stderr, ""%8.8g/%4.1f%% %8.8g/%4.1f%%  %s\\n"",
                table[i].counter.cells_since_period_start,
                100 * table[i].counter.cells_since_period_start /
                   ML_overall_counter.cells_since_period_start,
                table[i].counter.words_since_period_start,
                100 * table[i].counter.words_since_period_start /
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
    const ML_memprof_report_entry *e1 = (const ML_memprof_report_entry *) i1;
    const ML_memprof_report_entry *e2 = (const ML_memprof_report_entry *) i2;

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

:- pragma foreign_code("C#",
"
private static double user_time_at_start
    = System.Diagnostics.Process.GetCurrentProcess().UserProcessorTime
        .TotalSeconds;
private static double user_time_at_last_stat;

private static long real_time_at_start
    = real_time_at_last_stat = System.DateTime.Now.Ticks;
private static long real_time_at_last_stat;

private static void
ML_report_stats()
{
    double user_time_at_prev_stat = user_time_at_last_stat;
    user_time_at_last_stat = System.Diagnostics.Process.GetCurrentProcess()
        .UserProcessorTime.TotalSeconds;

    long real_time_at_prev_stat = real_time_at_last_stat;
    real_time_at_last_stat = System.DateTime.Now.Ticks;

    System.Console.WriteLine(System.String.Format(
        ""[User time: +{0:F2}s, {1:F2}s Real time: +{2:F2}s, {3:F2}s]"",
        (user_time_at_last_stat - user_time_at_prev_stat),
        (user_time_at_last_stat - user_time_at_start),
        ((real_time_at_last_stat - real_time_at_prev_stat)
            / (double) System.TimeSpan.TicksPerSecond),
        ((real_time_at_last_stat - real_time_at_start)
            / (double) System.TimeSpan.TicksPerSecond)
    ));

    /*
    ** XXX At this point there should be a whole bunch of memory usage
    ** statistics.
    */
}

private static void
ML_report_full_memory_stats()
{
    /*
    ** XXX The support for this predicate is even worse. Since we don't have
    ** access to memory usage statistics, all you get here is an apology.
    ** But at least it doesn't just crash with an error.
    */

    System.Console.Error.WriteLine(
        ""Sorry, report_full_memory_stats is not yet "" +
        ""implemented for the C# back-end."");
}
").

:- pragma foreign_code("Java",
"
private static int user_time_at_start = 0;
private static int user_time_at_last_stat = 0;
private static long real_time_at_start;
private static long real_time_at_last_stat;

public static void
ML_initialise()
{
    /*
    ** Class initialisation may be delayed so main() must explicitly initialise
    ** these variables at startup, otherwise the first call to `report_stats'
    ** will show the wrong elapsed time.
    */
    real_time_at_start = System.currentTimeMillis();
    real_time_at_last_stat = real_time_at_start;
}

private static void
ML_report_stats()
{
    int user_time_at_prev_stat = user_time_at_last_stat;
    user_time_at_last_stat = ML_get_user_cpu_milliseconds();

    long real_time_at_prev_stat = real_time_at_last_stat;
    real_time_at_last_stat = System.currentTimeMillis();

    System.err.print(
        ""[User time: +"" +
        ((user_time_at_last_stat - user_time_at_prev_stat) / 1000.0) +
        ""s, "" +
        ((user_time_at_last_stat - user_time_at_start) / 1000.0) +
        ""s"");

    System.err.print(
        "" Real time: +"" +
        ((real_time_at_last_stat - real_time_at_prev_stat) / 1000.0) +
        ""s, "" +
        ((real_time_at_last_stat - real_time_at_start) / 1000.0) +
        ""s"");

    /*
    ** XXX At this point there should be a whole bunch of memory usage
    ** statistics. Unfortunately the Java back-end does not yet support
    ** this amount of profiling, so cpu time is all you get.
    */

    System.err.println(""]"");
}

private static void
ML_report_full_memory_stats()
{
    /*
    ** XXX The support for this predicate is even worse. Since we don't have
    ** access to memory usage statistics, all you get here is an apology.
    ** But at least it doesn't just crash with an error.
    */

    System.err.println(""Sorry, report_full_memory_stats is not yet "" +
        ""implemented for the Java back-end."");
}
").

:- pragma foreign_code("Erlang",
"
'ML_report_stats'() ->
    {Time, TimeSinceLastCall} = statistics(runtime),
    TimeSecs = Time / 1000.0,
    TimeSinceLastCallSecs = TimeSinceLastCall / 1000.0,

    Bytes = erlang:memory(total),
    KBytes = Bytes / 1024.0,

    io:format(""[Time: ~.3fs, +~.3fs, Total used: ~.3fk]~n"",
        [TimeSecs, TimeSinceLastCallSecs, KBytes]).
").

%---------------------------------------------------------------------------%

:- pragma promise_pure(benchmark_det/5).
benchmark_det(Pred, In, Out, Repeats, Time) :-
    impure get_user_cpu_milliseconds(StartTime),
    impure benchmark_det_loop(Pred, In, Out, Repeats),
    impure get_user_cpu_milliseconds(EndTime),
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
    ( if Repeats > 1 then
        impure benchmark_det_loop(Pred, In, Out, Repeats - 1)
    else
        Out = Out0
    ).

:- pragma promise_pure(benchmark_func/5).

benchmark_func(Func, In, Out, Repeats, Time) :-
    impure get_user_cpu_milliseconds(StartTime),
    impure benchmark_func_loop(Func, In, Out, Repeats),
    impure get_user_cpu_milliseconds(EndTime),
    Time0 = EndTime - StartTime,
    cc_multi_equal(Time0, Time).

:- impure pred benchmark_func_loop(func(T1) = T2, T1, T2, int).
:- mode benchmark_func_loop(func(in) = out is det, in, out, in) is det.

benchmark_func_loop(Func, In, Out, Repeats) :-
    % The call to do_nothing/1 here is to make sure the compiler
    % doesn't optimize away the call to `Func'.
    Out0 = Func(In),
    impure do_nothing(Out0),
    ( if Repeats > 1 then
        impure benchmark_func_loop(Func, In, Out, Repeats - 1)
    else
        Out = Out0
    ).

:- pragma promise_pure(benchmark_det_io/7).

benchmark_det_io(Pred, InA, OutA, InB, OutB, Repeats, Time) :-
    impure get_user_cpu_milliseconds(StartTime),
    impure benchmark_det_loop_io(Pred, InA, OutA, InB, OutB, Repeats),
    impure get_user_cpu_milliseconds(EndTime),
    Time = EndTime - StartTime.
    % XXX cc_multi_equal(Time0, Time).

:- impure pred benchmark_det_loop_io(pred(T1, T2, T3, T3), T1, T2,
    T3, T3, int).
:- mode benchmark_det_loop_io(pred(in, out, di, uo) is det, in, out,
    di, uo, in) is cc_multi.

benchmark_det_loop_io(Pred, InA, OutA, InB, OutB, Repeats) :-
    % The call to do_nothing/1 here is to make sure the compiler
    % doesn't optimize away the call to `Pred'.
    Pred(InA, OutA0, InB, OutB0),
    impure do_nothing(OutA0),
    ( if Repeats > 1 then
        impure benchmark_det_loop_io(Pred, InA, OutA, OutB0, OutB, Repeats - 1)
    else
        OutA = OutA0,
        OutB = OutB0
    ).

:- pragma promise_pure(benchmark_nondet/5).

benchmark_nondet(Pred, In, Count, Repeats, Time) :-
    impure get_user_cpu_milliseconds(StartTime),
    impure benchmark_nondet_loop(Pred, In, Count, Repeats),
    impure get_user_cpu_milliseconds(EndTime),
    Time0 = EndTime - StartTime,
    cc_multi_equal(Time0, Time).

:- impure pred benchmark_nondet_loop(pred(T1, T2), T1, int, int).
:- mode benchmark_nondet_loop(pred(in, out) is nondet, in, out, in) is det.

benchmark_nondet_loop(Pred, In, Count, Repeats) :-
    impure new_mutvar(0, SolutionCounter),
    (
        impure repeat(Repeats),
        impure set_mutvar(SolutionCounter, 0),
        Pred(In, Out0),
        impure do_nothing(Out0),
        impure get_mutvar(SolutionCounter, Count),
        impure set_mutvar(SolutionCounter, Count + 1),
        fail
    ;
        true
    ),
    impure get_mutvar(SolutionCounter, Count).

:- impure pred repeat(int::in) is nondet.

repeat(N) :-
    N > 0,
    (
        true
    ;
        impure repeat(N - 1)
    ).

:- impure pred get_user_cpu_milliseconds(int::out) is det.

:- pragma foreign_export("C#", get_user_cpu_milliseconds(out),
    "ML_get_user_cpu_milliseconds").
:- pragma foreign_export("Java", get_user_cpu_milliseconds(out),
    "ML_get_user_cpu_milliseconds").

:- pragma foreign_proc("C",
    get_user_cpu_milliseconds(Time::out),
    [will_not_call_mercury, thread_safe],
"
    Time = MR_get_user_cpu_milliseconds();
").

:- pragma foreign_proc("C#",
    get_user_cpu_milliseconds(Time::out),
    [will_not_call_mercury, thread_safe],
"
    // This won't return the elapsed time since program start,
    // as it begins timing after the first call.
    // For computing time differences it should be fine.
    Time = (int) System.Diagnostics.Process.GetCurrentProcess()
        .UserProcessorTime.TotalMilliseconds;
").

:- pragma foreign_proc("Java",
    get_user_cpu_milliseconds(Time::out),
    [will_not_call_mercury, thread_safe, may_not_duplicate],
"
    try {
        java.lang.management.ThreadMXBean bean =
            java.lang.management.ManagementFactory.getThreadMXBean();
        long nsecs = bean.getCurrentThreadUserTime();
        if (nsecs == -1) {
            Time = -1;
        } else {
            Time = (int) (nsecs / 1000000L);
        }
    } catch (java.lang.UnsupportedOperationException e) {
        Time = -1;
    }
").

:- pragma foreign_proc("Erlang",
    get_user_cpu_milliseconds(Time::out),
    [will_not_call_mercury, thread_safe],
"
    {Time, _TimeSinceLastCall} = statistics(runtime)
").

/*
** To prevent the C compiler from optimizing the benchmark code
** away, we assign the benchmark output to a volatile global variable.
*/

:- pragma foreign_decl("C", "
    extern  volatile    MR_Word ML_benchmarking_dummy_word;
").
:- pragma foreign_code("C", "
    volatile        MR_Word ML_benchmarking_dummy_word;
").

:- impure pred do_nothing(T::in) is det.

:- pragma foreign_proc("C",
    do_nothing(X::in),
    [will_not_call_mercury, thread_safe],
"
    ML_benchmarking_dummy_word = (MR_Word) X;
").

:- pragma foreign_proc("C#",
    do_nothing(_X::in),
    [will_not_call_mercury, thread_safe],
"
").

:- pragma foreign_code("Java",
"
    static volatile Object ML_benchmarking_dummy_word;
").
:- pragma foreign_proc("Java",
    do_nothing(X::in),
    [will_not_call_mercury, thread_safe],
"
    ML_benchmarking_dummy_word = X;
").

:- pragma foreign_proc("Erlang",
    do_nothing(_X::in),
    [will_not_call_mercury, thread_safe],
"
    void
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma promise_pure(turn_off_profiling/2).

turn_off_profiling(!IO) :-
    impure turn_off_profiling.

:- pragma promise_pure(turn_on_profiling/2).

turn_on_profiling(!IO) :-
    impure turn_on_profiling.

turn_off_profiling :-
    impure turn_off_call_profiling,
    impure turn_off_time_profiling,
    impure turn_off_heap_profiling.

turn_on_profiling :-
    impure turn_on_call_profiling,
    impure turn_on_time_profiling,
    impure turn_on_heap_profiling.

%---------------------------------------------------------------------------%

:- pragma promise_pure(turn_off_call_profiling/2).

turn_off_call_profiling(!IO) :-
    impure turn_off_call_profiling.

:- pragma promise_pure(turn_on_call_profiling/2).

turn_on_call_profiling(!IO) :-
    impure turn_on_call_profiling.

:- pragma promise_pure(turn_off_time_profiling/2).

turn_off_time_profiling(!IO) :-
    impure turn_off_time_profiling.

:- pragma promise_pure(turn_on_time_profiling/2).

turn_on_time_profiling(!IO) :-
    impure turn_on_time_profiling.

:- pragma promise_pure(turn_off_heap_profiling/2).

turn_off_heap_profiling(!IO) :-
    impure turn_off_heap_profiling.

:- pragma promise_pure(turn_on_heap_profiling/2).

turn_on_heap_profiling(!IO) :-
    impure turn_on_heap_profiling.

%---------------------------------------------------------------------------%

:- pragma foreign_decl(c, local, "
#include ""mercury_prof.h""
#include ""mercury_heap_profile.h""
").

:- pragma foreign_proc(c, turn_off_call_profiling,
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
#ifdef MR_MPROF_PROFILE_CALLS
    MR_prof_turn_off_call_profiling();
#endif
").

:- pragma foreign_proc(c, turn_on_call_profiling,
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
#ifdef MR_MPROF_PROFILE_CALLS
    MR_prof_turn_on_call_profiling();
#endif
").

:- pragma foreign_proc(c, turn_off_time_profiling,
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
#ifdef MR_MPROF_PROFILE_TIME
    MR_prof_turn_off_time_profiling();
#endif
").

:- pragma foreign_proc(c, turn_on_time_profiling,
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
#ifdef MR_MPROF_PROFILE_TIME
    MR_prof_turn_on_time_profiling();
#endif
").

:- pragma foreign_proc(c, turn_off_heap_profiling,
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    MR_prof_turn_off_heap_profiling();
").

:- pragma foreign_proc(c, turn_on_heap_profiling,
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    MR_prof_turn_on_heap_profiling();
").

%---------------------------------------------------------------------------%

write_out_trace_counts(DumpFileName, MaybeErrorMsg, !IO) :-
    dump_trace_counts_to(DumpFileName, Result, !IO),
    ( if Result = 0 then
        MaybeErrorMsg = no
    else if Result = 1 then
        MaybeErrorMsg = yes("Couldn't dump trace counts to `" ++
            DumpFileName ++ "': no compiled with debugging")
    else if Result = 2 then
        MaybeErrorMsg = yes("Couldn't dump trace counts to `" ++
            DumpFileName ++ "': trace counting not turned on")
    else if Result = 3 then
        MaybeErrorMsg = yes("Couldn't dump trace counts to `" ++
            DumpFileName ++ "': couldn't open file")
    else
        MaybeErrorMsg = yes("Couldn't dump trace counts to `" ++
            DumpFileName ++ "'")
    ).

:- pred dump_trace_counts_to(string::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dump_trace_counts_to(FileName::in, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
#ifdef  MR_EXEC_TRACE
    FILE    *fp;

    if (MR_trace_count_enabled && MR_trace_func_enabled) {
        fp = fopen(FileName, ""w"");
        if (fp != NULL) {
            MR_trace_write_label_exec_counts(fp, MR_progname, MR_FALSE);
            Result = 0;
            (void) fclose(fp);
        } else {
            Result = 3;
        }
    } else {
        Result = 2;
    }
#else
    Result = 1;
#endif
").

% Default definition for non-C backends.
dump_trace_counts_to(_, 1, !IO).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    log_threadscope_message(Message::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, will_not_throw_exception, thread_safe,
        promise_pure, tabled_for_io],
"
#if MR_THREADSCOPE
    MR_threadscope_post_log_msg(Message);
#endif
").

%---------------------------------------------------------------------------%
