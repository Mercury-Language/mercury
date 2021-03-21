// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains code for implementing the predicates in the
// Mercury standard library that report statistics about the program's
// execution.

////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include "mercury_prof_mem.h"
#include "mercury_heap_profile.h"
#include "mercury_wrapper.h"        // for MR_user_time_at_last_stat
#include "mercury_report_stats.h"   // for MR_user_time_at_last_stat

#ifdef MR_MPROF_PROFILE_MEMORY

  #define MEMORY_PROFILE_SIZE   10  // Profile the top 10 entries.

  #define MAX_REPORT_LINES  10      // Display the top 10 entries.

  // local types

  typedef struct MR_memprof_float_counter
  {
    double      cells_at_period_end;
    double      words_at_period_end;
    double      cells_since_period_start;
    double      words_since_period_start;
  } MR_memprof_float_counter;

  typedef struct MR_memprof_report_entry
  {
    const char                  *name;
    MR_memprof_float_counter    counter;
  } MR_memprof_report_entry;

  // static variables

  static MR_memprof_float_counter   MR_overall_memprof_counter;

  // local function declarations

  static    void    MR_update_memprof_counter(MR_memprof_counter *counter,
                        MR_memprof_float_counter *float_counter);

  static    int     MR_insert_into_memprof_table(const MR_memprof_report_entry
                        *new_entry, MR_memprof_report_entry *table,
                        int table_size, int next_slot);

  static    int     MR_memory_profile_top_table(MR_memprof_record *node,
                        MR_memprof_report_entry *table,
                        int size, int next_slot);

  static    int     MR_memory_profile_fill_table(MR_memprof_record *node,
                        MR_memprof_report_entry *table, int next_slot);

  static    void    MR_memory_profile_report(FILE *fp,
                        const MR_memprof_report_entry *,
                        int num_entries, MR_bool complete);

  static    int     MR_memory_profile_compare_final(const void *i1,
                        const void *i2);

#endif // MR_MPROF_PROFILE_MEMORY

void
MR_report_standard_stats(FILE *fp)
{
    int                 user_time_at_prev_stat;
    int                 real_time_at_prev_stat;
#if !defined(MR_HIGHLEVEL_CODE) || !defined(MR_CONSERVATIVE_GC)
    MercuryEngine       *eng;
#endif
#ifdef MR_MPROF_PROFILE_MEMORY
    int                 num_table_entries;
    MR_memprof_report_entry table[MEMORY_PROFILE_SIZE];
#endif

    // Print timing and stack usage information.

    user_time_at_prev_stat = MR_user_time_at_last_stat;
    MR_user_time_at_last_stat = MR_get_user_cpu_milliseconds();

    real_time_at_prev_stat = MR_real_time_at_last_stat;
    MR_real_time_at_last_stat = MR_get_real_milliseconds();

#if !defined(MR_HIGHLEVEL_CODE) || !defined(MR_CONSERVATIVE_GC)
    eng = MR_get_engine();
#endif

    fprintf(fp, "[User time: +%.3fs, %.3fs,",
        (MR_user_time_at_last_stat - user_time_at_prev_stat) / 1000.0,
        (MR_user_time_at_last_stat - MR_user_time_at_start) / 1000.0
    );

    fprintf(fp, " Real time: +%.3fs, %.3fs,",
        (MR_real_time_at_last_stat - real_time_at_prev_stat) / 1000.0,
        (MR_real_time_at_last_stat - MR_real_time_at_start) / 1000.0
    );

#ifndef MR_HIGHLEVEL_CODE
    fprintf(fp, " D Stack: %.3fk, ND Stack: %.3fk,",
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
            fprintf(fp, " C Stack: %.3fk,",
                labs(&local_var - (char *)base.mem_base) / 1024.0);
        } else {
            fprintf(fp, " Cannot locate C stack base.");
        }
    }
#endif

#ifdef MR_USE_TRAIL
    #ifdef MR_THREAD_SAFE
        fprintf(fp, ", Trail: %.3fk,",
            ((char *) MR_trail_ptr -
            (char *) MR_CONTEXT(MR_ctxt_trail_zone)->MR_zone_min) / 1024.0
        );
    #else
        fprintf(fp, " Trail: %.3fk,",
            ((char *) MR_trail_ptr -
            (char *) MR_trail_zone->MR_zone_min) / 1024.0
        );
   #endif // !MR_THREAD_SAFE
#endif // !MR_USE_TRAIL

    // Print heap usage information.

#ifdef MR_CONSERVATIVE_GC
  #ifdef MR_BOEHM_GC
    fprintf(fp, "\n#GCs: %lu, ",
        (unsigned long) GC_get_gc_no());
    if (GC_mercury_calc_gc_time) {
        // Convert from unsigned long milliseconds to float seconds.
        fprintf(fp, "total GC time: %.2fs, ",
            (float) GC_total_gc_time / (float) 1000);
    }
    fprintf(fp, "Heap used since last GC: %.3fk, Total used: %.3fk",
        GC_get_bytes_since_gc() / 1024.0,
        GC_get_heap_size() / 1024.0
    );
  #endif
#else // !MR_CONSERVATIVE_GC
    fprintf(fp, "\nHeap: %.3fk",
        ((char *) MR_hp - (char *) eng->MR_eng_heap_zone->MR_zone_min) / 1024.0
    );
#endif // !MR_CONSERVATIVE_GC

#ifdef  MR_MPROF_PROFILE_MEMORY

    // Update the overall counter (this needs to be done first,
    // so that the percentages come out right).
    MR_update_memprof_counter(&MR_memprof_overall, &MR_overall_memprof_counter);

    // Print out the per-procedure memory profile (top N entries).
    num_table_entries = MR_memory_profile_top_table(MR_memprof_procs.root,
        table, MEMORY_PROFILE_SIZE, 0);
    fprintf(fp, "\nMemory profile by procedure\n");
    MR_memory_profile_report(fp, table, num_table_entries, MR_FALSE);

    // Print out the per-type memory profile (top N entries).
    num_table_entries = MR_memory_profile_top_table(MR_memprof_types.root,
        table, MEMORY_PROFILE_SIZE, 0);
    fprintf(fp, "\nMemory profile by type\n");
    MR_memory_profile_report(fp, table, num_table_entries, MR_FALSE);

    // Print out the overall memory usage.
    fprintf(fp, "Overall memory usage:"
        "+%8.8g %8.8g cells, +%8.8g %8.8g words\n",
        MR_overall_memprof_counter.cells_since_period_start,
        MR_overall_memprof_counter.cells_at_period_end,
        MR_overall_memprof_counter.words_since_period_start,
        MR_overall_memprof_counter.words_at_period_end
    );

#endif // MR_MPROF_PROFILE_MEMORY

    fprintf(fp, "]\n");
}

void
MR_report_full_memory_stats(FILE *fp)
{
#ifndef MR_MPROF_PROFILE_MEMORY
    fprintf(fp, "\nMemory profiling is not enabled.\n");
#else
    int                     num_table_entries;
    int                     table_size;
    MR_memprof_report_entry *table;

    // Update the overall counter (this needs to be done first,
    // so that the percentages come out right).
    MR_update_memprof_counter(&MR_memprof_overall, &MR_overall_memprof_counter);

    // Allocate space for the table,
    if (MR_memprof_procs.num_entries > MR_memprof_types.num_entries) {
        table_size = MR_memprof_procs.num_entries;
    } else {
        table_size = MR_memprof_types.num_entries;
    }
    table = MR_GC_NEW_ARRAY(MR_memprof_report_entry, table_size);

    // Print the by-procedure memory profile.
    num_table_entries = MR_memory_profile_fill_table(MR_memprof_procs.root,
        table, 0);
    qsort(table, MR_memprof_procs.num_entries, sizeof(MR_memprof_report_entry),
        MR_memory_profile_compare_final);
    fprintf(fp, "\nMemory profile by procedure\n");
    fprintf(fp, "%14s %14s  %s\n",
        "Cells", "Words", "Procedure label");
    MR_memory_profile_report(fp, table, num_table_entries, MR_TRUE);

    // Print the by-type memory profile.
    num_table_entries = MR_memory_profile_fill_table(MR_memprof_types.root,
        table, 0);
    qsort(table, MR_memprof_types.num_entries, sizeof(MR_memprof_report_entry),
        MR_memory_profile_compare_final);
    fprintf(fp, "\nMemory profile by type\n");
    fprintf(fp, "%14s %14s  %s\n",
        "Cells", "Words", "Procedure label");
    MR_memory_profile_report(fp, table, num_table_entries, MR_TRUE);

    // Deallocate space for the table.
    MR_GC_free(table);

    // Print the overall memory usage.
    fprintf(fp, "\nOverall memory usage: %8.8g cells, %8.8g words\n",
        MR_overall_memprof_counter.cells_at_period_end,
        MR_overall_memprof_counter.words_at_period_end
    );
#endif // MR_MPROF_PROFILE_MEMORY
}

#ifdef MR_MPROF_PROFILE_MEMORY

// MR_update_memprof_counter(counter, float_counter):
//
// Copy the data for a period from `counter' into `float_counter'
// (changing the format slightly as we go), and update `counter'
// to reflect the start of a new period.

static void
MR_update_memprof_counter(MR_memprof_counter *counter,
    MR_memprof_float_counter *float_counter)
{
    MR_add_two_dwords(counter->cells_at_period_start,
        counter->cells_since_period_start);
    MR_add_two_dwords(counter->words_at_period_start,
        counter->words_since_period_start);

    MR_convert_dword_to_double(counter->cells_since_period_start,
        float_counter->cells_since_period_start);
    MR_convert_dword_to_double(counter->words_since_period_start,
        float_counter->words_since_period_start);

    // Since the 'at start' numbers have already been incremented,
    // they now refer to the start of the *next* period.
    MR_convert_dword_to_double(counter->cells_at_period_start,
        float_counter->cells_at_period_end);
    MR_convert_dword_to_double(counter->words_at_period_start,
        float_counter->words_at_period_end);

    MR_zero_dword(counter->cells_since_period_start);
    MR_zero_dword(counter->words_since_period_start);
}

// Insert an entry into the table of the top `table_size' entries.
// Entries are ranked according to their words_since_period_start.
// (This is an arbitrary choice; we might equally well choose
// to order them by cells_since_period_start. I prefer words (zs).)
// Entries that are not in the top `table_size' are discarded.

static int
MR_insert_into_memprof_table(const MR_memprof_report_entry *new_entry,
    MR_memprof_report_entry *table, int table_size, int next_slot)
{
    int slot;

    // Ignore entries whose counts are zero (allowing for rounding).
    if (new_entry->counter.words_since_period_start < 1.0) {
        return next_slot;
    }

    // Find the slot where this entry should be inserted.
    // Start at the end and work backwards until we find
    // the start of the table or until we find a table
    // entry which ranks higher that the new entry.
    slot = next_slot;
    while (slot > 0 && table[slot - 1].counter.words_since_period_start
        < new_entry->counter.words_since_period_start)
    {
        slot--;
    }

    // If this entry fits in the table, then shuffle the displaced entries
    // to the right, insert the new entry in the table, and increment next_slot
    // (unless it is already at the end of the table).
    if (slot < table_size) {
#if 0
// The following code is disabled because it causes gcc (2.7.2) internal
// errors (``fixed or forbidden register spilled'') on x86 machines when
// using gcc global register variables.
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

// MR_memory_profile_top_table(node, table, table_size, next_slot):
//
// Insert the entries for `node' and its children into `table', which is
// big enough to hold the top `table_size' entries. `next_slot' specifies
// the number of entries currently in the table. Returns the new value
// of `next_slot'.

static int
MR_memory_profile_top_table(MR_memprof_record *node,
    MR_memprof_report_entry *table, int table_size, int next_slot)
{
    MR_memprof_report_entry new_entry;

    if (node != NULL) {
        next_slot = MR_memory_profile_top_table(node->left,
            table, table_size, next_slot);

        if (node->type_name != NULL) {
            new_entry.name = node->type_name;
        } else {
            new_entry.name = MR_lookup_entry_or_internal(node->proc);
        }
        MR_update_memprof_counter(&node->counter, &new_entry.counter);
        next_slot = MR_insert_into_memprof_table(&new_entry,
            table, table_size, next_slot);

        next_slot = MR_memory_profile_top_table(node->right,
            table, table_size, next_slot);
    }

    return next_slot;
}

// MR_memory_profile_fill_table(node, table, next_slot):
// Insert the entries for `node' and its children into `table', which the
// caller guarantees is big enough to hold them all. `next_slot' specifies
// the number of entries currently in the table. Returns the new value
// of `next_slot'.

static int
MR_memory_profile_fill_table(MR_memprof_record *node,
    MR_memprof_report_entry *table, int next_slot)
{
    if (node != NULL) {
        next_slot = MR_memory_profile_fill_table(node->left,
            table, next_slot);

        if (node->type_name != NULL) {
            table[next_slot].name = node->type_name;
        } else {
            table[next_slot].name = MR_lookup_entry_or_internal(node->proc);
        }
        MR_update_memprof_counter(&node->counter, &table[next_slot].counter);
        next_slot++;

        next_slot = MR_memory_profile_fill_table(node->right,
            table, next_slot);
    }
    return next_slot;
}

// MR_memory_profile_report(fp, table, num_entries, complete):
//
// Print out a profiling report for the specified table.

static void
MR_memory_profile_report(FILE *fp, const MR_memprof_report_entry *table,
    int num_entries, MR_bool complete)
{
    int         i;
    const char  *name;

    if (complete) {
        if (MR_overall_memprof_counter.cells_at_period_end < 1.0
        ||  MR_overall_memprof_counter.words_at_period_end < 1.0)
        {
            fprintf(fp, "no allocations to report\n");
            return;
        }
    } else {
        if (MR_overall_memprof_counter.cells_since_period_start < 1.0
        ||  MR_overall_memprof_counter.words_since_period_start < 1.0)
        {
            fprintf(fp, "no allocations to report\n");
            return;
        }
    }

    if (num_entries > MAX_REPORT_LINES && !complete) {
        num_entries = MAX_REPORT_LINES;
    }

    for (i = 0; i < num_entries; i++) {
        if (complete) {
            fprintf(fp, "%8.8g/%4.1f%% %8.8g/%4.1f%%  %s\n",
                table[i].counter.cells_at_period_end,
                100 * table[i].counter.cells_at_period_end /
                    MR_overall_memprof_counter.cells_at_period_end,
                table[i].counter.words_at_period_end,
                100 * table[i].counter.words_at_period_end /
                    MR_overall_memprof_counter.words_at_period_end,
                table[i].name
            );
        } else {
            fprintf(fp, "%8.8g/%4.1f%% %8.8g/%4.1f%%  %s\n",
                table[i].counter.cells_since_period_start,
                100 * table[i].counter.cells_since_period_start /
                   MR_overall_memprof_counter.cells_since_period_start,
                table[i].counter.words_since_period_start,
                100 * table[i].counter.words_since_period_start /
                   MR_overall_memprof_counter.words_since_period_start,
                table[i].name
            );
        }
    }
}

// Comparison routine used for qsort().
// Compares two MR_memprof_report_entry structures.

static int
MR_memory_profile_compare_final(const void *i1, const void *i2)
{
    const MR_memprof_report_entry *e1 = (const MR_memprof_report_entry *) i1;
    const MR_memprof_report_entry *e2 = (const MR_memprof_report_entry *) i2;

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

#endif // MR_MPROF_PROFILE_MEMORY
