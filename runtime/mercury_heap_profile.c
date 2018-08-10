// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997, 1999-2001, 2006, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// File: mercury_heap_profile.c.
// Main authors: zs, fjh, wangp.
//
// This module records information about the allocations of cells on the heap.
//
// The information recorded by this module is used by code in
// library/benchmarking.m.

////////////////////////////////////////////////////////////////////////////

#include "mercury_imp.h"

#include <stdio.h>
#include <string.h>

#include "mercury_prof_mem.h"
#include "mercury_dword.h"
#include "mercury_heap_profile.h"
#include "mercury_runtime_util.h"

// All fields of these variables are initialized to 0.
MR_memprof_counter  MR_memprof_overall;
MR_memprof_table    MR_memprof_procs;
MR_memprof_table    MR_memprof_types;

// Private global variables.
static MR_bool      profile_heap = MR_TRUE;

// Initialize a heap profiling counter.

static void
MR_init_counter(MR_memprof_counter *counter)
{
    MR_zero_dword(counter->cells_at_period_start);
    MR_zero_dword(counter->words_at_period_start);
    MR_zero_dword(counter->cells_since_period_start);
    MR_zero_dword(counter->words_since_period_start);
}

// Increment the fields in a heap profiling counter to record the allocation
// of a single cell of `size' words.

static void
MR_increment_counter(MR_memprof_counter *counter, int size)
{
    MR_increment_dword(counter->cells_since_period_start, 1);
    MR_increment_dword(counter->words_since_period_start, size);
}

// Search the specified `table' to find the entry for the given `name'
// allocating one if there isn't one already, and then increment
// the counters for that entry for an allocation of the specified `size'.

static void
MR_increment_table_entry(MR_memprof_table *table,
    const MR_Code *proc, const char *type_name, int size)
{
    MR_bool             found;
    MR_Integer          diff;
    MR_memprof_record   **node_addr;
    MR_memprof_record   *node;

    // Search the tree either by procedure address or by type name.

    found = MR_FALSE;
    node_addr = &table->root;
    if (proc != NULL) {
        while ((node = *node_addr) != NULL) {
            // The casts to MR_Integer are so that we work with C compilers
            // that do not support arithmetic with void pointers.

            diff = (MR_Integer)proc - (MR_Integer)node->proc;
            if (diff < 0) {
                node_addr = &node->left;
            } else if (diff > 0) {
                node_addr = &node->right;
            } else {
                found = MR_TRUE;
                break;
            }
        }
    } else {
        while ((node = *node_addr) != NULL) {
            diff = strcmp(type_name, node->type_name);
            if (diff < 0) {
                node_addr = &node->left;
            } else if (diff > 0) {
                node_addr = &node->right;
            } else {
                found = MR_TRUE;
                break;
            }
        }
    }

    // If the tree didn't already contain a node with this procedure address or
    // type name, create a new node for it.

    if (!found) {
        node = MR_PROF_NEW(MR_memprof_record);
        // We need to make a fresh copy of the name, rather than just copying
        // the pointer, because our caller may deallocate its copy of the name.
        // Normally the name will be a string literal, but even then it might
        // be a string literal from a dlopen()'ed module which will later
        // get dlclose()'d.

        if (type_name != NULL) {
            size_t len = strlen(type_name);
            char *copy_of_name = MR_PROF_NEW_ARRAY(char, len + 1);
            MR_memcpy(copy_of_name, type_name, len + 1);
            node->type_name = copy_of_name;
        } else {
            node->type_name = NULL;
        }
        node->proc = proc;
        node->left = NULL;
        node->right = NULL;
        MR_init_counter(&node->counter);

        *node_addr = node;

        table->num_entries++;
    }

    // Now record the counts in this node.
    MR_increment_counter(&node->counter, size);
}

// Record heap profiling information for an allocation of size `size'
// in procedure `proc' for an object of type `type'.

void
MR_record_allocation(int size, const MR_AllocSiteInfoPtr alloc_id,
    const char *type)
{
    if (!profile_heap || alloc_id == NULL || alloc_id->MR_asi_proc == NULL) {
        return;
    }

    // Increment the overall totals,
    // record the allocation in the per-procedure table, and
    // record the allocation in the per-type table.

    MR_increment_counter(&MR_memprof_overall, size);
    MR_increment_table_entry(&MR_memprof_procs, alloc_id->MR_asi_proc, NULL,
        size);
    if (type == NULL) {
        type = alloc_id->MR_asi_type;
        if (type == NULL) {
            type = "unknown";
        }
    }
    MR_increment_table_entry(&MR_memprof_types, NULL, type, size);
}

void
MR_prof_turn_on_heap_profiling(void)
{
    profile_heap = MR_TRUE;
}

void
MR_prof_turn_off_heap_profiling(void)
{
    profile_heap = MR_FALSE;
}

////////////////////////////////////////////////////////////////////////////
// Memory attribution profiling.
//
// Memory attribution profiling actually bears no particular relationship to
// regular memory profiling, but in the interests of reducing the number of
// grades both are lumped under the `--memory-profiling' option.
//
// For every memory cell, we allocate an extra memory word at the front of the
// object which points to an MR_AllocSiteInfo structure, indicating the
// procedure which allocated the cell, its type, and its true (desired) size.
// We call that the "attribution".
//
// When `benchmarking.report_memory_attribution' is called, we force a GC.
// Using hooks inserted into Boehm GC, a function is called for every live
// memory object on the heap. During that callback, we increment the counters
// in a hash table (for attributed objects), or counters in a binary tree (for
// unattributed objects, or Mercury runtime objects that we don't care to
// distinguish).

#ifdef  MR_MPROF_PROFILE_MEMORY_ATTRIBUTION

typedef struct MR_AttribCount_Struct    MR_AttribCount;
typedef struct MR_VarSizeCount_Struct   MR_VarSizeCount;

// Counts for attributed objects.

struct MR_AttribCount_Struct {
    unsigned                MR_atc_id;
    MR_AllocSiteInfo const  *MR_atc_alloc_site;
    MR_Unsigned             MR_atc_num_cells;
    MR_Unsigned             MR_atc_num_words;
};

// Objects which are unattributed, or explicitly attributed as runtime
// structures, may come in many different sizes. We store the counters for
// each different size as a separate node in a binary search tree.

struct MR_VarSizeCount_Struct {
    size_t              MR_vsc_size;
    MR_Unsigned         MR_vsc_count;
    MR_VarSizeCount     *MR_vsc_left;
    MR_VarSizeCount     *MR_vsc_right;
};

#define SNAPSHOTS_FILENAME              "Prof.Snapshots"
#define KNOWN_COUNT_TABLE_INITIAL_SIZE  (1 << 8)    // power of two

static MR_AttribCount   *attrib_count_table;
static size_t           attrib_count_table_size;
static size_t           attrib_count_table_used;
static MR_VarSizeCount  *runtime_count_tree;
static MR_VarSizeCount  *unknown_count_tree;
static int              snapshot_counter;
static FILE             *snapshot_file;

static void     add_attrib_count_entry(MR_AttribCount *table,
                    size_t table_size, size_t *table_used, unsigned id,
                    const MR_AllocSiteInfo *alloc_site);
static void     rehash_attrib_count_table(void);
static unsigned hash_addr(MR_Word key);
static void * GC_CALLBACK enumerate_reachable_objects_locked(void *data);
static GC_CALLBACK void
                reachable_object_callback(void *p, size_t bytes, void *data);
static MR_bool  increment_attrib_count(MR_Word addr, unsigned num_words);
static void     increment_var_size_count(MR_VarSizeCount **node, size_t words);
static void     finish_reachable_report(const char *label);
static void     write_attrib_counts(FILE *fp, MR_AttribCount *table,
                    size_t table_size);
static void     write_var_size_counts(FILE *fp, const char *prefix,
                    MR_VarSizeCount *node);
static const char *maybe_filename(const char *s);

#define MR_NUM_BUILTIN_ALLOC_SITES  9

MR_AllocSiteInfo MR_builtin_alloc_sites[MR_NUM_BUILTIN_ALLOC_SITES] = {
    // These must match the macros in mercury_memory.h.
    { NULL, "runtime", 0, "<runtime structs>",  0 },
    { NULL, "unknown", 0, "float.float/0",      MR_FLOAT_WORDS },
    { NULL, "unknown", 0, "string.string/0",    0 },
    { NULL, "unknown", 0, "type_info/0",        0 },
    { NULL, "unknown", 0, "<foreign>",          0 },
    { NULL, "unknown", 0, "<tabling structs>",  0 },
    { NULL, "unknown", 0, "<stm structs>",      0 },
    { NULL, "unknown", 0, "int64.int64/0",      MR_INT64_WORDS },
    { NULL, "unknown", 0, "uint64.uint64/0",    MR_UINT64_WORDS }
};

void
MR_register_alloc_sites(const MR_AllocSiteInfo *alloc_sites, int size)
{
    size_t      bytes;
    unsigned    id;
    int         i;

    if (attrib_count_table == NULL) {
        // We must not use GC allocation here.
        attrib_count_table_size = KNOWN_COUNT_TABLE_INITIAL_SIZE;
        bytes = attrib_count_table_size * sizeof(MR_AttribCount);
        attrib_count_table = MR_malloc(bytes);
        memset(attrib_count_table, 0, bytes);

        MR_register_alloc_sites(MR_builtin_alloc_sites,
            MR_NUM_BUILTIN_ALLOC_SITES);
    }

    for (i = 0; i < size; i++) {
        // Enlarge the hash table if necessary.
        if (attrib_count_table_size > 0 &&
            2 * attrib_count_table_used >= attrib_count_table_size)
        {
            rehash_attrib_count_table();
        }

        id = attrib_count_table_used + 1;
        add_attrib_count_entry(attrib_count_table, attrib_count_table_size,
            &attrib_count_table_used, id, &alloc_sites[i]);
    }
}

static void
add_attrib_count_entry(MR_AttribCount *table, size_t table_size,
    size_t *table_used, unsigned id, const MR_AllocSiteInfo *alloc_site)
{
    MR_AttribCount   *entry;
    unsigned        i;

    i = hash_addr((MR_Word) alloc_site) & (table_size - 1);
    for (;;) {
        assert(i < table_size);
        entry = &table[i];
        if (entry->MR_atc_alloc_site == alloc_site) {
            break;
        }
        if (entry->MR_atc_alloc_site == NULL) {
            entry->MR_atc_id = id;
            entry->MR_atc_alloc_site = alloc_site;
            (*table_used)++;
            break;
        }
        i = (i + 1) & (table_size - 1);
    }
}

static void
rehash_attrib_count_table(void)
{
    MR_AttribCount    *new_table;
    size_t     new_size;
    size_t     new_used;
    size_t     i;

    new_size = attrib_count_table_size * 2;
    new_table = MR_malloc(new_size * sizeof(MR_AttribCount));
    memset(new_table, 0, new_size * sizeof(MR_AttribCount));

    new_used = 0;
    for (i = 0; i < attrib_count_table_size; i++) {
        if (attrib_count_table[i].MR_atc_alloc_site != NULL) {
            add_attrib_count_entry(new_table, new_size, &new_used,
                attrib_count_table[i].MR_atc_id,
                attrib_count_table[i].MR_atc_alloc_site);
        }
    }

    MR_free(attrib_count_table);

    attrib_count_table_size = new_size;
    attrib_count_table = new_table;
    assert(attrib_count_table_used == new_used);
}

// http://www.concentric.net/~ttwang/tech/inthash.htm
static unsigned
hash_addr(MR_Word key)
{
    if (sizeof(MR_Word) == 4) {
        unsigned c2 = 0x27d4eb2d; // a prime or an odd constant
        key = (key ^ 61) ^ (key >> 16);
        key = key + (key << 3);
        key = key ^ (key >> 4);
        key = key * c2;
        key = key ^ (key >> 15);
        return key;
    } else {
        key = (~key) + (key << 18);
        key = key ^ (key >> 31);
        key = key * 21;
        key = key ^ (key >> 11);
        key = key + (key << 6);
        key = key ^ (key >> 22);
        return (unsigned) key;
    }
}

void
MR_report_memory_attribution(const char *label, MR_bool run_collect)
{
#ifdef MR_BOEHM_GC
  #ifndef MR_HIGHLEVEL_CODE
    // Clear out the stacks and registers before garbage collecting.
    MR_clear_zone_for_GC(MR_CONTEXT(MR_ctxt_detstack_zone), MR_sp + 1);
    MR_clear_zone_for_GC(MR_CONTEXT(MR_ctxt_nondetstack_zone),
        MR_maxfr + 1);
    MR_clear_regs_for_GC();
  #endif

    if (run_collect) {
        GC_gcollect();
    }

    GC_call_with_alloc_lock(enumerate_reachable_objects_locked, NULL);
    finish_reachable_report(label);
#endif
}

static void * GC_CALLBACK enumerate_reachable_objects_locked(void *data)
{
    GC_enumerate_reachable_objects_inner(reachable_object_callback, NULL);
    return NULL;
}

static GC_CALLBACK void
reachable_object_callback(void *p, size_t bytes, void *data)
{
    MR_Word addr;
    unsigned words = bytes / MR_BYTES_PER_WORD;

    addr = ((MR_Word *) p)[0];

    if ((void *) addr == MR_ALLOC_SITE_RUNTIME) {
        increment_var_size_count(&runtime_count_tree, words);
        return;
    }

    if (addr == (MR_Word) NULL ||
        !increment_attrib_count(addr, words))
    {
        increment_var_size_count(&unknown_count_tree, words);
    }
}

static MR_bool
increment_attrib_count(MR_Word addr, unsigned num_words)
{
    MR_AttribCount   *entry;
    MR_Unsigned     orig;
    MR_Unsigned     i;

    orig = i = hash_addr(addr) & (attrib_count_table_size - 1);
    do {
        assert(i < attrib_count_table_size);
        entry = &attrib_count_table[i];
        if ((MR_Word) entry->MR_atc_alloc_site == addr) {
            entry->MR_atc_num_cells++;
            entry->MR_atc_num_words += num_words;
            return MR_TRUE;
        }
        if (entry->MR_atc_alloc_site == NULL) {
            return MR_FALSE;
        }
        i = (i + 1) & (attrib_count_table_size - 1);
    } while (i != orig);

    return MR_FALSE;
}

static void
increment_var_size_count(MR_VarSizeCount **node, size_t words)
{
    while (*node != NULL) {
        if ((*node)->MR_vsc_size == words) {
            (*node)->MR_vsc_count++;
            return;
        } else if (words < (*node)->MR_vsc_size) {
            node = &(*node)->MR_vsc_left;
        } else {
            node = &(*node)->MR_vsc_right;
        }
    }

    // We must not use GC allocation here.
    *node = MR_NEW(MR_VarSizeCount);
    (*node)->MR_vsc_size = words;
    (*node)->MR_vsc_count = 1;
    (*node)->MR_vsc_left = NULL;
    (*node)->MR_vsc_right = NULL;
}

static void
finish_reachable_report(const char *label)
{
    if (snapshot_file == NULL) {
        snapshot_file = MR_checked_fopen(SNAPSHOTS_FILENAME, "create", "w");
    }

    snapshot_counter++;
    fprintf(snapshot_file, "start [%d] %s\n", snapshot_counter, label);

    write_attrib_counts(snapshot_file, attrib_count_table,
        attrib_count_table_size);
    write_var_size_counts(snapshot_file, "runtime", runtime_count_tree);
    write_var_size_counts(snapshot_file, "unknown", unknown_count_tree);

    fprintf(snapshot_file, "end [%d] %s\n", snapshot_counter, label);
}

static void
write_attrib_counts(FILE *fp, MR_AttribCount *table, size_t table_size)
{
    size_t i;

    for (i = 0; i < table_size; i++) {
        if (table[i].MR_atc_alloc_site != NULL &&
            table[i].MR_atc_num_cells != 0)
        {
            fprintf(fp, "%u "
                "%" MR_INTEGER_LENGTH_MODIFIER "u "
                "%" MR_INTEGER_LENGTH_MODIFIER "u\n",
                table[i].MR_atc_id,
                table[i].MR_atc_num_cells,
                table[i].MR_atc_num_words);

            table[i].MR_atc_num_cells = 0;
            table[i].MR_atc_num_words = 0;
        }
    }
}

static void
write_var_size_counts(FILE *fp, const char *prefix, MR_VarSizeCount *node)
{
    while (node != NULL) {
        write_var_size_counts(fp, prefix, node->MR_vsc_left);

        if (node->MR_vsc_count != 0) {
            fprintf(fp, "%s "
                "%" MR_INTEGER_LENGTH_MODIFIER "u "
                "%" MR_INTEGER_LENGTH_MODIFIER "u\n",
                prefix,
                node->MR_vsc_count,
                (MR_Unsigned) node->MR_vsc_size);
            node->MR_vsc_count = 0;
        }

        node = node->MR_vsc_right;
    }
}

void
MR_finish_prof_snapshots_file(void)
{
    FILE                    *fp;
    const MR_AllocSiteInfo  *site;
    size_t                  i;

    if (!(fp = snapshot_file)) {
        return;
    }

    fprintf(fp, "size_map");
    GC_mercury_write_size_map(fp);
    fprintf(fp, "\n");

    for (i = 0; i < attrib_count_table_size; i++) {
        site = attrib_count_table[i].MR_atc_alloc_site;
        if (site != NULL) {
            fprintf(fp, "%u\t", attrib_count_table[i].MR_atc_id);
            fprintf(fp, "%s\t", MR_lookup_entry_or_internal(site->MR_asi_proc));
            fprintf(fp, "%s\t", maybe_filename(site->MR_asi_file_name));
            fprintf(fp, "%d\t", site->MR_asi_line_number);
            fprintf(fp, "%s\t", site->MR_asi_type);
            fprintf(fp, "%d\n", site->MR_asi_words);
        }
    }

    MR_checked_fclose(snapshot_file, SNAPSHOTS_FILENAME);
    snapshot_file = NULL;
}

static const char *
maybe_filename(const char *s)
{
    if (s == NULL || s[0] == '\0') {
        return "(unknown)";
    } else {
        return s;
    }
}

#endif  // MR_MPROF_PROFILE_MEMORY_ATTRIBUTION

////////////////////////////////////////////////////////////////////////////
