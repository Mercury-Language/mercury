// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2007, 2011 The University of Melbourne.
// Copyright (C) 2014, 2016-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module contains the functions related to tabling that are not
// specific to minimal model tabling.

#include "mercury_imp.h"

#include "mercury_type_info.h"
#include "mercury_array_macros.h"
#include "mercury_builtin_types.h"
#include "mercury_deconstruct.h"
#include "mercury_deconstruct_macros.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef MR_TABLE_DEBUG
static  void    MR_table_assert_failed(const char *file, unsigned line);

#define MR_table_assert(cond)                                               \
    do {                                                                    \
        if (! (cond)) {                                                     \
            MR_table_assert_failed(__FILE__, __LINE__);                     \
        }                                                                   \
    } while (0)
#else
#define MR_table_assert(cond)  ((void) 0)
#endif

////////////////////////////////////////////////////////////////////////////

// This part deals with tabling using resizable hash tables.

// All hash table slot structures have the same fields, since they are
// manipulated by the same macro (MR_GENERIC_HASH_LOOKUP_OR_ADD).
// The variable size part is at the end, in order to make all the offsets
// the same.

typedef struct MR_IntHashTableSlot_Struct       MR_IntHashTableSlot;
typedef struct MR_Int64HashTableSlot_Struct     MR_Int64HashTableSlot;
typedef struct MR_UInt64HashTableSlot_Struct    MR_UInt64HashTableSlot;
typedef struct MR_FloatHashTableSlot_Struct     MR_FloatHashTableSlot;
typedef struct MR_StringHashTableSlot_Struct    MR_StringHashTableSlot;
typedef struct MR_BitmapHashTableSlot_Struct    MR_BitmapHashTableSlot;
typedef struct MR_WordHashTableSlot_Struct      MR_WordHashTableSlot;

typedef struct MR_AllocRecord_Struct            MR_AllocRecord;

struct MR_IntHashTableSlot_Struct {
    MR_IntHashTableSlot     *next;
    MR_TableNode            data;
    MR_Integer              key;
};

struct MR_FloatHashTableSlot_Struct {
    MR_FloatHashTableSlot   *next;
    MR_TableNode            data;
    MR_Float                key;
};

struct MR_Int64HashTableSlot_Struct {
    MR_Int64HashTableSlot   *next;
    MR_TableNode            data;
    int64_t                 key;
};

struct MR_UInt64HashTableSlot_Struct {
    MR_UInt64HashTableSlot  *next;
    MR_TableNode            data;
    uint64_t                key;
};

struct MR_StringHashTableSlot_Struct {
    MR_StringHashTableSlot  *next;
    MR_TableNode            data;
    MR_ConstString          key;
};

struct MR_BitmapHashTableSlot_Struct {
    MR_BitmapHashTableSlot *next;
    MR_TableNode            data;
    MR_ConstBitmapPtr    key;
};

struct MR_WordHashTableSlot_Struct {
    MR_WordHashTableSlot    *next;
    MR_TableNode            data;
    MR_Word                 key;
};

typedef union {
    MR_IntHashTableSlot     *int_slot_ptr;
    MR_Int64HashTableSlot   *int64_slot_ptr;
    MR_UInt64HashTableSlot  *uint64_slot_ptr;
    MR_FloatHashTableSlot   *float_slot_ptr;
    MR_StringHashTableSlot  *string_slot_ptr;
    MR_BitmapHashTableSlot  *bitmap_slot_ptr;
    MR_WordHashTableSlot    *word_slot_ptr;
} MR_HashTableSlotPtr;

struct MR_AllocRecord_Struct {
    MR_HashTableSlotPtr     chunk;
    MR_AllocRecord          *next;
};

// Our hash table design uses separate chaining to avoid the bad worst case
// behavior of open addressing. This is important, because the worst case
// can be expected to occur reasonably often in tabling workloads. The reason
// is that successive queries are not independent. Often, query N is a
// recursive call made from query N-1, which means that its input values are
// much more likely to fall into the same or next hash bucket than an
// independent query's input values would, especially for integer values.
// Repeated over many queries, such input pattern can give rise to "convoys",
// long sequences of occupied hash table slots. Any input value whose search
// for a free slot runs into the convoy will have very long search time.
//
// The `hash_table' field points to an array of `size' slots, each of which
// is a pointer to a hash table slot; hash table slots have embedded `next'
// pointers to chain together all the values that hash to the same value.
//
// To keep maximum chain lengths bounded (in a statistical sense), we record
// the number of values in the table (in the `value_count' field), and when
// this exceeds a certain fraction of the size of the hash table, we increase
// the size of the hash table and rehash all the existing entries. We do this
// when the value in the `value_count' field exceeds the one in the `threshold'
// field, which is set to `size' times MAX_LOAD_FACTOR whenever the size
// is changed. (This avoids a float multiplication on each insertion.)
//
// The reason why the hash table array contains pointers to slots instead of
// the slots themselves is that the latter would require the addresses of some
// hash table slots (those in the array itself and not in a chain) to change
// when the table is resized. As for why this is bad, see the documentation
// of the MR_TableNode type in mercury_tabling.h.
//
// To avoid calling GC_malloc on each insertion, we allocate memory in chunks,
// with each chunk containing CHUNK_SIZE hash table slots. The `freeleft'
// field contains count of the number of hash table slots left in the space
// allocated but not yet used; the `freespace' field point to the first
// of these slots.
//
// This design leads to pointers into the middle of GC_malloc'd memory.
// To make sure that the code works even without the boehm gc being compiled
// with interior pointers, we retain pointers to all the chunks we have
// allocated in the `allocrecord' field. This field has no purpose other than
// to serve as roots for boehm gc.

struct MR_HashTable_Struct {
    MR_Integer              size;
    MR_Integer              threshold;
    MR_Integer              value_count;
    MR_HashTableSlotPtr     *hash_table;
    MR_HashTableSlotPtr     freespace;
    MR_Integer              freeleft;
    MR_AllocRecord          *allocrecord;
};

#define CHUNK_SIZE      256
#define MAX_LOAD_FACTOR 0.65

// Prime numbers which are close to powers of 2. Used for choosing
// the next size for a hash table.

#define NUM_OF_PRIMES 16
static MR_Word primes[NUM_OF_PRIMES] = {
    127,
    257,
    509,
    1021,
    2053,
    4099,
    8191,
    16381,
    32771,
    65537,
    131071,
    262147,
    524287,
    1048573,
    2097143,
    4194301
};

// Initial size of a new table.
#define HASH_TABLE_START_SIZE primes[0]

static  MR_Integer      next_prime(MR_Integer);

// Return the next prime number greater than the number received.
// If no such prime number can be found, compute an approximate one.

static MR_Integer
next_prime(MR_Integer old_size)
{
    int i;

    i = 0;
    while ( (old_size >= primes[i]) && (i < NUM_OF_PRIMES) ) {
        i++;
    }

    if (i < NUM_OF_PRIMES) {
        return primes[i];
    } else {
        return 2 * old_size - 1;
    }
}

// The MR_GENERIC_HASH_LOOKUP_OR_ADD macro is intended to be the body of
// a function that looks to see if the given key is in the given hash table.
// If it is, it returns the address of the data pointer associated with
// the key. If it is not, it creates a new slot for the key in the table
// and returns the address of its data pointer.
//
// It in turn relies on three groups of macros to perform part of the task.
//
// The first group optionally records statistics about the number of successful
// and unsuccessful searches, and the number of key comparisons they needed.
// From this information, one can compute the average successful and
// unsuccessful search lengths. These macros are defined and undefined in the
// files mercury_tabling_stats_{defs,nodefs,undefs}.h.
//
// The second optionally prints debugging messages.
//
// The third implements the initial creation of the hash table.

#ifdef  MR_TABLE_DEBUG
  #define debug_key_msg(keyvalue, keyformat, keycast)                         \
        do {                                                                  \
            if (MR_hashdebug) {                                               \
                printf("HT search key " keyformat "\n",                       \
                    (keycast) keyvalue);                                      \
            }                                                                 \
        } while (0)

  #define debug_resize_msg(oldsize, newsize, newthreshold)                    \
        do {                                                                  \
            if (MR_hashdebug) {                                               \
                printf("HT expanding table from %d to %d(%d)\n",              \
                    (oldsize), (newsize), (newthreshold));                    \
            }                                                                 \
        } while (0)

  #define debug_rehash_msg(rehash_bucket)                                     \
        do {                                                                  \
            if (MR_hashdebug) {                                               \
                printf("HT rehashing bucket: %d\n",                           \
                    (rehash_bucket));                                         \
            }                                                                 \
        } while (0)

  #define debug_key_compare_msg(home_bucket)                                  \
        do {                                                                  \
            if (MR_hashdebug) {                                               \
                printf("HT comparing keys in bucket: %d\n",                   \
                    (home_bucket));                                           \
            }                                                                 \
        } while (0)

  #define debug_lookup_msg(home_bucket)                                       \
        do {                                                                  \
            if (MR_hashdebug) {                                               \
                printf("HT search successful in bucket: %d\n",                \
                    (home_bucket));                                           \
            }                                                                 \
        } while (0)

  #define debug_insert_msg(home_bucket)                                       \
        do {                                                                  \
            if (MR_hashdebug) {                                               \
                printf("HT search unsuccessful in bucket: %d\n",              \
                    (home_bucket));                                           \
            }                                                                 \
        } while (0)
#else
  #define debug_key_msg(keyvalue, keyformat, keycast)           ((void) 0)
  #define debug_resize_msg(oldsize, newsize, newthreshold)      ((void) 0)
  #define debug_rehash_msg(rehash_bucket)                       ((void) 0)
  #define debug_key_compare_msg(home_bucket)                    ((void) 0)
  #define debug_lookup_msg(home_bucket)                         ((void) 0)
  #define debug_insert_msg(home_bucket)                         ((void) 0)
#endif

// The MR_GENERIC_HASH_LOOKUP_OR_ADD macro, and its helper macro
// MR_CREATE_HASH_TABLE implement the bodies of the following functions:
//
// MR_int_hash_lookup_or_add
// MR_int_hash_lookup_or_add_stats
// MR_int_hash_lookup
// MR_float_hash_lookup_or_add
// MR_float_hash_lookup_or_add_stats
// MR_float_hash_lookup
// MR_string_hash_lookup_or_add
// MR_string_hash_lookup_or_add_stats
// MR_string_hash_lookup

#define MR_CREATE_HASH_TABLE(table_ptr, table_type, table_field, table_size)\
    do {                                                                    \
        MR_Word         i;                                                  \
        MR_HashTable    *newtable;                                          \
                                                                            \
        newtable = MR_TABLE_NEW(MR_HashTable);                              \
                                                                            \
        newtable->size = table_size;                                        \
        newtable->threshold = (MR_Integer)                                  \
            ((float) table_size * MAX_LOAD_FACTOR);                         \
        newtable->value_count = 0;                                          \
        newtable->freespace.table_field = NULL;                             \
        newtable->freeleft = 0;                                             \
        newtable->allocrecord = NULL;                                       \
        newtable->hash_table = MR_TABLE_NEW_ARRAY(MR_HashTableSlotPtr,      \
            table_size);                                                    \
                                                                            \
        for (i = 0; i < table_size; i++) {                                  \
            newtable->hash_table[i].table_field = NULL;                     \
        }                                                                   \
                                                                            \
        table_ptr = newtable;                                               \
    } while (0)

MR_TrieNode
MR_int_hash_lookup_or_add(MR_TrieNode t, MR_Integer key)
{
#define key_format              "ld"
#define key_cast                long
#define table_type              MR_IntHashTableSlot
#define table_field             int_slot_ptr
#define hash(key)               (key)
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_int_hash_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, MR_Integer key)
{
#define key_format              "ld"
#define key_cast                long
#define table_type              MR_IntHashTableSlot
#define table_field             int_slot_ptr
#define hash(key)               (key)
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_defs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_int_hash_lookup(MR_TrieNode t, MR_Integer key)
{
#define key_format              "ld"
#define key_cast                long
#define table_type              MR_IntHashTableSlot
#define table_field             int_slot_ptr
#define hash(key)               (key)
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_TRUE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_int64_hash_lookup_or_add(MR_TrieNode t, int64_t key)
{
#define key_format              PRId64
#define key_cast                int64_t
#define table_type              MR_Int64HashTableSlot
#define table_field             int64_slot_ptr
#define hash(key)               (MR_hash_int64(key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_int64_hash_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, int64_t key)
{
#define key_format              PRId64
#define key_cast                int64_t 
#define table_type              MR_Int64HashTableSlot
#define table_field             int64_slot_ptr
#define hash(key)               (MR_hash_int64(key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_defs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_int64_hash_lookup(MR_TrieNode t, int64_t key)
{
#define key_format              PRId64
#define key_cast                int64_t 
#define table_type              MR_Int64HashTableSlot
#define table_field             int64_slot_ptr
#define hash(key)               (MR_hash_int64(key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_TRUE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_uint64_hash_lookup_or_add(MR_TrieNode t, uint64_t key)
{
#define key_format              PRIu64
#define key_cast                uint64_t
#define table_type              MR_UInt64HashTableSlot
#define table_field             uint64_slot_ptr
#define hash(key)               (MR_hash_uint64(key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_uint64_hash_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, uint64_t key)
{
#define key_format              PRIu64
#define key_cast                uint64_t 
#define table_type              MR_UInt64HashTableSlot
#define table_field             uint64_slot_ptr
#define hash(key)               (MR_hash_uint64(key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_defs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_uint64_hash_lookup(MR_TrieNode t, uint64_t key)
{
#define key_format              PRIu64
#define key_cast                uint64_t 
#define table_type              MR_UInt64HashTableSlot
#define table_field             uint64_slot_ptr
#define hash(key)               (MR_hash_uint64(key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_TRUE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

// Note that the equal_keys operation should compare two floats for
// bit-for-bit equality. This is different from the usual == operator
// in the presence of NaNs, infinities, etc.

MR_TrieNode
MR_float_hash_lookup_or_add(MR_TrieNode t, MR_Float key)
{
#define key_format              "f"
#define key_cast                double
#define table_type              MR_FloatHashTableSlot
#define table_field             float_slot_ptr
#define hash(key)               (MR_hash_float(key))
#define equal_keys(k1, k2)      (memcmp(&(k1), &(k2), sizeof(MR_Float)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_float_hash_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, MR_Float key)
{
#define key_format              "f"
#define key_cast                double
#define table_type              MR_FloatHashTableSlot
#define table_field             float_slot_ptr
#define hash(key)               (MR_hash_float(key))
#define equal_keys(k1, k2)      (memcmp(&(k1), &(k2), sizeof(MR_Float)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_defs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_float_hash_lookup(MR_TrieNode t, MR_Float key)
{
#define key_format              "f"
#define key_cast                double
#define table_type              MR_FloatHashTableSlot
#define table_field             float_slot_ptr
#define hash(key)               (MR_hash_float(key))
#define equal_keys(k1, k2)      (memcmp(&(k1), &(k2), sizeof(MR_Float)) == 0)
#define lookup_only             MR_TRUE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_string_hash_lookup_or_add(MR_TrieNode t, MR_ConstString key)
{
#define key_format              "s"
#define key_cast                const char *
#define table_type              MR_StringHashTableSlot
#define table_field             string_slot_ptr
#define hash(key)               (MR_hash_string(key))
#define equal_keys(k1, k2)      (MR_strtest((k1), (k2)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_string_hash_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, MR_ConstString key)
{
#define key_format              "s"
#define key_cast                const char *
#define table_type              MR_StringHashTableSlot
#define table_field             string_slot_ptr
#define hash(key)               (MR_hash_string(key))
#define equal_keys(k1, k2)      (MR_strtest((k1), (k2)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_defs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_string_hash_lookup(MR_TrieNode t, MR_ConstString key)
{
#define key_format              "s"
#define key_cast                const char *
#define table_type              MR_StringHashTableSlot
#define table_field             string_slot_ptr
#define hash(key)               (MR_hash_string(key))
#define equal_keys(k1, k2)      (MR_strtest((k1), (k2)) == 0)
#define lookup_only             MR_TRUE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_bitmap_hash_lookup_or_add(MR_TrieNode t, MR_ConstBitmapPtr key)
{
#define key_format              "d"
#define key_cast                void *
#define table_type              MR_BitmapHashTableSlot
#define table_field             bitmap_slot_ptr
#define hash(key)               (MR_hash_bitmap(key))
#define equal_keys(k1, k2)      (MR_bitmap_cmp((k1), (k2)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_bitmap_hash_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, MR_ConstBitmapPtr key)
{
#define key_format              "d"
#define key_cast                MR_Word
#define table_type              MR_BitmapHashTableSlot
#define table_field             bitmap_slot_ptr
#define hash(key)               (MR_hash_bitmap(key))
#define equal_keys(k1, k2)      (MR_bitmap_cmp((k1), (k2)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_defs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_bitmap_hash_lookup(MR_TrieNode t, MR_ConstBitmapPtr key)
{
#define key_format              "d"
#define key_cast                MR_Word
#define table_type              MR_BitmapHashTableSlot
#define table_field             bitmap_slot_ptr
#define hash(key)               (MR_hash_bitmap(key))
#define equal_keys(k1, k2)      (MR_bitmap_cmp((k1), (k2)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_word_hash_lookup_or_add(MR_TrieNode t, MR_Word key)
{
#define key_format              "d"
#define key_cast                MR_Word
#define table_type              MR_WordHashTableSlot
#define table_field             word_slot_ptr
#define hash(key)               ((long) (key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_word_hash_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, MR_Word key)
{
#define key_format              "d"
#define key_cast                MR_Word
#define table_type              MR_WordHashTableSlot
#define table_field             word_slot_ptr
#define hash(key)               ((long) (key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_tabling_stats_defs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

MR_TrieNode
MR_word_hash_lookup(MR_TrieNode t, MR_Word key)
{
#define key_format              "d"
#define key_cast                MR_Word
#define table_type              MR_WordHashTableSlot
#define table_field             word_slot_ptr
#define hash(key)               ((long) (key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_TRUE
#include "mercury_tabling_stats_nodefs.h"
#include "mercury_hash_lookup_or_add_body.h"
#include "mercury_tabling_stats_undefs.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

static int
MR_cmp_ints(const void *p1, const void *p2)
{
    MR_Integer  i1 = * (MR_Integer *) p1;
    MR_Integer  i2 = * (MR_Integer *) p2;

    if (i1 < i2) {
        return -1;
    } else if (i1 > i2) {
        return 1;
    } else {
        return 0;
    }
}

static int
MR_cmp_floats(const void *p1, const void *p2)
{
    MR_Float  f1 = * (MR_Float *) p1;
    MR_Float  f2 = * (MR_Float *) p2;

    if (f1 < f2) {
        return -1;
    } else if (f1 > f2) {
        return 1;
    } else {
        return 0;
    }
}

static int
MR_cmp_strings(const void *p1, const void *p2)
{
    MR_ConstString  s1 = * (MR_ConstString *) p1;
    MR_ConstString  s2 = * (MR_ConstString *) p2;

    return strcmp(s1, s2);
}

static int
MR_cmp_bitmaps(const void *p1, const void *p2)
{
    MR_ConstBitmapPtr  s1 = * (MR_ConstBitmapPtr *) p1;
    MR_ConstBitmapPtr  s2 = * (MR_ConstBitmapPtr *) p2;

    return MR_bitmap_cmp(s1, s2);
}

// The MR_HASH_CONTENTS_FUNC_BODY macro implements the bodies of the
// following functions:
//
// MR_get_int_hash_table_contents
// MR_get_float_hash_table_contents
// MR_get_string_hash_table_contents

#define MR_INIT_HASH_CONTENTS_ARRAY_SIZE    100

#define MR_HASH_CONTENTS_FUNC_BODY                                      \
        MR_bool                                                         \
        func_name(MR_TrieNode t, type_name **values_ptr,                \
            int *value_next_ptr)                                        \
        {                                                               \
            type_name       *values;                                    \
            int             value_next;                                 \
            int             value_max;                                  \
            MR_HashTable    *table;                                     \
            int             bucket;                                     \
            table_type      *slot;                                      \
                                                                        \
            if (t->MR_hash_table == NULL) {                             \
                return MR_FALSE;                                        \
            }                                                           \
                                                                        \
            table = t->MR_hash_table;                                   \
            values = NULL;                                              \
            value_next = 0;                                             \
            value_max = 0;                                              \
                                                                        \
            for (bucket = 0; bucket < table->size; bucket++) {          \
                slot = table->hash_table[bucket].table_field;           \
                while (slot != NULL) {                                  \
                    MR_GC_ensure_room_for_next(value, type_name,        \
                        MR_INIT_HASH_CONTENTS_ARRAY_SIZE,               \
                        MR_ALLOC_SITE_TABLING);                         \
                    values[value_next] = slot->key;                     \
                    value_next++;                                       \
                    slot = slot->next;                                  \
                }                                                       \
            }                                                           \
                                                                        \
            qsort(values, value_next, sizeof(type_name), comp_func);    \
            *values_ptr = values;                                       \
            *value_next_ptr = value_next;                               \
            return MR_TRUE;                                             \
        }

#define func_name    MR_get_int_hash_table_contents
#define type_name    MR_Integer
#define table_type   MR_IntHashTableSlot
#define table_field  int_slot_ptr
#define comp_func    MR_cmp_ints
MR_HASH_CONTENTS_FUNC_BODY
#undef func_name
#undef type_name
#undef table_type
#undef table_field
#undef comp_func

#define func_name    MR_get_float_hash_table_contents
#define type_name    MR_Float
#define table_type   MR_FloatHashTableSlot
#define table_field  float_slot_ptr
#define comp_func    MR_cmp_floats
MR_HASH_CONTENTS_FUNC_BODY
#undef func_name
#undef type_name
#undef table_type
#undef table_field
#undef comp_func

#define func_name    MR_get_string_hash_table_contents
#define type_name    MR_ConstString
#define table_type   MR_StringHashTableSlot
#define table_field  string_slot_ptr
#define comp_func    MR_cmp_strings
MR_HASH_CONTENTS_FUNC_BODY
#undef func_name
#undef type_name
#undef table_type
#undef table_field
#undef comp_func

#define func_name    MR_get_bitmap_hash_table_contents
#define type_name    MR_ConstBitmapPtr
#define table_type   MR_BitmapHashTableSlot
#define table_field  bitmap_slot_ptr
#define comp_func    MR_cmp_bitmaps
MR_HASH_CONTENTS_FUNC_BODY
#undef func_name
#undef type_name
#undef table_type
#undef table_field
#undef comp_func

////////////////////////////////////////////////////////////////////////////

// This part deals with tabling using fixed size tables simply indexed
// by a given integer. t->MR_fix_table[i] contains the trie node for key i.
//
// The enum and the du_functor versions differ only in what statistics field
// we increment.

MR_TrieNode
MR_int_fix_index_enum_lookup_or_add(MR_TrieNode t, MR_Integer range,
    MR_Integer key)
{
#define MR_table_record_fix_alloc(numbytes)         ((void) 0)
#include "mercury_table_int_fix_index_body.h"
#undef  MR_table_record_fix_alloc
}

MR_TrieNode
MR_int_fix_index_enum_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, MR_Integer range, MR_Integer key)
{
#define MR_table_record_fix_alloc(numbytes)                                 \
        do {                                                                \
            stats->MR_tss_enum_num_node_allocs++;                           \
            stats->MR_tss_enum_num_node_alloc_bytes += (numbytes);          \
        } while (0)
#include "mercury_table_int_fix_index_body.h"
#undef  MR_table_record_fix_alloc
}

MR_TrieNode
MR_int_fix_index_du_lookup_or_add(MR_TrieNode t, MR_Integer range,
    MR_Integer key)
{
#define MR_table_record_fix_alloc(numbytes)         ((void) 0)
#include "mercury_table_int_fix_index_body.h"
#undef  MR_table_record_fix_alloc
}

MR_TrieNode
MR_int_fix_index_du_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode t, MR_Integer range, MR_Integer key)
{
#define MR_table_record_fix_alloc(numbytes)                                 \
        do {                                                                \
            stats->MR_tss_du_num_node_allocs++;                             \
            stats->MR_tss_du_num_node_alloc_bytes += (numbytes);            \
        } while (0)
#include "mercury_table_int_fix_index_body.h"
#undef  MR_table_record_fix_alloc
}

////////////////////////////////////////////////////////////////////////////

// This part deals with tabling using expandable tables simply indexed
// by the given integer minus a given starting point. t->MR_start_table[i+1]
// contains the trie node for key i - start. t->MR_start_table[0] contains
// the number of trienode slots currently allocated for the array; this does
// not include the slot used for the zeroeth element.

#define MR_START_TABLE_INIT_SIZE        1024

MR_TrieNode
MR_int_start_index_lookup_or_add(MR_TrieNode table, MR_Integer start,
    MR_Integer key)
{
#define MR_table_record_start_alloc(numbytes)   ((void) 0)
#include "mercury_table_int_start_index_body.h"
#undef  MR_table_record_start_alloc
}

MR_TrieNode
MR_int_start_index_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode table, MR_Integer start, MR_Integer key)
{
#define MR_table_record_start_alloc(numbytes)                               \
        do {                                                                \
            stats->MR_tss_start_num_allocs++;                               \
            stats->MR_tss_start_num_alloc_bytes += (numbytes);              \
        } while (0)
#include "mercury_table_int_start_index_body.h"
#undef  MR_table_record_start_alloc
}

////////////////////////////////////////////////////////////////////////////

MR_TrieNode
MR_type_info_lookup_or_add(MR_TrieNode table, MR_TypeInfo type_info)
{
#define tci_call(n, tci)    MR_int_hash_lookup_or_add((n), (tci))
#define rec_call(n, ti)     MR_type_info_lookup_or_add((n), (ti))
#include "mercury_table_typeinfo_body.h"
#undef  tci_call
#undef  rec_call
}

MR_TrieNode
MR_type_info_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode table, MR_TypeInfo type_info)
{
#define tci_call(n, tci)    MR_int_hash_lookup_or_add_stats(stats, (n), (tci))
#define rec_call(n, ti)     MR_type_info_lookup_or_add_stats(stats, (n), (ti))
#include "mercury_table_typeinfo_body.h"
#undef  tci_call
#undef  rec_call
}

MR_TrieNode
MR_type_class_info_lookup_or_add(MR_TrieNode table, MR_Word *type_class_info)
{
    MR_fatal_error("tabling of typeclass_infos not yet implemented");
    return NULL;
}

MR_TrieNode
MR_type_class_info_lookup_or_add_stats(MR_TableStepStats *stats,
    MR_TrieNode table, MR_Word *type_class_info)
{
    MR_fatal_error("tabling of typeclass_infos not yet implemented");
    return NULL;
}

////////////////////////////////////////////////////////////////////////////

MR_TrieNode
MR_table_type(MR_TrieNode table, MR_TypeInfo type_info, MR_Word data)
{
#define func    "MR_table_type"
#define STATS   NULL
#define DEBUG   MR_FALSE
#define BACK    MR_FALSE
#define MR_table_record_exist_lookup()      ((void) 0)
#define MR_table_record_arg_lookup()        ((void) 0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

MR_TrieNode
MR_table_type_debug(MR_TrieNode table, MR_TypeInfo type_info, MR_Word data)
{
#define func    "MR_table_type_debug"
#define STATS   NULL
#define DEBUG   MR_TRUE
#define BACK    MR_FALSE
#define MR_table_record_exist_lookup()      ((void) 0)
#define MR_table_record_arg_lookup()        ((void) 0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

MR_TrieNode
MR_table_type_stats(MR_TableStepStats *stats, MR_TrieNode table,
    MR_TypeInfo type_info, MR_Word data)
{
#define func    "MR_table_type_stats"
#define STATS   stats
#define DEBUG   MR_FALSE
#define BACK    MR_FALSE
#define MR_table_record_exist_lookup()                                      \
        do {                                                                \
            stats->MR_tss_du_num_exist_lookups++;                           \
        } while (0)
#define MR_table_record_arg_lookup()                                        \
        do {                                                                \
            stats->MR_tss_du_num_arg_lookups++;                             \
        } while (0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

MR_TrieNode
MR_table_type_stats_debug(MR_TableStepStats *stats, MR_TrieNode table,
    MR_TypeInfo type_info, MR_Word data)
{
#define func    "MR_table_type_stats_debug"
#define STATS   stats
#define DEBUG   MR_TRUE
#define BACK    MR_FALSE
#define MR_table_record_exist_lookup()                                      \
        do {                                                                \
            stats->MR_tss_du_num_exist_lookups++;                           \
        } while (0)
#define MR_table_record_arg_lookup()                                        \
        do {                                                                \
            stats->MR_tss_du_num_arg_lookups++;                             \
        } while (0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

MR_TrieNode
MR_table_type_back(MR_TrieNode table, MR_TypeInfo type_info, MR_Word data)
{
#define func    "MR_table_type"
#define STATS   NULL
#define DEBUG   MR_FALSE
#define BACK    MR_TRUE
#define MR_table_record_exist_lookup()      ((void) 0)
#define MR_table_record_arg_lookup()        ((void) 0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

MR_TrieNode
MR_table_type_debug_back(MR_TrieNode table, MR_TypeInfo type_info,
    MR_Word data)
{
#define func    "MR_table_type_debug"
#define STATS   NULL
#define DEBUG   MR_TRUE
#define BACK    MR_TRUE
#define MR_table_record_exist_lookup()      ((void) 0)
#define MR_table_record_arg_lookup()        ((void) 0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

MR_TrieNode
MR_table_type_stats_back(MR_TableStepStats *stats, MR_TrieNode table,
    MR_TypeInfo type_info, MR_Word data)
{
#define func    "MR_table_type_stats"
#define STATS   stats
#define DEBUG   MR_FALSE
#define BACK    MR_TRUE
#define MR_table_record_exist_lookup()                                      \
        do {                                                                \
            stats->MR_tss_du_num_exist_lookups++;                           \
        } while (0)
#define MR_table_record_arg_lookup()                                        \
        do {                                                                \
            stats->MR_tss_du_num_arg_lookups++;                             \
        } while (0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

MR_TrieNode
MR_table_type_stats_debug_back(MR_TableStepStats *stats, MR_TrieNode table,
    MR_TypeInfo type_info, MR_Word data)
{
#define func    "MR_table_type_stats_debug"
#define STATS   stats
#define DEBUG   MR_TRUE
#define BACK    MR_TRUE
#define MR_table_record_exist_lookup()                                      \
        do {                                                                \
            stats->MR_tss_du_num_exist_lookups++;                           \
        } while (0)
#define MR_table_record_arg_lookup()                                        \
        do {                                                                \
            stats->MR_tss_du_num_arg_lookups++;                             \
        } while (0)
#include "mercury_table_type_body.h"
#undef  func
#undef  STATS
#undef  DEBUG
#undef  BACK
#undef  MR_table_record_exist_lookup
#undef  MR_table_record_arg_lookup
}

////////////////////////////////////////////////////////////////////////////

void
MR_table_report_statistics(FILE *fp)
{
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
  #ifdef MR_TABLE_STATISTICS
     MR_minimal_model_report_stats(fp);
  #endif
#endif
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS
     MR_mm_own_stacks_report_stats(fp);
#endif
}

////////////////////////////////////////////////////////////////////////////

const char *
MR_loopcheck_status(MR_Unsigned status)
{
    switch (status) {
        case MR_LOOP_INACTIVE:
            return "INACTIVE";

        case MR_LOOP_ACTIVE:
            return "ACTIVE";
    }

    return "INVALID";
}

const char *
MR_memo_status(MR_Unsigned status)
{
    switch (status) {
        case MR_MEMO_INACTIVE:
            return "INACTIVE";

        case MR_MEMO_ACTIVE:
            return "ACTIVE";

        case MR_MEMO_SUCCEEDED:
            return "SUCCEEDED";

        case MR_MEMO_FAILED:
            return "FAILED";

        default:
            return "SUCCESS_BLOCK";
    }

    return "INVALID";
}

const char *
MR_memo_non_status(MR_MemoNonStatus status)
{
    switch (status) {
        case MR_MEMO_NON_INACTIVE:
            return "INACTIVE";

        case MR_MEMO_NON_ACTIVE:
            return "ACTIVE";

        case MR_MEMO_NON_INCOMPLETE:
            return "INCOMPLETE";

        case MR_MEMO_NON_COMPLETE:
            return "COMPLETE";
    }

    return "INVALID";
}

void
MR_print_loopcheck_tip(FILE *fp, const MR_ProcLayout *proc, MR_TrieNode table)
{
    switch (table->MR_loop_status) {
        case MR_LOOP_INACTIVE:
            fprintf(fp, "uninitialized\n");
            break;
        case MR_LOOP_ACTIVE:
            fprintf(fp, "working\n");
            break;
        default:
            MR_fatal_error("MR_print_loopcheck: bad status");
    }
}

void
MR_print_memo_tip(FILE *fp, const MR_ProcLayout *proc, MR_TrieNode table)
{
    switch (table->MR_memo_status) {
        case MR_MEMO_INACTIVE:
            fprintf(fp, "uninitialized\n");
            break;
        case MR_MEMO_ACTIVE:
            fprintf(fp, "working\n");
            break;
        case MR_MEMO_FAILED:
            fprintf(fp, "failed\n");
            break;
        case MR_MEMO_SUCCEEDED:
            fprintf(fp, "succeeded (no outputs)\n");
            break;
        default:
            fprintf(fp, "succeeded <");
            MR_print_answerblock(fp, proc, table->MR_answerblock);
            fprintf(fp, ">\n");
            break;
    }
}

void
MR_print_memo_non_record(FILE *fp, const MR_ProcLayout *proc,
    MR_MemoNonRecordPtr record)
{
    MR_AnswerList   answer_list;
    int             i;

    if (record == NULL) {
        fprintf(fp, "inactive\n");
        return;
    }

    switch (record->MR_mn_status) {
        case MR_MEMO_NON_INACTIVE:
            fprintf(fp, "inactive\n");
            return;
        case MR_MEMO_NON_ACTIVE:
            fprintf(fp, "active\n");
            break;
        case MR_MEMO_NON_INCOMPLETE:
            fprintf(fp, "incomplete\n");
            break;
        case MR_MEMO_NON_COMPLETE:
            fprintf(fp, "complete\n");
            break;
        default:
            MR_fatal_error("MR_print_memo_non_record: bad status");
            break;
    }

    answer_list = record->MR_mn_answer_list;
    i = 1;
    while (answer_list != NULL) {
        fprintf(fp, "answer #%d: <", i);
        MR_print_answerblock(fp, proc, answer_list->MR_aln_answer_block);
        fprintf(fp, ">\n");
        answer_list = answer_list->MR_aln_next_answer;
        i++;
    }
}

void
MR_print_answerblock(FILE *fp, const MR_ProcLayout *proc,
    MR_Word *answer_block)
{
    const MR_PseudoTypeInfo *ptis;
    MR_PseudoTypeInfo       pti;
    MR_TypeCtorInfo         tci;
    int                     num_inputs;
    int                     num_outputs;
    int                     i;

    num_inputs = proc->MR_sle_table_info.MR_table_proc->MR_pt_num_inputs;
    num_outputs = proc->MR_sle_table_info.MR_table_proc->MR_pt_num_outputs;

    ptis = proc->MR_sle_table_info.MR_table_proc->MR_pt_ptis;
    ptis += num_inputs;

    for (i = 0; i < num_outputs; i++) {
        if (i > 0) {
            fprintf(fp, ", ");
        }

        pti = ptis[i];
        if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pti)) {
            fprintf(fp, "poly");
            continue;
        }

        tci = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(pti);
        if (tci == &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0)) {
            fprintf(fp, "%ld", (long) answer_block[i]);
        } else if (tci == &MR_TYPE_CTOR_INFO_NAME(builtin, float, 0)) {
            fprintf(fp, "%f",
#ifdef  MR_HIGHLEVEL_CODE
                (double) MR_unbox_float((MR_Box) answer_block[i]));
#else
                (double) MR_word_to_float(answer_block[i]));
#endif
        } else if (tci == &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0)) {
            fprintf(fp, "\"%s\"", (char *) answer_block[i]);
        } else {
            fprintf(fp, "value of unsupported type");
        }
    }
}

#ifdef  MR_HIGHLEVEL_CODE
    static void MR_CALL
    mercury__table_builtin__table_memo_return_all_answers_2_p_0(
        MR_AnswerList answer_list0, MR_Box *boxed_answer_block,
        MR_Cont cont, void *cont_env_ptr);

    static void MR_CALL
    mercury__table_builtin__table_memo_return_all_answers_2_p_0(
        MR_AnswerList answer_list0, MR_Box *boxed_answer_block_ptr,
        MR_Cont cont, void *cont_env_ptr)
    {
        MR_AnswerList answer_list;

        while (answer_list0 != NULL) {
            answer_list = answer_list0->MR_aln_next_answer;
            *boxed_answer_block_ptr =
                 (MR_Box) answer_list0->MR_aln_answer_block;
            cont(cont_env_ptr);
            answer_list0 = answer_list;
        }
    }

    void MR_CALL
    mercury__table_builtin__table_memo_return_all_answers_multi_2_p_0(
        MR_Box boxed_record, MR_Box *boxed_answer_block_ptr,
        MR_Cont cont, void *cont_env_ptr)
    {
        MR_MemoNonRecordPtr record;
        MR_AnswerList       list;

        record = (MR_MemoNonRecordPtr) boxed_record;
        list = record->MR_mn_answer_list;
        if (list == NULL) {
            MR_fatal_error(
                "table_memo_return_all_answers_multi: no answers");
        }
        mercury__table_builtin__table_memo_return_all_answers_2_p_0(list,
            boxed_answer_block_ptr, cont, cont_env_ptr);
    }

    void MR_CALL
    mercury__table_builtin__table_memo_return_all_answers_nondet_2_p_0(
        MR_Box boxed_record, MR_Box *boxed_answer_block_ptr,
        MR_Cont cont, void *cont_env_ptr)
    {
        MR_MemoNonRecordPtr record;
        MR_AnswerList       list;

        record = (MR_MemoNonRecordPtr) boxed_record;
        list = record->MR_mn_answer_list;
        mercury__table_builtin__table_memo_return_all_answers_2_p_0(list,
            boxed_answer_block_ptr, cont, cont_env_ptr);
    }

#else   // MR_HIGHLEVEL_CODE

MR_define_extern_entry(MR_MEMO_NON_RET_ALL_NONDET_ENTRY);
MR_define_extern_entry(MR_MEMO_NON_RET_ALL_MULTI_ENTRY);

MR_EXTERN_USER_PROC_ID_PROC_LAYOUT(MR_DETISM_NON, 0, -1,
    MR_PREDICATE, table_builtin, table_memo_return_all_answers_nondet, 2, 0);
MR_EXTERN_USER_PROC_ID_PROC_LAYOUT(MR_DETISM_NON, 0, -1,
    MR_PREDICATE, table_builtin, table_memo_return_all_answers_multi, 2, 0);

#define MEMO_NON_RET_ALL_NONDET_LABEL(name)                             \
    MR_label_name(MR_MEMO_NON_RET_ALL_NONDET_ENTRY, name)
#define MEMO_NON_RET_ALL_MULTI_LABEL(name)                              \
    MR_label_name(MR_MEMO_NON_RET_ALL_MULTI_ENTRY, name)

MR_declare_label(MEMO_NON_RET_ALL_NONDET_LABEL(Next));
MR_declare_label(MEMO_NON_RET_ALL_MULTI_LABEL(Next));

MR_MAKE_USER_INTERNAL_LAYOUT(table_builtin,
    table_memo_return_all_answers_nondet, 2, 0, Next);
MR_MAKE_USER_INTERNAL_LAYOUT(table_builtin,
    table_memo_return_all_answers_multi, 2, 0, Next);

MR_BEGIN_MODULE(table_memo_non_module)
    MR_init_entry_sl(MR_MEMO_NON_RET_ALL_NONDET_ENTRY);
    MR_init_label_sl(MEMO_NON_RET_ALL_NONDET_LABEL(Next));
    MR_init_entry_sl(MR_MEMO_NON_RET_ALL_MULTI_ENTRY);
    MR_init_label_sl(MEMO_NON_RET_ALL_MULTI_LABEL(Next));
MR_BEGIN_CODE

MR_define_entry(MR_MEMO_NON_RET_ALL_NONDET_ENTRY);
{
    MR_MemoNonRecordPtr record;
    MR_AnswerList       cur_node0;
    MR_AnswerList       cur_node;
    MR_AnswerBlock      answer_block;

    record = (MR_MemoNonRecordPtr) MR_r1;
    cur_node0 = record->MR_mn_answer_list;

  #ifdef MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("picking up all answers in %p -> %p\n",
            record->MR_mn_back_ptr, record);
    }
  #endif

    if (cur_node0 == NULL) {
        MR_redo();
    }

    answer_block = cur_node0->MR_aln_answer_block;
    cur_node = cur_node0->MR_aln_next_answer;

    // Consider not creating the stack frame if cur_node is NULL.

    MR_mkframe("pred table_builtin.table_memo_return_all_answers_nondet/2-0",
        1, MR_LABEL(MEMO_NON_RET_ALL_NONDET_LABEL(Next)));
    MR_framevar(1) = (MR_Word) cur_node;
    MR_r1 = (MR_Word) answer_block;
}
    MR_succeed();

MR_define_label(MEMO_NON_RET_ALL_NONDET_LABEL(Next));
{
    MR_AnswerList   cur_node0;
    MR_AnswerList   cur_node;
    MR_AnswerBlock  answer_block;

    cur_node0 = (MR_AnswerList) MR_framevar(1);
    if (cur_node0 == NULL) {
        MR_fail();
    }

    answer_block = cur_node0->MR_aln_answer_block;
    cur_node = cur_node0->MR_aln_next_answer;
    MR_framevar(1) = (MR_Word) cur_node;
    MR_r1 = (MR_Word) answer_block;
}
    MR_succeed();

MR_define_entry(MR_MEMO_NON_RET_ALL_MULTI_ENTRY);
{
    MR_MemoNonRecordPtr record;
    MR_AnswerList       cur_node0;
    MR_AnswerList       cur_node;
    MR_AnswerBlock      answer_block;

    record = (MR_MemoNonRecordPtr) MR_r1;
    cur_node0 = record->MR_mn_answer_list;

  #ifdef MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("picking up all answers in %p -> %p\n",
            record->MR_mn_back_ptr, record);
    }
  #endif

    if (cur_node0 == NULL) {
        MR_fatal_error("table_memo_return_all_answers_multi: no answers");
    }

    answer_block = cur_node0->MR_aln_answer_block;
    cur_node = cur_node0->MR_aln_next_answer;

    // Consider not creating the stack frame if cur_node is NULL.

    MR_mkframe("pred table_builtin.table_memo_return_all_answers_multi/2-0",
        1, MR_LABEL(MEMO_NON_RET_ALL_MULTI_LABEL(Next)));
    MR_framevar(1) = (MR_Word) cur_node;
    MR_r1 = (MR_Word) answer_block;
}
    MR_succeed();

MR_define_label(MEMO_NON_RET_ALL_MULTI_LABEL(Next));
{
    MR_AnswerList   cur_node0;
    MR_AnswerList   cur_node;
    MR_AnswerBlock  answer_block;

    cur_node0 = (MR_AnswerList) MR_framevar(1);
    if (cur_node0 == NULL) {
        MR_fail();
    }

    answer_block = cur_node0->MR_aln_answer_block;
    cur_node = cur_node0->MR_aln_next_answer;
    MR_framevar(1) = (MR_Word) cur_node;
    MR_r1 = (MR_Word) answer_block;
}
    MR_succeed();

MR_END_MODULE

#endif  // MR_HIGHLEVEL_CODE

// Ensure that the initialization code for the above modules gets to run.
/*
INIT mercury_sys_init_table_modules
*/

#ifndef MR_HIGHLEVEL_CODE
MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc table_memo_non_module;
#endif

// Forward declarations to suppress gcc -Wmissing-decl warnings.
void mercury_sys_init_table_modules_init(void);
void mercury_sys_init_table_modules_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_table_modules_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    table_memo_non_module();
#endif  // MR_HIGHLEVEL_CODE
}

void mercury_sys_init_table_modules_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp)
{
    // No proc_statics to write out.
    // XXX We need to fix the deep profiling
    // of model_non memo tabled predicates.
}
#endif

////////////////////////////////////////////////////////////////////////////

#ifdef MR_TABLE_DEBUG
static void
MR_table_assert_failed(const char *file, unsigned line)
{
    MR_fatal_error("assertion failed: file %s, line %d", file, line);
}
#endif

////////////////////////////////////////////////////////////////////////////
