/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1997-2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module contains the functions related to tabling that are not
** specific to minimal model tabling.
*/

#include "mercury_imp.h"

#include "mercury_type_info.h"
#include "mercury_array_macros.h"
#include "mercury_builtin_types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*---------------------------------------------------------------------------*/

/*
** This part deals with tabling using resizable hash tables.
*/

/*
** All hash table slot structures have the same fields, since they are
** manipulated by the same macro (MR_GENERIC_HASH_LOOKUP_OR_ADD).
** The variable size part is at the end, in order to make all the offsets
** the same.
*/

typedef struct MR_IntHashTableSlot_Struct       MR_IntHashTableSlot;
typedef struct MR_FloatHashTableSlot_Struct     MR_FloatHashTableSlot;
typedef struct MR_StringHashTableSlot_Struct    MR_StringHashTableSlot;
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

struct MR_StringHashTableSlot_Struct {
    MR_StringHashTableSlot  *next;
    MR_TableNode            data;
    MR_ConstString          key;
};

struct MR_WordHashTableSlot_Struct {
    MR_WordHashTableSlot    *next;
    MR_TableNode            data;
    MR_Word                 key;
};

typedef union {
    MR_IntHashTableSlot     *int_slot_ptr;
    MR_FloatHashTableSlot   *float_slot_ptr;
    MR_StringHashTableSlot  *string_slot_ptr;
    MR_WordHashTableSlot    *word_slot_ptr;
} MR_HashTableSlotPtr;

struct MR_AllocRecord_Struct {
    MR_HashTableSlotPtr     chunk;
    MR_AllocRecord          *next;
};

/*
** Our hash table design uses separate chaining to avoid the bad worst case
** behavior of open addressing. This is important, because the worst case
** can be expected to occur reasonably often in tabling workloads. The reason
** is that successive queries are not independent. Often, query N is a
** recursive call made from query N-1, which means that its input values are
** much more likely to fall into the same or next hash bucket than an
** independent query's input values would, especially for integer values.
** Repeated over many queries, such input pattern can give rise to "convoys",
** long sequences of occupied hash table slots. Any input value whose search
** for a free slot runs into the convoy will have very long search time.
**
** The `hash_table' field points to an array of `size' slots, each of which
** is a pointer to a hash table slot; hash table slots have embedded `next'
** pointers to chain together all the values that hash to the same value.
**
** To keep maximum chain lengths bounded (in a statistical sense), we record
** the number of values in the table (in the `value_count' field), and when
** this exceeds a certain fraction of the size of the hash table, we increase
** the size of the hash table and rehash all the existing entries. We do this
** when the value in the `value_count' field exceeds the one in the `threshold'
** field, which is set to `size' times MAX_LOAD_FACTOR whenever the size
** is changed. (This avoids a float multiplication on each insertion.)
**
** The reason why the hash table array contains pointers to slots instead of
** the slots themselves is that the latter would require the addresses of some
** hash table slots (those in the array itself and not in a chain) to change
** when the table is resized. As for why this is bad, see the documentation
** of the MR_TableNode type in mercury_tabling.h.
**
** To avoid calling GC_malloc on each insertion, we allocate memory in chunks,
** with each chunk containing CHUNK_SIZE hash table slots. The `freeleft'
** field contains count of the number of hash table slots left in the space
** allocated but not yet used; the `freespace' field point to the first
** of these slots.
**
** This design leads to pointers into the middle of GC_malloc'd memory.
** To make sure that the code works even without the boehm gc being compiled
** with interior pointers, we retain pointers to all the chunks we have
** allocated in the `allocrecord' field. This field has no purpose other than
** to serve as roots for boehm gc.
*/

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

/*
** Prime numbers which are close to powers of 2.  Used for choosing
** the next size for a hash table.
*/

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

/* Initial size of a new table */
#define HASH_TABLE_START_SIZE primes[0]

static  MR_Integer      next_prime(MR_Integer);

/*
** Return the next prime number greater than the number received.
** If no such prime number can be found, compute an approximate one.
*/

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

/*
** The MR_GENERIC_HASH_LOOKUP_OR_ADD macro is intended to be the body of
** a function that looks to see if the given key is in the given hash table.
** If it is, it returns the address of the data pointer associated with
** the key. If it is not, it creates a new slot for the key in the table
** and returns the address of its data pointer.
**
** It in turn relies on three groups of macros to perform part of the task.
**
** The first group optionally records statistics about the number of successful
** and unsuccessful searches, and the number of probes they needed. From this
** information, one can compute the average successful and unsuccessful
** search lengths.
**
** The second optionally prints debugging messages.
**
** The third implements the initial creation of the hash table.
*/

#ifdef  MR_TABLE_STATISTICS
static  MR_Unsigned     MR_table_hash_resizes = 0;
static  MR_Unsigned     MR_table_hash_resize_old_entries = 0;
static  MR_Unsigned     MR_table_hash_resize_new_entries = 0;
static  MR_Unsigned     MR_table_hash_allocs  = 0;
static  MR_Unsigned     MR_table_hash_lookups = 0;
static  MR_Unsigned     MR_table_hash_inserts = 0;
static  MR_Unsigned     MR_table_hash_lookup_probes = 0;
static  MR_Unsigned     MR_table_hash_insert_probes = 0;
#endif

#ifdef  MR_TABLE_STATISTICS
  #define DECLARE_PROBE_COUNT   MR_Integer      probe_count = 0;
  #define record_probe_count()  do { probe_count++; } while (0)
  #define record_lookup_count() do {                                          \
                                        MR_table_hash_lookup_probes +=        \
                                                probe_count;                  \
                                        MR_table_hash_lookups++;              \
                                } while (0)
  #define record_insert_count() do {                                          \
                                        MR_table_hash_insert_probes +=        \
                                                probe_count;                  \
                                        MR_table_hash_inserts++;              \
                                } while (0)
  #define record_resize_count(old, new)                                       \
                                do {                                          \
                                    MR_table_hash_resizes++;                  \
                                    MR_table_hash_resize_old_entries += (old);\
                                    MR_table_hash_resize_new_entries += (new);\
                                } while (0)
  #define record_alloc_count()  do { MR_table_hash_allocs++; } while (0)
#else
  #define DECLARE_PROBE_COUNT
  #define record_probe_count()  ((void) 0)
  #define record_lookup_count() ((void) 0)
  #define record_insert_count() ((void) 0)
  #define record_resize_count(old, new)                                       \
                                ((void) 0)
  #define record_alloc_count()  ((void) 0)
#endif

#ifdef  MR_TABLE_DEBUG
  #define debug_key_msg(keyvalue, keyformat, keycast)                         \
        do {                                                                  \
                if (MR_hashdebug) {                                           \
                        printf("HT search key " keyformat "\n",               \
                                (keycast) keyvalue);                          \
                }                                                             \
        } while (0)

  #define debug_resize_msg(oldsize, newsize, newthreshold)                    \
        do {                                                                  \
                if (MR_hashdebug) {                                           \
                        printf("HT expanding table from %d to %d(%d)\n",      \
                                (oldsize), (newsize), (newthreshold));        \
                }                                                             \
        } while (0)

  #define debug_rehash_msg(rehash_bucket)                                     \
        do {                                                                  \
                if (MR_hashdebug) {                                           \
                        printf("HT rehashing bucket: %d\n",                   \
                                (rehash_bucket));                             \
                }                                                             \
        } while (0)

  #define debug_probe_msg(probe_bucket)                                       \
        do {                                                                  \
                if (MR_hashdebug) {                                           \
                        printf("HT probing bucket: %d\n", (probe_bucket));    \
                }                                                             \
        } while (0)

  #define debug_lookup_msg(home_bucket)                                       \
        do {                                                                  \
                if (MR_hashdebug) {                                           \
                        printf("HT search successful in bucket: %d\n",        \
                                (home_bucket));                               \
                }                                                             \
        } while (0)

  #define debug_insert_msg(home_bucket)                                       \
        do {                                                                  \
                if (MR_hashdebug) {                                           \
                        printf("HT search unsuccessful in bucket: %d\n",      \
                                (home_bucket));                               \
                }                                                             \
        } while (0)
#else
  #define debug_key_msg(keyvalue, keyformat, keycast)           ((void) 0)
  #define debug_resize_msg(oldsize, newsize, newthreshold)      ((void) 0)
  #define debug_rehash_msg(rehash_bucket)                       ((void) 0)
  #define debug_probe_msg(probe_bucket)                         ((void) 0)
  #define debug_lookup_msg(home_bucket)                         ((void) 0)
  #define debug_insert_msg(home_bucket)                         ((void) 0)
#endif

/*
** The MR_GENERIC_HASH_LOOKUP_OR_ADD macro, and its helper macro
** MR_CREATE_HASH_TABLE implement the bodies of the following functions:
**
** MR_int_hash_lookup_or_add
** MR_float_hash_lookup_or_add
** MR_string_hash_lookup_or_add
** MR_int_hash_lookup
** MR_float_hash_lookup
** MR_string_hash_lookup
*/

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
                        table_size);                                        \
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
#define key_format              "%ld"
#define key_cast                long
#define table_type              MR_IntHashTableSlot
#define table_field             int_slot_ptr
#define hash(key)               (key)
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_hash_lookup_or_add_body.h"
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
#define key_format              "%ld"
#define key_cast                long
#define table_type              MR_IntHashTableSlot
#define table_field             int_slot_ptr
#define hash(key)               (key)
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_TRUE
#include "mercury_hash_lookup_or_add_body.h"
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
#undef  lookup_only
}

/*
** Note that the equal_keys operation should compare two floats for
** bit-for-bit equality. This is different from the usual == operator
** in the presence of NaNs, infinities, etc.
*/

MR_TrieNode
MR_float_hash_lookup_or_add(MR_TrieNode t, MR_Float key)
{
#define key_format              "%f"
#define key_cast                double
#define table_type              MR_FloatHashTableSlot
#define table_field             float_slot_ptr
#define hash(key)               (MR_hash_float(key))
#define equal_keys(k1, k2)      (memcmp(&(k1), &(k2), sizeof(MR_Float)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_hash_lookup_or_add_body.h"
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
#define key_format              "%f"
#define key_cast                double
#define table_type              MR_FloatHashTableSlot
#define table_field             float_slot_ptr
#define hash(key)               (MR_hash_float(key))
#define equal_keys(k1, k2)      (memcmp(&(k1), &(k2), sizeof(MR_Float)) == 0)
#define lookup_only             MR_TRUE
#include "mercury_hash_lookup_or_add_body.h"
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
#define key_format              "%s"
#define key_cast                const char *
#define table_type              MR_StringHashTableSlot
#define table_field             string_slot_ptr
#define hash(key)               (MR_hash_string(key))
#define equal_keys(k1, k2)      (MR_strtest((k1), (k2)) == 0)
#define lookup_only             MR_FALSE
#include "mercury_hash_lookup_or_add_body.h"
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
#define key_format              "%s"
#define key_cast                const char *
#define table_type              MR_StringHashTableSlot
#define table_field             string_slot_ptr
#define hash(key)               (MR_hash_string(key))
#define equal_keys(k1, k2)      (MR_strtest((k1), (k2)) == 0)
#define lookup_only             MR_TRUE
#include "mercury_hash_lookup_or_add_body.h"
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
#define key_format              "%d"
#define key_cast                MR_Word
#define table_type              MR_WordHashTableSlot
#define table_field             word_slot_ptr
#define hash(key)               ((long) (key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_FALSE
#include "mercury_hash_lookup_or_add_body.h"
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
#define key_format              "%d"
#define key_cast                MR_Word
#define table_type              MR_WordHashTableSlot
#define table_field             word_slot_ptr
#define hash(key)               ((long) (key))
#define equal_keys(k1, k2)      ((k1) == (k2))
#define lookup_only             MR_TRUE
#include "mercury_hash_lookup_or_add_body.h"
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

/*
** The MR_HASH_CONTENTS_FUNC_BODY macro implements the bodies of the
** following functions:
**
** MR_get_int_hash_table_contents
** MR_get_float_hash_table_contents
** MR_get_string_hash_table_contents
*/

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
                        MR_INIT_HASH_CONTENTS_ARRAY_SIZE);              \
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

/*---------------------------------------------------------------------------*/

/*
** This part deals with tabling using fixed size tables simply indexed
** by a given integer. t->MR_fix_table[i] contains the trie node for
** key i.
*/

MR_TrieNode
MR_int_fix_index_lookup_or_add(MR_TrieNode t, MR_Integer range, MR_Integer key)
{
    if (t->MR_fix_table == NULL) {
        t->MR_fix_table = MR_TABLE_NEW_ARRAY(MR_TableNode, range);
        memset(t->MR_fix_table, 0, sizeof(MR_TableNode) * range);
    }

#ifdef  MR_TABLE_DEBUG
    if (key >= range) {
        MR_fatal_error("MR_int_fix_index_lookup_or_add: key out of range");
    }
#endif

    return &t->MR_fix_table[key];
}

/*---------------------------------------------------------------------------*/

/*
** This part deals with tabling using expandable tables simply indexed
** by the given integer minus a given starting point. t->MR_start_table[i+1]
** contains the trie node for key i - start. t->MR_start_table[0] contains
** the number of trienode slots currently allocated for the array; this does
** not include the slot used for the zeroeth element.
*/

#define MR_START_TABLE_INIT_SIZE        1024

MR_TrieNode
MR_int_start_index_lookup_or_add(MR_TrieNode table, MR_Integer start,
    MR_Integer key)
{
    MR_Integer      diff, size;

    diff = key - start;

#ifdef  MR_TABLE_DEBUG
    if (key < start) {
        MR_fatal_error("MR_int_start_index_lookup_or_add: small too key");
    }
#endif

    if (table->MR_start_table == NULL) {
        size = MR_max(MR_START_TABLE_INIT_SIZE, diff + 1);
        table->MR_start_table = MR_TABLE_NEW_ARRAY(MR_TableNode, size + 1);
        memset(table->MR_start_table + 1, 0, sizeof(MR_TableNode) * size);
        table->MR_start_table[0].MR_integer = size;
    } else {
        size = table->MR_start_table[0].MR_integer;
    }

    if (diff >= size) {
        MR_TableNode    *new_array;
        MR_Integer      new_size, i;

        new_size = MR_max(2 * size, diff + 1);
        new_array = MR_TABLE_NEW_ARRAY(MR_TableNode, new_size + 1);

        new_array[0].MR_integer = new_size;

        for (i = 0; i < size; i++) {
            new_array[i + 1] = table->MR_start_table[i + 1];
        }

        for (; i < new_size; i++) {
            new_array[i + 1].MR_integer = 0;
        }

        table->MR_start_table = new_array;
    }

    return &table->MR_start_table[diff + 1];
}

/*---------------------------------------------------------------------------*/

MR_TrieNode
MR_type_info_lookup_or_add(MR_TrieNode table, MR_TypeInfo type_info)
{
    MR_TypeCtorInfo     type_ctor_info;
    MR_TrieNode         node;
    MR_TypeInfo         *arg_vector;
    int                 arity;
    int                 i;

    /* XXX memory allocation here should be optimized */
    type_info = MR_collapse_equivalences(type_info);

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    node = MR_int_hash_lookup_or_add(table, (MR_Integer) type_ctor_info);

    /*
    ** All calls to MR_type_info_lookup_or_add that have the same value
    ** of node at this point agree on the type_ctor_info of the type
    ** being tabled. They must therefore also agree on its arity.
    ** This is why looping over all the arguments works.
    **
    ** If type_info has a zero-arity type_ctor, then it may be stored
    ** using a one-cell type_info, and type_info_args does not make
    ** sense. This is OK, because in that case it will never be used.
    */

    if (MR_type_ctor_has_variable_arity(type_ctor_info)) {
        arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
        arg_vector = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info);
        node = MR_int_hash_lookup_or_add(node, arity);
    } else {
        arity = type_ctor_info->MR_type_ctor_arity;
        arg_vector = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info);
    }

    for (i = 1; i <= arity; i++) {
        node = MR_type_info_lookup_or_add(node, arg_vector[i]);
    }

    return node;
}

MR_TrieNode
MR_type_class_info_lookup_or_add(MR_TrieNode table, MR_Word *type_class_info)
{
    MR_fatal_error("tabling of typeclass_infos not yet implemented");
    return NULL;
}

/*---------------------------------------------------------------------------*/

/*
** This part defines the MR_table_type() function.
**
** NOTE: changes to this function will probably also have to be reflected
** in the places listed in mercury_type_info.h.
*/

MR_TrieNode
MR_table_type(MR_TrieNode table, MR_TypeInfo type_info, MR_Word data)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_DuTypeLayout du_type_layout;

    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("ENTRY %p %x, data rep: %d\n",
            table, data, MR_type_ctor_rep(type_ctor_info));
    }
#endif  /* MR_TABLE_DEBUG */

    if (! MR_type_ctor_has_valid_rep(type_ctor_info)) {
        MR_fatal_error("MR_table_type: term of unknown representation");
    }

    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_ENUM: 
        case MR_TYPECTOR_REP_ENUM_USEREQ: 
            MR_DEBUG_TABLE_ENUM(table,
                MR_type_ctor_num_functors(type_ctor_info), data);
            return table;

        case MR_TYPECTOR_REP_DUMMY: 
            /*
            ** If we are ever asked to table a value of a dummy type, we treat
            ** it mostly as an enum, with the exception being that we ignore
            ** the actual value to be table (since it contains garbage) and
            ** substitute the constant zero, which ought to be the enum value
            ** assigned to the type's only function symbol.
            **
            ** It would of course be preferable for the compiler to simply
            ** not insert any arguments of dummy types into tables.
            */
            MR_DEBUG_TABLE_ENUM(table, 1, 0);
            return table;

        case MR_TYPECTOR_REP_RESERVED_ADDR: 
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ: 
            {
                int                         i;
                MR_ReservedAddrTypeLayout   ra_layout;
                
                ra_layout = MR_type_ctor_layout(type_ctor_info).
                            MR_layout_reserved_addr;

                /*
                ** First check if this value is one of
                ** the numeric reserved addresses.
                */
                if ((MR_Unsigned) data <
                    (MR_Unsigned) ra_layout->MR_ra_num_res_numeric_addrs)
                {
                    MR_DEBUG_TABLE_ENUM(table,
                        MR_type_ctor_num_functors(type_ctor_info),
                        ra_layout->MR_ra_constants[data]->
                            MR_ra_functor_ordinal);
                    break;
                }

                /*
                ** Next check if this value is one of the
                ** the symbolic reserved addresses.
                */
                for (i = 0; i < ra_layout->MR_ra_num_res_symbolic_addrs; i++) {
                    if (data == (MR_Word)
                        ra_layout->MR_ra_res_symbolic_addrs[i])
                    {
                        int offset = i + ra_layout->MR_ra_num_res_numeric_addrs;
                        MR_DEBUG_TABLE_ENUM(table,
                            MR_type_ctor_num_functors(type_ctor_info),
                            ra_layout->MR_ra_constants[offset]->
                                MR_ra_functor_ordinal);
                        /* "break" here would just exit the "for" loop */
                        return table;
                    }
                }
                    
                /*
                ** Otherwise, it is not one of the reserved addresses,
                ** so handle it like a normal DU type.
                */
                du_type_layout = ra_layout->MR_ra_other_functors;
                goto du_type;
            }

        case MR_TYPECTOR_REP_DU: 
        case MR_TYPECTOR_REP_DU_USEREQ: 
            du_type_layout = MR_type_ctor_layout(type_ctor_info).MR_layout_du;
            /* fall through */
        
        /*
        ** This label handles both the DU case and the second half of the
        ** RESERVED_ADDR case.  `du_type_layout' must be set before
        ** this code is entered.
        */
        du_type:
            {
                MR_MemoryList           allocated_memory_cells = NULL;
                const MR_DuPtagLayout   *ptag_layout;
                const MR_DuFunctorDesc  *functor_desc;
                const MR_DuExistInfo    *exist_info;
                MR_TypeInfo             arg_type_info;
                int                     ptag;
                MR_Word                 sectag;
                MR_Word                 *arg_vector;
                int                     meta_args;
                int                     i;

                ptag = MR_tag(data);
                ptag_layout = &du_type_layout[ptag];

                switch (ptag_layout->MR_sectag_locn) {

                case MR_SECTAG_NONE:
                    functor_desc = ptag_layout->MR_sectag_alternatives[0];
                    arg_vector = (MR_Word *) MR_body(data, ptag);
                    break;

                case MR_SECTAG_LOCAL:
                    sectag = MR_unmkbody(data);
                    functor_desc = ptag_layout->MR_sectag_alternatives[sectag];
                    assert(functor_desc->MR_du_functor_orig_arity == 0);
                    assert(functor_desc->MR_du_functor_exist_info == NULL);
                    arg_vector = NULL;
                    break;

                case MR_SECTAG_REMOTE:
                    sectag = MR_field(ptag, data, 0);
                    functor_desc = ptag_layout->MR_sectag_alternatives[sectag];
                    arg_vector = (MR_Word *) MR_body(data, ptag) + 1;
                    break;

                case MR_SECTAG_VARIABLE:
                    MR_fatal_error("MR_table_type(): unexpected variable");

                default:
                    MR_fatal_error("MR_table_type(): unknown sectag_locn");

                }

                MR_DEBUG_TABLE_ENUM(table,
                    MR_type_ctor_num_functors(type_ctor_info),
                    functor_desc->MR_du_functor_ordinal);

                exist_info = functor_desc->MR_du_functor_exist_info;
                if (exist_info != NULL) {
                    int                     num_ti_plain;
                    int                     num_ti_in_tci;
                    int                     num_tci;
                    const MR_DuExistLocn    *locns;

                    num_ti_plain = exist_info->MR_exist_typeinfos_plain;
                    num_ti_in_tci = exist_info->MR_exist_typeinfos_in_tci;
                    num_tci = exist_info->MR_exist_tcis;
                    locns = exist_info->MR_exist_typeinfo_locns;

                    for (i = 0; i < num_ti_plain + num_ti_in_tci; i++) {
                        if (locns[i].MR_exist_offset_in_tci < 0) {
                            MR_DEBUG_TABLE_TYPEINFO(table, (MR_TypeInfo)
                                arg_vector[locns[i].MR_exist_arg_num]);
                        } else {
                            MR_DEBUG_TABLE_TYPEINFO(table, (MR_TypeInfo)
                                MR_typeclass_info_param_type_info(
                                    arg_vector[locns[i].MR_exist_arg_num],
                                    locns[i].MR_exist_offset_in_tci));
                        }
                    }
                    meta_args = num_ti_plain + num_tci;
                } else {
                    meta_args = 0;
                }

                for (i = 0; i < functor_desc->MR_du_functor_orig_arity; i++) {
                    if (MR_arg_type_may_contain_var(functor_desc, i)) {
                        arg_type_info = MR_make_type_info_maybe_existq(
                            MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                            functor_desc->MR_du_functor_arg_types[i],
                            arg_vector, functor_desc, &allocated_memory_cells);
                    } else {
                        arg_type_info = MR_pseudo_type_info_is_ground(
                            functor_desc->MR_du_functor_arg_types[i]);
                    }

                    MR_DEBUG_TABLE_ANY(table, arg_type_info,
                        arg_vector[meta_args + i]);
                }

                MR_deallocate(allocated_memory_cells);
            }
            return table;

        case MR_TYPECTOR_REP_NOTAG: 
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
            {
                MR_MemoryList       allocated_memory_cells = NULL;
                MR_TypeInfo         eqv_type_info;

                eqv_type_info = MR_make_type_info(
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                        MR_notag_functor_arg_type, &allocated_memory_cells);
                MR_DEBUG_TABLE_ANY(table, eqv_type_info, data);
                MR_deallocate(allocated_memory_cells);
            }
            return table;

        case MR_TYPECTOR_REP_NOTAG_GROUND: 
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            MR_DEBUG_TABLE_ANY(table, MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_notag->
                MR_notag_functor_arg_type), data);
            return table;

        case MR_TYPECTOR_REP_EQUIV:
            {
                MR_MemoryList       allocated_memory_cells = NULL;
                MR_TypeInfo         eqv_type_info;

                eqv_type_info = MR_make_type_info(
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).MR_layout_equiv,
                    &allocated_memory_cells);
                MR_DEBUG_TABLE_ANY(table, eqv_type_info, data);
                MR_deallocate(allocated_memory_cells);
            }

            return table;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            MR_DEBUG_TABLE_ANY(table, MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).MR_layout_equiv), data);
            return table;

        case MR_TYPECTOR_REP_INT:
            MR_DEBUG_TABLE_INT(table, data);
            return table;

        case MR_TYPECTOR_REP_CHAR:
            MR_DEBUG_TABLE_CHAR(table, data);
            return table;

        case MR_TYPECTOR_REP_FLOAT:
            MR_DEBUG_TABLE_FLOAT(table, data);
            return table;

        case MR_TYPECTOR_REP_STRING:
            MR_DEBUG_TABLE_STRING(table, (MR_String) data);
            return table;

        case MR_TYPECTOR_REP_FUNC:
        case MR_TYPECTOR_REP_PRED:
            {
                /*
                ** XXX tabling of the closures by tabling their code address
                ** and arguments is not yet implemented, due to the overhead
                ** of figuring out the closure argument types.
                */
        #if 0
                MR_closure  closure;
                MR_Word     num_hidden_args;
                int         i;

                closure = (MR_Closure *) data;
                num_hidden_args = closure->MR_closure_num_hidden_args;
                MR_DEBUG_TABLE_INT(table, closure->MR_closure_code);
                for (i = 1; i <= num_hidden_args; i++) {
                    MR_DEBUG_TABLE_ANY(table,
                        <type_info for hidden closure argument number i>,
                        closure->MR_closure_hidden_args(i));
                }
        #else
                /*
                ** Instead, we use the following rather simplistic means of
                ** tabling closures: we just table based on the closure address.
                */
                MR_DEBUG_TABLE_INT(table, data);
        #endif

                return table;
            }

        case MR_TYPECTOR_REP_TUPLE:
           {
                MR_Word     *data_value;
                MR_TypeInfo *arg_type_info_vector;
                int         arity;
                int         i;

                data_value = (MR_Word *) data;
                arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
                arg_type_info_vector =
                    MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info);
                for (i = 0; i < arity; i++) {
                    /* type_infos are counted starting at one */
                    MR_DEBUG_TABLE_ANY(table, arg_type_info_vector[i + 1],
                        data_value[i]);
                }

                return table;
            }

        case MR_TYPECTOR_REP_SUBGOAL:
            MR_fatal_error("Cannot table a subgoal");

        case MR_TYPECTOR_REP_VOID:
            MR_fatal_error("Cannot table a void type");

        case MR_TYPECTOR_REP_C_POINTER:
            MR_fatal_error("Attempt to table a C_POINTER");

        case MR_TYPECTOR_REP_STABLE_C_POINTER:
            /*
            ** This works because a stable C pointer guarantees that the
            ** data structures pointed to, indirectly as well as directly,
            ** will remain stable until the program exits.
            */
            MR_DEBUG_TABLE_INT(table, data);

        case MR_TYPECTOR_REP_STABLE_FOREIGN:
            /*
            ** This works because a stable foreign type guarantees that the
            ** data structures pointed to, indirectly as well as directly,
            ** will remain stable until the program exits.
            */
            MR_DEBUG_TABLE_INT(table, data);

        case MR_TYPECTOR_REP_TYPEINFO:
        case MR_TYPECTOR_REP_TYPEDESC:
            MR_DEBUG_TABLE_TYPEINFO(table, (MR_TypeInfo) data);
            return table;

        case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
            MR_fatal_error("Attempt to table a pseudo_type_desc");

        case MR_TYPECTOR_REP_TYPECTORINFO:
            MR_fatal_error("Attempt to table a type_ctor_info");

        case MR_TYPECTOR_REP_TYPECTORDESC:
            MR_fatal_error("Attempt to table a type_ctor_desc");

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            MR_fatal_error("Attempt to table a type_class_info");

        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
            MR_fatal_error("Attempt to table a base_type_class_info");

        case MR_TYPECTOR_REP_ARRAY:
            {
                MR_TypeInfo     new_type_info;
                MR_MemoryList   allocated_memory_cells = NULL;
                MR_ArrayType    *array;
                MR_Integer      array_size;
                int             i;

                array = (MR_ArrayType *) data;
                array_size = array->size;

                new_type_info = MR_make_type_info(
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                    (MR_PseudoTypeInfo) 1, &allocated_memory_cells);

                for (i = 0; i < array_size; i++) {
                    MR_DEBUG_TABLE_ANY(table, new_type_info,
                        array->elements[i]);
                }

                MR_deallocate(allocated_memory_cells);
                return table;
            }

        case MR_TYPECTOR_REP_SUCCIP:
            MR_fatal_error("Attempt to table a saved succip");

        case MR_TYPECTOR_REP_HP:
            MR_fatal_error("Attempt to table a saved hp");

        case MR_TYPECTOR_REP_CURFR:
            MR_fatal_error("Attempt to table a saved curfr");

        case MR_TYPECTOR_REP_MAXFR:
            MR_fatal_error("Attempt to table a saved maxfr");

        case MR_TYPECTOR_REP_REDOFR:
            MR_fatal_error("Attempt to table a saved redofr");

        case MR_TYPECTOR_REP_REDOIP:
            MR_fatal_error("Attempt to table a saved redoip");

        case MR_TYPECTOR_REP_TRAIL_PTR:
            MR_fatal_error("Attempt to table a saved trail pointer");

        case MR_TYPECTOR_REP_TICKET:
            MR_fatal_error("Attempt to table a saved ticket");

        case MR_TYPECTOR_REP_FOREIGN:
            MR_fatal_error("Attempt to table a value of a foreign type");

        case MR_TYPECTOR_REP_REFERENCE:
            MR_fatal_error("Attempt to table a value of a reference type");

        case MR_TYPECTOR_REP_UNKNOWN: /* fallthru */
            MR_fatal_error("Unknown layout tag in table_any");
    }

    MR_fatal_error("MR_table_type: unexpected fallthrough");
}

/*---------------------------------------------------------------------------*/

void
MR_table_report_statistics(FILE *fp)
{
    fprintf(fp, "hash table search/insert statistics:\n");

#ifdef  MR_TABLE_STATISTICS
    if (MR_table_hash_lookups == 0) {
        fprintf(fp, "no successful probes\n");
    } else {
        fprintf(fp, "successful   %8d, with an average of %6.3f comparisons\n",
            MR_table_hash_lookups,
            (float) MR_table_hash_lookup_probes /
                (float) MR_table_hash_lookups);
    }

    if (MR_table_hash_inserts == 0) {
        fprintf(fp, "no unsuccessful probes\n");
    } else {
        fprintf(fp, "unsuccessful %8d, with an average of %6.3f comparisons\n",
            MR_table_hash_inserts,
            (float) MR_table_hash_insert_probes /
                (float) MR_table_hash_inserts);
    }

    fprintf(fp, "rehash operations: %d, per search: %6.3f%%\n",
        MR_table_hash_resizes,
        (float) (100 * MR_table_hash_resizes) /
            (float) (MR_table_hash_lookups + MR_table_hash_inserts));
    fprintf(fp, "slots rehashed by rehash operations: %d\n",
        MR_table_hash_resize_old_entries);
    fprintf(fp, "slots initialized by rehash operations: %d\n",
        MR_table_hash_resize_new_entries);
    fprintf(fp, "chunk allocations: %d\n", MR_table_hash_allocs);

  #ifdef    MR_USE_MINIMAL_MODEL_STACK_COPY
    fprintf(fp, "\n");
    MR_minimal_model_report_stats(fp);
  #endif
  #ifdef    MR_USE_MINIMAL_MODEL_OWN_STACKS
    fprintf(fp, "\n");
    MR_mm_own_stacks_report_stats(fp);
  #endif
#else
    fprintf(fp, "not enabled\n");
#endif
}

/*---------------------------------------------------------------------------*/

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
MR_print_loopcheck_tip(FILE *fp, const MR_Proc_Layout *proc, MR_TrieNode table)
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
MR_print_memo_tip(FILE *fp, const MR_Proc_Layout *proc, MR_TrieNode table)
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
MR_print_memo_non_record(FILE *fp, const MR_Proc_Layout *proc,
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
MR_print_answerblock(FILE *fp, const MR_Proc_Layout *proc,
    MR_Word *answer_block)
{
    const MR_PseudoTypeInfo *ptis;
    MR_PseudoTypeInfo   pti;
    MR_TypeCtorInfo     tci;
    int         num_inputs;
    int         num_outputs;
    int         i;

    num_inputs = proc->MR_sle_table_info.MR_table_gen->
        MR_table_gen_num_inputs;
    num_outputs = proc->MR_sle_table_info.MR_table_gen->
        MR_table_gen_num_outputs;

    ptis = proc->MR_sle_table_info.MR_table_gen->MR_table_gen_ptis;
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
                (double) MR_unbox_float(
                        (MR_Box) answer_block[i]));
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
    #ifdef MR_USE_GCC_NESTED_FUNCTIONS
      
        static void MR_CALL
        mercury__table_builtin__table_memo_return_all_answers_2_p_0(
            MR_AnswerList answer_list0, MR_Box *boxed_answer_block,
            MR_NestedCont cont);

        static void MR_CALL
        mercury__table_builtin__table_memo_return_all_answers_2_p_0(
                MR_AnswerList answer_list0, MR_Box *boxed_answer_block_ptr,
                MR_NestedCont cont)
        {
            MR_AnswerList answer_list;
            while (answer_list0 != NULL) {
                answer_list = answer_list0->MR_aln_next_answer;
                *boxed_answer_block_ptr =
                     (MR_Box) answer_list0->MR_aln_answer_block;
                cont();
                answer_list0 = answer_list;
            }
        }
        
        void MR_CALL
        mercury__table_builtin__table_memo_return_all_answers_multi_2_p_0(
            MR_Box boxed_record, MR_Box *boxed_answer_block_ptr,
            MR_NestedCont cont)
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
                boxed_answer_block_ptr, cont);
        }
       
        void MR_CALL
        mercury__table_builtin__table_memo_return_all_answers_nondet_2_p_0(
            MR_Box boxed_record, MR_Box *boxed_answer_block_ptr,
            MR_NestedCont cont)
        {
            MR_MemoNonRecordPtr record;
            MR_AnswerList       list;

            record = (MR_MemoNonRecordPtr) boxed_record;
            list = record->MR_mn_answer_list;
            mercury__table_builtin__table_memo_return_all_answers_2_p_0(list,
                boxed_answer_block_ptr, cont);
        }

    #else   /* ! MR_USE_GCC_NESTED_FUNCTIONS */

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

    #endif /* MR_USE_GCC_NESTED_FUNCTIONS */
#else   /* MR_HIGHLEVEL_CODE */

MR_define_extern_entry(MR_MEMO_NON_RET_ALL_NONDET_ENTRY);
MR_define_extern_entry(MR_MEMO_NON_RET_ALL_MULTI_ENTRY);

MR_EXTERN_USER_PROC_ID_PROC_LAYOUT(MR_DETISM_NON, 0, -1,
    MR_PREDICATE, table_builtin, table_memo_return_all_answers_nondet, 2, 0);
MR_EXTERN_USER_PROC_ID_PROC_LAYOUT(MR_DETISM_NON, 0, -1,
    MR_PREDICATE, table_builtin, table_memo_return_all_answers_multi, 2, 0);

#define MEMO_NON_RET_ALL_NONDET_LABEL(name)                             \
    MR_label_name(MR_MEMO_NON_RET_ALL_NONDET_ENTRY, name)
#define MEMO_NON_RET_ALL_MULTI_LABEL(name)                             \
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

    /* Consider not creating the stack frame if cur_node is NULL. */

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

    /* Consider not creating the stack frame if cur_node is NULL. */

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

#endif  /* MR_HIGHLEVEL_CODE */

/* Ensure that the initialization code for the above modules gets to run. */
/*
INIT mercury_sys_init_table_modules
*/

MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc table_memo_non_module;

/* forward declarations to suppress gcc -Wmissing-decl warnings */
void mercury_sys_init_table_modules_init(void);
void mercury_sys_init_table_modules_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_table_modules_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    table_memo_non_module();
#endif  /* MR_HIGHLEVEL_CODE */
}

void mercury_sys_init_table_modules_init_type_tables(void)
{
    /* no types to register */
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp)
{
    /* no proc_statics to write out */
    /* XXX we need to fix the deep profiling */
    /* of model_non memo tabled predicates */
}
#endif

/*---------------------------------------------------------------------------*/
