/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1997-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"

#include "mercury_type_info.h"
#include "mercury_ho_call.h"
#include <stdio.h>
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

typedef union {
        MR_IntHashTableSlot     *int_slot_ptr;
        MR_FloatHashTableSlot   *float_slot_ptr;
        MR_StringHashTableSlot  *string_slot_ptr;
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
** the slots themselves is that the latter would equire the addresses of some
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
static MR_Word primes[NUM_OF_PRIMES] =
        {127, 257, 509, 1021, 2053, 4099, 8191, 16381, 32771, 65537, 131071,
        262147, 524287, 1048573, 2097143, 4194301};

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
  #define record_resize_count() do { MR_table_hash_resizes++; } while (0)
  #define record_alloc_count()  do { MR_table_hash_allocs++; } while (0)
#else
  #define DECLARE_PROBE_COUNT
  #define record_probe_count()  ((void) 0)
  #define record_lookup_count() ((void) 0)
  #define record_insert_count() ((void) 0)
  #define record_resize_count() ((void) 0)
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

#define MR_CREATE_HASH_TABLE(table_ptr, table_type, table_field, table_size)  \
        do {                                                                  \
                MR_Word         i;                                            \
                MR_HashTable    *newtable;                                    \
                                                                              \
                newtable = MR_TABLE_NEW(MR_HashTable);                        \
                                                                              \
                newtable->size = table_size;                                  \
                newtable->threshold = (MR_Integer) ((float) table_size        \
                                * MAX_LOAD_FACTOR);                           \
                newtable->value_count = 0;                                    \
                newtable->freespace.table_field = NULL;                       \
                newtable->freeleft = 0;                                       \
                newtable->allocrecord = NULL;                                 \
                newtable->hash_table = MR_TABLE_NEW_ARRAY(MR_HashTableSlotPtr,\
                                table_size);                                  \
                                                                              \
                for (i = 0; i < table_size; i++) {                            \
                        newtable->hash_table[i].table_field = NULL;           \
                }                                                             \
                                                                              \
                table_ptr = newtable;                                         \
        } while (0)

#define MR_GENERIC_HASH_LOOKUP_OR_ADD                                         \
        MR_HashTable    *table;                                               \
        table_type      *slot;                                                \
        MR_Integer      abs_hash;                                             \
        MR_Integer      home;                                                 \
        DECLARE_PROBE_COUNT                                                   \
                                                                              \
        debug_key_msg(key, key_format, key_cast);                             \
                                                                              \
        /* Has the table been built? */                                       \
        if (t->MR_hash_table == NULL) {                                       \
                MR_CREATE_HASH_TABLE(t->MR_hash_table, table_type,            \
                        table_field, HASH_TABLE_START_SIZE);                  \
        }                                                                     \
                                                                              \
        table = t->MR_hash_table; /* Deref the table pointer */               \
                                                                              \
        /* Rehash the table if it has grown too full */                       \
        if (table->value_count > table->threshold) {                          \
                MR_HashTableSlotPtr     *new_hash_table;                      \
                int                     new_size;                             \
                int                     new_threshold;                        \
                int                     old_bucket;                           \
                int                     new_bucket;                           \
                table_type              *next_slot;                           \
                                                                              \
                new_size = next_prime(table->size);                           \
                new_threshold = (MR_Integer) ((float) new_size                \
                                * MAX_LOAD_FACTOR);                           \
                debug_resize_msg(table->size, new_size, new_threshold);       \
                record_resize_count();                                        \
                                                                              \
                new_hash_table = MR_TABLE_NEW_ARRAY(MR_HashTableSlotPtr,      \
                                new_size);                                    \
                for (new_bucket = 0; new_bucket < new_size; new_bucket++) {   \
                        new_hash_table[new_bucket].table_field = NULL;        \
                }                                                             \
                                                                              \
                for (old_bucket = 0; old_bucket < table->size; old_bucket++) {\
                        slot = table->hash_table[old_bucket].table_field;     \
                        while (slot != NULL) {                                \
                                debug_rehash_msg(old_bucket);                 \
                                                                              \
                                abs_hash = hash(slot->key);                   \
                                if (abs_hash < 0) {                           \
                                        abs_hash = -abs_hash;                 \
                                }                                             \
                                                                              \
                                new_bucket = abs_hash % new_size;             \
                                next_slot = slot->next;                       \
                                slot->next = new_hash_table[new_bucket].      \
                                        table_field;                          \
                                new_hash_table[new_bucket].table_field = slot;\
                                                                              \
                                slot = next_slot;                             \
                        }                                                     \
                }                                                             \
                                                                              \
                MR_table_free(table->hash_table);                             \
                table->hash_table = new_hash_table;                           \
                table->size = new_size;                                       \
                table->threshold = new_threshold;                             \
        }                                                                     \
                                                                              \
        abs_hash = hash(key);                                                 \
        if (abs_hash < 0) {                                                   \
                abs_hash = -abs_hash;                                         \
        }                                                                     \
                                                                              \
        home = abs_hash % table->size;                                        \
                                                                              \
        /* Find if the element is present. If not add it */                   \
        slot = table->hash_table[home].table_field;                           \
        while (slot != NULL) {                                                \
                debug_probe_msg(home);                                        \
                record_probe_count();                                         \
                                                                              \
                if (equal_keys(key, slot->key)) {                             \
                        record_lookup_count();                                \
                        debug_lookup_msg(home);                               \
                        return &slot->data;                                   \
                }                                                             \
                                                                              \
                slot = slot->next;                                            \
        }                                                                     \
                                                                              \
        debug_insert_msg(home);                                               \
        record_insert_count();                                                \
                                                                              \
        if (table->freeleft == 0) {                                           \
                MR_AllocRecord  *record;                                      \
                                                                              \
                table->freespace.table_field = MR_TABLE_NEW_ARRAY(            \
                                table_type, CHUNK_SIZE);                      \
                table->freeleft = CHUNK_SIZE;                                 \
                                                                              \
                record = MR_TABLE_NEW(MR_AllocRecord);                        \
                record->chunk.table_field = table->freespace.table_field;     \
                record->next = table->allocrecord;                            \
                table->allocrecord = record;                                  \
                                                                              \
                record_alloc_count();                                         \
        }                                                                     \
                                                                              \
        slot = table->freespace.table_field;                                  \
        table->freespace.table_field++;                                       \
        table->freeleft--;                                                    \
                                                                              \
        slot->key = key;                                                      \
        slot->data.MR_integer = 0;                                            \
        slot->next = table->hash_table[home].table_field;                     \
        table->hash_table[home].table_field = slot;                           \
                                                                              \
        table->value_count++;                                                 \
                                                                              \
        return &slot->data;

MR_TrieNode
MR_int_hash_lookup_or_add(MR_TrieNode t, MR_Integer key)
{
#define key_format              "%ld"
#define key_cast                long
#define table_type              MR_IntHashTableSlot
#define table_field             int_slot_ptr
#define hash(key)               (key)
#define equal_keys(k1, k2)      (k1 == k2)
MR_GENERIC_HASH_LOOKUP_OR_ADD
#undef  key_format
#undef  key_cast
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
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
MR_GENERIC_HASH_LOOKUP_OR_ADD
#undef  key_format
#undef  key_cast
#undef  debug_search_key
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
}

MR_TrieNode
MR_string_hash_lookup_or_add(MR_TrieNode t, MR_ConstString key)
{
#define key_format              "%s"
#define key_cast                const char *
#define table_type              MR_StringHashTableSlot
#define table_field             string_slot_ptr
#define hash(key)               (MR_hash_string((MR_Word) key))
#define equal_keys(k1, k2)      (MR_strtest(k1, k2) == 0)
MR_GENERIC_HASH_LOOKUP_OR_ADD
#undef  key_format
#undef  key_cast
#undef  debug_search_key
#undef  table_type
#undef  table_field
#undef  hash
#undef  equal_keys
}

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
                MR_fatal_error(
                        "MR_int_fix_index_lookup_or_add: key out of range");
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
MR_int_start_index_lookup_or_add(MR_TrieNode table,
        MR_Integer start, MR_Integer key)
{
        MR_Integer      diff, size;

        diff = key - start;

#ifdef  MR_TABLE_DEBUG
        if (key < start) {
                MR_fatal_error(
                        "MR_int_start_index_lookup_or_add: small too key");
        }
#endif

        if (table->MR_start_table == NULL) {
                size = MR_max(MR_START_TABLE_INIT_SIZE, diff + 1);
                table->MR_start_table = MR_TABLE_NEW_ARRAY(MR_TableNode,
                                        size + 1);
                memset(table->MR_start_table + 1, 0,
                                        sizeof(MR_TableNode) * size);
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
        MR_TypeCtorInfo         type_ctor_info;
        MR_TrieNode             node;
        MR_TypeInfo             *arg_vector;
        int                     arity;
        int                     i;

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

        if (MR_type_ctor_rep_is_variable_arity(
                MR_type_ctor_rep(type_ctor_info)))
        {
                arity = MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info);
                arg_vector = MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(
                        type_info);
                node = MR_int_hash_lookup_or_add(node, arity);
        } else {
                arity = type_ctor_info->MR_type_ctor_arity;
                arg_vector = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
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
** Due to the depth of the control here, we'll use 4 space indentation.
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

    switch (MR_type_ctor_rep(type_ctor_info)) {
        case MR_TYPECTOR_REP_ENUM: 
        case MR_TYPECTOR_REP_ENUM_USEREQ: 
            MR_DEBUG_TABLE_ENUM(table,
                    MR_type_ctor_num_functors(type_ctor_info), data);
            break;

        case MR_TYPECTOR_REP_RESERVED_ADDR: 
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ: 
            {
                int i;
                MR_ReservedAddrTypeLayout ra_layout =
                            MR_type_ctor_layout(type_ctor_info).layout_reserved_addr;

                /*
                ** First check if this value is one of
                ** the numeric reserved addresses.
                */
                if ((MR_Unsigned) data <
                    (MR_Unsigned) ra_layout->MR_ra_num_res_numeric_addrs)
                {
                    MR_DEBUG_TABLE_ENUM(table,
                        MR_type_ctor_num_functors(type_ctor_info),
                        ra_layout->MR_ra_constants[data]->MR_ra_functor_ordinal);
                    break;
                }

                /*
                ** Next check if this value is one of the
                ** the symbolic reserved addresses.
                */
                for (i = 0; i < ra_layout->MR_ra_num_res_symbolic_addrs; i++) {
                    if (data == (MR_Word) ra_layout->MR_ra_res_symbolic_addrs[i]) {
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
            du_type_layout = MR_type_ctor_layout(type_ctor_info).layout_du;
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
                                MR_typeclass_info_type_info(
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
                            MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
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
            break;

        case MR_TYPECTOR_REP_NOTAG: 
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
            {
                MR_MemoryList       allocated_memory_cells = NULL;
                MR_TypeInfo         eqv_type_info;

                eqv_type_info = MR_make_type_info(
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).layout_notag->
                        MR_notag_functor_arg_type, &allocated_memory_cells);
                MR_DEBUG_TABLE_ANY(table, eqv_type_info, data);
                MR_deallocate(allocated_memory_cells);
            }
            break;

        case MR_TYPECTOR_REP_NOTAG_GROUND: 
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            MR_DEBUG_TABLE_ANY(table, MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).layout_notag->
                MR_notag_functor_arg_type), data);
            break;

        case MR_TYPECTOR_REP_EQUIV:
            {
                MR_MemoryList       allocated_memory_cells = NULL;
                MR_TypeInfo         eqv_type_info;

                eqv_type_info = MR_make_type_info(
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                    MR_type_ctor_layout(type_ctor_info).layout_equiv,
                    &allocated_memory_cells);
                MR_DEBUG_TABLE_ANY(table, eqv_type_info, data);
                MR_deallocate(allocated_memory_cells);
            }
            break;

        case MR_TYPECTOR_REP_EQUIV_GROUND:
            MR_DEBUG_TABLE_ANY(table, MR_pseudo_type_info_is_ground(
                MR_type_ctor_layout(type_ctor_info).layout_equiv), data);
            break;

        case MR_TYPECTOR_REP_INT:
            MR_DEBUG_TABLE_INT(table, data);
            break;

        case MR_TYPECTOR_REP_CHAR:
            MR_DEBUG_TABLE_CHAR(table, data);
            break;

        case MR_TYPECTOR_REP_FLOAT:
            MR_DEBUG_TABLE_FLOAT(table, data);
            break;

        case MR_TYPECTOR_REP_STRING:
            MR_DEBUG_TABLE_STRING(table, (MR_String) data);
            break;

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
                break;
            }

        case MR_TYPECTOR_REP_TUPLE:
           {
                MR_Word     *data_value;
                MR_TypeInfo *arg_type_info_vector;
                int         arity;
                int         i;

                data_value = (MR_Word *) data;
                arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);
                arg_type_info_vector =
                        MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info);
                for (i = 0; i < arity; i++) {
                    /* type_infos are counted starting at one */
                    MR_DEBUG_TABLE_ANY(table, arg_type_info_vector[i + 1],
                        data_value[i]);
                }
                break;
            }

        case MR_TYPECTOR_REP_VOID:
            MR_fatal_error("Cannot table a void type");
            break;

        case MR_TYPECTOR_REP_C_POINTER:
            MR_fatal_error("Attempt to table a C_POINTER");
            break;

        case MR_TYPECTOR_REP_TYPEINFO:
            MR_DEBUG_TABLE_TYPEINFO(table, (MR_TypeInfo) data);
            break;

        case MR_TYPECTOR_REP_TYPECTORINFO:
            MR_fatal_error("Attempt to table a type_ctor_info");
            break;

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            MR_fatal_error("Attempt to table a type_class_info");
            break;

        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
            MR_fatal_error("Attempt to table a base_type_class_info");
            break;

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
                    MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info),
                    (MR_PseudoTypeInfo) 1, &allocated_memory_cells);

                for (i = 0; i < array_size; i++) {
                    MR_DEBUG_TABLE_ANY(table, new_type_info,
                        array->elements[i]);
                }

                MR_deallocate(allocated_memory_cells);
                break;
            }

        case MR_TYPECTOR_REP_SUCCIP:
            MR_fatal_error("Attempt to table a saved succip");
            break;

        case MR_TYPECTOR_REP_HP:
            MR_fatal_error("Attempt to table a saved hp");
            break;

        case MR_TYPECTOR_REP_CURFR:
            MR_fatal_error("Attempt to table a saved curfr");
            break;

        case MR_TYPECTOR_REP_MAXFR:
            MR_fatal_error("Attempt to table a saved maxfr");
            break;

        case MR_TYPECTOR_REP_REDOFR:
            MR_fatal_error("Attempt to table a saved redofr");
            break;

        case MR_TYPECTOR_REP_REDOIP:
            MR_fatal_error("Attempt to table a saved redoip");
            break;

        case MR_TYPECTOR_REP_TRAIL_PTR:
            MR_fatal_error("Attempt to table a saved trail pointer");
            break;

        case MR_TYPECTOR_REP_TICKET:
            MR_fatal_error("Attempt to table a saved ticket");
            break;

        case MR_TYPECTOR_REP_UNKNOWN: /* fallthru */
        default:
            MR_fatal_error("Unknown layout tag in table_any");
            break;
    }

    return table;
} /* end table_any() */

/*---------------------------------------------------------------------------*/

void
MR_table_report_statistics(FILE *fp)
{
        fprintf(fp, "hash table search statistics:\n");

#ifdef  MR_TABLE_STATISTICS
        if (MR_table_hash_lookups == 0) {
                fprintf(fp, "no successful searches\n");
        } else {
                fprintf(fp, "successful   %6d, "
                                "with an average of %6.3f comparisons\n",
                        MR_table_hash_lookups,
                        (float) MR_table_hash_lookup_probes /
                                (float) MR_table_hash_lookups);
        }

        if (MR_table_hash_inserts == 0) {
                fprintf(fp, "no unsuccessful searches\n");
        } else {
                fprintf(fp, "unsuccessful %6d, "
                                "with an average of %6.3f comparisons\n",
                        MR_table_hash_inserts,
                        (float) MR_table_hash_insert_probes /
                                (float) MR_table_hash_inserts);
        }

        fprintf(fp, "rehash operations: %d, per search: %6.3f%%\n",
                        MR_table_hash_resizes,
                        (float) (100 * MR_table_hash_resizes) /
                        (float) (MR_table_hash_lookups
                                 + MR_table_hash_inserts));
        fprintf(fp, "chunk allocations: %d\n", MR_table_hash_allocs);
#else
        fprintf(fp, "not enabled\n");
#endif
}

/*---------------------------------------------------------------------------*/

#ifdef  MR_USE_MINIMAL_MODEL

/*
** Save the current state of the Mercury abstract machine, so that the
** current computation may be suspended for a while, and restored later.
** The generator_{maxfr,sp} arguments give the points from which we need
** to copy the nondet and the det stacks. The parts of those stacks below
** the given points will not change between the suspension and the resumption
** of this state, or if they do, the stack segments in the saved state
** will be extended (via extend_consumer_stacks).
*/

static void
save_state(MR_SavedState *saved_state,
        MR_Word *generator_maxfr, MR_Word *generator_sp,
        const char *who, const char *what)
{
        MR_restore_transient_registers();

  #ifdef MR_HIGHLEVEL_CODE
        MR_fatal_error("sorry, not implemented: "
                "minimal model tabling with --high-level-code");
  #else
        saved_state->succ_ip = MR_succip;
        saved_state->s_p = MR_sp;
        saved_state->cur_fr = MR_curfr;
        saved_state->max_fr = MR_maxfr;

        saved_state->non_stack_block_start = generator_maxfr + 1;
        if (MR_maxfr > generator_maxfr) {
                saved_state->non_stack_block_size = MR_maxfr - generator_maxfr;
                saved_state->non_stack_block =
                        MR_table_allocate_words(saved_state->non_stack_block_size);
                MR_table_copy_words(saved_state->non_stack_block,
                        saved_state->non_stack_block_start,
                        saved_state->non_stack_block_size);
        } else {
                saved_state->non_stack_block_size = 0;
                saved_state->non_stack_block = NULL;
        }

        saved_state->det_stack_block_start = generator_sp;
        if (MR_sp > generator_sp) {
                saved_state->det_stack_block_size = (MR_sp - 1) - generator_sp;
                saved_state->det_stack_block =
                        MR_table_allocate_words(saved_state->det_stack_block_size);
                MR_table_copy_words(saved_state->det_stack_block,
                        saved_state->det_stack_block_start,
                        saved_state->det_stack_block_size);
        } else {
                saved_state->det_stack_block_size = 0;
                saved_state->det_stack_block = NULL;
        }

  #endif /* ! MR_HIGHLEVEL_CODE */

        saved_state->gen_next = MR_gen_next;
        saved_state->generator_stack_block = MR_table_allocate_bytes(
                        MR_gen_next * sizeof(MR_GeneratorStackFrame));
        MR_table_copy_bytes(saved_state->generator_stack_block,
                MR_gen_stack, MR_gen_next * sizeof(MR_GeneratorStackFrame));

        saved_state->cut_next = MR_cut_next;
        saved_state->cut_stack_block = MR_table_allocate_bytes(
                        MR_cut_next * sizeof(MR_CutStackFrame));
        MR_table_copy_bytes(saved_state->cut_stack_block,
                MR_cut_stack, MR_cut_next * sizeof(MR_CutStackFrame));

  #ifdef MR_USE_TRAIL
        /*
        ** Saving the trail state here would not be sufficient to handle
        ** the combination of trailing and minimal model tabling.
        ** Consider the following sequence of events:
        **
        **      execution enters a goal being committed across
        **      a new entry is pushed on the trail
        **      a tabled goal suspends,
        **              causing the saving of a trail segment
        **              and then a failure
        **      the goal being committed across fails,
        **              which invokes a failed commit on the trail entry
        **      ...
        **      the tabled goal is resumed,
        **              causing the restoring of the saved trail segment
        **              and then a success
        **      the goal being committed across now succeeds,
        **              which invokes a successful commit on the trail entry
        **
        ** The trail handler will be thoroughly confused by such a sequence.
        */

        MR_fatal_error("Sorry, not implemented: "
                "can't have both minimal model tabling and trailing");
  #endif

  #ifdef MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("\n%s saves %s stacks: ", who, what);
                printf("%d non, %d det, %d generator, %d cut\n",
                        saved_state->non_stack_block_size,
                        saved_state->det_stack_block_size,
                        MR_gen_next, MR_cut_next);

    #ifdef MR_HIGHLEVEL_CODE
                MR_fatal_error("sorry, not implemented: "
                        "minimal model tabling with --high-level-code");
    #else
                printf("non region from ");
                MR_printnondstackptr(saved_state->non_stack_block_start);
                printf(" to ");
                MR_printnondstackptr(MR_maxfr);
                printf(" (both inclusive)\n");
                printf("stored at %p to %p (both inclusive)\n",
                        saved_state->non_stack_block,
                        saved_state->non_stack_block +
                                saved_state->non_stack_block_size - 1);

                printf("det region from ");
                MR_printdetstackptr(saved_state->det_stack_block_start);
                printf(" to ");
                MR_printdetstackptr(MR_sp);
                printf(" (both inclusive)\n");
                printf("stored at %p to %p (both inclusive)\n",
                        saved_state->det_stack_block,
                        saved_state->det_stack_block +
                                saved_state->det_stack_block_size - 1);

                printf("succip = %p, sp = ", (void *) MR_succip);
                MR_printdetstackptr(MR_sp);
                printf("\nmaxfr = ");
                MR_printnondstackptr(MR_maxfr);
                printf(", curfr = ");
                MR_printnondstackptr(MR_curfr);
                printf("\n\n");
    #endif

                MR_print_gen_stack(stdout);

    #ifndef MR_HIGHLEVEL_CODE
                if (MR_tablestackdebug) {
                        MR_dump_nondet_stack(stdout, MR_maxfr);
                }
    #endif
        }
  #endif /* MR_TABLE_DEBUG */

        MR_save_transient_registers();
}

/*
** Restore the state of the Mercury abstract machine from saved_state.
*/

static void
restore_state(MR_SavedState *saved_state, const char *who, const char *what)
{
        MR_restore_transient_registers();

  #ifdef MR_HIGHLEVEL_CODE

        MR_fatal_error("sorry, not implemented: "
                "minimal model tabling with --high-level-code");

  #else

        MR_succip = saved_state->succ_ip;
        MR_sp = saved_state->s_p;
        MR_curfr = saved_state->cur_fr;
        MR_maxfr = saved_state->max_fr;

        MR_table_copy_words(saved_state->non_stack_block_start,
                saved_state->non_stack_block,
                saved_state->non_stack_block_size);

        MR_table_copy_words(saved_state->det_stack_block_start,
                saved_state->det_stack_block,
                saved_state->det_stack_block_size);

  #endif

        MR_gen_next = saved_state->gen_next;
        MR_table_copy_bytes(MR_gen_stack, saved_state->generator_stack_block,
                saved_state->gen_next * sizeof(MR_GeneratorStackFrame));

        MR_cut_next = saved_state->cut_next;
        MR_table_copy_bytes(MR_cut_stack, saved_state->cut_stack_block,
                saved_state->cut_next * sizeof(MR_CutStackFrame));

  #ifdef MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("\n%s restores %s stacks: ", who, what);
                printf("%d non, %d det, %d generator, %d cut\n",
                        saved_state->non_stack_block_size,
                        saved_state->det_stack_block_size,
                        saved_state->gen_next, saved_state->cut_next);

                printf("non region from ");
                MR_printnondstackptr(saved_state->non_stack_block_start);
                printf(" to ");
                MR_printnondstackptr(saved_state->non_stack_block_start +
                        saved_state->non_stack_block_size - 1);
                printf(" (both inclusive)\n");
                printf("stored at %p to %p (both inclusive)\n",
                        saved_state->non_stack_block,
                        saved_state->non_stack_block +
                                saved_state->non_stack_block_size - 1);

                printf("det region from ");
                MR_printdetstackptr(saved_state->det_stack_block_start);
                printf(" to ");
                MR_printdetstackptr(saved_state->det_stack_block_start +
                        saved_state->det_stack_block_size - 1);
                printf(" (both inclusive)\n");
                printf("stored at %p to %p (both inclusive)\n",
                        saved_state->det_stack_block,
                        saved_state->det_stack_block +
                                saved_state->det_stack_block_size - 1);

                printf("succip = %p, sp = ", (void *) MR_succip);
                MR_printdetstackptr(MR_sp);
                printf("\nmaxfr = ");
                MR_printnondstackptr(MR_maxfr);
                printf(", curfr = ");
                MR_printnondstackptr(MR_curfr);
                printf("\n");

                MR_print_gen_stack(stdout);

                if (MR_tablestackdebug) {
                        MR_dump_nondet_stack_from_layout(stdout, MR_maxfr);
                }
        }
  #endif /* MR_table_debug */

        MR_save_transient_registers();
}

static void
print_saved_state_stacks(MR_SavedState *saved_state)
{
        int     i;

        printf("saved state parameters:\n");
        printf("succip:\t");
        MR_printlabel(stdout, saved_state->succ_ip);
        printf("sp:\t");
        MR_printdetstackptr(saved_state->s_p);
        printf("\ncurfr:\t");
        MR_printnondstackptr(saved_state->cur_fr);
        printf("\nmaxfr:\t");
        MR_printnondstackptr(saved_state->max_fr);

        printf("\n\nnondet stack block: %d words from %p\n",
                saved_state->non_stack_block_size,
                saved_state->non_stack_block_start);
        for (i = 0; i < saved_state->non_stack_block_size; i++) {
                printf("%2d: %x\n", i, saved_state->non_stack_block[i]);
        }

        printf("\ndet stack block: %d words from %p\n",
                saved_state->det_stack_block_size,
                saved_state->det_stack_block_start);
        for (i = 0; i < saved_state->det_stack_block_size; i++) {
                printf("%2d: %x\n", i, saved_state->det_stack_block[i]);
        }

        printf("\n");
}

/*
** The saved state of a consumer for a subgoal (say subgoal A) includes
** the stack segments between the tops of the stack at the time that
** A's generator was entered and the time that A's consumer was entered.
** When A becomes a follower of another subgoal B, the responsibility for
** scheduling A's consumers passes to B's generator. Since by definition
** B's nondet stack frame is lower in the stack than A's generator's,
** we need to extend the stack segments of A's consumers to also include
** the parts of the stacks between the generator of B and the generator of A.
*/

MR_declare_entry(mercury__table_builtin__table_nondet_resume_1_0);

static void
extend_consumer_stacks(MR_Subgoal *leader, MR_Consumer *suspension)
{
        MR_Word *arena_block;
        MR_Word *arena_start;
        MR_Word arena_size;
        MR_Word extension_size;
        MR_Word *saved_fr;
        MR_Word *real_fr;
        MR_Word frame_size;
        MR_Word offset;

#ifdef  MR_TABLE_DEBUG
        if (MR_tablestackdebug) {
                printf("\nextending saved consumer stacks\n");
                print_saved_state_stacks(&suspension->saved_state);
        }
#endif

        arena_start = leader->generator_sp;
        extension_size = suspension->saved_state.det_stack_block_start
                        - arena_start;
        arena_size  = extension_size
                        + suspension->saved_state.det_stack_block_size;
        if (arena_size != 0) {
                assert(arena_start + arena_size
                                == suspension->saved_state.s_p - 1);
        }

        arena_block = MR_table_allocate_words(arena_size);

        MR_table_copy_words(arena_block, arena_start, extension_size);
        MR_table_copy_words(arena_block + extension_size,
                suspension->saved_state.det_stack_block,
                suspension->saved_state.det_stack_block_size);

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("extending det stack of suspension %p for %p\n",
                        suspension, leader);
                printf("start: old %p, new %p\n",
                        suspension->saved_state.det_stack_block_start,
                        arena_start);
                printf("size:  old %d, new %d\n",
                        suspension->saved_state.det_stack_block_size,
                        arena_size);
                printf("block: old %p, new %p\n",
                        suspension->saved_state.det_stack_block,
                        arena_block);
        }
#endif

        suspension->saved_state.det_stack_block = arena_block;
        suspension->saved_state.det_stack_block_size = arena_size;
        suspension->saved_state.det_stack_block_start = arena_start;

        arena_start = leader->generator_maxfr + 1;
        extension_size = suspension->saved_state.non_stack_block_start
                        - arena_start;
        arena_size  = extension_size
                        + suspension->saved_state.non_stack_block_size;
        assert(leader->generator_maxfr + arena_size
                        == suspension->saved_state.max_fr);

        arena_block = MR_table_allocate_words(arena_size);

        MR_table_copy_words(arena_block, arena_start, extension_size);
        MR_table_copy_words(arena_block + extension_size,
                suspension->saved_state.non_stack_block,
                suspension->saved_state.non_stack_block_size);

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("extending non stack of suspension %p for %p\n",
                        suspension, leader);
                printf("start: old %p, new %p\n",
                        suspension->saved_state.non_stack_block_start,
                        arena_start);
                printf("size:  old %d, new %d\n",
                        suspension->saved_state.non_stack_block_size,
                        arena_size);
                printf("block: old %p, new %p\n",
                        suspension->saved_state.non_stack_block,
                        arena_block);
        }
#endif

        suspension->saved_state.non_stack_block = arena_block;
        suspension->saved_state.non_stack_block_size = arena_size;
        suspension->saved_state.non_stack_block_start = arena_start;

#ifdef  MR_TABLE_DEBUG
        if (MR_tablestackdebug) {
                printf("\nbefore pickling nondet stack\n");
                print_saved_state_stacks(&suspension->saved_state);
        }
#endif

        saved_fr = suspension->saved_state.non_stack_block +
                suspension->saved_state.non_stack_block_size - 1;
        real_fr = suspension->saved_state.non_stack_block_start +
                suspension->saved_state.non_stack_block_size - 1;
        while (saved_fr > suspension->saved_state.non_stack_block) {
                frame_size = real_fr - MR_prevfr_slot(saved_fr);

                if (saved_fr - frame_size
                        > suspension->saved_state.non_stack_block)
                {
                        *MR_redoip_addr(saved_fr) =
                                (MR_Word) MR_ENTRY(MR_do_fail);

#ifdef  MR_TABLE_DEBUG
                        if (MR_tabledebug) {
                                printf("do_fail to redoip at %p (%d)\n",
                                        MR_redoip_addr(saved_fr),
                                        MR_redoip_addr(saved_fr) -
                                        suspension->
                                        saved_state.non_stack_block);
                        }
#endif
                } else {
                        *MR_redoip_addr(saved_fr) = (MR_Word)
                                MR_ENTRY(mercury__table_builtin__table_nondet_resume_1_0);
#ifdef  MR_TABLE_DEBUG
                        if (MR_tabledebug) {
                                printf("resume to redoip at %p (%d)\n",
                                        MR_redoip_addr(saved_fr),
                                        MR_redoip_addr(saved_fr) -
                                        suspension->
                                        saved_state.non_stack_block);
                        }
#endif
                }

                saved_fr -= frame_size;
                real_fr -= frame_size;
        }

#ifdef  MR_TABLE_DEBUG
        if (MR_tablestackdebug) {
                printf("\nfinished extending saved consumer stacks\n");
                print_saved_state_stacks(&suspension->saved_state);
        }
#endif
}

/*
** When we discover that two subgoals depend on each other, neither can be
** completed alone. We therefore pass responsibility for completing all
** the subgoals in an SCC to the subgoal whose nondet stack frame is
** lowest in the nondet stack.
*/

static void
make_subgoal_follow_leader(MR_Subgoal *this_follower, MR_Subgoal *leader)
{
        MR_Consumer             *suspension;
        MR_SubgoalList          sub_followers;
        MR_ConsumerList         suspend_list;

        MR_restore_transient_registers();

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("making %p follow %p\n", this_follower, leader);
        }
#endif

        for (sub_followers = this_follower->followers;
                sub_followers != NULL; sub_followers = sub_followers->next)
        {
                for (suspend_list = sub_followers->item->consumer_list;
                        suspend_list != NULL;
                        suspend_list = suspend_list->next)
                {
                        MR_save_transient_registers();
                        extend_consumer_stacks(leader, suspend_list->item);
                        MR_restore_transient_registers();
                }
        }

        this_follower->leader = leader;
        *(leader->followers_tail) = this_follower->followers;
        this_follower->followers = NULL;

        MR_save_transient_registers();
}

/*
** The following procedure saves the state of the Mercury runtime
** so that it may be used in the table_nondet_resume procedure below to return
** answers through this saved state. The procedure table_nondet_suspend is
** declared as nondet but the code below is obviously of detism failure;
** the reason for this is quite simple. Normally when a nondet proc
** is called it will first return all of its answers and then fail. In the
** case of calls to this procedure this is reversed: first the call will fail
** then later on, when the answers are found, answers will be returned.
** It is also important to note that the answers are returned not from the
** procedure that was originally called (table_nondet_suspend) but from the
** procedure table_nondet_resume. So essentially what is below is the code
** to do the initial fail; the code to return the answers is in
** table_nondet_resume.
*/

#ifndef MR_HIGHLEVEL_CODE

MR_declare_entry(mercury__table_builtin__table_nondet_resume_1_0);
MR_declare_entry(MR_do_trace_redo_fail);
MR_declare_entry(MR_table_nondet_commit);
MR_define_extern_entry(mercury__table_builtin__table_nondet_suspend_2_0);
MR_MAKE_PROC_LAYOUT(mercury__table_builtin__table_nondet_suspend_2_0,
        MR_DETISM_NON, 0, MR_LONG_LVAL_TYPE_UNKNOWN,
        MR_PREDICATE, "table_builtin", "table_nondet_suspend", 2, 0);
MR_BEGIN_MODULE(table_nondet_suspend_module)
        MR_init_entry_sl(mercury__table_builtin__table_nondet_suspend_2_0);
        MR_INIT_PROC_LAYOUT_ADDR(mercury__table_builtin__table_nondet_suspend_2_0);
MR_BEGIN_CODE

MR_define_entry(mercury__table_builtin__table_nondet_suspend_2_0);
{
        MR_TrieNode     table;
        MR_Subgoal      *subgoal;
        MR_Consumer     *consumer;
        MR_ConsumerList listnode;
        MR_Integer      cur_gen;
        MR_Integer      cur_cut;
        MR_Word         *fr;
        MR_Word         *prev_fr;
        MR_Word         *stop_addr;
        MR_Word         offset;
        MR_Word         *clobber_addr;

        /*
        ** This frame is not used in table_nondet_suspend, but it is copied
        ** to the suspend list as part of the saved nondet stack fragment,
        ** and it *will* be used when table_nondet_resume copies back the
        ** nondet stack fragment. The framevar slot is for use by
        ** table_nondet_resume.
        */
        MR_mkframe("mercury__table_builtin__table_nondet_suspend", 1,
                MR_ENTRY(MR_do_fail));

        table = (MR_TrieNode) MR_r1;
        subgoal = table->MR_subgoal;
        consumer = MR_table_allocate_bytes(sizeof(MR_Consumer));
        consumer->remaining_answer_list_ptr = &subgoal->answer_list;

        MR_save_transient_registers();
        save_state(&(consumer->saved_state),
                subgoal->generator_maxfr, subgoal->generator_sp,
                "suspension", "consumer");
        MR_restore_transient_registers();

        cur_gen = MR_gen_next - 1;
        cur_cut = MR_cut_next - 1;
        stop_addr = consumer->saved_state.non_stack_block_start;
        for (fr = MR_maxfr; fr > stop_addr; fr = MR_prevfr_slot(fr))
        {
                offset = MR_redoip_addr(fr) -
                        consumer->saved_state.non_stack_block_start;
                clobber_addr = consumer->saved_state.non_stack_block + offset;
#if 0
                if (MR_tablestackdebug) {
                        printf("redoip addr ");
                        MR_printnondstackptr(MR_redoip_addr(fr));
                        printf(", offset %d from start, ", offset);
                        printf("saved copy at %p\n", clobber_addr);
                }
#endif

                if (fr == MR_gen_stack[cur_gen].generator_frame) {
                        if (MR_gen_stack[cur_gen].generator_table->MR_subgoal
                                        == subgoal)
                        {
                                /*
                                ** This is the nondet stack frame of the
                                ** generator corresponding to this consumer.
                                */

                                assert(MR_prevfr_slot(fr) == (stop_addr - 1));
                                *clobber_addr = (MR_Word)
                                        MR_ENTRY(mercury__table_builtin__table_nondet_resume_1_0);
#ifdef  MR_TABLE_DEBUG
                                if (MR_tablestackdebug) {
                                        printf("completing redoip "
                                                "of frame at ");
                                        MR_printnondstackptr(fr);
                                        printf(" (in saved copy)\n");
                                }
#endif

                                consumer->saved_state.gen_next = cur_gen + 1;
#ifdef  MR_TABLE_DEBUG
                                if (MR_tabledebug) {
                                        printf("saved gen_next set to %d\n",
                                                cur_gen + 1);
                                }
#endif
                        } else {
                                /*
                                ** This is the nondet stack frame of some
                                ** other generator.
                                */

                                assert(MR_prevfr_slot(fr) != (stop_addr - 1));

                                *clobber_addr = (MR_Word) MR_ENTRY(MR_do_fail);
#ifdef  MR_TABLE_DEBUG
                                if (MR_tablestackdebug) {
                                        printf("clobbering redoip "
                                                "of frame at ");
                                        MR_printnondstackptr(fr);
                                        printf(" (in saved copy)\n");
                                }
#endif

                                MR_save_transient_registers();
                                make_subgoal_follow_leader(
                                        MR_gen_stack[cur_gen].
                                                generator_table->MR_subgoal,
                                        subgoal);
                                MR_restore_transient_registers();
                        }

                        cur_gen--;
                } else if (cur_cut > 0 && fr == MR_cut_stack[cur_cut].frame) {
                        *clobber_addr = (MR_Word) MR_ENTRY(MR_table_nondet_commit);
#ifdef  MR_TABLE_DEBUG
                        if (MR_tablestackdebug) {
                                printf("committing redoip of frame at ");
                                MR_printnondstackptr(fr);
                                printf(" (in saved copy)\n");
                        }
#endif

                        cur_cut--;
                } else {
                        *clobber_addr = (MR_Word) MR_ENTRY(MR_do_fail);
#ifdef  MR_TABLE_DEBUG
                        if (MR_tablestackdebug) {
                                printf("clobbering redoip of frame at ");
                                MR_printnondstackptr(fr);
                                printf(" (in saved copy)\n");
                        }
#endif
                }
        }

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("adding suspension node %p to table %p",
                        consumer, subgoal);
                printf(" at slot %p\n", subgoal->consumer_list_tail);
        }
#endif

        assert(*(subgoal->consumer_list_tail) == NULL);
        listnode = MR_table_allocate_bytes(sizeof(MR_ConsumerListNode));
        *(subgoal->consumer_list_tail) = listnode;
        subgoal->consumer_list_tail = &(listnode->next);
        listnode->item = consumer;
        listnode->next = NULL;
}
        MR_fail();
MR_END_MODULE

MR_Subgoal      *MR_cur_leader;

/*
** The procedure defined below restores answers to suspended consumers.
** It works by restoring the consumer state saved by the consumer's call
** to table_nondet_suspend. By restoring such states and then returning
** answers, table_nondet_resume is essentially returning answers out of
** the call to table_nondet_suspend, not out of the call to
** table_nondet_resume.
**
** The code is arranged as a three level iteration to a fixpoint.
** The three levels are: iterating over all subgoals in a connected component,
** iterating over all consumers of each of those subgoals, and iterating
** over all the answers to be returned to each of those consumers.
** Note that returning an answer could lead to further answers for
** any of the subgoals in the connected component; it can even lead
** to the expansion of the component (i.e. the addition of more subgoals
** to it).
*/

MR_define_extern_entry(mercury__table_builtin__table_nondet_resume_1_0);
MR_declare_label(mercury__table_builtin__table_nondet_resume_1_0_ChangeLoop);
MR_declare_label(mercury__table_builtin__table_nondet_resume_1_0_ReachedFixpoint);
MR_declare_label(mercury__table_builtin__table_nondet_resume_1_0_LoopOverSuspensions);
MR_declare_label(mercury__table_builtin__table_nondet_resume_1_0_ReturnAnswer);
MR_declare_label(mercury__table_builtin__table_nondet_resume_1_0_RedoPoint);

MR_MAKE_PROC_LAYOUT(mercury__table_builtin__table_nondet_resume_1_0,
        MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, MR_LONG_LVAL_TYPE_UNKNOWN,
        MR_PREDICATE, "table_builtin", "table_nondet_resume", 1, 0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
        mercury__table_builtin__table_nondet_resume_1_0_ChangeLoop,
        mercury__table_builtin__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
        mercury__table_builtin__table_nondet_resume_1_0_ReachedFixpoint,
        mercury__table_builtin__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
        mercury__table_builtin__table_nondet_resume_1_0_LoopOverSubgoals,
        mercury__table_builtin__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
        mercury__table_builtin__table_nondet_resume_1_0_LoopOverSuspensions,
        mercury__table_builtin__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
        mercury__table_builtin__table_nondet_resume_1_0_ReturnAnswer,
        mercury__table_builtin__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
        mercury__table_builtin__table_nondet_resume_1_0_RedoPoint,
        mercury__table_builtin__table_nondet_resume_1_0);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
        mercury__table_builtin__table_nondet_resume_1_0_RestartPoint,
        mercury__table_builtin__table_nondet_resume_1_0);

MR_BEGIN_MODULE(table_nondet_resume_module)
        MR_init_entry_sl(mercury__table_builtin__table_nondet_resume_1_0);
        MR_INIT_PROC_LAYOUT_ADDR(mercury__table_builtin__table_nondet_resume_1_0);
        MR_init_label_sl(mercury__table_builtin__table_nondet_resume_1_0_ChangeLoop);
        MR_init_label_sl(mercury__table_builtin__table_nondet_resume_1_0_ReachedFixpoint);
        MR_init_label_sl(mercury__table_builtin__table_nondet_resume_1_0_LoopOverSubgoals);
        MR_init_label_sl(mercury__table_builtin__table_nondet_resume_1_0_LoopOverSuspensions);
        MR_init_label_sl(mercury__table_builtin__table_nondet_resume_1_0_ReturnAnswer);
        MR_init_label_sl(mercury__table_builtin__table_nondet_resume_1_0_RedoPoint);
        MR_init_label_sl(mercury__table_builtin__table_nondet_resume_1_0_RestartPoint);
MR_BEGIN_CODE

MR_define_entry(mercury__table_builtin__table_nondet_resume_1_0);
        MR_cur_leader = MR_top_generator_table();

        if (MR_cur_leader->leader != NULL) {
                /*
                ** The predicate that called table_nondet_resume
                ** is not the leader of its component.
                ** We will leave all answers to be returned
                ** by the leader.
                */

#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("non-leader table_nondet_resume fails\n");
                }
#endif

                (void) MR_pop_generator();
                MR_redo();
        }

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("table_nondet_resume enter: current leader is %p\n",
                        MR_cur_leader);
        }
#endif

        if (MR_cur_leader->resume_info != NULL) {
#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("using existing resume info %p\n",
                                MR_cur_leader->resume_info);
                }
#endif
        } else {
                MR_cur_leader->resume_info = MR_TABLE_NEW(MR_ResumeInfo);

                MR_save_transient_registers();
                save_state(&(MR_cur_leader->resume_info->leader_state),
                        MR_cur_leader->generator_maxfr,
                        MR_cur_leader->generator_sp,
                        "resumption", "generator");
                MR_restore_transient_registers();

#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("creating new resume info %p\n",
                                MR_cur_leader->resume_info);
                }
#endif
        }

        MR_cur_leader->resume_info->changed = MR_TRUE;

MR_define_label(mercury__table_builtin__table_nondet_resume_1_0_ChangeLoop);

        if (MR_cur_leader->resume_info->changed) {
#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("changed flag set\n");
                }
#endif
        } else {
                MR_SubgoalList  table_list;

                for (table_list = MR_cur_leader->resume_info->subgoal_list;
                        table_list != NULL; table_list = table_list->next)
                {
                        if (table_list->item->num_committed_ans
                                != table_list->item->num_ans)
                        {
                                MR_cur_leader->resume_info->changed = MR_TRUE;
#ifdef  MR_TABLE_DEBUG
                                if (MR_tabledebug) {
                                        printf("table %p has new answers\n",
                                                table_list->item);
                                }
#endif
                        }
                }
        }

        if (! MR_cur_leader->resume_info->changed) {
#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("no more changes\n");
                }
#endif
                MR_GOTO_LABEL(
                        mercury__table_builtin__table_nondet_resume_1_0_ReachedFixpoint);
        }

        MR_cur_leader->resume_info->subgoal_list = MR_cur_leader->followers;

        /* For each of the subgoals on our list of followers */
MR_define_label(mercury__table_builtin__table_nondet_resume_1_0_LoopOverSubgoals);

        if (MR_cur_leader->resume_info->subgoal_list == NULL) {
#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("no more subgoals in the followers list\n");
                }
#endif

                MR_GOTO_LABEL(mercury__table_builtin__table_nondet_resume_1_0_ChangeLoop);
        }

        MR_cur_leader->resume_info->cur_subgoal =
                MR_cur_leader->resume_info->subgoal_list->item;
        MR_cur_leader->resume_info->subgoal_list =
                MR_cur_leader->resume_info->subgoal_list->next;

        MR_cur_leader->resume_info->consumer_list =
                MR_cur_leader->resume_info->cur_subgoal->consumer_list;

        MR_cur_leader->resume_info->changed = MR_FALSE;
        MR_cur_leader->resume_info->cur_subgoal->num_committed_ans =
                MR_cur_leader->resume_info->cur_subgoal->num_ans;

        /* For each of the suspended nodes for cur_subgoal */
MR_define_label(mercury__table_builtin__table_nondet_resume_1_0_LoopOverSuspensions);

        if (MR_cur_leader->resume_info->consumer_list == NULL) {
#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("no more suspensions for current subgoal\n");
                }
#endif
                MR_GOTO_LABEL(
                        mercury__table_builtin__table_nondet_resume_1_0_LoopOverSubgoals);
        }

        MR_cur_leader->resume_info->cur_consumer =
                MR_cur_leader->resume_info->consumer_list->item;
        MR_cur_leader->resume_info->consumer_list =
                MR_cur_leader->resume_info->consumer_list->next;

        MR_cur_leader->resume_info->cur_consumer_answer_list =
                *(MR_cur_leader->resume_info->cur_consumer->
                        remaining_answer_list_ptr);

        if (MR_cur_leader->resume_info->cur_consumer_answer_list == NULL) {
#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                        printf("no first answer for this suspension\n");
                }
#endif
                MR_GOTO_LABEL(
                        mercury__table_builtin__table_nondet_resume_1_0_LoopOverSuspensions);
        }

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("resuming consumer %p from table %p\n",
                        (void *) MR_cur_leader->resume_info->cur_consumer,
                        (void *) MR_cur_leader->resume_info->cur_subgoal);
        }
#endif

        MR_save_transient_registers();
        restore_state(
                &(MR_cur_leader->resume_info->cur_consumer->saved_state),
                "resumption", "consumer");
        MR_restore_transient_registers();

        /* check that there is room for exactly one framevar */
        assert((MR_maxfr - MR_prevfr_slot(MR_maxfr)) ==
                (MR_NONDET_FIXED_SIZE + 1));

        MR_gen_next = MR_cur_leader->resume_info->leader_state.gen_next;
        MR_redoip_slot(MR_maxfr) =
                MR_LABEL(mercury__table_builtin__table_nondet_resume_1_0_RedoPoint);
        MR_redofr_slot(MR_maxfr) = MR_maxfr;
        MR_based_framevar(MR_maxfr, 1) = (MR_Word) MR_cur_leader;

MR_define_label(mercury__table_builtin__table_nondet_resume_1_0_ReturnAnswer);

        /*
        ** Return the next answer in MR_cur_leader->resume_info->
        ** cur_consumer_answer_list to the current consumer. Since we have
        ** already restored the context of the suspended consumer before
        ** we returned the first answer, we don't need to restore it again,
        ** since will not have changed in the meantime.
        */

        MR_r1 = (MR_Word) &MR_cur_leader->resume_info->
                cur_consumer_answer_list->answer_data;

        MR_cur_leader->resume_info->cur_consumer->remaining_answer_list_ptr =
                &(MR_cur_leader->resume_info->cur_consumer_answer_list->
                next_answer);

        MR_cur_leader->resume_info->cur_consumer_answer_list =
                MR_cur_leader->resume_info->cur_consumer_answer_list->
                next_answer;

        /*
        ** Return the answer. Since we just restored the state of the
        ** computation that existed when suspend was called, the code
        ** that we return to is the code following the call to suspend.
        */
        MR_succeed();

MR_define_label(mercury__table_builtin__table_nondet_resume_1_0_RedoPoint);
        MR_update_prof_current_proc(MR_LABEL(mercury__table_builtin__table_nondet_resume_1_0));

        /*
        ** This is where the current consumer suspension will go on
        ** backtracking when it wants the next solution. If there is a solution
        ** we haven't returned to this consumer yet, we do so, otherwise we
        ** remember how many answers we have returned to this consumer so far
        ** and move on to the next suspended consumer of the current subgoal.
        */

        MR_cur_leader = (MR_Subgoal *) MR_based_framevar(MR_maxfr, 1);

MR_define_label(mercury__table_builtin__table_nondet_resume_1_0_RestartPoint);
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("cur_consumer_answer_list: %p\n",
                        MR_cur_leader->resume_info->cur_consumer_answer_list);
                printf("*cur_consumer->remaining_answer_list_ptr: %p\n",
                        *(MR_cur_leader->resume_info->cur_consumer->
                                remaining_answer_list_ptr));
        }
#endif

        if (MR_cur_leader->resume_info->cur_consumer_answer_list != NULL) {
                MR_GOTO_LABEL(mercury__table_builtin__table_nondet_resume_1_0_ReturnAnswer);
        }

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
                printf("no more unreturned answers for this suspension\n");
        }
#endif

        if (MR_cur_leader->resume_info->cur_subgoal->num_committed_ans
                != MR_cur_leader->resume_info->cur_subgoal->num_ans)
        {
                MR_cur_leader->resume_info->changed = MR_TRUE;
        }

        MR_GOTO_LABEL(mercury__table_builtin__table_nondet_resume_1_0_LoopOverSuspensions);

MR_define_label(mercury__table_builtin__table_nondet_resume_1_0_ReachedFixpoint);
        {
                MR_SubgoalList  table_list;

                for (table_list = MR_cur_leader->followers;
                        table_list != NULL; table_list = table_list->next)
                {
#ifdef  MR_TABLE_DEBUG
                        if (MR_tabledebug) {
                                printf("marking table %p complete\n",
                                        table_list->item);
                        }
#endif

                        table_list->item->status = MR_SUBGOAL_COMPLETE;
                        table_list->item->num_committed_ans = -1;
                }
        }

        /* Restore the state we had when table_nondet_resume was called */
        MR_save_transient_registers();
        restore_state(&(MR_cur_leader->resume_info->leader_state),
                "resumption", "generator");
        MR_restore_transient_registers();

        /* XXX we should free this cell and its components */
        MR_cur_leader->resume_info = NULL;

        /* We are done with this generator */
        (void) MR_pop_generator();

        MR_proceed();
MR_END_MODULE

MR_define_extern_entry(MR_table_nondet_commit);
MR_BEGIN_MODULE(table_nondet_commit_module)
        MR_init_entry_an(MR_table_nondet_commit);
MR_BEGIN_CODE
MR_define_entry(MR_table_nondet_commit);
        MR_commit_cut();
        MR_fail();
MR_END_MODULE

#endif /* ! MR_HIGHLEVEL_CODE */

#endif  /* MR_USE_MINIMAL_MODEL */

#ifdef MR_HIGHLEVEL_CODE

/*
** We need to define stubs for these, even if MR_USE_MINIMAL_MODEL
** is not enabled, since they are declared as `:- external', and
** hence for profiling grades the generated code will take their
** address to store in the label table.
*/

/* Declare them first, to avoid warnings from gcc -Wmissing-decls */
void MR_CALL mercury__table_builtin__table_nondet_resume_1_p_0(
        MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
        MR_Cont cont, void *cont_env_ptr);
void MR_CALL mercury__table_builtin__table_nondet_suspend_2_p_0(
        MR_C_Pointer subgoal_table_node);

void MR_CALL
mercury__table_builtin__table_nondet_resume_1_p_0(
        MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
        MR_Cont cont, void *cont_env_ptr)
{
        MR_fatal_error("sorry, not implemented: "
                "minimal model tabling with --high-level-code");
}

void MR_CALL
mercury__table_builtin__table_nondet_suspend_2_p_0(
        MR_C_Pointer subgoal_table_node)
{
        MR_fatal_error("sorry, not implemented: "
                "minimal model tabling with --high-level-code");
}

#endif /* MR_HIGHLEVEL_CODE */

/* Ensure that the initialization code for the above modules gets to run. */
/*
INIT mercury_sys_init_table_modules
*/

#ifdef  MR_USE_MINIMAL_MODEL
MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc table_nondet_suspend_module;
MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc table_nondet_resume_module;
MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc table_nondet_commit_module;
#endif

/* forward declarations to suppress gcc -Wmissing-decl warnings */
void mercury_sys_init_table_modules_init(void);
void mercury_sys_init_table_modules_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_table_modules_init(void)
{
#ifdef  MR_USE_MINIMAL_MODEL
        table_nondet_suspend_module();
        table_nondet_resume_module();
        table_nondet_commit_module();
#endif
}

void mercury_sys_init_table_modules_init_type_tables(void)
{
        /* no types to register */
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp)
{
        /* no proc_statics to write out */
}
#endif
