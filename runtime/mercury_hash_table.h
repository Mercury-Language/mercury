// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-1995, 1997-1998,2000, 2004-2005 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Defines the interface to the hash table module.
//
// This file supplies data manipulation routines to other modules;
// it does not store any data itself. Its routines are generic,
// applicable to the storage of any kind of data structure with
// a primary key and a hash function on it.
//
// Note that this module has nothing to do with the implementation
// of the "tabling" pragmas such as `pragma memo' -- the implementation
// of those features mostly uses Tries, not hash tables, and is defined
// in mercury_tabling.h.

#ifndef MERCURY_HASH_TABLE_H
#define MERCURY_HASH_TABLE_H

#include "mercury_std.h"    // for MR_bool
#include "mercury_dlist.h"  // for MR_Dlist

typedef struct {
    int         MR_ht_size;
    MR_Dlist    **MR_ht_store;
    const void  *(*MR_ht_key)(const void *);    // applied to entries
    int         (*MR_ht_hash)(const void *);    // applied to keys
    MR_bool     (*MR_ht_equal)(const void *, const void *);
                                                // applied to two keys
} MR_Hash_Table;

#define MR_tablekey(table)      (*(table->MR_ht_key))
#define MR_tablehash(table)     (*(table->MR_ht_hash))
#define MR_tableequal(table)    (*(table->MR_ht_equal))

// Initialize a table.

extern  void        MR_ht_init_table(MR_Hash_Table *);

// Look up and return the entry corresponding to the key in a table.

extern  void        *MR_ht_lookup_table(const MR_Hash_Table *,
                        const void *);

// Try to insert a new entry into the table. If the table already had an entry
// with the same key, return the entry that was there before and do not insert
// the new entry. If the entry is new, return NULL.

extern  const void  *MR_ht_insert_table(const MR_Hash_Table *, void *);

// Return all table entries in a list.

extern  MR_Dlist    *MR_ht_get_all_entries(const MR_Hash_Table *);

// Process all table entries with the specified function.

extern  void        MR_ht_process_all_entries(const MR_Hash_Table *,
                        void f(const void *));

// Convert a string to a positive int. The return value mod the table size
// is a good hash value.

extern  int         MR_ht_str_to_int(const char *);

// These wrappers around calls to the above functions can avoid the need for
// explicit address-of operators and casts.

#define MR_init_hash_table(t)           MR_ht_init_table(&t)
#define MR_lookup_hash_table(t, k)      MR_ht_lookup_table(&t, (const void *) k)
#define MR_insert_hash_table(t, e)      MR_ht_insert_table(&t, (void *) e)
#define MR_get_all_entries(t)           MR_ht_get_all_entries(&t)
#define MR_process_all_entries(t, f)    MR_ht_process_all_entries(&t, f)
#define MR_str_to_int(val)              MR_ht_str_to_int(val)

#endif // not MERCURY_HASH_TABLE_H
