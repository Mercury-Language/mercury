// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-2000, 2004-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Hash table handling module.
//
// This file supplies data manipulation routines to other modules;
// it does not store any data itself. Its routines are generic,
// applicable to the storage of any kind of data structure with
// a primary key and a hash function on it.

#include    "mercury_imp.h"

#include    <stdio.h>
#include    "mercury_std.h"
#include    "mercury_dlist.h"
#include    "mercury_hash_table.h"

void
MR_ht_init_table(MR_Hash_Table *table)
{
    int i;

    table->MR_ht_store = MR_GC_NEW_ARRAY_ATTRIB(MR_Dlist *, table->MR_ht_size,
        MR_ALLOC_SITE_RUNTIME);

    for (i = 0; i < table->MR_ht_size; i++) {
        table->MR_ht_store[i] = NULL;
    }
}

void *
MR_ht_lookup_table(const MR_Hash_Table *table, const void *key)
{
    MR_Dlist    *ptr;
    int         h;

    h = MR_tablehash(table)(key);

#ifdef  MR_HASHDEBUG
    if (! (0 <= h && h < table->MR_ht_size)) {
        fprintf(stderr, "internal error: bad hash index in lookup_table: "
            "%d, table size %d\n",
            h, table->MR_ht_size);
    }
#endif

    MR_for_dlist (ptr, table->MR_ht_store[h]) {
        if (MR_tableequal(table)(key, MR_tablekey(table)(MR_dlist_data(ptr))))
        {
            return (void *) MR_dlist_data(ptr);
        }
    }

    return NULL;
}

const void *
MR_ht_insert_table(const MR_Hash_Table *table, void *entry)
{
    MR_Dlist    *ptr;
    const void  *key;
    int         h;

    key = MR_tablekey(table)(entry);
    h   = MR_tablehash(table)(key);

#ifdef  MR_HASHDEBUG
    if (! (0 <= h && h < table->MR_ht_size)) {
        fprintf(stderr, "internal error: bad hash index in lookup_table: "
            "%d, table size %d\n",
            h, table->MR_ht_size);
    }
#endif

    MR_for_dlist (ptr, table->MR_ht_store[h]) {
        if (MR_tableequal(table)(key, MR_tablekey(table)(MR_dlist_data(ptr))))
        {
            return MR_dlist_data(ptr);
        }
    }

    table->MR_ht_store[h] = MR_dlist_addhead(table->MR_ht_store[h], entry);
    return NULL;
}

MR_Dlist *
MR_ht_get_all_entries(const MR_Hash_Table *table)
{
    MR_Dlist    *list;
    int         i;

    list = MR_dlist_makelist0();
    for (i = 0; i < table->MR_ht_size; i++) {
        MR_dlist_addndlist(list, table->MR_ht_store[i]);
    }

    return list;
}

void
MR_ht_process_all_entries(const MR_Hash_Table *table, void f(const void *))
{
    MR_Dlist    *ptr;
    int         i;

    for (i = 0; i < table->MR_ht_size; i++) {
        MR_for_dlist (ptr, table->MR_ht_store[i]) {
            f(MR_dlist_data(ptr));
        }
    }
}

int
MR_ht_str_to_int(const char *cs)
{
    int         h;
    const char  *s;

    s = cs;
    for (h = 0; *s != '\0'; s++) {
        h = (h << 1) + *s;
    }

    if (h < 0) {
        h = -h;
    }

    return h;
}
