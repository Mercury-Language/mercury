// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2004, 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// The code in this file implements several functions in mercury_tabling.h.
// It is intended to be used only in that file, in the environment of the
// macros defined in that file.

    MR_HashTable        *table;
    MR_HashTableSlotPtr *hash_table;
    table_type          *slot;
    MR_Integer          abs_hash;
    MR_Integer          home;
    MR_TABLE_DECLARE_KEY_COMPARE_COUNT

    debug_key_msg(key, "%"key_format, key_cast);

    // Deref the table pointer.
    table = t->MR_hash_table;
    // Has the table been built?
    if (table == NULL) {
        MR_table_record_hash_table_alloc_count(sizeof(MR_HashTable) +
            (HASH_TABLE_START_SIZE * sizeof(MR_HashTableSlotPtr)));
        MR_CREATE_HASH_TABLE(t->MR_hash_table, table_type,
            table_field, HASH_TABLE_START_SIZE);
        // Deref the table pointer.
        table = t->MR_hash_table;
    }

    // Rehash the table if it has grown too full.
    if (table->value_count > table->threshold) {
        MR_HashTableSlotPtr     *old_hash_table;
        MR_HashTableSlotPtr     *new_hash_table;
        int                     new_threshold;
        int                     old_bucket;
        int                     new_bucket;
        int                     old_size;
        int                     new_size;
        table_type              *next_slot;

        old_size = table->size;
        new_size = next_prime(old_size);
        new_threshold = (MR_Integer) ((float) new_size  * MAX_LOAD_FACTOR);
        debug_resize_msg(old_size, new_size, new_threshold);
        MR_table_record_hash_resize_count(old_size, new_size);

        new_hash_table = MR_TABLE_NEW_ARRAY(MR_HashTableSlotPtr, new_size);
        for (new_bucket = 0; new_bucket < new_size; new_bucket++) {
            new_hash_table[new_bucket].table_field = NULL;
        }

        old_hash_table = table->hash_table;
        for (old_bucket = 0; old_bucket < old_size; old_bucket++) {
            slot = old_hash_table[old_bucket].table_field;
            while (slot != NULL) {
                debug_rehash_msg(old_bucket);

                abs_hash = hash(slot->key);
                if (abs_hash < 0) {
                    abs_hash = -abs_hash;
                }

                new_bucket = abs_hash % new_size;
                next_slot = slot->next;
                slot->next = new_hash_table[new_bucket].table_field;
                new_hash_table[new_bucket].table_field = slot;

                slot = next_slot;
            }
        }

        MR_table_free(old_hash_table);
        table->hash_table = new_hash_table;
        table->size = new_size;
        table->threshold = new_threshold;
    }

    abs_hash = hash(key);
    if (abs_hash < 0) {
            abs_hash = -abs_hash;
    }

    home = abs_hash % table->size;

    // Find the element if it is present.
    hash_table = table->hash_table;
    slot = hash_table[home].table_field;
    while (slot != NULL) {
        debug_key_compare_msg(home);
        MR_table_record_hash_key_compare_count();

        if (equal_keys(key, slot->key)) {
            MR_table_record_hash_dupl_count();
            debug_lookup_msg(home);
            return &slot->data;
        }

        slot = slot->next;
    }

    // Check whether we are allowed to add the element.
    if (lookup_only) {
        return NULL;
    }

    // Add the element.
    debug_insert_msg(home);
    MR_table_record_hash_not_dupl_count();

    if (table->freeleft == 0) {
        MR_AllocRecord  *record;

        table->freespace.table_field = MR_TABLE_NEW_ARRAY(table_type,
            CHUNK_SIZE);
        table->freeleft = CHUNK_SIZE;

        record = MR_TABLE_NEW(MR_AllocRecord);
        record->chunk.table_field = table->freespace.table_field;
        record->next = table->allocrecord;
        table->allocrecord = record;

        MR_table_record_hash_links_alloc_count(sizeof(MR_AllocRecord) +
            (CHUNK_SIZE * sizeof(table_type)));
    }

    slot = table->freespace.table_field;
    table->freespace.table_field++;
    table->freeleft--;

    slot->key = key;
    slot->data.MR_integer = 0;
    slot->next = hash_table[home].table_field;
    hash_table[home].table_field = slot;

    table->value_count++;

    return &slot->data;
