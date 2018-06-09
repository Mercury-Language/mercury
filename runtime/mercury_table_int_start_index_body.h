// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This files defines the bodies of the variants of the
// MR_int_start_index_lookup_or_add() function.

    MR_Integer      diff, size;

    diff = key - start;

#ifdef  MR_TABLE_DEBUG
    if (key < start) {
        MR_fatal_error("MR_int_start_index_lookup_or_add: too small key");
    }
#endif

    if (table->MR_start_table == NULL) {
        size = MR_max(MR_START_TABLE_INIT_SIZE, diff + 1);
        table->MR_start_table = MR_TABLE_NEW_ARRAY(MR_TableNode, size + 1);
        MR_table_record_start_alloc(sizeof(MR_TableNode) * (size + 1));
        MR_memset(table->MR_start_table + 1, 0, sizeof(MR_TableNode) * size);
        table->MR_start_table[0].MR_integer = size;
    } else {
        size = table->MR_start_table[0].MR_integer;
    }

    if (diff >= size) {
        MR_TableNode    *new_array;
        MR_Integer      new_size, i;

        new_size = MR_max(2 * size, diff + 1);
        new_array = MR_TABLE_NEW_ARRAY(MR_TableNode, new_size + 1);
        MR_table_record_start_alloc(sizeof(MR_TableNode) * (new_size + 1));

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
