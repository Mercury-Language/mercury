// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This files defines the bodies of the variants of the
// MR_int_fix_index_lookup_or_add() function.
//
// NOTE: changes to this function will probably also have to be reflected
// in the places listed in mercury_type_info.h.

    if (t->MR_fix_table == NULL) {
        MR_table_record_fix_alloc(sizeof(MR_TableNode) * range);
        t->MR_fix_table = MR_TABLE_NEW_ARRAY(MR_TableNode, range);
        MR_memset(t->MR_fix_table, 0, sizeof(MR_TableNode) * range);
    }

#ifdef  MR_TABLE_DEBUG
    if (key >= range) {
        MR_fatal_error("MR_int_fix_index_lookup_or_add: key out of range");
    }
#endif

    return &t->MR_fix_table[key];
