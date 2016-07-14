// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006-2007 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

#define MR_TABLE_DECLARE_KEY_COMPARE_COUNT
#define MR_table_record_hash_key_compare_count()            ((void) 0)
#define MR_table_record_hash_dupl_count()                   ((void) 0)
#define MR_table_record_hash_not_dupl_count()               ((void) 0)
#define MR_table_record_hash_resize_count(old, new)         ((void) 0)
#define MR_table_record_hash_table_alloc_count(numbytes)    ((void) 0)
#define MR_table_record_hash_links_alloc_count(numbytes)    ((void) 0)
