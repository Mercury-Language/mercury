// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#define MR_TABLE_DECLARE_KEY_COMPARE_COUNT                                  \
        MR_Integer  key_compare_count = 0;
#define MR_table_record_hash_key_compare_count()                            \
        do {                                                                \
            key_compare_count++;                                            \
        } while (0)
#define MR_table_record_hash_dupl_count()                                   \
        do {                                                                \
            stats->MR_tss_hash_num_key_compares_dupl += key_compare_count;  \
        } while (0)
#define MR_table_record_hash_not_dupl_count()                               \
        do {                                                                \
            stats->MR_tss_hash_num_key_compares_not_dupl += key_compare_count;\
        } while (0)
#define MR_table_record_hash_resize_count(old, new)                         \
        do {                                                                \
            stats->MR_tss_hash_num_resizes++;                               \
            stats->MR_tss_hash_resize_old_entries += (old);                 \
            stats->MR_tss_hash_resize_new_entries += (new);                 \
        } while (0)
#define MR_table_record_hash_table_alloc_count(numbytes)                    \
        do {                                                                \
            stats->MR_tss_hash_num_table_allocs++;                          \
            stats->MR_tss_hash_num_table_alloc_bytes += (numbytes);         \
        } while (0)
#define MR_table_record_hash_links_alloc_count(numbytes)                    \
        do {                                                                \
            stats->MR_tss_hash_num_link_chunk_allocs++;                     \
            stats->MR_tss_hash_num_link_chunk_alloc_bytes += (numbytes);    \
        } while (0)
