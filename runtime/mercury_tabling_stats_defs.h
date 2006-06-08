/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#define DECLARE_PROBE_COUNT   MR_Integer      probe_count = 0;
#define record_probe_count()  do { probe_count++; } while (0)
#define record_lookup_count()                                               \
        do {                                                                \
            stats->MR_tss_num_lookup_probes += probe_count;                 \
            stats->MR_tss_num_lookups++;                                    \
        } while (0)
#define record_insert_count()                                               \
        do {                                                                \
            stats->MR_tss_num_insert_probes += probe_count;                 \
            stats->MR_tss_num_inserts++;                                    \
        } while (0)
#define record_resize_count(old, new)                                       \
        do {                                                                \
            stats->MR_tss_num_resizes++;                                    \
            stats->MR_tss_num_resizes_old_entries += (old);                 \
            stats->MR_tss_num_resizes_new_entries += (new);                 \
        } while (0)
#define record_alloc_count()                                                \
        do {                                                                \
            stats->MR_tss_num_allocs++;                                     \
        } while (0)
