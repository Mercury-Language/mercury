// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2000,2002-2007 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_tabling_macros.h
//
// This file defines macros used by the implementation of tabling
// (which means mostly the procedures defined in library/private_builtin.m).
// These macros just call the real implementation routines defined in
// runtime/mercury_tabling.c, but they also optionally print debugging
// information.

#include "mercury_deep_copy.h"  // for MR_make_permanent

#define MR_RAW_TABLE_ANY(table, type_info, value)                           \
    MR_table_type((table), (type_info), (value))

#define MR_RAW_TABLE_ANY_DEBUG(table, type_info, value)                     \
    MR_table_type_debug((table), (type_info), (value))

#define MR_RAW_TABLE_ANY_STATS(stats, table, type_info, value)              \
    MR_table_type_stats((stats), (table), (type_info), (value))

#define MR_RAW_TABLE_ANY_STATS_DEBUG(stats, table, type_info, value)        \
    MR_table_type_stats_debug((stats), (table), (type_info), (value))

#define MR_RAW_TABLE_ANY_ADDR(table, type_info, value)                      \
    MR_word_hash_lookup_or_add((table), (value))

#define MR_RAW_TABLE_ANY_ADDR_STATS(stats, table, type_info, value)         \
    MR_word_hash_lookup_or_add_stats((stats), (table), (value))

#define MR_RAW_TABLE_ENUM(table, range, value)                              \
    MR_int_fix_index_enum_lookup_or_add((table), (range), (value))

#define MR_RAW_TABLE_FOREIGN_ENUM(table, value)                             \
    MR_int_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_ENUM_STATS(stats, table, range, value)                 \
    MR_int_fix_index_enum_lookup_or_add_stats((stats), (table),             \
        (range), (value))

#define MR_RAW_TABLE_FOREIGN_ENUM_STATS(stats, table, value)                \
    MR_int_hash_lookup_or_add_stats((stats), (table), (value))

#define MR_RAW_TABLE_DU(table, range, value)                                \
    MR_int_fix_index_du_lookup_or_add((table), (range), (value))

#define MR_RAW_TABLE_DU_STATS(stats, table, range, value)                   \
    MR_int_fix_index_du_lookup_or_add_stats((stats), (table),               \
        (range), (value))

#define MR_RAW_TABLE_START_INT(table, start, value)                         \
    MR_int_start_index_lookup_or_add((table), (start), (value));

#define MR_RAW_TABLE_START_INT_STATS(stats, table, start, value)            \
    MR_int_start_index_lookup_or_add_stats((stats), (table),                \
        (start), (value));

#define MR_RAW_TABLE_WORD(table, value)                                     \
    MR_int_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_WORD_STATS(stats, table, value)                        \
    MR_int_hash_lookup_or_add_stats((stats), (table), (value));

#define MR_RAW_TABLE_INT(table, value)                                      \
    MR_int_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_INT_STATS(stats, table, value)                         \
    MR_int_hash_lookup_or_add_stats((stats), (table), (value));

#define MR_RAW_TABLE_UINT(table, value)                                     \
    MR_word_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_UINT_STATS(stats, table, value)                        \
    MR_word_hash_lookup_or_add_stats((stats), (table), (value));

// XXX It doesn't make much sense to *hash* -128..127 or 0..255 into a hash
// table that has 127 or 257 slots (the first two hash table sizes).

#define MR_RAW_TABLE_INT8(table, value)                                     \
    MR_int_hash_lookup_or_add((table), (MR_Integer)(value));

#define MR_RAW_TABLE_INT8_STATS(stats, table, value)                        \
    MR_int_hash_lookup_or_add_stats((stats), (table), (MR_Integer)(value));

#define MR_RAW_TABLE_UINT8(table, value)                                    \
    MR_word_hash_lookup_or_add((table), (MR_Word)(value));

#define MR_RAW_TABLE_UINT8_STATS(stats, table, value)                       \
    MR_word_hash_lookup_or_add_stats((stats), (table), (MR_Word)(value));

#define MR_RAW_TABLE_INT16(table, value)                                    \
    MR_int_hash_lookup_or_add((table), (MR_Integer)(value));

#define MR_RAW_TABLE_INT16_STATS(stats, table, value)                       \
    MR_int_hash_lookup_or_add_stats((stats), (table), (MR_Integer)(value));

#define MR_RAW_TABLE_UINT16(table, value)                                   \
    MR_word_hash_lookup_or_add((table), (MR_Word)(value));

#define MR_RAW_TABLE_UINT16_STATS(stats, table, value)                      \
    MR_word_hash_lookup_or_add_stats((stats), (table), (MR_Word)(value));

#define MR_RAW_TABLE_INT32(table, value)                                    \
    MR_int_hash_lookup_or_add((table), (MR_Integer)(value));

#define MR_RAW_TABLE_INT32_STATS(stats, table, value)                       \
    MR_int_hash_lookup_or_add_stats((stats), (table), (MR_Integer)(value));

#define MR_RAW_TABLE_UINT32(table, value)                                   \
    MR_word_hash_lookup_or_add((table), (MR_Word)(value));

#define MR_RAW_TABLE_UINT32_STATS(stats, table, value)                      \
    MR_word_hash_lookup_or_add_stats((stats), (table), (MR_Word)(value));

#define MR_RAW_TABLE_CHAR(table, value)                                     \
    MR_int_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_CHAR_STATS(stats, table, value)                        \
    MR_int_hash_lookup_or_add_stats((stats), (table), (value));

#define MR_RAW_TABLE_FLOAT(table, value)                                    \
    MR_float_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_FLOAT_STATS(stats, table, value)                       \
    MR_float_hash_lookup_or_add_stats((stats), (table), (value));

#define MR_RAW_TABLE_STRING(table, value)                                   \
    MR_string_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_STRING_STATS(stats, table, value)                      \
    MR_string_hash_lookup_or_add_stats((stats), (table), (value));

#define MR_RAW_TABLE_BITMAP(table, value)                                   \
    MR_bitmap_hash_lookup_or_add((table), (value));

#define MR_RAW_TABLE_BITMAP_STATS(stats, table, value)                      \
    MR_bitmap_hash_lookup_or_add_stats((stats), (table), (value));

#define MR_RAW_TABLE_TYPEINFO(table, type_info)                             \
    MR_type_info_lookup_or_add((table), (type_info))

#define MR_RAW_TABLE_TYPEINFO_STATS(stats, table, type_info)                \
    MR_type_info_lookup_or_add_stats((stats), (table), (type_info))

#define MR_RAW_TABLE_TYPECLASSINFO(table, typeclass_info)                   \
    MR_type_class_info_lookup_or_add((table), (typeclass_info))

#define MR_RAW_TABLE_TYPECLASSINFO_STATS(stats, table, typeclass_info)      \
    MR_type_class_info_lookup_or_add_stats((stats), (table), (typeclass_info))

////////////////////////////////////////////////////////////////////////////

#define MR_TABLE_ANY(stats, debug, back, kind, t, t0, type_info, value)     \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_ANY_STATS((stats), (t0),                     \
                (type_info), (value));                                      \
        } else {                                                            \
            (t) = MR_RAW_TABLE_ANY((t0), (type_info), (value));             \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: %s %lx type %p => %p\n",                      \
                (t0), (kind), (long) (value), (type_info), (t));            \
        }                                                                   \
    } while (0)

#define MR_TABLE_ANY_ADDR(stats, debug, back, kind, t, t0, type_info, value)\
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_ANY_ADDR_STATS((stats), (t0),                \
                (type_info), (value));                                      \
        } else {                                                            \
            (t) = MR_RAW_TABLE_ANY_ADDR((t0), (type_info), (value));        \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: %s %lx type %p => %p\n",                      \
                (t0), (kind), (long) (value), (type_info), (t));            \
        }                                                                   \
    } while (0)

#define MR_TABLE_ENUM(stats, debug, back, t, t0, count, value)              \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_ENUM_STATS((stats), (t0), (count), (value)); \
        } else {                                                            \
            (t) = MR_RAW_TABLE_ENUM((t0), (count), (value));                \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: enum %ld of %ld => %p\n",                     \
                (t0), (long) (value), (long) (count), (t));                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_FOREIGN_ENUM(stats, debug, back, t, t0, value)             \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_FOREIGN_ENUM_STATS((stats), (t0), (value));  \
        } else {                                                            \
            (t) = MR_RAW_TABLE_FOREIGN_ENUM((t0), (value));                 \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: foreign enum %ld => %p\n",                    \
                (t0), (long) (value), (t));                                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_DU(stats, debug, back, t, t0, count, value)                \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_DU_STATS((stats), (t0), (count),             \
                (value));                                                   \
        } else {                                                            \
            (t) = MR_RAW_TABLE_DU((t0), (count), (value));                  \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: enum %ld of %ld => %p\n",                     \
                (t0), (long) (value), (long) (count), (t));                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_START_INT(stats, debug, back, t, t0, start, value)         \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_START_INT_STATS((stats), (t0),               \
                (start), (value));                                          \
        } else {                                                            \
            (t) = MR_RAW_TABLE_START_INT((t0), (start), (value));           \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: int %ld - %ld => %p\n",                       \
                (t0), (long) (value), (long) (start), (t));                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_WORD(stats, debug, back, t, t0, value)                     \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_WORD_STATS((stats), (t0), (value));          \
        } else {                                                            \
            (t) = MR_RAW_TABLE_WORD((t0), (value));                         \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: word %ld => %p\n",                            \
                (t0), (long) (value), (t));                                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_INT(stats, debug, back, t, t0, value)                      \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_INT_STATS((stats), (t0), (value));           \
        } else {                                                            \
            (t) = MR_RAW_TABLE_INT((t0), (value));                          \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: int %ld => %p\n",                             \
                (t0), (long) (value), (t));                                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_UINT(stats, debug, back, t, t0, value)                     \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_UINT_STATS((stats), (t0), (value));          \
        } else {                                                            \
            (t) = MR_RAW_TABLE_UINT((t0), (value));                         \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: uint %lu => %p\n",                            \
                (t0), (unsigned long) (value), (t));                        \
        }                                                                   \
    } while (0)

#define MR_TABLE_INT8(stats, debug, back, t, t0, value)                     \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_INT8_STATS((stats), (t0), (value));          \
        } else {                                                            \
            (t) = MR_RAW_TABLE_INT8((t0), (value));                         \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: int8 %ld => %p\n",                            \
                (t0), (long) (value), (t));                                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_UINT8(stats, debug, back, t, t0, value)                    \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_UINT8_STATS((stats), (t0), (value));         \
        } else {                                                            \
            (t) = MR_RAW_TABLE_UINT8((t0), (value));                        \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: uint8 %lu => %p\n",                           \
                (t0), (unsigned long) (value), (t));                        \
        }                                                                   \
    } while (0)

#define MR_TABLE_INT16(stats, debug, back, t, t0, value)                    \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_INT16_STATS((stats), (t0), (value));         \
        } else {                                                            \
            (t) = MR_RAW_TABLE_INT16((t0), (value));                        \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: int16 %ld => %p\n",                           \
                (t0), (long) (value), (t));                                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_UINT16(stats, debug, back, t, t0, value)                   \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_UINT16_STATS((stats), (t0), (value));        \
        } else {                                                            \
            (t) = MR_RAW_TABLE_UINT16((t0), (value));                       \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: uint16 %lu => %p\n",                          \
                (t0), (unsigned long) (value), (t));                        \
        }                                                                   \
    } while (0)

#define MR_TABLE_INT32(stats, debug, back, t, t0, value)                    \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_INT32_STATS((stats), (t0), (value));         \
        } else {                                                            \
            (t) = MR_RAW_TABLE_INT32((t0), (value));                        \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: int32 %ld => %p\n",                           \
                (t0), (long) (value), (t));                                 \
        }                                                                   \
    } while (0)


#define MR_TABLE_UINT32(stats, debug, back, t, t0, value)                   \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_UINT32_STATS((stats), (t0), (value));        \
        } else {                                                            \
            (t) = MR_RAW_TABLE_UINT32((t0), (value));                       \
        }                                                                   \
        if (MR_tabledebug) {                                                \
            printf("TABLE %p: uint32 %lu => %p\n",                          \
                (t0), (unsigned long) (value), (t));                        \
        }                                                                   \
    } while (0)

#define MR_TABLE_CHAR(stats, debug, back, t, t0, value)                     \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_CHAR_STATS((stats), (t0), (value));          \
        } else {                                                            \
            (t) = MR_RAW_TABLE_CHAR((t0), (value));                         \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: char `%c'/%d => %p\n",                        \
                (t0), (int) (value), (int) (value), (t));                   \
        }                                                                   \
    } while (0)

#define MR_TABLE_FLOAT(stats, debug, back, t, t0, value)                    \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_FLOAT_STATS((stats), (t0), (value));         \
        } else {                                                            \
            (t) = MR_RAW_TABLE_FLOAT((t0), (value));                        \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: float %f => %p\n",                            \
                (t0), (double) (value), (t));                               \
        }                                                                   \
    } while (0)

#define MR_TABLE_STRING(stats, debug, back, t, t0, value)                   \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_STRING_STATS((stats), (t0), (value));        \
        } else {                                                            \
            (t) = MR_RAW_TABLE_STRING((t0), (value));                       \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: string `%s' => %p\n",                         \
                (t0), (char *) (value), (t));                               \
        }                                                                   \
    } while (0)

#define MR_TABLE_BITMAP(stats, debug, back, t, t0, value)                   \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_BITMAP_STATS((stats), (t0), (value));        \
        } else {                                                            \
            (t) = MR_RAW_TABLE_BITMAP((t0), (value));                       \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            /* XXX print value */                                           \
            printf("TABLE %p: bitmap => %p\n",                              \
                (t0), (t));                                                 \
        }                                                                   \
    } while (0)

#define MR_TABLE_TYPEINFO(stats, debug, back, t, t0, value)                 \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_TYPEINFO_STATS((stats), (t0), (value));      \
        } else {                                                            \
            (t) = MR_RAW_TABLE_TYPEINFO((t0), (value));                     \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: typeinfo %p => %p\n",                         \
                (t0), (value), (t));                                        \
        }                                                                   \
    } while (0)

#define MR_TABLE_TYPECLASSINFO(stats, debug, back, t, t0, value)            \
    do {                                                                    \
        if (stats != NULL) {                                                \
            (t) = MR_RAW_TABLE_TYPECLASSINFO_STATS((stats), (t0), (value)); \
        } else {                                                            \
            (t) = MR_RAW_TABLE_TYPECLASSINFO((t0), (value));                \
        }                                                                   \
        if (debug && MR_tabledebug) {                                       \
            printf("TABLE %p: typeclassinfo %p => %p\n",                    \
                (t0), (value), (t));                                        \
        }                                                                   \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_TABLE_CREATE_ANSWER_BLOCK(debug, table, num_slots)               \
    do {                                                                    \
        (table)->MR_answerblock = MR_TABLE_NEW_ARRAY(MR_Word, (num_slots)); \
        if (debug && MR_tabledebug)  {                                      \
            printf("allocated answer block %p -> %p, %d words\n",           \
                (table), (table)->MR_answerblock, (int) (num_slots));       \
        }                                                                   \
    } while (0)

#define MR_TABLE_CREATE_NODE_ANSWER_BLOCK(debug, block_ptr, num_slots)      \
    do {                                                                    \
        *block_ptr = MR_TABLE_NEW_ARRAY(MR_Word, (num_slots));              \
        if (debug && MR_tabledebug) {                                       \
            printf("allocated node block %p -> %p, %d words\n",             \
                block_ptr, *block_ptr, (int) (num_slots));                  \
        }                                                                   \
    } while (0)

#define MR_TABLE_GET_ANSWER(debug, ab, offset)                              \
    (( (debug && MR_tabledebug) ?                                           \
        printf("using answer block: %p, slot %d\n", (ab), (int) (offset))   \
    :                                                                       \
        (void) 0 /* do nothing */                                           \
    ),                                                                      \
    (ab)[(offset)])

#define MR_TABLE_SAVE_ANSWER(debug, ab, offset, value, type_info)           \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("saving to answer block: %p, slot %d = %lx\n",           \
                (ab), (int) (offset), (long) (value));                      \
        }                                                                   \
        (ab)[offset] =                                                      \
            MR_make_permanent((value), (MR_TypeInfo) (type_info));          \
    } while (0)
