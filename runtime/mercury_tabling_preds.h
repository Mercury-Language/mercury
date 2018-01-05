// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2004-2007 The University of Melbourne.
// Copyright (C) 2016-2018 The Mercury team.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// This file contains C macros that represent the bodies of predicates
// in library/table_builtin.m.

#ifdef  MR_HIGHLEVEL_CODE
  #define MR_table_box_float(F)         (MR_Word) MR_box_float(F)
  #define MR_table_unbox_float(W)       MR_unbox_float(W)
#else
  #define MR_table_box_float(F)         MR_float_to_word(F)
  #define MR_table_unbox_float(W)       MR_word_to_float(W)
#endif

#ifndef MR_ALLOW_RESET
#define MR_table_lookup_insert_int(a, b, c)                                 \
        MR_tbl_lookup_insert_int(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_uint(a, b, c)                                \
        MR_tbl_lookup_insert_uint(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_int8(a, b, c)                                \
        MR_tbl_lookup_insert_int8(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_uint8(a, b, c)                               \
        MR_tbl_lookup_insert_uint8(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_int16(a, b, c)                               \
        MR_tbl_lookup_insert_int16(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_uint16(a, b, c)                              \
        MR_tbl_lookup_insert_uint16(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_int32(a, b, c)                               \
        MR_tbl_lookup_insert_int32(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_uint32(a, b, c)                              \
        MR_tbl_lookup_insert_uint32(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_int64(a, b, c)                               \
        MR_tbl_lookup_insert_int64(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_uint64(a, b, c)                              \
        MR_tbl_lookup_insert_uint64(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_start_int(a, b, c, d)                        \
        MR_tbl_lookup_insert_start_int(NULL, MR_FALSE, MR_FALSE, a, b, c, d)
#define MR_table_lookup_insert_char(a, b, c)                                \
        MR_tbl_lookup_insert_char(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_string(a, b, c)                              \
        MR_tbl_lookup_insert_string(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_float(a, b, c)                               \
        MR_tbl_lookup_insert_float(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_enum(a, b, c, d)                             \
        MR_tbl_lookup_insert_enum(NULL, MR_FALSE, MR_FALSE, a, b, c, d)
#define MR_table_lookup_insert_foreign_enum(a, b, c)                        \
        MR_tbl_lookup_insert_foreign_enum(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_gen(a, b, c, d)                              \
        MR_tbl_lookup_insert_gen(NULL, MR_FALSE, MR_FALSE, a, b, c, d)
#define MR_table_lookup_insert_gen_addr(a, b, c, d)                         \
        MR_tbl_lookup_insert_gen_addr(NULL, MR_FALSE, MR_FALSE, a, b, c, d)
#define MR_table_lookup_insert_gen_poly(a, b, c, d)                         \
        MR_tbl_lookup_insert_gen_poly(NULL, MR_FALSE, MR_FALSE, a, b, c, d)
#define MR_table_lookup_insert_gen_poly_addr(a, b, c, d)                    \
        MR_tbl_lookup_insert_gen_poly_addr(NULL, MR_FALSE, MR_FALSE, a, b, c, d)
#define MR_table_lookup_insert_typeinfo(a, b, c)                            \
        MR_tbl_lookup_insert_typeinfo(NULL, MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_lookup_insert_typeclassinfo(a, b, c)                       \
        MR_tbl_lookup_insert_typeclassinfo(NULL, MR_FALSE, MR_FALSE, a, b, c)

#define MR_table_save_int_answer(a, b, c)                                   \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_uint_answer(a, b, c)                                  \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_int8_answer(a, b, c)                                  \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_uint8_answer(a, b, c)                                 \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_int16_answer(a, b, c)                                 \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_uint16_answer(a, b, c)                                \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_int32_answer(a, b, c)                                 \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_uint32_answer(a, b, c)                                \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_int64_answer(a, b, c)                                 \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_uint64_answer(a, b, c)                                \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c)
#define MR_table_save_char_answer(a, b, c)                                  \
        MR_tbl_save_char_answer(MR_FALSE, a, b, c)
#define MR_table_save_string_answer(a, b, c)                                \
        MR_tbl_save_string_answer(MR_FALSE, a, b, c)
#define MR_table_save_float_answer(a, b, c)                                 \
        MR_tbl_save_float_answer(MR_FALSE, a, b, c)
#define MR_table_save_io_state_answer(a, b, c)                              \
        MR_tbl_save_io_state_answer(MR_FALSE, a, b, c)
#define MR_table_save_any_answer(a, b, c, d)                                \
        MR_tbl_save_any_answer(MR_FALSE, a, b, c, d)

#define MR_table_restore_int_answer(a, b, c)                                \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_uint_answer(a, b, c)                               \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_int8_answer(a, b, c)                               \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_uint8_answer(a, b, c)                              \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_int16_answer(a, b, c)                              \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_uint16_answer(a, b, c)                             \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_int32_answer(a, b, c)                              \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_uint32_answer(a, b, c)                             \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_int64_answer(a, b, c)                              \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_uint64_answer(a, b, c)                             \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)
#define MR_table_restore_char_answer(a, b, c)                               \
        MR_tbl_restore_char_answer(MR_FALSE, a, b, c)
#define MR_table_restore_string_answer(a, b, c)                             \
        MR_tbl_restore_string_answer(MR_FALSE, a, b, c)
#define MR_table_restore_float_answer(a, b, c)                              \
        MR_tbl_restore_float_answer(MR_FALSE, a, b, c)
#define MR_table_restore_io_state_answer(a, b, c)                           \
        MR_tbl_restore_io_state_answer(MR_FALSE, a, b, c)
#define MR_table_restore_any_answer(a, b, c)                                \
        MR_tbl_restore_any_answer(MR_FALSE, a, b, c)

#define MR_table_loop_setup(a, b)                                           \
        MR_tbl_loop_setup(MR_FALSE, MR_FALSE, a, b)
#define MR_table_loop_setup_shortcut(a, b, c)                               \
        MR_tbl_loop_setup_shortcut(a, b, c)
#define MR_table_loop_mark_as_inactive(a)                                   \
        MR_tbl_loop_mark_as_inactive(MR_FALSE, a)
#define MR_table_loop_mark_as_inactive_and_fail(a)                          \
        MR_tbl_loop_mark_as_inactive_and_fail(MR_FALSE, a)
#define MR_table_loop_mark_as_active_and_fail(a)                            \
        MR_tbl_loop_mark_as_active_and_fail(MR_FALSE, a)

#define MR_table_memo_det_setup(a, b)                                       \
        MR_tbl_memo_det_setup(MR_FALSE, MR_FALSE, a, b)
#define MR_table_memo_semi_setup(a, b)                                      \
        MR_tbl_memo_semi_setup(MR_FALSE, MR_FALSE, a, b)
#define MR_table_memo_non_setup(a, b, c)                                    \
        MR_tbl_memo_non_setup(MR_FALSE, MR_FALSE, a, b, c)
#define MR_table_memo_det_setup_shortcut(a, b, c)                           \
        MR_tbl_memo_det_setup_shortcut(a, b, c)
#define MR_table_memo_semi_setup_shortcut(a, b, c)                          \
        MR_tbl_memo_semi_setup_shortcut(a, b, c)
#define MR_table_memo_non_setup_shortcut(a, b, c, d)                        \
        MR_tbl_memo_non_setup_shortcut(a, b, c, d)

#define MR_table_memo_mark_as_succeeded(a)                                  \
        MR_tbl_memo_mark_as_succeeded(MR_FALSE, a)
#define MR_table_memo_mark_as_failed(a)                                     \
        MR_tbl_memo_mark_as_failed(MR_FALSE, a)
#define MR_table_memo_mark_as_incomplete(a)                                 \
        MR_tbl_memo_mark_as_incomplete(MR_FALSE, a)
#define MR_table_memo_mark_as_active_and_fail(a)                            \
        MR_tbl_memo_mark_as_active_and_fail(MR_FALSE, a)
#define MR_table_memo_mark_as_complete_and_fail(a)                          \
        MR_tbl_memo_mark_as_complete_and_fail(MR_FALSE, a)

#define MR_table_memo_create_answer_block(a, b, c)                          \
        MR_tbl_memo_create_answer_block(MR_FALSE, a, b, c)
#define MR_table_memo_fill_answer_block_shortcut(a)                         \
        MR_tbl_memo_fill_answer_block_shortcut(a)

#define MR_table_memo_get_answer_block(a, b)                                \
        MR_tbl_memo_get_answer_block(MR_FALSE, a, b)
#define MR_table_memo_get_answer_block_shortcut(a)                          \
        MR_tbl_memo_get_answer_block_shortcut(a)

#define MR_table_memo_non_get_answer_table(a, b)                            \
        MR_tbl_memo_non_get_answer_table(MR_FALSE, a, b)

#define MR_table_memo_non_create_answer_block(a, b, c)                      \
        MR_tbl_memo_non_create_answer_block(MR_FALSE, a, b, c)
#define MR_table_memo_non_create_answer_block_shortcut(a)                   \
        MR_tbl_memo_non_create_answer_block_shortcut(a)

#define MR_table_memo_non_return_all_shortcut(a)                            \
        MR_tbl_memo_non_return_all_shortcut(a)

#define MR_table_memo_non_answer_is_not_duplicate(a, b)                     \
        MR_tbl_memo_non_answer_is_not_duplicate(MR_FALSE, a, b)
#define MR_table_memo_non_answer_is_not_duplicate_shortcut(a, b)            \
        MR_tbl_memo_non_answer_is_not_duplicate_shortcut(a, b)

#define MR_table_io_in_range(a, b, c, d)                                    \
        MR_tbl_io_in_range(MR_FALSE, a, b, c, d)
#define MR_table_io_has_occurred(a, b)                                      \
        MR_tbl_io_has_occurred(MR_FALSE, a, b)
#define MR_table_io_left_bracket_unitized_goal(a)                           \
        MR_tbl_io_left_bracket_unitized_goal(a)
#define MR_table_io_right_bracket_unitized_goal(a)                          \
        MR_tbl_io_right_bracket_unitized_goal(a)

#endif

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_lookup_insert_int(stats, debug, back, T0, V, T)              \
    do {                                                                    \
        MR_TABLE_INT(stats, debug, back, T, T0, (MR_Integer) V);            \
    } while (0)

#define MR_tbl_lookup_insert_uint(stats, debug, back, T0, V, T)             \
    do {                                                                    \
        MR_TABLE_UINT(stats, debug, back, T, T0, (MR_Unsigned) V);          \
    } while (0)

#define MR_tbl_lookup_insert_int8(stats, debug, back, T0, V, T)             \
    do {                                                                    \
        MR_TABLE_INT8(stats, debug, back, T, T0, (MR_Integer) V);           \
    } while (0)

#define MR_tbl_lookup_insert_uint8(stats, debug, back, T0, V, T)            \
    do {                                                                    \
        MR_TABLE_UINT8(stats, debug, back, T, T0, (MR_Unsigned) V);         \
    } while (0)

#define MR_tbl_lookup_insert_int16(stats, debug, back, T0, V, T)            \
    do {                                                                    \
        MR_TABLE_INT16(stats, debug, back, T, T0, (MR_Integer) V);          \
    } while (0)

#define MR_tbl_lookup_insert_uint16(stats, debug, back, T0, V, T)           \
    do {                                                                    \
        MR_TABLE_UINT16(stats, debug, back, T, T0, (MR_Unsigned) V);        \
    } while (0)

#define MR_tbl_lookup_insert_int32(stats, debug, back, T0, V, T)            \
    do {                                                                    \
        MR_TABLE_INT32(stats, debug, back, T, T0, (MR_Integer) V);          \
    } while (0)

#define MR_tbl_lookup_insert_uint32(stats, debug, back, T0, V, T)           \
    do {                                                                    \
        MR_TABLE_UINT32(stats, debug, back, T, T0, (MR_Unsigned) V);        \
    } while (0)

#define MR_tbl_lookup_insert_int64(stats, debug, back, T0, V, T)            \
    do {                                                                    \
        MR_TABLE_INT64(stats, debug, back, T, T0, V);                       \
    } while (0)

#define MR_tbl_lookup_insert_uint64(stats, debug, back, T0, V, T)           \
    do {                                                                    \
        MR_TABLE_UINT64(stats, debug, back, T, T0, V);                      \
    } while (0)

#define MR_tbl_lookup_insert_start_int(stats, debug, back, T0, S, V, T)     \
    do {                                                                    \
        MR_TABLE_START_INT(stats, debug, back, T, T0,                       \
            (MR_Integer) S, (MR_Integer) V);                                \
    } while (0)

#define MR_tbl_lookup_insert_char(stats, debug, back, T0, V, T)             \
    do {                                                                    \
        MR_TABLE_CHAR(stats, debug, back, T, T0, (MR_Integer) V);           \
    } while (0)

#define MR_tbl_lookup_insert_string(stats, debug, back, T0, V, T)           \
    do {                                                                    \
        MR_TABLE_STRING(stats, debug, back, T, T0, (MR_String) V);          \
    } while (0)

#define MR_tbl_lookup_insert_float(stats, debug, back, T0, V, T)            \
    do {                                                                    \
        MR_TABLE_FLOAT(stats, debug, back, T, T0, V);                       \
    } while (0)

#define MR_tbl_lookup_insert_enum(stats, debug, back, T0, R, V, T)          \
    do {                                                                    \
        MR_TABLE_ENUM(stats, debug, back, T, T0, R, V);                     \
    } while (0)

#define MR_tbl_lookup_insert_foreign_enum(stats, debug, back, T0, V, T)     \
    do {                                                                    \
        MR_TABLE_FOREIGN_ENUM(stats, debug, back, T, T0, V);                \
    } while (0)

#define MR_tbl_lookup_insert_gen(stats, debug, back, T0, TI, V, T)          \
    do {                                                                    \
        MR_TABLE_ANY(stats, debug, back, "gen", T, T0,                      \
            (MR_TypeInfo) TI, (MR_Word) V);                                 \
    } while (0)

#define MR_tbl_lookup_insert_gen_poly(stats, debug, back, T0, TI, V, T)     \
    do {                                                                    \
        MR_TABLE_ANY(stats, debug, back, "gen poly", T, T0,                 \
            (MR_TypeInfo) TI, (MR_Word) V);                                 \
    } while (0)

#define MR_tbl_lookup_insert_gen_addr(stats, debug, back, T0, TI, V, T)     \
    do {                                                                    \
        MR_TABLE_ANY_ADDR(stats, debug, back, "gen addr", T, T0,            \
            (MR_TypeInfo) TI, (MR_Word) V);                                 \
    } while (0)

#define MR_tbl_lookup_insert_gen_poly_addr(stats, debug, back, T0, TI, V, T) \
    do {                                                                     \
        MR_TABLE_ANY_ADDR(stats, debug, back, "gen poly addr", T, T0,        \
            (MR_TypeInfo) TI, (MR_Word) V);                                  \
    } while (0)

#define MR_tbl_lookup_insert_typeinfo(stats, debug, back, T0, TI, T)        \
    do {                                                                    \
        MR_TABLE_TYPEINFO(stats, debug, back, T, T0, (MR_TypeInfo) TI);     \
    } while (0)

#define MR_tbl_lookup_insert_typeclassinfo(stats, debug, back, T0, TCI, T)  \
    do {                                                                    \
        MR_TABLE_TYPECLASSINFO(stats, debug, back, T, T0, (MR_Word *) TCI); \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_save_int_answer(debug, AB, Offset, V)                        \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0));                      \
    } while (0)

#define MR_tbl_save_uint_answer(debug, AB, Offset, V)                       \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, uint, 0));                     \
    } while (0)

#define MR_tbl_save_int8_answer(debug, AB, Offset, V)                       \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, int8, 0));                     \
    } while (0)

#define MR_tbl_save_uint8_answer(debug, AB, Offset, V)                      \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, uint8, 0));                    \
    } while (0)

#define MR_tbl_save_int16_answer(debug, AB, Offset, V)                      \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, int16, 0));                    \
    } while (0)

#define MR_tbl_save_uint16_answer(debug, AB, Offset, V)                     \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, uint16, 0));                   \
    } while (0)

#define MR_tbl_save_int32_answer(debug, AB, Offset, V)                      \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, int32, 0));                    \
    } while (0)

#define MR_tbl_save_uint32_answer(debug, AB, Offset, V)                     \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, uint32, 0));                   \
    } while (0)

#define MR_tbl_save_int64_answer(debug, AB, Offset, V)                      \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, int64, 0));                    \
    } while (0)

#define MR_tbl_save_uint64_answer(debug, AB, Offset, V)                     \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, uint64, 0));                   \
    } while (0)

#define MR_tbl_save_char_answer(debug, AB, Offset, V)                       \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, V,                          \
            &MR_TYPE_CTOR_INFO_NAME(builtin, character, 0));                \
    } while (0)

#define MR_tbl_save_string_answer(debug, AB, Offset, V)                     \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, (MR_Word) V,                \
            &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0));                   \
    } while (0)

#define MR_tbl_save_float_answer(debug, AB, Offset, V)                      \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, MR_table_box_float(V),      \
            &MR_TYPE_CTOR_INFO_NAME(builtin, float, 0));                    \
    } while (0)

#define MR_tbl_save_io_state_answer(debug, AB, Offset, V)                   \
    do {                                                                    \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, (MR_Word) V,                \
            &MR_TYPE_CTOR_INFO_NAME(io, state, 0));                         \
    } while (0)

#define MR_tbl_save_any_answer(debug, AB, Offset, TI, V)                       \
    do {                                                                       \
        MR_TABLE_SAVE_ANSWER(debug, AB, Offset, (MR_Word) V, (MR_TypeInfo) TI);\
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_restore_int_answer(debug, AB, Offset, V)                     \
    do {                                                                    \
        V = (MR_Integer) MR_TABLE_GET_ANSWER(debug, AB, Offset);            \
    } while (0)

#define MR_tbl_restore_uint_answer(debug, AB, Offset, V)                    \
    do {                                                                    \
        V = (MR_Unsigned) MR_TABLE_GET_ANSWER(debug, AB, Offset);           \
    } while (0)

#define MR_tbl_restore_int8_answer(debug, AB, Offset, V)                    \
    do {                                                                    \
        V = (int8_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);                \
    } while (0)

#define MR_tbl_restore_uint8_answer(debug, AB, Offset, V)                   \
    do {                                                                    \
        V = (uint8_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);               \
    } while (0)

#define MR_tbl_restore_int16_answer(debug, AB, Offset, V)                   \
    do {                                                                    \
        V = (int16_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);               \
    } while (0)

#define MR_tbl_restore_uint16_answer(debug, AB, Offset, V)                  \
    do {                                                                    \
        V = (uint16_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);              \
    } while (0)

#define MR_tbl_restore_int32_answer(debug, AB, Offset, V)                   \
    do {                                                                    \
        V = (int32_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);               \
    } while (0)

#define MR_tbl_restore_uint32_answer(debug, AB, Offset, V)                  \
    do {                                                                    \
        V = (uint32_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);              \
    } while (0)

#define MR_tbl_restore_int64_answer(debug, AB, Offset, V)                   \
    do {                                                                    \
        V = (int64_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);               \
    } while (0)

#define MR_tbl_restore_uint64_answer(debug, AB, Offset, V)                  \
    do {                                                                    \
        V = (uint64_t) MR_TABLE_GET_ANSWER(debug, AB, Offset);              \
    } while (0)

#define MR_tbl_restore_char_answer(debug, AB, Offset, V)                    \
    do {                                                                    \
        V = (MR_Char) MR_TABLE_GET_ANSWER(debug, AB, Offset);               \
    } while (0)

#define MR_tbl_restore_string_answer(debug, AB, Offset, V)                  \
    do {                                                                    \
        V = (MR_String) MR_TABLE_GET_ANSWER(debug, AB, Offset);             \
    } while (0)

#define MR_tbl_restore_float_answer(debug, AB, Offset, V)                   \
    do {                                                                    \
        V = MR_table_unbox_float(MR_TABLE_GET_ANSWER(debug, AB, Offset));   \
    } while (0)

#define MR_tbl_restore_io_state_answer(debug, AB, Offset, V)                \
    do {                                                                    \
        V = (MR_Word) MR_TABLE_GET_ANSWER(debug, AB, Offset);               \
    } while (0)

#define MR_tbl_restore_any_answer(debug, AB, Offset, V)                     \
    do {                                                                    \
        V = (MR_Word) MR_TABLE_GET_ANSWER(debug, AB, Offset);               \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_loop_setup(debug, back, T, Status)                           \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("status of loop table %p: %ld (%lx)\n",                  \
                T, (long) T->MR_loop_status,                                \
                (long) T->MR_loop_status);                                  \
        }                                                                   \
        Status = T->MR_loop_status;                                         \
        if (Status == MR_LOOP_INACTIVE) {                                   \
            T->MR_loop_status = MR_LOOP_ACTIVE;                             \
        }                                                                   \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                        \
    } while (0)

#define MR_tbl_loop_setup_shortcut(T0, T, Status)     ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_loop_mark_as_inactive(debug, T)                              \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as inactive\n", T);                          \
        }                                                                   \
                                                                            \
        T->MR_loop_status = MR_LOOP_INACTIVE;                               \
    } while (0)

#define MR_tbl_loop_mark_as_inactive_and_fail(debug, T)                     \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as inactive\n", T);                          \
        }                                                                   \
                                                                            \
        T->MR_loop_status = MR_LOOP_INACTIVE;                               \
    } while (0)

#define MR_tbl_loop_mark_as_active_and_fail(debug, T)                       \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as active\n", T);                            \
        }                                                                   \
                                                                            \
        T->MR_loop_status = MR_LOOP_ACTIVE;                                 \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_setup(debug, back, T, Status)                           \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("status of memo table %p: %ld (%lx)\n",                  \
                T, (long) T->MR_memo_status, (long) T->MR_memo_status);     \
        }                                                                   \
                                                                            \
        if (T->MR_integer >= MR_MEMO_BLOCK) {                               \
            Status = MR_MEMO_SUCCEEDED;                                     \
        } else {                                                            \
            Status = T->MR_loop_status;                                     \
            if (Status == MR_MEMO_INACTIVE) {                               \
                T->MR_loop_status = MR_MEMO_ACTIVE;                         \
            }                                                               \
        }                                                                   \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                        \
    } while (0)

#define MR_tbl_memo_det_setup(debug, back, T, Status)                       \
    MR_tbl_memo_setup(debug, back, T, Status)

#define MR_tbl_memo_semi_setup(debug, back, T, Status)                      \
    MR_tbl_memo_setup(debug, back, T, Status)

#define MR_tbl_memo_non_setup(debug, back, T, Record, Status)               \
    do {                                                                    \
        MR_save_transient_registers();                                      \
        if (T->MR_memo_non_record == NULL) {                                \
            if (debug && MR_tabledebug) {                                   \
                printf("setting up of memo non table for %p\n", T);         \
            }                                                               \
                                                                            \
            Status = MR_MEMO_NON_INACTIVE;                                  \
            Record = MR_TABLE_NEW(MR_MemoNonRecord);                        \
            Record->MR_mn_back_ptr = T;                                     \
            Record->MR_mn_status = MR_MEMO_NON_ACTIVE;                      \
            Record->MR_mn_num_answers = 0;                                  \
            Record->MR_mn_answer_table.MR_integer = 0;                      \
            Record->MR_mn_answer_list = NULL;                               \
            Record->MR_mn_answer_list_tail = &Record->MR_mn_answer_list;    \
            T->MR_memo_non_record = Record;                                 \
        } else {                                                            \
            Record = T->MR_memo_non_record;                                 \
            Status = Record->MR_mn_status;                                  \
        }                                                                   \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("status of memo non table %p -> %p: %s\n",               \
                Record->MR_mn_back_ptr, Record,                             \
                MR_memo_non_status(Record->MR_mn_status));                  \
        }                                                                   \
                                                                            \
        MR_restore_transient_registers();                                   \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                        \
    } while (0)

#define MR_tbl_memo_det_setup_shortcut(T0, T, Status)     ((void) 0)

#define MR_tbl_memo_semi_setup_shortcut(T0, T, Status)    ((void) 0)

#define MR_tbl_memo_non_setup_shortcut(T0, T, R, Status)  ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_mark_as_succeeded(debug, T)                             \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as succeeded\n", T);                         \
        }                                                                   \
                                                                            \
        T->MR_memo_status = MR_MEMO_SUCCEEDED;                              \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_mark_as_failed(debug, T)                                \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as failed\n", T);                            \
        }                                                                   \
        T->MR_memo_status = MR_MEMO_FAILED;                                 \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_mark_as_incomplete(debug, R)                            \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as incomplete\n", R);                        \
        }                                                                   \
                                                                            \
        R->MR_mn_status = MR_MEMO_NON_INCOMPLETE;                           \
    } while (0)

#define MR_tbl_memo_mark_as_active_and_fail(debug, R)                       \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as active\n", R);                            \
        }                                                                   \
                                                                            \
        R->MR_mn_status = MR_MEMO_NON_ACTIVE;                               \
    } while (0)

#define MR_tbl_memo_mark_as_complete_and_fail(debug, R)                     \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("marking %p as complete\n", R);                          \
        }                                                                   \
                                                                            \
        R->MR_mn_status = MR_MEMO_NON_COMPLETE;                             \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_create_answer_block(debug, T, Size, AnswerBlock)        \
    do {                                                                    \
        MR_TABLE_CREATE_ANSWER_BLOCK(debug, T, Size);                       \
        AnswerBlock = T->MR_answerblock;                                    \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_fill_answer_block_shortcut(T)     ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_get_answer_block(debug, T, AnswerBlock)                 \
    do {                                                                    \
        if (debug) {                                                        \
            if (MR_tabledebug) {                                            \
                printf("getting answer block %p -> %p\n",                   \
                    T, T->MR_answerblock);                                  \
            }                                                               \
                                                                            \
            if (T->MR_memo_status < MR_MEMO_BLOCK) {                        \
                MR_fatal_error("table_memo_get_answer_block: no block");    \
            }                                                               \
        }                                                                   \
                                                                            \
        AnswerBlock = T->MR_answerblock;                                    \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_get_answer_block_shortcut(T)      ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_non_get_answer_table(debug, Record, AnswerTable)        \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("getting answer table %p -> %p\n",                       \
                Record, &(Record->MR_mn_answer_table));                     \
        }                                                                   \
                                                                            \
        AnswerTable = &(Record->MR_mn_answer_table);                        \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_non_create_answer_block(debug, Record, Size, AnswerBlock) \
    do {                                                                      \
        MR_AnswerListNode   *answer_node;                                     \
        MR_Word             **Slot;                                           \
                                                                              \
        Record->MR_mn_num_answers++;                                          \
                                                                              \
        /*                                                                    \
        ** We fill in the answer_data slot with a dummy value.                \
        ** This slot will be filled in by the next piece of code              \
        ** to be executed after we return, which is why we return             \
        ** its address.                                                       \
        */                                                                    \
                                                                              \
        answer_node = MR_TABLE_NEW(MR_AnswerListNode);                        \
        answer_node->MR_aln_answer_block = NULL;                              \
        answer_node->MR_aln_next_answer = NULL;                               \
                                                                              \
        if (debug && MR_tabledebug) {                                         \
            printf("new answer slot %d at %p(%p)\n",                          \
                Record->MR_mn_num_answers, answer_node,                       \
                &answer_node->MR_aln_answer_block);                           \
            printf("\tstoring into %p\n",                                     \
                Record->MR_mn_answer_list_tail);                              \
        }                                                                     \
                                                                              \
        *(Record->MR_mn_answer_list_tail) = answer_node;                      \
        Record->MR_mn_answer_list_tail = &(answer_node->MR_aln_next_answer);  \
        Slot = &(answer_node->MR_aln_answer_block);                           \
        MR_TABLE_CREATE_NODE_ANSWER_BLOCK(debug, Slot, Size);                 \
        AnswerBlock = *Slot;                                                  \
    } while (0)

#define MR_tbl_memo_non_create_answer_block_shortcut(Record)                \
    ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_non_return_all_shortcut(Record)                         \
    ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_memo_non_answer_is_not_duplicate(debug, T, succ)             \
    do {                                                                    \
        MR_bool     is_new_answer;                                          \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("checking if %p is a duplicate answer: %ld\n",           \
                T, (long) T->MR_integer);                                   \
        }                                                                   \
                                                                            \
        is_new_answer = (T->MR_integer == 0);                               \
        T->MR_integer = 1;  /* Any nonzero value will do. */                \
        succ = is_new_answer;                                               \
    } while (0)

#define MR_tbl_memo_non_answer_is_not_duplicate_shortcut(R, succ)           \
    ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_io_in_range(debug, T, Counter, Start, Succ)                  \
    if (MR_io_tabling_enabled) {                                            \
        MR_Unsigned old_counter;                                            \
                                                                            \
        if (debug && MR_io_tabling_debug) {                                 \
            printf(                                                         \
                "checking table_io_in_range: prev %ld, start %ld, hwm %ld", \
                (long) MR_io_tabling_counter, (long) MR_io_tabling_start,   \
                (long) MR_io_tabling_counter_hwm);                          \
        }                                                                   \
                                                                            \
        old_counter = MR_io_tabling_counter;                                \
        MR_io_tabling_counter++;                                            \
                                                                            \
        if (MR_io_tabling_start < MR_io_tabling_counter                     \
            && MR_io_tabling_counter <= MR_io_tabling_end)                  \
        {                                                                   \
            T = &MR_io_tabling_pointer;                                     \
            Counter = (MR_Word) old_counter;                                \
            Start = MR_io_tabling_start;                                    \
            if (MR_io_tabling_counter > MR_io_tabling_counter_hwm)          \
            {                                                               \
                MR_io_tabling_counter_hwm = MR_io_tabling_counter;          \
            }                                                               \
                                                                            \
            if (debug && MR_io_tabling_debug) {                             \
                printf(" in range\n");                                      \
            }                                                               \
                                                                            \
            Succ = MR_TRUE;                                                 \
        } else {                                                            \
            if (debug && MR_io_tabling_debug) {                             \
                printf(" not in range\n");                                  \
            }                                                               \
                                                                            \
            Succ = MR_FALSE;                                                \
        }                                                                   \
    } else {                                                                \
        Succ = MR_FALSE;                                                    \
    }

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_io_has_occurred(debug, T, Succ)                              \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("checking %p for previous execution: %p\n",              \
                T, T->MR_answerblock);                                      \
        }                                                                   \
                                                                            \
        Succ = (T->MR_answerblock != NULL);                                 \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_io_left_bracket_unitized_goal(TraceEnabled)                  \
    do {                                                                    \
        TraceEnabled = MR_debug_enabled;                                    \
        MR_debug_enabled = MR_FALSE;                                        \
        MR_update_trace_func_enabled();                                     \
        MR_io_tabling_enabled = MR_FALSE;                                   \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_io_right_bracket_unitized_goal(TraceEnabled)                 \
    do {                                                                    \
        MR_io_tabling_enabled = MR_TRUE;                                    \
        MR_debug_enabled = TraceEnabled;                                    \
        MR_update_trace_func_enabled();                                     \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_mm_setup(debug, back, T, Subgoal, Status)                    \
    do {                                                                    \
        MR_save_transient_registers();                                      \
        Subgoal = MR_setup_subgoal(T);                                      \
        Status = Subgoal->MR_sg_status;                                     \
        if (Status == MR_SUBGOAL_INACTIVE) {                                \
            MR_push_generator(MR_curfr, Subgoal);                           \
            MR_register_generator_ptr(Subgoal);                             \
            Subgoal->MR_sg_status = MR_SUBGOAL_ACTIVE;                      \
        }                                                                   \
        MR_restore_transient_registers();                                   \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                        \
    } while (0)

#define MR_tbl_mm_setup_shortcut(Subgoal, Status)                           \
    do {                                                                    \
        MR_fatal_error("MR_tbl_mm_setup_shortcut");                         \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_mm_return_all_shortcut(AnswerBlock)    ((void) 0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_mm_get_answer_table(debug, Subgoal, AnswerTable)             \
    do {                                                                    \
        if (debug && MR_tabledebug) {                                       \
            printf("getting answer table %p -> %p\n",                       \
                Subgoal, &(Subgoal->MR_sg_answer_table));                   \
        }                                                                   \
                                                                            \
        AnswerTable = &(Subgoal->MR_sg_answer_table);                       \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_mm_answer_is_not_duplicate(debug, T, succ)                   \
    do {                                                                    \
        MR_bool     is_new_answer;                                          \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("checking if %p is a duplicate answer: %ld\n",           \
                T, (long) T->MR_integer);                                   \
        }                                                                   \
                                                                            \
        is_new_answer = (T->MR_integer == 0);                               \
        T->MR_integer = 1;  /* Any nonzero value will do. */                \
        succ = is_new_answer;                                               \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_mm_create_answer_block(debug, Subgoal, Size, AnswerBlock)    \
    do {                                                                    \
        MR_AnswerListNode   *answer_node;                                   \
        MR_Word             **Slot;                                         \
                                                                            \
        Subgoal->MR_sg_num_ans++;                                           \
                                                                            \
        /*                                                                  \
        ** We fill in the answer_data slot with a dummy value.              \
        ** This slot will be filled in by the next piece of code            \
        ** to be executed after we return, which is why we return           \
        ** its address.                                                     \
        */                                                                  \
                                                                            \
        answer_node = MR_TABLE_NEW(MR_AnswerListNode);                      \
        answer_node->MR_aln_answer_block = NULL;                            \
        answer_node->MR_aln_next_answer = NULL;                             \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("%s: new answer slot %d at %p(%p)\n",                    \
                MR_subgoal_addr_name(Subgoal),                              \
                Subgoal->MR_sg_num_ans, answer_node,                        \
                &answer_node->MR_aln_answer_block);                         \
            printf("\tstoring into %p\n",                                   \
                Subgoal->MR_sg_answer_list_tail);                           \
        }                                                                   \
                                                                            \
        *(Subgoal->MR_sg_answer_list_tail) = answer_node;                   \
        Subgoal->MR_sg_answer_list_tail =                                   \
            &(answer_node->MR_aln_next_answer);                             \
        Slot = &(answer_node->MR_aln_answer_block);                         \
        MR_TABLE_CREATE_NODE_ANSWER_BLOCK(debug, Slot, Size);               \
        AnswerBlock = *Slot;                                                \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_mm_fill_answer_block_shortcut(Subgoal) ((void) 0)

////////////////////////////////////////////////////////////////////////////

#else   // MR_USE_MINIMAL_MODEL_STACK_COPY

#define MR_MMSC_ERROR                                                   \
        "stack copy minimal model code entered when not enabled"

#define MR_tbl_mm_setup(debug, back, T, Subgoal, Status)                    \
    do {                                                                    \
        MR_fatal_error(MR_MMSC_ERROR);                                      \
    } while (0)
#define MR_tbl_mm_setup_shortcut(Subgoal, Status)                           \
    do {                                                                    \
        MR_fatal_error(MR_MMSC_ERROR);                                      \
    } while (0)
#define MR_tbl_mm_return_all_shortcut(AnswerBlock)                          \
    do {                                                                    \
        MR_fatal_error(MR_MMSC_ERROR);                                      \
    } while (0)
#define MR_tbl_mm_get_answer_table(debug, Subgoal, AnswerTable)             \
    do {                                                                    \
        MR_fatal_error(MR_MMSC_ERROR);                                      \
    } while (0)
#define MR_tbl_mm_answer_is_not_duplicate(debug, T, Succ)                   \
    do {                                                                    \
        MR_fatal_error(MR_MMSC_ERROR);                                      \
    } while (0)
#define MR_tbl_mm_create_answer_block(debug, Subgoal, Size, AnswerBlock)    \
    do {                                                                    \
        MR_fatal_error(MR_MMSC_ERROR);                                      \
    } while (0)
#define MR_tbl_mm_fill_answer_block_shortcut(Subgoal)                       \
    do {                                                                    \
        MR_fatal_error(MR_MMSC_ERROR);                                      \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#define MR_tbl_mmos_answer_is_not_duplicate(debug, T, succ)                 \
    do {                                                                    \
        MR_bool     is_new_answer;                                          \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("checking if %p is a duplicate answer: %ld\n",           \
                T, (long) T->MR_integer);                                   \
        }                                                                   \
                                                                            \
        is_new_answer = (T->MR_integer == 0);                               \
        T->MR_integer = 1;  /* Any nonzero value will do. */                \
        succ = is_new_answer;                                               \
    } while (0)

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS

#define MR_tbl_mmos_create_answer_block(debug, Generator, Size, AnswerBlock)\
    do {                                                                    \
        MR_AnswerListNode   *answer_node;                                   \
        MR_Word             **Slot;                                         \
                                                                            \
        /*                                                                  \
        ** We fill in the answer_data slot with a dummy value.              \
        ** This slot will be filled in by the next piece of code            \
        ** to be executed after we return, which is why we return           \
        ** its address.                                                     \
        */                                                                  \
                                                                            \
        answer_node = MR_TABLE_NEW(MR_AnswerListNode);                      \
        answer_node->MR_aln_answer_block = NULL;                            \
        answer_node->MR_aln_next_answer = NULL;                             \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("%s: new answer slot %d at %p(%p)\n",                    \
                MR_gen_addr_name(Generator),                                \
                Generator->MR_gen_num_answers, answer_node,                 \
                &answer_node->MR_aln_answer_block);                         \
            printf("\tstoring into %p\n",                                   \
                Generator->MR_gen_answer_list_tail);                        \
        }                                                                   \
                                                                            \
        Generator->MR_gen_num_answers++;                                    \
                                                                            \
        *(Generator->MR_gen_answer_list_tail) = answer_node;                \
        Generator->MR_gen_answer_list_tail =                                \
            &(answer_node->MR_aln_next_answer);                             \
        Slot = &(answer_node->MR_aln_answer_block);                         \
        MR_TABLE_CREATE_NODE_ANSWER_BLOCK(debug, Slot, Size);               \
        AnswerBlock = *Slot;                                                \
    } while (0)

#define MR_tbl_mmos_return_answer(debug, generator)                         \
    do {                                                                    \
        MR_Dlist        *list;                                              \
        MR_ConsumerPtr  consumer;                                           \
        MR_Code         *target;                                            \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("generator %s returning answer #%d\n",                   \
                MR_gen_addr_name(generator),                                \
                generator->MR_gen_num_answers);                             \
        }                                                                   \
                                                                            \
        /*                                                                  \
        ** The choice of the consumer to return an answer to, made here,    \
        ** implements the scheduling strategy. There is a wide range of     \
        ** possible strategies, and we should investigate several.          \
        ** For now, we use the simplest possible strategy.                  \
        */                                                                  \
                                                                            \
        MR_for_dlist (list, generator->MR_gen_consumers) {                  \
            consumer = (MR_ConsumerPtr) MR_dlist_data(list);                \
            if (*(consumer->MR_cons_remaining_answer_list_ptr) != NULL) {   \
                target = consumer->MR_cons_context->MR_ctxt_resume;         \
                if (debug && MR_tabledebug) {                               \
                    printf("scheduling consumer %s\n",                      \
                        MR_cons_addr_name(consumer));                       \
                    printf("resume target %p (%s)\n",                       \
                        target, MR_lookup_entry_or_internal(target));       \
                }                                                           \
                                                                            \
                MR_save_context(generator->MR_gen_context);                 \
                generator->MR_gen_context->MR_ctxt_resume =                 \
                    MR_ENTRY(MR_do_redo);                                   \
                MR_load_context(consumer->MR_cons_context);                 \
                MR_ENGINE(MR_eng_this_context) = consumer->MR_cons_context; \
                MR_GOTO(consumer->MR_cons_context->MR_ctxt_resume);         \
            }                                                               \
        }                                                                   \
                                                                            \
        MR_fatal_error("MR_tbl_mmos_return_answer: no waiting consumers");  \
    } while (0)

#define MR_tbl_mmos_completion(debug, generator)                            \
    do {                                                                    \
        /* This code is a placeholder, since it doesn't work with coups. */ \
        generator->MR_gen_is_complete = MR_TRUE;                            \
                                                                            \
        if (debug && MR_tabledebug) {                                       \
            printf("completing generator %s\n",                             \
                MR_gen_addr_name(generator));                               \
        }                                                                   \
                                                                            \
        MR_fail();                                                          \
    } while (0)

#else   // MR_USE_MINIMAL_MODEL_OWN_STACKS

#define MR_MMOS_ERROR                                                   \
        "own stack minimal model code entered when not enabled"

#define MR_tbl_mmos_create_answer_block(debug, Generator, Size, AnswerBlock)\
    do {                                                                    \
        MR_fatal_error(MR_MMOS_ERROR);                                      \
    } while (0)
#define MR_tbl_mmos_return_answer(debug, generator)                         \
    do {                                                                    \
        MR_fatal_error(MR_MMOS_ERROR);                                      \
    } while (0)
#define MR_tbl_mmos_completion(debug, generator)                            \
    do {                                                                    \
        MR_fatal_error(MR_MMOS_ERROR);                                      \
    } while (0)

#endif  // MR_USE_MINIMAL_MODEL_OWN_STACKS

////////////////////////////////////////////////////////////////////////////
