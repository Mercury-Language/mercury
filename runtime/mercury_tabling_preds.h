/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2004-2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
** This file contains C macros that represent the bodies of predicates
** in library/table_builtin.m.
*/

#ifdef  MR_HIGHLEVEL_CODE
  #define MR_table_box_float(F)         (MR_Word) MR_box_float(F)
  #define MR_table_unbox_float(W)       MR_unbox_float(W)
#else
  #define MR_table_box_float(F)         MR_float_to_word(F)
  #define MR_table_unbox_float(W)       MR_word_to_float(W)
#endif

/***********************************************************************/

#define MR_table_lookup_insert_int(T0, V, T)                            \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_INT(T, T0, (MR_Integer) V);                  \
    } while(0)

#define MR_table_lookup_insert_start_int(T0, S, V, T)                   \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_START_INT(T, T0, (MR_Integer) S, (MR_Integer) V); \
    } while(0)

#define MR_table_lookup_insert_char(T0, V, T)                           \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_CHAR(T, T0, (MR_Integer) V);                 \
    } while(0)

#define MR_table_lookup_insert_string(T0, V, T)                         \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_STRING(T, T0, (MR_String) V);                \
    } while(0)

#define MR_table_lookup_insert_float(T0, V, T)                          \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_FLOAT(T, T0, V);                             \
    } while(0)

#define MR_table_lookup_insert_enum(T0, R, V, T)                        \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_ENUM(T, T0, R, V);                           \
    } while(0)

#define MR_table_lookup_insert_user(T0, TI, V, T)                       \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_ANY(T, T0, (MR_TypeInfo) TI, V);             \
    } while(0)

#define MR_table_lookup_insert_poly(T0, TI, V, T)                       \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_ANY(T, T0, (MR_TypeInfo) TI, V);             \
    } while(0)

#define MR_table_lookup_insert_typeinfo(T0, TI, T)                      \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_TYPEINFO(T, T0, (MR_TypeInfo) TI);           \
    } while(0)

#define MR_table_lookup_insert_typeclassinfo(T0, TCI, T)                \
    do {                                                                \
        MR_DEBUG_NEW_TABLE_TYPECLASSINFO(T, T0, (MR_Word *) TCI);       \
    } while(0)

/***********************************************************************/

#define MR_table_save_int_answer(AB, Offset, V)                         \
    do {                                                                \
        MR_TABLE_SAVE_ANSWER(AB, Offset, V,                             \
            &MR_TYPE_CTOR_INFO_NAME(builtin, int, 0));                  \
    } while(0)

#define MR_table_save_char_answer(AB, Offset, V)                        \
    do {                                                                \
        MR_TABLE_SAVE_ANSWER(AB, Offset, V,                             \
            &MR_TYPE_CTOR_INFO_NAME(builtin, character, 0));            \
    } while(0)

#define MR_table_save_string_answer(AB, Offset, V)                      \
    do {                                                                \
        MR_TABLE_SAVE_ANSWER(AB, Offset, (MR_Word) V,                   \
            &MR_TYPE_CTOR_INFO_NAME(builtin, string, 0));               \
    } while(0)

#define MR_table_save_float_answer(AB, Offset, V)                       \
    do {                                                                \
        MR_TABLE_SAVE_ANSWER(AB, Offset, MR_table_box_float(V),         \
            &MR_TYPE_CTOR_INFO_NAME(builtin, float, 0));                \
    } while(0)

#define MR_table_save_io_state_answer(AB, Offset, V)                    \
    do {                                                                \
        MR_TABLE_SAVE_ANSWER(AB, Offset, (MR_Word) V,                   \
            &MR_TYPE_CTOR_INFO_NAME(io, state, 0));                     \
    } while(0)

#define MR_table_save_any_answer(AB, Offset, TI, V)                     \
    do {                                                                \
        MR_TABLE_SAVE_ANSWER(AB, Offset, (MR_Word) V, (MR_TypeInfo) TI);\
    } while(0)

/***********************************************************************/

#define MR_table_restore_int_answer(AB, Offset, V)                      \
    do {                                                                \
        V = (MR_Integer) MR_TABLE_GET_ANSWER(AB, Offset);               \
    } while(0)

#define MR_table_restore_char_answer(AB, Offset, V)                     \
    do {                                                                \
        V = (MR_Char) MR_TABLE_GET_ANSWER(AB, Offset);                  \
    } while(0)

#define MR_table_restore_string_answer(AB, Offset, V)                   \
    do {                                                                \
        V = (MR_String) MR_TABLE_GET_ANSWER(AB, Offset);                \
    } while(0)

#define MR_table_restore_float_answer(AB, Offset, V)                    \
    do {                                                                \
        V = MR_table_unbox_float(MR_TABLE_GET_ANSWER(AB, Offset));      \
    } while(0)

#define MR_table_restore_io_state_answer(AB, Offset, V)                 \
    do {                                                                \
        V = (MR_Word) MR_TABLE_GET_ANSWER(AB, Offset);                  \
    } while(0)

#define MR_table_restore_any_answer(AB, Offset, V)                      \
    do {                                                                \
        V = (MR_Word) MR_TABLE_GET_ANSWER(AB, Offset);                  \
    } while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_loop_setup_msg(T)                                    \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("status of loop table %p: %ld (%lx)\n",              \
                T, (long) T->MR_loop_status,                            \
                (long) T->MR_loop_status);                              \
        }                                                               \
    } while(0)
#else
  #define MR_table_loop_setup_msg(T)            ((void) 0)
#endif

#define MR_table_loop_setup(T, Status)                                  \
    do {                                                                \
        MR_table_loop_setup_msg(T);                                     \
        Status = T->MR_loop_status;                                     \
        if (Status == MR_LOOP_INACTIVE) {                               \
            T->MR_loop_status = MR_LOOP_ACTIVE;                         \
        }                                                               \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                    \
    } while (0)

#define MR_table_loop_setup_shortcut(T0, T, Status)     ((void) 0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_loop_mark_as_inactive_msg(T)                         \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("marking %p as inactive\n", T);                      \
        }                                                               \
    } while (0)
  #define MR_table_loop_mark_as_active_msg(T)                           \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("marking %p as active\n", T);                        \
        }                                                               \
    } while (0)
#else
  #define MR_table_loop_mark_as_inactive_msg(T)     ((void) 0)
  #define MR_table_loop_mark_as_active_msg(T)       ((void) 0)
#endif

#define MR_table_loop_mark_as_inactive(T)                               \
    do {                                                                \
        MR_table_loop_mark_as_inactive_msg(T);                          \
        T->MR_loop_status = MR_LOOP_INACTIVE;                           \
    } while (0)

#define MR_table_loop_mark_as_inactive_and_fail(T)                      \
    do {                                                                \
        MR_table_loop_mark_as_inactive_msg(T);                          \
        T->MR_loop_status = MR_LOOP_INACTIVE;                           \
    } while (0)

#define MR_table_loop_mark_as_active_and_fail(T)                        \
    do {                                                                \
        MR_table_loop_mark_as_active_msg(T);                            \
        T->MR_loop_status = MR_LOOP_ACTIVE;                             \
    } while (0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_setup_msg(T)                                    \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("status of memo table %p: %ld (%lx)\n",              \
                T, (long) T->MR_memo_status,                            \
                (long) T->MR_memo_status);                              \
        }                                                               \
    } while(0)
  #define MR_table_memo_non_setup_msg(T)                                \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("setting up of memo non table for %p\n", T);         \
        }                                                               \
    } while(0)
  #define MR_table_memo_non_status_msg(R)                               \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("status of memo non table %p -> %p: %s\n",           \
                R->MR_mn_back_ptr, R,                                   \
                MR_memo_non_status(R->MR_mn_status));                   \
        }                                                               \
    } while(0)
#else
  #define MR_table_memo_setup_msg(T)            ((void) 0)
  #define MR_table_memo_non_setup_msg(T)        ((void) 0)
  #define MR_table_memo_non_status_msg(R)       ((void) 0)
#endif

#define MR_table_memo_setup(T, Status)                                  \
    do {                                                                \
        MR_table_memo_setup_msg(T);                                     \
        if (T->MR_integer >= MR_MEMO_BLOCK) {                           \
            Status = MR_MEMO_SUCCEEDED;                                 \
        } else {                                                        \
            Status = T->MR_loop_status;                                 \
            if (Status == MR_MEMO_INACTIVE) {                           \
                T->MR_loop_status = MR_MEMO_ACTIVE;                     \
            }                                                           \
        }                                                               \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                    \
    } while (0)

#define MR_table_memo_det_setup(T, Status)                              \
    MR_table_memo_setup(T, Status)

#define MR_table_memo_semi_setup(T, Status)                             \
    MR_table_memo_setup(T, Status)

#define MR_table_memo_non_setup(T, Record, Status)                      \
    do {                                                                \
        MR_save_transient_registers();                                  \
        if (T->MR_memo_non_record == NULL) {                            \
            MR_table_memo_non_setup_msg(T);                             \
            Status = MR_MEMO_NON_INACTIVE;                              \
            Record = MR_TABLE_NEW(MR_MemoNonRecord);                    \
            Record->MR_mn_back_ptr = T;                                 \
            Record->MR_mn_status = MR_MEMO_NON_ACTIVE;                  \
            Record->MR_mn_num_answers = 0;                              \
            Record->MR_mn_answer_table.MR_integer = 0;                  \
            Record->MR_mn_answer_list = NULL;                           \
            Record->MR_mn_answer_list_tail = &Record->MR_mn_answer_list;\
            T->MR_memo_non_record = Record;                             \
        } else {                                                        \
            Record = T->MR_memo_non_record;                             \
            Status = Record->MR_mn_status;                              \
        }                                                               \
        MR_table_memo_non_status_msg(Record);                           \
        MR_restore_transient_registers();                               \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                    \
    } while(0)

#define MR_table_memo_det_setup_shortcut(T0, T, Status)     ((void) 0)

#define MR_table_memo_semi_setup_shortcut(T0, T, Status)    ((void) 0)

#define MR_table_memo_non_setup_shortcut(T0, T, R, Status)  ((void) 0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_mark_as_succeeded_msg(T)                        \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("marking %p as succeeded\n", T);                     \
        }                                                               \
    } while(0)
#else
  #define MR_table_memo_mark_as_succeeded_msg(T)        ((void) 0)
#endif

#define MR_table_memo_mark_as_succeeded(T)                              \
    do {                                                                \
        MR_table_memo_mark_as_succeeded_msg(T);                         \
        T->MR_memo_status = MR_MEMO_SUCCEEDED;                          \
    } while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_mark_as_failed_msg(T)                           \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("marking %p as failed\n", T);                        \
        }                                                               \
    } while(0)
#else
  #define MR_table_memo_mark_as_failed_msg(T)           ((void) 0)
#endif

#define MR_table_memo_mark_as_failed(T)                                 \
    do {                                                                \
        MR_table_memo_mark_as_failed_msg(T);                            \
        T->MR_memo_status = MR_MEMO_FAILED;                             \
    } while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_mark_as_complete_msg(R)                         \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("marking %p as complete\n", R);                      \
        }                                                               \
    } while (0)
  #define MR_table_memo_mark_as_incomplete_msg(R)                       \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("marking %p as incomplete\n", R);                    \
        }                                                               \
    } while (0)
  #define MR_table_memo_mark_as_active_msg(R)                           \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("marking %p as active\n", R);                        \
        }                                                               \
    } while (0)
#else
  #define MR_table_memo_mark_as_complete_msg(R)     ((void) 0)
  #define MR_table_memo_mark_as_incomplete_msg(R)   ((void) 0)
  #define MR_table_memo_mark_as_active_msg(R)       ((void) 0)
#endif

#define MR_table_memo_mark_as_incomplete(R)                             \
    do {                                                                \
        MR_table_memo_mark_as_incomplete_msg(R);                        \
        R->MR_mn_status = MR_MEMO_NON_INCOMPLETE;                       \
    } while (0)

#define MR_table_memo_mark_as_active_and_fail(R)                        \
    do {                                                                \
        MR_table_memo_mark_as_active_msg(R);                            \
        R->MR_mn_status = MR_MEMO_NON_ACTIVE;                           \
    } while (0)

#define MR_table_memo_mark_as_complete_and_fail(R)                      \
    do {                                                                \
        MR_table_memo_mark_as_complete_msg(R);                          \
        R->MR_mn_status = MR_MEMO_NON_COMPLETE;                         \
    } while (0)

/***********************************************************************/

#define MR_table_memo_create_answer_block(T, Size, AnswerBlock)         \
    do {                                                                \
        MR_TABLE_CREATE_ANSWER_BLOCK(T, Size);                          \
        AnswerBlock = T->MR_answerblock;                                \
    } while(0)

/***********************************************************************/

#define MR_table_memo_fill_answer_block_shortcut(T)     ((void) 0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_get_answer_block_msg(T)                         \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("getting answer block %p -> %p\n",                   \
                T, T->MR_answerblock);                                  \
        }                                                               \
                                                                        \
        if (T->MR_memo_status < MR_MEMO_BLOCK) {                        \
            MR_fatal_error("table_memo_get_answer_block: "              \
                "no block");                                            \
        }                                                               \
    } while(0)
#else
  #define MR_table_memo_get_answer_block_msg(T) ((void) 0)
#endif

#define MR_table_memo_get_answer_block(T, AnswerBlock)                  \
    do {                                                                \
        MR_table_memo_get_answer_block_msg(T);                          \
        AnswerBlock = T->MR_answerblock;                                \
    } while(0)

/***********************************************************************/

#define MR_table_memo_get_answer_block_shortcut(T)      ((void) 0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_non_get_answer_table_msg(Record)                \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("getting answer table %p -> %p\n",                   \
                Record,                                                 \
                &(Record->MR_mn_answer_table));                         \
        }                                                               \
    } while(0)
#else
  #define MR_table_memo_non_get_answer_table_msg(Record) ((void) 0)
#endif

#define MR_table_memo_non_get_answer_table(Record, AnswerTable)         \
    do {                                                                \
        MR_table_memo_non_get_answer_table_msg(Record);                 \
        AnswerTable = &(Record->MR_mn_answer_table);                    \
    } while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_non_create_answer_block_msg(Record, answer_node)\
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("new answer slot %d at %p(%p)\n",                    \
                Record->MR_mn_num_answers, answer_node,                 \
                &answer_node->MR_aln_answer_block);                     \
            printf("\tstoring into %p\n",                               \
                Record->MR_mn_answer_list_tail);                        \
        }                                                               \
    } while(0)
#else
  #define MR_table_memo_non_create_answer_block_msg(Record, answer_node)\
    ((void) 0)
#endif

#define MR_table_memo_non_create_answer_block(Record, Size, AnswerBlock)\
    do {                                                                \
        MR_AnswerListNode   *answer_node;                               \
        MR_Word             **Slot;                                     \
                                                                        \
        Record->MR_mn_num_answers++;                                    \
                                                                        \
        /*                                                              \
        ** We fill in the answer_data slot with a dummy value.          \
        ** This slot will be filled in by the next piece of code        \
        ** to be executed after we return, which is why we return       \
        ** its address.                                                 \
        */                                                              \
                                                                        \
        answer_node = MR_TABLE_NEW(MR_AnswerListNode);                  \
        answer_node->MR_aln_answer_block = NULL;                        \
        answer_node->MR_aln_next_answer = NULL;                         \
                                                                        \
        MR_table_memo_non_create_answer_block_msg(Record, answer_node); \
        *(Record->MR_mn_answer_list_tail) = answer_node;                \
        Record->MR_mn_answer_list_tail =                                \
            &(answer_node->MR_aln_next_answer);                         \
        Slot = &(answer_node->MR_aln_answer_block);                     \
        MR_TABLE_CREATE_NODE_ANSWER_BLOCK(Slot, Size);                  \
        AnswerBlock = *Slot;                                            \
    } while(0)

#define MR_table_memo_non_create_answer_block_shortcut(Record)          \
    ((void) 0)

/***********************************************************************/

#define MR_table_memo_non_return_all_shortcut(Record)                   \
    ((void) 0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_non_answer_is_not_duplicate_msg(T)              \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("checking if %p is a duplicate answer: %ld\n",       \
                T, (long) T->MR_integer);                               \
        }                                                               \
    } while(0)
#else
  #define MR_table_memo_non_answer_is_not_duplicate_msg(T)    ((void) 0)
#endif

#define MR_table_memo_non_answer_is_not_duplicate(T, succ)              \
    do {                                                                \
        MR_bool     is_new_answer;                                      \
        MR_table_memo_non_answer_is_not_duplicate_msg(T);               \
        is_new_answer = (T->MR_integer == 0);                           \
        T->MR_integer = 1;  /* any nonzero value will do */             \
        succ = is_new_answer;                                           \
    } while(0)

#define MR_table_memo_non_answer_is_not_duplicate_shortcut(R, succ)     \
    ((void) 0)

/***********************************************************************/

#ifdef  MR_DEBUG_RETRY
  #define MR_table_io_in_range_check_msg                                \
    if (MR_io_tabling_debug) {                                          \
        printf("checking table_io_in_range: "                           \
            "prev %d, start %d, hwm %d",                                \
            MR_io_tabling_counter, MR_io_tabling_start,                 \
            MR_io_tabling_counter_hwm);                                 \
    }
  #define MR_table_io_in_range_in_range_msg                             \
    if (MR_io_tabling_debug) {                                          \
        printf(" in range\n");                                          \
    }
  #define MR_table_io_in_range_not_in_range_msg                         \
    if (MR_io_tabling_debug) {                                          \
        printf(" not in range\n");                                      \
    }
#else
  #define MR_table_io_in_range_check_msg            ((void) 0)
  #define MR_table_io_in_range_in_range_msg         ((void) 0)
  #define MR_table_io_in_range_not_in_range_msg     ((void) 0)
#endif

#define MR_table_io_in_range(T, Counter, Start, Succ)                   \
    if (MR_io_tabling_enabled) {                                        \
        MR_Unsigned old_counter;                                        \
                                                                        \
        MR_table_io_in_range_check_msg;                                 \
        old_counter = MR_io_tabling_counter;                            \
        MR_io_tabling_counter++;                                        \
                                                                        \
        if (MR_io_tabling_start < MR_io_tabling_counter                 \
            && MR_io_tabling_counter <= MR_io_tabling_end)              \
        {                                                               \
            T = &MR_io_tabling_pointer;                                 \
            Counter = (MR_Word) old_counter;                            \
            Start = MR_io_tabling_start;                                \
            if (MR_io_tabling_counter > MR_io_tabling_counter_hwm)      \
            {                                                           \
                MR_io_tabling_counter_hwm =                             \
                    MR_io_tabling_counter;                              \
            }                                                           \
                                                                        \
            MR_table_io_in_range_in_range_msg;                          \
            Succ = MR_TRUE;                                             \
        } else {                                                        \
            MR_table_io_in_range_not_in_range_msg;                      \
        Succ = MR_FALSE;                                                \
        }                                                               \
    } else {                                                            \
        Succ = MR_FALSE;                                                \
    }

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_io_has_occurred_msg(T)                               \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("checking %p for previous execution: %p\n",          \
                T, T->MR_answerblock);                                  \
        }                                                               \
    } while(0)
#else
  #define MR_table_io_has_occurred_msg(T)       ((void) 0)
#endif

#define MR_table_io_has_occurred(T, Succ)                               \
    do {                                                                \
        MR_table_io_has_occurred_msg(T);                                \
        Succ = (T->MR_answerblock != NULL);                             \
    } while(0)

/***********************************************************************/

#define MR_table_io_left_bracket_unitized_goal(TraceEnabled)            \
    do {                                                                \
        TraceEnabled = MR_debug_enabled;                                \
        MR_debug_enabled = MR_FALSE;                                    \
        MR_update_trace_func_enabled();                                 \
        MR_io_tabling_enabled = MR_FALSE;                               \
    } while(0)

/***********************************************************************/

#define MR_table_io_right_bracket_unitized_goal(TraceEnabled)           \
    do {                                                                \
        MR_io_tabling_enabled = MR_TRUE;                                \
        MR_debug_enabled = TraceEnabled;                                \
        MR_update_trace_func_enabled();                                 \
    } while(0)

/***********************************************************************/

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

/***********************************************************************/

#define MR_table_mm_setup(T, Subgoal, Status)                           \
    do {                                                                \
        MR_save_transient_registers();                                  \
        Subgoal = MR_setup_subgoal(T);                                  \
        Status = Subgoal->MR_sg_status;                                 \
        if (Status == MR_SUBGOAL_INACTIVE) {                            \
            MR_push_generator(MR_curfr, Subgoal);                       \
            MR_register_generator_ptr(Subgoal);                         \
            Subgoal->MR_sg_status = MR_SUBGOAL_ACTIVE;                  \
        }                                                               \
        MR_restore_transient_registers();                               \
        Status = MR_CONVERT_C_ENUM_CONSTANT(Status);                    \
    } while(0)

#define MR_table_mm_setup_shortcut(Subgoal, Status)                     \
    do {                                                                \
        MR_fatal_error("MR_table_mm_setup_shortcut");                   \
    } while(0)

/***********************************************************************/

#define MR_table_mm_return_all_shortcut(AnswerBlock)    ((void) 0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_mm_get_answer_table_msg(Subgoal)                     \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("getting answer table %p -> %p\n",                   \
                Subgoal,                                                \
                &(Subgoal->MR_sg_answer_table));                        \
        }                                                               \
    } while(0)
#else
  #define MR_table_mm_get_answer_table_msg(Subgoal) ((void) 0)
#endif

#define MR_table_mm_get_answer_table(Subgoal, AnswerTable)              \
    do {                                                                \
        MR_table_mm_get_answer_table_msg(Subgoal);                      \
        AnswerTable = &(Subgoal->MR_sg_answer_table);                   \
    } while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_mm_answer_is_not_duplicate_msg(T)                    \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("checking if %p is a duplicate answer: %ld\n",       \
                T, (long) T->MR_integer);                               \
        }                                                               \
    } while(0)
#else
  #define MR_table_mm_answer_is_not_duplicate_msg(T)    ((void) 0)
#endif

#ifdef  MR_TABLE_STATISTICS
  #define MR_table_mm_answer_is_not_duplicate_stats(T, is_new_answer)   \
    do {                                                                \
        MR_minmodel_stats_cnt_dupl_check++;                             \
        if (is_new_answer) {                                            \
            MR_minmodel_stats_cnt_dupl_check_not_dupl++;                \
        }                                                               \
    } while(0)
#else
  #define MR_table_mm_answer_is_not_duplicate_stats(T, is_new_answer)   \
    ((void) 0)
#endif

#define MR_table_mm_answer_is_not_duplicate(T, SUCCESS_INDICATOR)       \
    do {                                                                \
        MR_bool     is_new_answer;                                      \
        MR_table_mm_answer_is_not_duplicate_msg(T);                     \
        is_new_answer = (T->MR_integer == 0);                           \
        MR_table_mm_answer_is_not_duplicate_stats(T, is_new_answer);    \
        T->MR_integer = 1;  /* any nonzero value will do */             \
        SUCCESS_INDICATOR = is_new_answer;                              \
    } while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_mm_create_answer_block_msg(Subgoal, answer_node)     \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("%s: new answer slot %d at %p(%p)\n",                \
                MR_subgoal_addr_name(Subgoal),                          \
                Subgoal->MR_sg_num_ans, answer_node,                    \
                &answer_node->MR_aln_answer_block);                     \
            printf("\tstoring into %p\n",                               \
                Subgoal->MR_sg_answer_list_tail);                       \
        }                                                               \
    } while(0)
#else
  #define MR_table_mm_create_answer_block_msg(Subgoal, answer_node)     \
    ((void) 0)
#endif

#define MR_table_mm_create_answer_block(Subgoal, Size, AnswerBlock)     \
    do {                                                                \
        MR_AnswerListNode   *answer_node;                               \
        MR_Word             **Slot;                                     \
                                                                        \
        Subgoal->MR_sg_num_ans++;                                       \
                                                                        \
        /*                                                              \
        ** We fill in the answer_data slot with a dummy value.          \
        ** This slot will be filled in by the next piece of code        \
        ** to be executed after we return, which is why we return       \
        ** its address.                                                 \
        */                                                              \
                                                                        \
        answer_node = MR_TABLE_NEW(MR_AnswerListNode);                  \
        answer_node->MR_aln_answer_block = NULL;                        \
        answer_node->MR_aln_next_answer = NULL;                         \
                                                                        \
        MR_table_mm_create_answer_block_msg(Subgoal, answer_node);      \
        *(Subgoal->MR_sg_answer_list_tail) = answer_node;               \
        Subgoal->MR_sg_answer_list_tail =                               \
            &(answer_node->MR_aln_next_answer);                         \
        Slot = &(answer_node->MR_aln_answer_block);                     \
        MR_TABLE_CREATE_NODE_ANSWER_BLOCK(Slot, Size);                  \
        AnswerBlock = *Slot;                                            \
    } while(0)

/***********************************************************************/

#define MR_table_mm_fill_answer_block_shortcut(Subgoal) ((void) 0)

/***********************************************************************/

#else   /* MR_USE_MINIMAL_MODEL_STACK_COPY */

#define MR_MMSC_ERROR  \
        "stack copy minimal model code entered when not enabled"

#define MR_table_mm_setup(T, Subgoal, Status)                           \
    do {                                                                \
        MR_fatal_error(MR_MMSC_ERROR);                                  \
    } while(0)
#define MR_table_mm_setup_shortcut(Subgoal, Status)                     \
    do {                                                                \
        MR_fatal_error(MR_MMSC_ERROR);                                  \
    } while(0)
#define MR_table_mm_return_all_shortcut(AnswerBlock)                    \
    do {                                                                \
        MR_fatal_error(MR_MMSC_ERROR);                                  \
    } while(0)
#define MR_table_mm_get_answer_table(Subgoal, AnswerTable)              \
    do {                                                                \
        MR_fatal_error(MR_MMSC_ERROR);                                  \
    } while(0)
#define MR_table_mm_create_answer_block(Subgoal, Size, AnswerBlock)     \
    do {                                                                \
        MR_fatal_error(MR_MMSC_ERROR);                                  \
    } while(0)
#define MR_table_mm_fill_answer_block_shortcut(Subgoal)                 \
    do {                                                                \
        MR_fatal_error(MR_MMSC_ERROR);                                  \
    } while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_mmos_answer_is_not_duplicate_msg(T)                    \
    do {                                                                \
        if (MR_tabledebug) {                                            \
            printf("checking if %p is a duplicate answer: %ld\n",       \
                T, (long) T->MR_integer);                               \
        }                                                               \
    } while(0)
#else
  #define MR_table_mmos_answer_is_not_duplicate_msg(T)    ((void) 0)
#endif

#ifdef  MR_TABLE_STATISTICS
  #define MR_table_mmos_answer_is_not_duplicate_stats(T, is_new_answer) \
    do {                                                                \
        MR_mmos_stats_cnt_dupl_check++;                                 \
        if (is_new_answer) {                                            \
            MR_mmos_stats_cnt_dupl_check_not_dupl++;                    \
        }                                                               \
    } while(0)
#else
  #define MR_table_mmos_answer_is_not_duplicate_stats(T, is_new_answer) \
    ((void) 0)
#endif

#define MR_table_mmos_answer_is_not_duplicate(T, SUCCESS_INDICATOR)     \
    do {                                                                \
        MR_bool     is_new_answer;                                      \
        MR_table_mmos_answer_is_not_duplicate_msg(T);                   \
        is_new_answer = (T->MR_integer == 0);                           \
        MR_table_mmos_answer_is_not_duplicate_stats(T, is_new_answer);  \
        T->MR_integer = 1;  /* any nonzero value will do */             \
        SUCCESS_INDICATOR = is_new_answer;                              \
    } while(0)

#endif  /* MR_USE_MINIMAL_MODEL_STACK_COPY */

/***********************************************************************/

#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS

#define MR_MMOS_ERROR  \
        "own stack minimal model code entered when not enabled"

#else   /* MR_USE_MINIMAL_MODEL_OWN_STACKS */

#define MR_MMOS_ERROR  \
        "own stack minimal model code entered when not enabled"

#define MR_table_mmos_save_inputs_shortcut(num_inputs)                  \
    do {                                                                \
        MR_fatal_error(MR_MMOS_ERROR);                                  \
    } while(0)
#define MR_table_mmos_consume_next_answer_nondet(consumer, answerblock, succ) \
    do {                                                                \
        MR_fatal_error(MR_MMOS_ERROR);                                  \
    } while(0)
#define MR_table_mmos_get_answer_table(generator, trienode)             \
    do {                                                                \
        MR_fatal_error(MR_MMOS_ERROR);                                  \
    } while(0)
#define MR_table_mmos_create_answer_block(generator, blocksize, answerblock) \
    do {                                                                \
        MR_fatal_error(MR_MMOS_ERROR);                                  \
    } while(0)
#define MR_table_mmos_return_answer(generator, answerblock)             \
    do {                                                                \
        MR_fatal_error(MR_MMOS_ERROR);                                  \
    } while(0)
#define MR_table_mmos_completion(generator)                             \
    do {                                                                \
        MR_fatal_error(MR_MMOS_ERROR);                                  \
    } while(0)

#endif  /* MR_USE_MINIMAL_MODEL_OWN_STACKS */

/***********************************************************************/
