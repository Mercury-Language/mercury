/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
** This file contains C macros that represent the bodies of predicates
** in library/table_builtin.m.
*/

/***********************************************************************/

#ifdef	MR_TABLE_DEBUG
  #define MR_table_loop_setup_msg(T)					\
	do {								\
		if (MR_tabledebug) {					\
			printf("status of loop table %p: %ld (%lx)\n",	\
				T, (long) T->MR_loop_status,		\
				(long) T->MR_loop_status);		\
		}							\
	} while(0)
#else
  #define MR_table_loop_setup_msg(T)			((void) 0)
#endif

#define	MR_table_loop_setup(T, Status)					\
	do {								\
		MR_table_loop_setup_msg(T);				\
		Status = T->MR_loop_status;				\
		if (Status == MR_LOOP_INACTIVE) {			\
			T->MR_loop_status = MR_LOOP_ACTIVE;		\
		}							\
		Status = MR_CONVERT_C_ENUM_CONSTANT(Status);		\
	} while (0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_loop_mark_as_inactive_msg(T)				\
	do {								\
		if (MR_tabledebug) {					\
			printf("marking %p as uninitialized\n", T);	\
		}							\
	} while (0)
#else
  #define MR_table_loop_mark_as_inactive_msg(T)		((void) 0)
#endif

#define	MR_table_loop_mark_as_inactive(T)				\
	do {								\
		MR_table_loop_mark_as_inactive_msg(T);			\
		T->MR_loop_status = MR_LOOP_INACTIVE;			\
	} while (0)

/***********************************************************************/

#ifdef	MR_TABLE_DEBUG
  #define MR_table_memo_setup_msg(T)					\
	do {								\
		if (MR_tabledebug) {					\
			printf("status of memo table %p: %ld (%lx)\n",	\
				T, (long) T->MR_memo_status,		\
				(long) T->MR_memo_status);		\
		}							\
	} while(0)
#else
  #define MR_table_memo_setup_msg(T)			((void) 0)
#endif

#define	MR_table_memo_setup(T, Status)					\
	do {								\
		MR_table_memo_setup_msg(T);				\
		if (T->MR_integer >= MR_MEMO_BLOCK) {			\
			Status = MR_MEMO_SUCCEEDED;			\
		} else {						\
			Status = T->MR_loop_status;			\
			if (Status == MR_MEMO_INACTIVE) {		\
				T->MR_loop_status = MR_MEMO_ACTIVE;	\
			}						\
		}							\
		Status = MR_CONVERT_C_ENUM_CONSTANT(Status);		\
	} while (0)

#define	MR_table_memo_det_setup(T, Status)				\
	MR_table_memo_setup(T, Status)

#define	MR_table_memo_semi_setup(T, Status)				\
	MR_table_memo_setup(T, Status)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_mark_as_succeeded_msg(T)			\
	do {								\
		if (MR_tabledebug) {					\
			printf("marking %p as succeeded\n", T);		\
		}							\
	} while(0)
#else
  #define MR_table_memo_mark_as_succeeded_msg(T)	((void) 0)
#endif

#define MR_table_memo_mark_as_succeeded(T)				\
	do {								\
		MR_table_memo_mark_as_succeeded_msg(T);			\
		T->MR_memo_status = MR_MEMO_SUCCEEDED;			\
	} while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_mark_as_failed_msg(T)				\
	do {								\
		if (MR_tabledebug) {					\
			printf("marking %p as failed\n", T);		\
		}							\
	} while(0)
#else
  #define MR_table_memo_mark_as_failed_msg(T)	((void) 0)
#endif

#define MR_table_memo_mark_as_failed(T)					\
	do {								\
		MR_table_memo_mark_as_failed_msg(T);			\
		T->MR_memo_status = MR_MEMO_FAILED;			\
	} while(0)

/***********************************************************************/

#define	MR_table_memo_create_answer_block(T, Size, AnswerBlock)		\
	do {								\
		MR_TABLE_CREATE_ANSWER_BLOCK(T, Size);			\
		AnswerBlock = T->MR_answerblock;			\
	} while(0)

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_memo_get_answer_block_msg(T)				\
	do {								\
		if (MR_tabledebug) {					\
			printf("getting answer block %p -> %p\n",	\
				T, T->MR_answerblock);			\
		}							\
									\
		if (T->MR_memo_status < MR_MEMO_BLOCK) {		\
			MR_fatal_error("table_memo_get_answer_block: "	\
				"no block");				\
		}							\
	} while(0)
#else
  #define MR_table_memo_get_answer_block_msg(T)	((void) 0)
#endif

#define MR_table_memo_get_answer_block(T, AnswerBlock)			\
	do {								\
		MR_table_memo_get_answer_block_msg(T);			\
		AnswerBlock = T->MR_answerblock;			\
	} while(0)

/***********************************************************************/

#ifdef	MR_DEBUG_RETRY
  #define MR_table_io_in_range_check_msg				\
	if (MR_io_tabling_debug) {					\
		printf("checking table_io_in_range: "			\
			"prev %d, start %d, hwm %d",			\
			MR_io_tabling_counter, MR_io_tabling_start,	\
			MR_io_tabling_counter_hwm);			\
	}
  #define MR_table_io_in_range_in_range_msg				\
	if (MR_io_tabling_debug) {					\
		printf(" in range\n");					\
	}
  #define MR_table_io_in_range_not_in_range_msg				\
	if (MR_io_tabling_debug) {					\
		printf(" not in range\n");				\
	}
#else
  #define MR_table_io_in_range_check_msg		((void) 0)
  #define MR_table_io_in_range_in_range_msg		((void) 0)
  #define MR_table_io_in_range_not_in_range_msg		((void) 0)
#endif

#define MR_table_io_in_range(T, Counter, Start, Succ)			\
	if (MR_io_tabling_enabled) {					\
		MR_Unsigned	old_counter;				\
									\
		MR_table_io_in_range_check_msg;				\
		old_counter = MR_io_tabling_counter;			\
		MR_io_tabling_counter++;				\
									\
		if (MR_io_tabling_start < MR_io_tabling_counter 	\
			&& MR_io_tabling_counter <= MR_io_tabling_end)	\
		{							\
			T = &MR_io_tabling_pointer;			\
			Counter = (MR_Word) old_counter;		\
			Start = MR_io_tabling_start;			\
			if (MR_io_tabling_counter > MR_io_tabling_counter_hwm)\
			{						\
				MR_io_tabling_counter_hwm =		\
					MR_io_tabling_counter;		\
			}						\
									\
			MR_table_io_in_range_in_range_msg;		\
			Succ = MR_TRUE;					\
		} else {						\
			MR_table_io_in_range_not_in_range_msg;		\
		Succ = MR_FALSE;					\
		}							\
	} else {							\
		Succ = MR_FALSE;					\
	}

/***********************************************************************/

#ifdef  MR_TABLE_DEBUG
  #define MR_table_io_has_occurred_msg(T)				\
	do {								\
		if (MR_tabledebug) {					\
			printf("checking %p for previous execution: %p\n", \
				T, T->MR_answerblock);			\
		}							\
	} while(0)
#else
  #define MR_table_io_has_occurred_msg(T)		((void) 0)
#endif

#define MR_table_io_has_occurred(T, Succ)				\
	do {								\
		MR_table_io_has_occurred_msg(T);			\
		Succ = (T->MR_answerblock != NULL);			\
	} while(0)

/***********************************************************************/

#define MR_table_io_left_bracket_unitized_goal(TraceEnabled)		\
	do {								\
		TraceEnabled = MR_trace_enabled;			\
		MR_trace_enabled = MR_FALSE;				\
		MR_io_tabling_enabled = MR_FALSE;			\
	} while(0)

/***********************************************************************/

#define MR_table_io_right_bracket_unitized_goal(TraceEnabled)		\
	do {								\
		MR_io_tabling_enabled = MR_TRUE;			\
		MR_trace_enabled = TraceEnabled;			\
	} while(0)

/***********************************************************************/

#ifdef	MR_USE_MINIMAL_MODEL

/***********************************************************************/

#define	MR_table_mm_setup(T, Subgoal, Status)				\
	do {								\
		MR_save_transient_registers();				\
		Subgoal = MR_setup_subgoal(T);				\
		Status = Subgoal->MR_sg_status;				\
		if (Status == MR_SUBGOAL_INACTIVE) {			\
			MR_push_generator(MR_curfr, Subgoal);		\
			MR_register_generator_ptr(Subgoal);		\
			Subgoal->MR_sg_status = MR_SUBGOAL_ACTIVE;	\
		}							\
		MR_restore_transient_registers();			\
		Status = MR_CONVERT_C_ENUM_CONSTANT(Status);		\
	} while(0)

/***********************************************************************/

#ifdef	MR_TABLE_DEBUG
  #define MR_table_mm_get_answer_table_msg(Subgoal)			\
	do {								\
		if (MR_tabledebug) {					\
			printf("getting answer table %p -> %p\n",	\
				Subgoal,				\
				&(Subgoal->MR_sg_answer_table));	\
		}							\
	} while(0)
#else
  #define MR_table_mm_get_answer_table_msg(Subgoal)	((void) 0)
#endif

#define	MR_table_mm_get_answer_table(Subgoal, AnswerTable)		\
	do {								\
		MR_table_mm_get_answer_table_msg(Subgoal);		\
		AnswerTable = &(Subgoal->MR_sg_answer_table);		\
	} while(0)

/***********************************************************************/

#ifdef	MR_MINIMAL_MODEL_DEBUG
  #define MR_table_mm_create_answer_block_set(Subgoal, answer_node)	\
	do {								\
		answer_node->MR_aln_answer_num = Subgoal->MR_sg_num_ans; \
	} while(0)
#else
  #define MR_table_mm_create_answer_block_set(Subgoal, answer_node)	\
	((void) 0)
#endif

#ifdef	MR_TABLE_DEBUG
  #define MR_table_mm_create_answer_block_msg(Subgoal, answer_node)	\
	do {								\
		if (MR_tabledebug) {					\
			printf("%s: new answer slot %d at %p(%p)\n",	\
				MR_subgoal_addr_name(Subgoal),		\
				Subgoal->MR_sg_num_ans, answer_node,	\
				&answer_node->MR_aln_answer_data);	\
			printf("\tstoring into %p\n",			\
				Subgoal->MR_sg_answer_list_tail);	\
		}							\
	} while(0)
#else
  #define MR_table_mm_create_answer_block_msg(Subgoal, answer_node)	\
	((void) 0)
#endif

#define	MR_table_mm_create_answer_block(Subgoal, Size, AnswerBlock)	\
	do {								\
		MR_AnswerListNode	*answer_node;			\
		MR_TrieNode		Slot;				\
									\
		Subgoal->MR_sg_num_ans++;				\
									\
		/*							\
		** We fill in the answer_data slot with a dummy value.	\
		** This slot will be filled in by the next piece of code  \
		** to be executed after we return, which is why we return \
		** its address.						\
		*/							\
									\
		answer_node = MR_TABLE_NEW(MR_AnswerListNode);		\
		answer_node->MR_aln_answer_data.MR_integer = 0;		\
		answer_node->MR_aln_next_answer = NULL;			\
									\
		MR_table_mm_create_answer_block_set(Subgoal, answer_node); \
		MR_table_mm_create_answer_block_msg(Subgoal, answer_node); \
		*(Subgoal->MR_sg_answer_list_tail) = answer_node;	\
		Subgoal->MR_sg_answer_list_tail =			\
			&(answer_node->MR_aln_next_answer);		\
		Slot = &(answer_node->MR_aln_answer_data);		\
		MR_TABLE_CREATE_ANSWER_BLOCK(Slot, Size);		\
		AnswerBlock = Slot->MR_answerblock;			\
	} while(0)

/***********************************************************************/

#else	/* MR_USE_MINIMAL_MODEL */

#define	MR_table_mm_setup(T, Subgoal, Status)				\
	do {								\
		MR_fatal_error("minimal model code entered when not enabled");\
	} while(0)
#define	MR_table_mm_get_answer_table(Subgoal, AnswerTable)		\
	do {								\
		MR_fatal_error("minimal model code entered when not enabled");\
	} while(0)
#define	MR_table_mm_create_answer_block(Subgoal, Size, AnswerBlock)	\
	do {								\
		MR_fatal_error("minimal model code entered when not enabled");\
	} while(0)

#endif	/* MR_USE_MINIMAL_MODEL */

/***********************************************************************/
