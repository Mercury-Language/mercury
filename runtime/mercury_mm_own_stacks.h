/*
** Copyright (C) 2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_mm_own_stacks.h:
** definitions of some basic stuff used for the own stack style of
** minimal model tabling.
*/

#ifndef	MERCURY_MM_OWN_STACKS_H
#define	MERCURY_MM_OWN_STACKS_H

#include "mercury_imp.h"
#include "mercury_tabling.h"
#include "mercury_stack_layout.h"

#ifdef	MR_USE_MINIMAL_MODEL_OWN_STACKS

/*
** There is one MR_Generator structure per active generator.
**
** The back_ptr field points back to the MR_TrieNode that points to this
** generator.
**
** The stack_pair field points back to the MR_GenStackPair struct whose
** MR_gen_pair_gen field points here; it is used to find the generator's
** stacks.
**
** The proc_layout field is set only if debugging is enabled; it allows us
** to find out what subgoal we are the generator for, and to interpret
** the answer table.
**
** The pred_id field is currently always set to a string identifying the
** main predicate of the subgoal we are the generator for. It is used for
** debugging. Once this stuff is debugged, we can disable this field.
**
** The leader field points to the leader of the clique this generator belongs
** to; if this generator is the leader, this field points to its own structure.
**
** If this generator is the leader of its clique, the led_generators field will
** point to a list of all generators in the clique, including itself. If this
** generator is not the leader of its clique, this field will contain an empty
** list.
*/

struct MR_Generator_Struct {
	MR_TrieNode		MR_gen_back_ptr;
	MR_Context		*MR_gen_context;
	const MR_Proc_Layout	*MR_gen_proc_layout;
	MR_String		*MR_gen_pred_id;
	MR_Generator		*MR_gen_leader;
	MR_Dlist		*MR_gen_led_generators;
	MR_Dlist		*MR_gen_consumers;
	MR_Integer		MR_gen_num_answers;
	MR_TableNode		MR_gen_answer_table;
	MR_AnswerList		MR_gen_answer_list;
	MR_AnswerList		*MR_gen_answer_list_tail;
};

struct MR_Consumer_Struct {
	MR_Generator	*MR_cons_answer_generator;
	MR_Generator	*MR_cons_containing_generator;
	MR_Integer	MR_cons_num_returned_answers;
	MR_AnswerList	*MR_cons_remaining_answer_list_ptr;
};

/*---------------------------------------------------------------------------*/

extern	const MR_Proc_Layout	*MR_subgoal_debug_cur_proc;

extern	void		MR_enter_cons_debug(MR_Consumer *consumer);
extern	MR_ConsDebug 	*MR_lookup_cons_debug_addr(MR_Consumer *consumer);
extern	MR_ConsDebug 	*MR_lookup_cons_debug_num(int consumer_index);
extern	const char	*MR_cons_debug_name(MR_ConsDebug *consumer_dbg);
extern	const char	*MR_cons_addr_name(MR_Consumer *consumer);
extern	const char	*MR_cons_num_name(int consumer_index);

extern	void		MR_enter_gen_debug(MR_Generator *gen);
extern	MR_GenDebug	*MR_lookup_gen_debug_addr(MR_Generator *gen);
extern	MR_GenDebug	*MR_lookup_gen_debug_num(int gen_index);
extern	const char	*MR_gen_debug_name(MR_GenDebug *gen_debug);
extern	const char	*MR_gen_addr_name(MR_Generator *gen);
extern	const char	*MR_gen_num_name(int gen_index);

extern	void		MR_print_gen_debug(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_GenDebug *gen_debug);
extern	void		MR_print_generator(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_Generator *gen);
extern	void		MR_print_cons_debug(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_ConsDebug *consumer_debug);
extern	void		MR_print_consumer(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_Consumer *consumer);

extern	MR_ConsumerPtr	MR_table_mmos_setup_consumer(MR_TableNode trie_node,
				MR_Integer num_input_args,
				MR_Word *generator_pred, MR_String pred_id);
extern	MR_GeneratorPtr	MR_mmos_setup_generator(MR_String pred_id,
				MR_TrieNode trie_node);
extern	MR_AnswerBlock	MR_table_mmos_consumer_get_next_answer(
				MR_ConsumerPtr consumer);
extern	MR_TrieNode	MR_table_mmos_get_answer_table(MR_GeneratorPtr
				generator);
extern	MR_AnswerBlock	MR_table_mmos_create_answer_block(MR_GeneratorPtr
				generator);

#endif	/* MR_USE_MINIMAL_MODEL_OWN_STACKS */

#ifdef	MR_HIGHLEVEL_CODE

  extern void MR_CALL
  mercury__table_builtin__table_mmos_consume_next_answer_nondet_2_p_0(
	MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
	MR_Cont cont, void *cont_env_ptr);
  extern void MR_CALL
  mercury__table_builtin__table_mmos_consume_next_answer_multi_2_p_0(
	MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
	MR_Cont cont, void *cont_env_ptr);

#else	/* MR_HIGHLEVEL_CODE */

  #define MR_MMOS_RET_ALL_NONDET_ENTRY                                  \
	MR_proc_entry_user_name(table_builtin,                          \
		table_mmos_consume_next_answer_nondet, 2, 0)
  #define MR_MMOS_RET_ALL_MULTI_ENTRY                                   \
	MR_proc_entry_user_name(table_builtin,                          \
		table_mmos_consume_next_answer_multi, 2, 0)

#endif	/* MR_HIGHLEVEL_CODE */

#endif	/* MERCURY_MM_OWN_STACKS_H */
