/*
** Copyright (C) 2003 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_minimal_model.h - definitions of some basic stuff used for tabling.
** For tabling code, the Mercury compiler (compiler/table_gen.m) generates
** references to special procedures defined in library/table_builtin.m.
** The types and macros defined here are used by the procedures defined in
** library/table_builtin.m.
*/

#ifndef	MERCURY_MINIMAL_MODEL_H
#define	MERCURY_MINIMAL_MODEL_H

#include "mercury_imp.h"
#include "mercury_tabling.h"
#include "mercury_reg_workarounds.h"
#include "mercury_goto.h"		/* for MR_declare_entry */

struct MR_AnswerListNode_Struct {
	MR_Integer	MR_aln_answer_num;
	MR_TableNode	MR_aln_answer_data;
				/* always uses the MR_answerblock member */
	MR_AnswerList	MR_aln_next_answer;
};

/*
** The saved state of a generator or a consumer. While consumers get
** suspended while they are waiting for generators to produce more solutions,
** generators need their state saved when they restore the state of a consumer
** to consume a new solution.
**
** The saved state contains copies of
**
** - several virtual machine registers:
**   MR_succip, MR_sp, MR_curfr and MR_maxfr
**
** - segments of the nondet and det stacks:
**   the parts that cannot possibly change between the times of saving
**   and restoring the saved state are not saved.
**
**   The segments are described by three fields each. The *_real_start
**   field gives the address of the first word in the real stack
**   that is part of the saved segment, the *_block_size field
**   gives the size of the saved segment in words, and the *_saved_block
**   field points to the area of memory containing the saved segment.
**
** - the entire generator stack and the entire cut stack:
**   they are usually so small, it is faster to save them all
**   than to figure out which parts need saving.
**
**   Each stack is described by its size in words and a pointer to
**   an area of memory containing the entire saved stack.
*/

typedef struct {
	MR_Code			*MR_ss_succ_ip;
	MR_Word			*MR_ss_s_p;
	MR_Word			*MR_ss_cur_fr;
	MR_Word			*MR_ss_max_fr;
	MR_Word			*MR_ss_common_ancestor_fr;

	MR_Word			*MR_ss_non_stack_real_start;
	MR_Word			MR_ss_non_stack_block_size;
	MR_Word			*MR_ss_non_stack_saved_block;

	MR_Word			*MR_ss_det_stack_real_start;
	MR_Word			MR_ss_det_stack_block_size;
	MR_Word			*MR_ss_det_stack_saved_block;

	MR_Integer		MR_ss_gen_next;
	MR_GenStackFrame	*MR_ss_gen_stack_saved_block;

	MR_Integer		MR_ss_cut_next;
	MR_CutStackFrame	*MR_ss_cut_stack_saved_block;

	MR_Integer		MR_ss_pneg_next;
	MR_PNegStackFrame	*MR_ss_pneg_stack_saved_block;
} MR_SavedState;

/* The state of a consumer subgoal */
struct MR_Consumer_Struct {
	MR_SavedState		MR_cns_saved_state;
	MR_AnswerList		*MR_cns_remaining_answer_list_ptr;
#ifdef	MR_TABLE_DEBUG
	MR_Subgoal		*MR_cns_subgoal;
#endif
};

struct MR_ConsumerListNode_Struct {
	MR_Consumer		*MR_cl_item;
	MR_ConsumerList		MR_cl_next;
};

/*
** The following structure is used to hold the state and variables used in 
** the table_resume procedure.
*/

typedef struct {
	MR_SavedState		MR_ri_leader_state;
	MR_SubgoalList		MR_ri_subgoal_list;
	MR_Subgoal		*MR_ri_cur_subgoal;
	MR_ConsumerList		MR_ri_consumer_list; /* for the cur subgoal */
	MR_Consumer		*MR_ri_cur_consumer;
	MR_AnswerList		MR_ri_cur_consumer_answer_list;
	MR_bool			MR_ri_changed;
	MR_Code			*MR_ri_saved_succip;
} MR_ResumeInfo;

struct MR_SubgoalListNode_Struct {
	MR_Subgoal		*MR_sl_item;
	MR_SubgoalList		MR_sl_next;
};

/*
** The subgoal structure represents a subgoal, i.e. a call to a minimal model
** predicate with a given set of input arguments.
**
** The MR_sg_status obviously gives the current status of the subgoal.
**
** The MR_sg_leader field points to the leader of the clique of subgoals this
** subgoal is a member of, if the leader is not the subgoal itself. If the
** subgoal is the leader of its clique, then this field will be NULL.
**
** The MR_sg_followers field lists all the subgoals that follow this one.
** Each subgoal occurs in its own followers list.
**
** The MR_sg_followers_tail field points to the NULL pointer at the tail of
** the MR_sg_followers list, to allow us to add new elements at the tail in
** constant time.
**
** The MR_sg_resume_info field, when non-NULL, points to a data structure
** containing the local variables of the algorithm executed by the builtin
** predicate table_nondet_resume. The execution of that algorithm invokes
** general Mercury code and moves stack segments around many times, which
** is why its local variables cannot be stored in a stack frame.
**
** The MR_sg_answer_table field points to the trie of answers returned for
** this subgoal so far (to enable efficient checks for duplicate answers).
**
** The MR_sg_answer_list field also contains the answers so far, in a list.
** The answers will be returned to consumers in the order given by this list.
** The MR_sg_answer_list_tail field points to the NULL pointer at the tail
** of this list, to allow us to add new elements to the list at the tail in
** constant time.
**
** The MR_sg_num_ans field gives the number of answers computed so far, i.e.
** the number of answers in MR_sg_answer_table and MR_sg_answer_list.
** The MR_sg_num_committed_ans field gives the number of answers that the
** leader is committed to returning to all consumers.
**
** The MR_sg_consumer_list gives the list of consumer goals, with the field
** MR_sg_consumer_list_tail allowing fast appending to the end.
**
** The MR_sg_generator_fr points to the generator's nondet stack frame.
*/

struct MR_Subgoal_Struct {
	MR_SubgoalStatus	MR_sg_status;
	MR_Subgoal		*MR_sg_leader;
	MR_SubgoalList		MR_sg_followers;
	MR_SubgoalList		*MR_sg_followers_tail;
	MR_ResumeInfo		*MR_sg_resume_info;
	MR_Word			MR_sg_answer_table;
	MR_Integer		MR_sg_num_ans;
	MR_Integer		MR_sg_num_committed_ans;
	MR_AnswerList		MR_sg_answer_list;
	MR_AnswerList		*MR_sg_answer_list_tail;
	MR_ConsumerList		MR_sg_consumer_list;
	MR_ConsumerList		*MR_sg_consumer_list_tail;
	MR_Word			*MR_sg_generator_fr;
#ifdef	MR_TABLE_DEBUG
	const MR_Proc_Layout	*MR_sg_proc_layout;
#endif	MR_TABLE_DEBUG
};

/*---------------------------------------------------------------------------*/

extern	const MR_Proc_Layout	*MR_subgoal_debug_cur_proc;

extern	void		MR_enter_consumer_debug(MR_Consumer *consumer);
extern	MR_ConsumerDebug *MR_lookup_consumer_debug_addr(MR_Consumer *consumer);
extern	MR_ConsumerDebug *MR_lookup_consumer_debug_num(int consumer_index);
extern	const char	*MR_consumer_debug_name(MR_ConsumerDebug *consumer_dbg);
extern	const char	*MR_consumer_addr_name(MR_Consumer *consumer);
extern	const char	*MR_consumer_num_name(int consumer_index);

extern	void		MR_enter_subgoal_debug(MR_Subgoal *subgoal);
extern	MR_SubgoalDebug	*MR_lookup_subgoal_debug_addr(MR_Subgoal *subgoal);
extern	MR_SubgoalDebug	*MR_lookup_subgoal_debug_num(int subgoal_index);
extern	const char	*MR_subgoal_debug_name(MR_SubgoalDebug *subgoal_debug);
extern	const char	*MR_subgoal_addr_name(MR_Subgoal *subgoal);
extern	const char	*MR_subgoal_num_name(int subgoal_index);
extern	const char	*MR_subgoal_status(MR_SubgoalStatus status);

extern	void		MR_print_subgoal_debug(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_SubgoalDebug *subgoal_debug);
extern	void		MR_print_subgoal(FILE *fp, const MR_Proc_Layout *proc,
				MR_Subgoal *subgoal);
extern	void		MR_print_consumer_debug(FILE *fp,
				const MR_Proc_Layout *proc,
				MR_ConsumerDebug *consumer_debug);
extern	void		MR_print_consumer(FILE *fp, const MR_Proc_Layout *proc,
				MR_Consumer *consumer);


#ifndef	MR_HIGHLEVEL_CODE

  #define MR_SUSPEND_ENTRY     mercury__table_builtin__table_nondet_suspend_2_0
  #define MR_RESUME_ENTRY      mercury__table_builtin__table_nondet_resume_1_0

  MR_declare_entry(MR_SUSPEND_ENTRY);
  MR_declare_entry(MR_RESUME_ENTRY);

#endif	/* !MR_HIGHLEVEL_CODE */

#endif	/* MERCURY_MINIMAL_MODEL_H */
