// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2004-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_mm_own_stacks.h:
// definitions of some basic stuff used for the own stack style of
// minimal model tabling.

#ifndef MERCURY_MM_OWN_STACKS_H
#define MERCURY_MM_OWN_STACKS_H

#include "mercury_imp.h"
#include "mercury_tabling.h"
#include "mercury_stack_layout.h"

#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS

// There is one MR_Generator structure per active generator.
//
// The back_ptr field points back to the MR_TrieNode that points to this
// generator.
//
// The context field points to the MR_Context struct of the context that holds
// the state of this generator, including its pair of stacks.
//
// The proc_layout field is set only if debugging is enabled; it allows us
// to find out what subgoal we are the generator for, and to interpret
// the answer table.
//
// The pred_id field is currently always set to a string identifying the
// main predicate of the subgoal we are the generator for. It is used for
// debugging. Once the mmos grade is debugged, we can disable this field.
//
// The leader field points to the leader of the clique this generator belongs
// to; if this generator is the leader, this field points to its own structure.
// Not yet functional.
//
// If this generator is the leader of its clique, the led_generators field will
// point to a list of all generators in the clique, including itself. If this
// generator is not the leader of its clique, this field will contain an empty
// list. Not yet functional.
//
// XXX more fields
//
// XXX Try encoding the value of MR_cons_registered in the redoip.

struct MR_Generator_Struct {
    MR_TrieNode             MR_gen_back_ptr;
    MR_Context              *MR_gen_context;
    const MR_ProcLayout     *MR_gen_proc_layout;
    MR_ConstString          MR_gen_pred_id;
    MR_Integer              MR_gen_num_input_args;
    MR_Word                 *MR_gen_input_args;
    MR_Word                 MR_gen_closure;
    MR_GeneratorPtr         MR_gen_leader;
    MR_Dlist                *MR_gen_led_generators;
    MR_Dlist                *MR_gen_consumers;
    MR_Integer              MR_gen_num_answers;
    MR_TableNode            MR_gen_answer_table;
    MR_AnswerList           MR_gen_answer_list;
    MR_AnswerList           *MR_gen_answer_list_tail;
    MR_bool                 MR_gen_is_complete;
};

struct MR_Consumer_Struct {
    MR_ConstString          MR_cons_pred_id;
    MR_GeneratorPtr         MR_cons_answer_generator;
    MR_GeneratorPtr         MR_cons_containing_generator;
    MR_Context              *MR_cons_context;
    MR_bool                 MR_cons_registered;
    MR_Integer              MR_cons_num_returned_answers;
    MR_AnswerList           *MR_cons_remaining_answer_list_ptr;
};

////////////////////////////////////////////////////////////////////////////

extern  MR_Word         MR_mmos_arg_regs[MR_MAX_FAKE_REG];
extern  MR_GeneratorPtr MR_mmos_new_generator;

extern  MR_GeneratorPtr MR_mmos_returning_generator;

#define MR_table_mmos_save_input_arg(pos, arg)                          \
                        do {                                            \
                            MR_mmos_arg_regs[(pos)] = (arg);            \
                        } while (0)

#define MR_table_mmos_pickup_input_arg(pos, arg)                        \
                        do {                                            \
                            (arg) = MR_mmos_arg_regs[(pos)];            \
                        } while (0)

#define MR_table_mmos_get_answer_table(gen)                             \
                        &(gen)->MR_gen_answer_table;

extern  const MR_ProcLayout *MR_subgoal_debug_cur_proc;

extern  void            MR_enter_cons_debug(MR_Consumer *consumer);
extern  MR_ConsDebug    *MR_lookup_cons_debug_addr(MR_Consumer *consumer);
extern  MR_ConsDebug    *MR_lookup_cons_debug_num(int consumer_index);
extern  const char      *MR_cons_debug_name(MR_ConsDebug *consumer_dbg);
extern  const char      *MR_cons_addr_name(MR_Consumer *consumer);
extern  const char      *MR_cons_addr_short_name(MR_Consumer *consumer);
extern  const char      *MR_cons_num_name(int consumer_index);

extern  void            MR_enter_gen_debug(MR_Generator *gen);
extern  MR_GenDebug     *MR_lookup_gen_debug_addr(MR_Generator *gen);
extern  MR_GenDebug     *MR_lookup_gen_debug_num(int gen_index);
extern  const char      *MR_gen_debug_name(MR_GenDebug *gen_debug);
extern  const char      *MR_gen_addr_name(MR_Generator *gen);
extern  const char      *MR_gen_addr_short_name(MR_Generator *gen);
extern  const char      *MR_gen_subgoal(MR_Generator *gen);
extern  const char      *MR_gen_num_name(int gen_index);

extern  void            MR_print_gen_debug(FILE *fp,
                            const MR_ProcLayout *proc,
                            MR_GenDebug *gen_debug);
extern  void            MR_print_generator(FILE *fp,
                            const MR_ProcLayout *proc, MR_Generator *gen);
extern  void            MR_print_cons_debug(FILE *fp,
                            const MR_ProcLayout *proc,
                            MR_ConsDebug *consumer_debug);
extern  void            MR_print_consumer(FILE *fp,
                            const MR_ProcLayout *proc, MR_Consumer *consumer);

extern  void            MR_mm_own_stacks_report_stats(FILE *fp);

extern  MR_ConsumerPtr  MR_table_mmos_setup_consumer(MR_GeneratorPtr generator,
                            MR_ConstString pred_id);
extern  MR_GeneratorPtr MR_table_mmos_setup_generator(MR_TrieNode trie_node,
                            MR_Integer num_input_args,
                            MR_Word genererator_pred,
                            MR_ConstString pred_id);

#endif  // MR_USE_MINIMAL_MODEL_OWN_STACKS

#ifdef  MR_HIGHLEVEL_CODE

  extern void MR_CALL
      mercury__table_builtin__table_mmos_consume_next_answer_nondet_2_p_0(
        MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
        MR_Cont cont, void *cont_env_ptr);
  extern void MR_CALL
      mercury__table_builtin__table_mmos_consume_next_answer_multi_2_p_0(
        MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
        MR_Cont cont, void *cont_env_ptr);

#else   // MR_HIGHLEVEL_CODE

  #define MR_MMOS_RET_ALL_NONDET_ENTRY                                  \
    MR_proc_entry_user_name(table_builtin,                              \
        table_mmos_consume_next_answer_nondet, 2, 0)
  #define MR_MMOS_RET_ALL_MULTI_ENTRY                                   \
    MR_proc_entry_user_name(table_builtin,                              \
        table_mmos_consume_next_answer_multi, 2, 0)

#endif  // MR_HIGHLEVEL_CODE

#endif  // MERCURY_MM_OWN_STACKS_H
