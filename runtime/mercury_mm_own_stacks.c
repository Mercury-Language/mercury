// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2004-2007, 2011 The University of Melbourne.
// Copyright (C) 2013-2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module contains the functions related specifically to the own stack
// style of minimal model tabling.

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_tabling.h"
#include "mercury_mm_own_stacks.h"
#include "mercury_dlist.h"
#include "mercury_windows.h"                  // for Sleep()

#include <stdio.h>
#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>                         // for sleep()
#endif

#ifdef  MR_MINIMAL_MODEL_DEBUG
  // MR_MINIMAL_MODEL_DEBUG implies MR_TABLE_DEBUG in this file, since
  // if we want to debug minimal model tabling we need to enable all the
  // debugging facilities of this file. However, since MR_TABLE_DEBUG
  // increases object file sizes and link times significantly (by implying
  // MR_DEBUG_LABEL_NAMES), we don't necessarily want this implication
  // to hold globally.

  #define   MR_TABLE_DEBUG
#endif

#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS

MR_declare_entry(MR_mmos_initialize_generator);

MR_Word                 MR_mmos_arg_regs[MR_MAX_FAKE_REG];
MR_GeneratorPtr         MR_mmos_new_generator;
MR_GeneratorPtr         MR_mmos_returning_generator;

static MR_Context *     MR_get_context_for_gen(MR_GeneratorPtr generator);

#if 0
static  void            MR_table_mmos_make_gen_follow_leader(
                            MR_GeneratorPtr this_follower,
                            MR_GeneratorPtr leader);
#endif

////////////////////////////////////////////////////////////////////////////

// This part of the file maintains data structures that can be used
// to debug minimal model tabling. It does so by allowing the debugger
// to refer to tabling data structures such as subgoals and consumers
// by small, easily remembered numbers, not memory addresses.

// Set by MR_trace_event, used by table_nondet_setup.
const MR_ProcLayout     *MR_subgoal_debug_cur_proc = NULL;

struct MR_ConsDebug_Struct
{
    MR_Consumer     *MR_cod_consumer;
    int             MR_cod_sequence_num;
    int             MR_cod_version_num;
    int             MR_cod_valid;
};

struct MR_GenDebug_Struct
{
    MR_Generator    *MR_gd_generator;
    int             MR_gd_sequence_num;
    int             MR_gd_version_num;
    int             MR_gd_valid;
};

#define MR_CONS_DEBUG_INIT      10
#define MR_GEN_DEBUG_INIT       10
#define MR_NAME_BUF             1024

static  MR_ConsDebug    *MR_cons_debug_infos = NULL;
static  int             MR_cons_debug_info_next = 0;
static  int             MR_cons_debug_info_max  = 0;

static  MR_GenDebug     *MR_gen_debug_infos = NULL;
static  int             MR_gen_debug_info_next = 0;
static  int             MR_gen_debug_info_max  = 0;

void
MR_enter_cons_debug(MR_Consumer *consumer)
{
    int i;

    for (i = 0; i < MR_cons_debug_info_next; i++) {
        if (MR_cons_debug_infos[i].MR_cod_consumer == consumer) {
            MR_cons_debug_infos[i].MR_cod_version_num++;
            MR_cons_debug_infos[i].MR_cod_valid = MR_TRUE;
            return;
        }
    }

    MR_ensure_room_for_next(MR_cons_debug_info, MR_ConsDebug,
        MR_CONS_DEBUG_INIT);
    i = MR_cons_debug_info_next;
    MR_cons_debug_infos[i].MR_cod_consumer = consumer;
    MR_cons_debug_infos[i].MR_cod_sequence_num = i;
    MR_cons_debug_infos[i].MR_cod_version_num = 0;
    MR_cons_debug_infos[i].MR_cod_valid = MR_TRUE;
    MR_cons_debug_info_next++;
}

MR_ConsDebug *
MR_lookup_cons_debug_addr(MR_Consumer *consumer)
{
    int i;

    for (i = 0; i < MR_cons_debug_info_next; i++) {
        if (MR_cons_debug_infos[i].MR_cod_consumer == consumer) {
            return &MR_cons_debug_infos[i];
        }
    }

    return NULL;
}

MR_ConsDebug *
MR_lookup_cons_debug_num(int cons_index)
{
    int i;

    for (i = 0; i < MR_cons_debug_info_next; i++) {
        if (MR_cons_debug_infos[i].MR_cod_sequence_num == cons_index) {
            return &MR_cons_debug_infos[i];
        }
    }

    return NULL;
}

const char *
MR_cons_debug_name(MR_ConsDebug *cons_debug)
{
    const char  *warning;
    char        buf[MR_NAME_BUF];

    if (cons_debug == NULL) {
        return "unknown";
    }

    if (cons_debug->MR_cod_valid) {
        warning = "";
    } else {
        warning = " INVALID";
    }

    if (cons_debug->MR_cod_version_num > 0) {
        sprintf(buf, "con%d/%d (%p)%s of %s",
            cons_debug->MR_cod_sequence_num,
            cons_debug->MR_cod_version_num,
            cons_debug->MR_cod_consumer, warning,
            MR_gen_subgoal(cons_debug->MR_cod_consumer->
                MR_cons_answer_generator));
    } else {
        sprintf(buf, "con%d (%p)%s of %s",
            cons_debug->MR_cod_sequence_num,
            cons_debug->MR_cod_consumer, warning,
            MR_gen_subgoal(cons_debug->MR_cod_consumer->
                MR_cons_answer_generator));
    }

    return strdup(buf);
}

const char *
MR_cons_addr_name(MR_Consumer *consumer)
{
    MR_ConsDebug *cons_debug;

    if (consumer == NULL) {
        return "NULL";
    }

    cons_debug = MR_lookup_cons_debug_addr(consumer);
    return MR_cons_debug_name(cons_debug);
}

const char *
MR_cons_addr_short_name(MR_Consumer *consumer)
{
    MR_ConsDebug *cons_debug;
    char        buf[MR_NAME_BUF];

    if (consumer == NULL) {
        return "NULL";
    }

    cons_debug = MR_lookup_cons_debug_addr(consumer);
    sprintf(buf, "con%d", cons_debug->MR_cod_sequence_num);
    return strdup(buf);
}

const char *
MR_cons_num_name(int cons_index)
{
    MR_ConsDebug *cons_debug;

    cons_debug = MR_lookup_cons_debug_num(cons_index);
    return MR_cons_debug_name(cons_debug);
}

void
MR_enter_gen_debug(MR_Generator *generator)
{
    int i;

    for (i = 0; i < MR_gen_debug_info_next; i++) {
        if (MR_gen_debug_infos[i].MR_gd_generator == generator) {
            MR_gen_debug_infos[i].MR_gd_version_num++;
            MR_gen_debug_infos[i].MR_gd_valid = MR_TRUE;
            return;
        }
    }

    MR_ensure_room_for_next(MR_gen_debug_info, MR_GenDebug,
        MR_GEN_DEBUG_INIT);
    i = MR_gen_debug_info_next;
    MR_gen_debug_infos[i].MR_gd_generator = generator;
    MR_gen_debug_infos[i].MR_gd_sequence_num = i;
    MR_gen_debug_infos[i].MR_gd_version_num = 0;
    MR_gen_debug_infos[i].MR_gd_valid = MR_TRUE;
    MR_gen_debug_info_next++;
}

MR_GenDebug *
MR_lookup_gen_debug_addr(MR_Generator *generator)
{
    int i;

    for (i = 0; i < MR_gen_debug_info_next; i++) {
        if (MR_gen_debug_infos[i].MR_gd_generator == generator) {
            return &MR_gen_debug_infos[i];
        }
    }

    return NULL;
}

MR_GenDebug *
MR_lookup_gen_debug_num(int gen_index)
{
    int i;

    for (i = 0; i < MR_gen_debug_info_next; i++) {
        if (MR_gen_debug_infos[i].MR_gd_sequence_num == gen_index) {
            return &MR_gen_debug_infos[i];
        }
    }

    return NULL;
}

const char *
MR_gen_debug_name(MR_GenDebug *gen_debug)
{
    const char  *warning;
    char        buf[MR_NAME_BUF];

    if (gen_debug == NULL) {
        return "unknown";
    }

    if (gen_debug->MR_gd_valid) {
        warning = "";
    } else {
        warning = " INVALID";
    }

    if (gen_debug->MR_gd_version_num > 0) {
        sprintf(buf, "gen%d/%d (%p)%s for %s",
            gen_debug->MR_gd_sequence_num,
            gen_debug->MR_gd_version_num,
            gen_debug->MR_gd_generator, warning,
            MR_gen_subgoal(gen_debug->MR_gd_generator));
    } else {
        sprintf(buf, "gen%d (%p)%s for %s",
            gen_debug->MR_gd_sequence_num,
            gen_debug->MR_gd_generator, warning,
            MR_gen_subgoal(gen_debug->MR_gd_generator));
    }

    return strdup(buf);
}

const char *
MR_gen_addr_name(MR_Generator *generator)
{
    MR_GenDebug *gen_debug;

    if (generator == NULL) {
        return "NULL";
    }

    gen_debug = MR_lookup_gen_debug_addr(generator);
    return MR_gen_debug_name(gen_debug);
}

const char *
MR_gen_addr_short_name(MR_Generator *generator)
{
    MR_GenDebug *gen_debug;
    char        buf[MR_NAME_BUF];

    if (generator == NULL) {
        return "NULL";
    }

    gen_debug = MR_lookup_gen_debug_addr(generator);
    sprintf(buf, "gen%d", gen_debug->MR_gd_sequence_num);
    return strdup(buf);
}

const char *
MR_gen_subgoal(MR_Generator *generator)
{
    char        buf[MR_NAME_BUF];

    if (generator == NULL) {
        return "NULL";
    }

    // The argument registers will be meaningful only if they contain integers,
    // but that is sufficient for debugging. Likewise, debugging can be done
    // with predicates of no more than three input arguments; printing more
    // arguments would lead to excessively long event report lines in mdb.

    switch (generator->MR_gen_num_input_args) {
        case 0:
                sprintf(buf, "%s",
                    generator->MR_gen_pred_id);
                break;

        case 1:
                sprintf(buf, "%s(%" MR_INTEGER_LENGTH_MODIFIER "d)",
                    generator->MR_gen_pred_id,
                    generator->MR_gen_input_args[0]);
                break;

        case 2:
                sprintf(buf, "%s(%" MR_INTEGER_LENGTH_MODIFIER
                   "d,%" MR_INTEGER_LENGTH_MODIFIER "d)",
                    generator->MR_gen_pred_id,
                    generator->MR_gen_input_args[0],
                    generator->MR_gen_input_args[1]);
                break;

        case 3:
                sprintf(buf, "%s(%" MR_INTEGER_LENGTH_MODIFIER
                    "d,%" MR_INTEGER_LENGTH_MODIFIER
                    "d,%" MR_INTEGER_LENGTH_MODIFIER "d)",
                    generator->MR_gen_pred_id,
                    generator->MR_gen_input_args[0],
                    generator->MR_gen_input_args[1],
                    generator->MR_gen_input_args[2]);
                break;

        default:
                sprintf(buf, "%s(%" MR_INTEGER_LENGTH_MODIFIER
                    "d,%" MR_INTEGER_LENGTH_MODIFIER
                    "d,%" MR_INTEGER_LENGTH_MODIFIER
                    "d,...)",
                    generator->MR_gen_pred_id,
                    generator->MR_gen_input_args[0],
                    generator->MR_gen_input_args[1],
                    generator->MR_gen_input_args[2]);
                break;
    }

    return strdup(buf);
}

const char *
MR_gen_num_name(int gen_index)
{
    MR_GenDebug *gen_debug;

    gen_debug = MR_lookup_gen_debug_num(gen_index);
    return MR_gen_debug_name(gen_debug);
}

void
MR_print_gen_debug(FILE *fp, const MR_ProcLayout *proc,
    MR_GenDebug *gen_debug)
{
    if (gen_debug == NULL) {
        fprintf(fp, "NULL gen_debug\n");
    } else {
        MR_print_generator(fp, proc, gen_debug->MR_gd_generator);
    }
}

void
MR_print_generator(FILE *fp, const MR_ProcLayout *proc,
    MR_Generator *generator)
{
    MR_AnswerList   answer_list;
    MR_Word         *answer;
    MR_Dlist        *list;
    MR_ConsumerPtr  consumer;
    MR_GeneratorPtr follower;
    int             answer_num;

    if (generator == NULL) {
        fprintf(fp, "NULL generator\n");
        return;
    }

#ifdef  MR_TABLE_DEBUG
    if (proc == NULL && generator->MR_gen_proc_layout != NULL) {
        proc = generator->MR_gen_proc_layout;
    }
#endif

    fprintf(fp, "generator %s", MR_gen_addr_name(generator));
    if (generator->MR_gen_back_ptr == NULL) {
        fprintf(fp, ", DELETED");
    }
    fprintf(fp, "\n");

    if (proc != NULL) {
        fprintf(fp, "proc: ");
        MR_print_proc_id(fp, proc);
        fprintf(fp, "\n");
    }

    if (generator->MR_gen_is_complete) {
        fprintf(fp, "complete\n");
    } else {
        fprintf(fp, "not complete\n");
    }

    if (generator->MR_gen_leader == generator) {
        fprintf(fp, "own leader\n");
    } else {
        fprintf(fp, "leader: %s\n",
            MR_gen_addr_name(generator->MR_gen_leader));
    }

    if (MR_dlist_maybe_null_length(generator->MR_gen_led_generators) > 0) {
        fprintf(fp, "followers:\n");
        MR_for_dlist (list, generator->MR_gen_led_generators) {
            follower = (MR_GeneratorPtr) MR_dlist_data(list);
            fprintf(fp, "\t%s\n", MR_gen_addr_name(follower));
        }
    }

    fprintf(fp, "\n");
    fprintf(fp, "consumers:\n");
    MR_for_dlist (list, generator->MR_gen_consumers) {
        consumer = (MR_ConsumerPtr) MR_dlist_data(list);

        fprintf(fp, "\t%s", MR_cons_addr_name(consumer));

        if (consumer->MR_cons_containing_generator != NULL) {
            fprintf(fp, " in %s",
                MR_gen_addr_name(consumer->MR_cons_containing_generator));
        } else {
            fprintf(fp, " in main context");
        }

        fprintf(fp, " (returned %" MR_INTEGER_LENGTH_MODIFIER "d)\n",
            consumer->MR_cons_num_returned_answers);
    }

    fprintf(fp, "\n");
    fprintf(fp, "answers: %" MR_INTEGER_LENGTH_MODIFIER "d\n",
        generator->MR_gen_num_answers);

    if (proc != NULL) {
        answer_list = generator->MR_gen_answer_list;
        answer_num = 0;
        while (answer_list != NULL) {
            fprintf(fp, "answer #%d: <", answer_num);
            MR_print_answerblock(fp, proc, answer_list->MR_aln_answer_block);
            fprintf(fp, ">\n");
            answer_list = answer_list->MR_aln_next_answer;
            answer_num++;
        }
    }
}

void
MR_print_cons_debug(FILE *fp, const MR_ProcLayout *proc,
    MR_ConsDebug *cons_debug)
{
    if (cons_debug == NULL) {
        fprintf(fp, "NULL cons_debug\n");
    } else {
        MR_print_consumer(fp, proc, cons_debug->MR_cod_consumer);
    }
}

void
MR_print_consumer(FILE *fp, const MR_ProcLayout *proc, MR_Consumer *consumer)
{
    if (consumer == NULL) {
        fprintf(fp, "NULL consumer\n");
        return;
    }

    fprintf(fp, "consumer %s", MR_cons_addr_name(consumer));

    // XXX check semantics of DELETED
    if (consumer->MR_cons_answer_generator == NULL) {
        fprintf(fp, ", DELETED\n");
    } else {
        fprintf(fp, ", answer generator %s",
            MR_gen_addr_name(consumer->MR_cons_answer_generator));
        if (consumer->MR_cons_containing_generator != NULL) {
            fprintf(fp, ", in generator %s\n",
                MR_gen_addr_name(consumer->MR_cons_answer_generator));
        } else {
            fprintf(fp, ", in main context\n\n");
        }
        fprintf(fp, "returned answers %" MR_INTEGER_LENGTH_MODIFIER "d,",
            consumer->MR_cons_num_returned_answers);
        fprintf(fp, " remaining answers ptr %p\n",
            consumer->MR_cons_remaining_answer_list_ptr);
    }
}

void
MR_mm_own_stacks_report_stats(FILE *fp)
{
}

////////////////////////////////////////////////////////////////////////////

static  int MR_next_gen_context = 1;

static MR_Context *
MR_get_context_for_gen(MR_GeneratorPtr generator)
{
    MR_Dlist        *list;
    MR_Dlist        *item;
    MR_Context      *ctxt;

    list = MR_ENGINE(MR_eng_free_contexts);
    if (MR_dlist_length(list) > 0) {
        item = MR_dlist_first_ptr(list);
        ctxt = (MR_Context *) MR_dlist_data(item);
        MR_dlist_delete(list, item, NULL);
    } else {
        char    buf[80];    // ought to be plenty big enough

        sprintf(buf, "gen%d", MR_next_gen_context);
        MR_next_gen_context++;
        ctxt = MR_create_context(strdup(buf), MR_CONTEXT_SIZE_SMALL,
            generator);
        MR_copy_eng_this_context_fields(ctxt, MR_ENGINE(MR_eng_this_context));
        ctxt->MR_ctxt_next = NULL;
    }

    ctxt->MR_ctxt_owner_generator = generator;
    return ctxt;
}

MR_ConsumerPtr
MR_table_mmos_setup_consumer(MR_GeneratorPtr generator, MR_ConstString pred_id)
{
    MR_ConsumerPtr  consumer;
    MR_GeneratorPtr containing_generator;

    consumer = MR_TABLE_NEW(MR_Consumer);
    consumer->MR_cons_pred_id = pred_id;
    consumer->MR_cons_answer_generator = generator;
    containing_generator =
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_owner_generator;
    consumer->MR_cons_containing_generator = containing_generator;
    if (containing_generator != NULL) {
        consumer->MR_cons_context = containing_generator->MR_gen_context;

        // XXX We need to do something here to make `containing_generator'
        // depend on `generator'.
        //
        // If the dependency is mutual, we need to make them part of the
        // same clique. We may delay this latter action until there is a
        // solution to return (since by then there may be more dependencies),
        // but doing it here may be better, since there may be many solutions.

    } else {
        consumer->MR_cons_context = MR_ENGINE(MR_eng_main_context);
    }
    consumer->MR_cons_registered = MR_FALSE;
    consumer->MR_cons_num_returned_answers = 0;
    consumer->MR_cons_remaining_answer_list_ptr =
        &generator->MR_gen_answer_list;

#ifdef  MR_TABLE_DEBUG
    MR_enter_cons_debug(consumer);

    if (MR_tabledebug) {
        // The pred_id and address are printed as part of the consumer name.
        printf("setting up consumer %s\n",
            MR_cons_addr_name(consumer));
        printf("the relevant generator is %s\n", MR_gen_addr_name(generator));
    }
#endif

    return consumer;
}

MR_GeneratorPtr
MR_table_mmos_setup_generator(MR_TrieNode trie_node, MR_Integer num_input_args,
    MR_Word generator_pred, MR_ConstString pred_id)
{
    MR_GeneratorPtr generator;
    MR_Context      *context;
    int             i;

    generator = MR_TABLE_NEW(MR_Generator);
    context = MR_get_context_for_gen(generator);
    context->MR_ctxt_resume = MR_ENTRY(MR_mmos_initialize_generator);
    generator->MR_gen_closure = generator_pred;

    generator->MR_gen_back_ptr = trie_node;
    generator->MR_gen_context = context;

    generator->MR_gen_leader = generator;
    generator->MR_gen_led_generators = MR_dlist_makelist(generator);
    generator->MR_gen_consumers = MR_dlist_makelist0();
    generator->MR_gen_num_answers = 0;
    generator->MR_gen_answer_table.MR_integer = 0;
    generator->MR_gen_answer_list = NULL;
    generator->MR_gen_answer_list_tail = &generator->MR_gen_answer_list;

    // The following fields are for debugging
    generator->MR_gen_pred_id = pred_id;
    generator->MR_gen_num_input_args = num_input_args;
    generator->MR_gen_input_args = MR_TABLE_NEW_ARRAY(MR_Word, num_input_args);

    for (i = 0; i < num_input_args; i++) {
        generator->MR_gen_input_args[i] = MR_mmos_arg_regs[i];
    }

    // MR_subgoal_debug_cur_proc refers to the last procedure
    // that executed a call event, if any. If the procedure that is
    // executing table_mmos_setup_consumer is traced, this will be that
    // procedure, and recording the layout structure of the
    // processor in the generator allows us to interpret the contents
    // of the generator's answer tables. If the procedure executing
    // table_mmos_setup_consumer is not traced, then the layout structure
    // belongs to another procedure and any use of the MR_gen_proc_layout
    // field will probably cause a core dump.
    // For implementors debugging minimal model tabling, this is
    // the right tradeoff.

    generator->MR_gen_proc_layout = MR_subgoal_debug_cur_proc;

#ifdef  MR_TABLE_DEBUG
    MR_enter_gen_debug(generator);

    if (MR_tabledebug) {
        // The pred_id is printed as part of the generator name.
        printf("setting up generator %p -> %s\n",
            trie_node, MR_gen_addr_name(generator));
        printf("answer slot %p\n", generator->MR_gen_answer_list_tail);
    }

    if (MR_maxfr != MR_curfr) {
        MR_fatal_error("MR_maxfr != MR_curfr at table setup\n");
    }
#endif

    trie_node->MR_generator = generator;
    // MR_save_transient_registers();
    return generator;
}

#endif  // MR_USE_MINIMAL_MODEL_OWN_STACKS

#ifdef MR_HIGHLEVEL_CODE

void MR_CALL
mercury__table_builtin__table_mmos_consume_next_answer_nondet_2_p_0(
    MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
    MR_Cont cont, void *cont_env_ptr)
{
    MR_fatal_error("sorry, not implemented: "
        "minimal model tabling with --high-level-code");
}

void MR_CALL
mercury__table_builtin__table_mmos_consume_next_answer_multi_2_p_0(
    MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
    MR_Cont cont, void *cont_env_ptr)
{
    MR_fatal_error("sorry, not implemented: "
        "minimal model tabling with --high-level-code");
}

#else   // ! MR_HIGHLEVEL_CODE

MR_define_extern_entry(MR_MMOS_RET_ALL_NONDET_ENTRY);
MR_define_extern_entry(MR_MMOS_RET_ALL_MULTI_ENTRY);

MR_EXTERN_USER_PROC_ID_PROC_LAYOUT(MR_DETISM_NON, 0, -1,
    MR_PREDICATE, table_builtin, table_mmos_consume_next_answer_nondet, 2, 0);
MR_EXTERN_USER_PROC_ID_PROC_LAYOUT(MR_DETISM_NON, 0, -1,
    MR_PREDICATE, table_builtin, table_mmos_consume_next_answer_multi, 2, 0);

#ifndef  MR_USE_MINIMAL_MODEL_OWN_STACKS

MR_BEGIN_MODULE(mmos_module)
    MR_init_entry_sl(MR_MMOS_RET_ALL_NONDET_ENTRY);
    MR_init_entry_sl(MR_MMOS_RET_ALL_MULTI_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_MMOS_RET_ALL_NONDET_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_MMOS_RET_ALL_MULTI_ENTRY);
MR_BEGIN_CODE

MR_define_entry(MR_MMOS_RET_ALL_NONDET_ENTRY);
    MR_fatal_error("call to table_mmos_consume_next_answer_nondet/2 in a "
    "grade without own stack minimal model tabling");

MR_define_entry(MR_MMOS_RET_ALL_MULTI_ENTRY);
    MR_fatal_error("call to table_mmos_consume_next_answer_multi/2 in a "
    "grade without own stack minimal model tabling");

MR_END_MODULE

#else   // MR_USE_MINIMAL_MODEL_OWN_STACKS

MR_declare_entry(mercury__do_call_closure_0);

#define MR_RETURN_ALL_LABEL(name) \
    MR_label_name(MR_MMOS_RET_ALL_NONDET_ENTRY, name)

MR_declare_label(MR_RETURN_ALL_LABEL(FromTable));

MR_BEGIN_MODULE(mmos_module)
    MR_init_entry_sl(MR_MMOS_RET_ALL_NONDET_ENTRY);
    MR_init_entry_sl(MR_MMOS_RET_ALL_MULTI_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_MMOS_RET_ALL_NONDET_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_MMOS_RET_ALL_MULTI_ENTRY);

    MR_init_entry(MR_mmos_initialize_generator);

    MR_init_label_an(MR_RETURN_ALL_LABEL(FromTable));
MR_BEGIN_CODE

MR_define_entry(MR_MMOS_RET_ALL_NONDET_ENTRY);
{
    MR_GeneratorPtr generator;
    MR_ConsumerPtr  consumer;

    consumer = (MR_ConsumerPtr) MR_r1;
    generator = consumer->MR_cons_answer_generator;

    MR_mkframe("pred table_mmos_consume_next_answer_nondet/2-0", 2,
        MR_LABEL(MR_RETURN_ALL_LABEL(FromTable)));
    MR_fv(1) = (MR_Word) consumer;
    MR_fv(2) = (MR_Word) generator;

    // fall through
}

MR_define_label(MR_RETURN_ALL_LABEL(FromTable));
{
    MR_ConsumerPtr  consumer;
    MR_GeneratorPtr generator;
    MR_AnswerList   answer_list;

    // We could and should delay the assignment to generator until we know
    // whether answer_list == NULL, but we need the value of generator
    // for the debugging code below.

    consumer = (MR_ConsumerPtr) MR_fv(1);
    generator = (MR_GeneratorPtr) MR_fv(2);

    answer_list = *(consumer->MR_cons_remaining_answer_list_ptr);
    if (answer_list != NULL) {
        if (consumer->MR_cons_num_returned_answers
            >= generator->MR_gen_num_answers)
        {
            MR_fatal_error("table_mmos_consume_next_answer_nondet: "
                "answer list says solution, numbers say no solution");
        }

        if (MR_tabledebug) {
            printf("consumer %s returning answer #%"
                MR_INTEGER_LENGTH_MODIFIER "d from table\n",
                MR_cons_addr_name(consumer),
                consumer->MR_cons_num_returned_answers);
        }

        consumer->MR_cons_num_returned_answers++;

        MR_r1 = (MR_Word) answer_list->MR_aln_answer_block;
        consumer->MR_cons_remaining_answer_list_ptr =
            &answer_list->MR_aln_next_answer;
        MR_succeed();
    } else {
        if (generator->MR_gen_is_complete) {
            if (MR_tabledebug) {
                printf("consumer %s failing; generator %s complete\n",
                    MR_cons_addr_name(consumer),
                    MR_gen_addr_name(generator));
            }

            MR_fail();
        } else {
            // XXX Investigate the performance effects of adding at tail.
            if (!consumer->MR_cons_registered) {
                if (MR_tabledebug) {
                    printf("registering %s with %s\n",
                        MR_cons_addr_name(consumer),
                        MR_gen_addr_name(generator));
                }

                generator->MR_gen_consumers =
                    MR_dlist_addhead(generator->MR_gen_consumers, consumer);
                consumer->MR_cons_registered = MR_TRUE;
            }

            if (MR_tabledebug) {
                printf("consumer %s needs #%"
                    MR_INTEGER_LENGTH_MODIFIER "d from %s\n",
                    MR_cons_addr_name(consumer),
                    consumer->MR_cons_num_returned_answers,
                    MR_gen_addr_name(generator));

                // The sleep is to allow the infinite loop that happens here
                // to be interrupted from the keyboard.
                #if defined(MR_HAVE_SLEEP)
                    sleep(1);
                #elif defined(MR_HAVE_CAPITAL_S_SLEEP)
                    Sleep(1000);
                #endif
            }

            MR_save_context(consumer->MR_cons_context);
            consumer->MR_cons_context->MR_ctxt_resume =
                MR_LABEL(MR_RETURN_ALL_LABEL(FromTable));
            MR_load_context(generator->MR_gen_context);
            MR_ENGINE(MR_eng_this_context) = generator->MR_gen_context;
            MR_GOTO(generator->MR_gen_context->MR_ctxt_resume);
        }
    }
}

MR_define_entry(MR_MMOS_RET_ALL_MULTI_ENTRY);
    // Although the two predicates differ in their determinism,
    // their implementation is the same.

    MR_GOTO_ENTRY(MR_MMOS_RET_ALL_NONDET_ENTRY);

MR_define_entry(MR_mmos_initialize_generator);
{
    MR_Context      *context;
    MR_GeneratorPtr generator;

    context = MR_ENGINE(MR_eng_this_context);
    generator = context->MR_ctxt_owner_generator;
    assert(generator != NULL);
    MR_r1 = generator->MR_gen_closure;
    MR_GOTO_ENTRY(mercury__do_call_closure_0);
}

MR_END_MODULE

#endif  // MR_USE_MINIMAL_MODEL_OWN_STACKS
#endif  // MR_HIGHLEVEL_CODE

// Ensure that the initialization code for the above module gets to run.
/*
INIT mercury_sys_init_mmos_modules
*/

#ifndef MR_HIGHLEVEL_CODE
MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc mmos_module;
#endif

// Forward declarations to suppress gcc -Wmissing-decl warnings.
void mercury_sys_init_mmos_modules_init(void);
void mercury_sys_init_mmos_modules_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_mmos_modules_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_mmos_modules_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    mmos_module();
#endif  // MR_HIGHLEVEL_CODE
}

void mercury_sys_init_mmos_modules_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_mmos_modules_write_out_proc_statics(FILE *fp)
{
    // No proc_statics to write out.
    // XXX We need to fix the deep profiling
    // of minimal model tabled predicates.
}
#endif
