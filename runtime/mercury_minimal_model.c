/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2003-2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module contains the functions related specifically to minimal model
** tabling.
*/

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_tabling.h"
#include "mercury_minimal_model.h"

#include <stdio.h>

#ifdef  MR_USE_MINIMAL_MODEL

static  MR_Word *nearest_common_ancestor(MR_Word *fr1, MR_Word *fr2);
static  void    save_state(MR_SavedState *saved_state, MR_Word *generator_fr,
                    const char *who, const char *what,
                    const MR_Label_Layout *top_layout);
static  void    restore_state(MR_SavedState *saved_state, const char *who,
                    const char *what);
static  void    extend_consumer_stacks(MR_Subgoal *leader,
                    MR_Consumer *suspension);
static  void    make_subgoal_follow_leader(MR_Subgoal *this_follower,
                    MR_Subgoal *leader);
static  void    print_saved_state(FILE *fp, MR_SavedState *saved_state);
static  void    print_stack_segment(FILE *fp, MR_Word *segment,
                    MR_Integer size);

/*---------------------------------------------------------------------------*/

/*
** This part of the file maintains data structures that can be used
** to debug minimal model tabling. It does so by allowing the debugger
** to refer to tabling data structures such as subgoals and consumers
** by small, easily remembered numbers, not memory addresses.
*/

/* set by MR_trace_event, used by table_nondet_setup */
const MR_Proc_Layout          *MR_subgoal_debug_cur_proc = NULL;

struct MR_ConsumerDebug_Struct
{
    MR_Consumer *MR_cod_consumer;
    int         MR_cod_sequence_num;
    int         MR_cod_version_num;
    int         MR_cod_valid;
};

struct MR_SubgoalDebug_Struct
{
    MR_Subgoal  *MR_sgd_subgoal;
    int         MR_sgd_sequence_num;
    int         MR_sgd_version_num;
    int         MR_sgd_valid;
};

#define MR_CONSUMER_DEBUG_INIT  10
#define MR_SUBGOAL_DEBUG_INIT   10
#define MR_NAME_BUF             1024

static  MR_ConsumerDebug *MR_consumer_debug_infos = NULL;
static  int             MR_consumer_debug_info_next = 0;
static  int             MR_consumer_debug_info_max  = 0;

static  MR_SubgoalDebug *MR_subgoal_debug_infos = NULL;
static  int             MR_subgoal_debug_info_next = 0;
static  int             MR_subgoal_debug_info_max  = 0;

void
MR_enter_consumer_debug(MR_Consumer *consumer)
{
    int i;

    for (i = 0; i < MR_consumer_debug_info_next; i++) {
        if (MR_consumer_debug_infos[i].MR_cod_consumer == consumer) {
            MR_consumer_debug_infos[i].MR_cod_version_num++;
            MR_consumer_debug_infos[i].MR_cod_valid = MR_TRUE;
            return;
        }
    }

    MR_ensure_room_for_next(MR_consumer_debug_info, MR_ConsumerDebug,
        MR_CONSUMER_DEBUG_INIT);
    i = MR_consumer_debug_info_next;
    MR_consumer_debug_infos[i].MR_cod_consumer = consumer;
    MR_consumer_debug_infos[i].MR_cod_sequence_num = i;
    MR_consumer_debug_infos[i].MR_cod_version_num = 0;
    MR_consumer_debug_infos[i].MR_cod_valid = MR_TRUE;
    MR_consumer_debug_info_next++;
}

MR_ConsumerDebug *
MR_lookup_consumer_debug_addr(MR_Consumer *consumer)
{
    int i;

    for (i = 0; i < MR_consumer_debug_info_next; i++) {
        if (MR_consumer_debug_infos[i].MR_cod_consumer == consumer) {
            return &MR_consumer_debug_infos[i];
        }
    }

    return NULL;
}

MR_ConsumerDebug *
MR_lookup_consumer_debug_num(int consumer_index)
{
    int i;

    for (i = 0; i < MR_consumer_debug_info_next; i++) {
        if (MR_consumer_debug_infos[i].MR_cod_sequence_num == consumer_index) {
            return &MR_consumer_debug_infos[i];
        }
    }

    return NULL;
}

const char *
MR_consumer_debug_name(MR_ConsumerDebug *consumer_debug)
{
    const char  *warning;
    char        buf[MR_NAME_BUF];

    if (consumer_debug == NULL) {
        return "unknown";
    }

    if (consumer_debug->MR_cod_valid) {
        warning = "";
    } else {
        warning = " INVALID";
    }

    if (consumer_debug->MR_cod_version_num > 0) {
        sprintf(buf, "con %d/%d (%p)%s", consumer_debug->MR_cod_sequence_num,
            consumer_debug->MR_cod_version_num,
            consumer_debug->MR_cod_consumer, warning);
    } else {
        sprintf(buf, "con %d (%p)%s", consumer_debug->MR_cod_sequence_num,
            consumer_debug->MR_cod_consumer, warning);
    }

    return strdup(buf);
}

const char *
MR_consumer_addr_name(MR_Consumer *consumer)
{
    MR_ConsumerDebug *consumer_debug;

    if (consumer == NULL) {
        return "NULL";
    }

    consumer_debug = MR_lookup_consumer_debug_addr(consumer);
    return MR_consumer_debug_name(consumer_debug);
}

const char *
MR_consumer_num_name(int consumer_index)
{
    MR_ConsumerDebug *consumer_debug;

    consumer_debug = MR_lookup_consumer_debug_num(consumer_index);
    return MR_consumer_debug_name(consumer_debug);
}

void
MR_enter_subgoal_debug(MR_Subgoal *subgoal)
{
    int i;

    for (i = 0; i < MR_subgoal_debug_info_next; i++) {
        if (MR_subgoal_debug_infos[i].MR_sgd_subgoal == subgoal) {
            MR_subgoal_debug_infos[i].MR_sgd_version_num++;
            MR_subgoal_debug_infos[i].MR_sgd_valid = MR_TRUE;
            return;
        }
    }

    MR_ensure_room_for_next(MR_subgoal_debug_info, MR_SubgoalDebug,
        MR_SUBGOAL_DEBUG_INIT);
    i = MR_subgoal_debug_info_next;
    MR_subgoal_debug_infos[i].MR_sgd_subgoal = subgoal;
    MR_subgoal_debug_infos[i].MR_sgd_sequence_num = i;
    MR_subgoal_debug_infos[i].MR_sgd_version_num = 0;
    MR_subgoal_debug_infos[i].MR_sgd_valid = MR_TRUE;
    MR_subgoal_debug_info_next++;
}

MR_SubgoalDebug *
MR_lookup_subgoal_debug_addr(MR_Subgoal *subgoal)
{
    int i;

    for (i = 0; i < MR_subgoal_debug_info_next; i++) {
        if (MR_subgoal_debug_infos[i].MR_sgd_subgoal == subgoal) {
            return &MR_subgoal_debug_infos[i];
        }
    }

    return NULL;
}

MR_SubgoalDebug *
MR_lookup_subgoal_debug_num(int subgoal_index)
{
    int i;

    for (i = 0; i < MR_subgoal_debug_info_next; i++) {
        if (MR_subgoal_debug_infos[i].MR_sgd_sequence_num == subgoal_index) {
            return &MR_subgoal_debug_infos[i];
        }
    }

    return NULL;
}

const char *
MR_subgoal_debug_name(MR_SubgoalDebug *subgoal_debug)
{
    const char  *warning;
    char        buf[MR_NAME_BUF];

    if (subgoal_debug == NULL) {
        return "unknown";
    }

    if (subgoal_debug->MR_sgd_valid) {
        warning = "";
    } else {
        warning = " INVALID";
    }

    if (subgoal_debug->MR_sgd_version_num > 0) {
        sprintf(buf, "sub %d/%d (%p)%s", subgoal_debug->MR_sgd_sequence_num,
            subgoal_debug->MR_sgd_version_num,
            subgoal_debug->MR_sgd_subgoal, warning);
    } else {
        sprintf(buf, "sub %d (%p)%s", subgoal_debug->MR_sgd_sequence_num,
            subgoal_debug->MR_sgd_subgoal, warning);
    }

    return strdup(buf);
}

const char *
MR_subgoal_addr_name(MR_Subgoal *subgoal)
{
    MR_SubgoalDebug *subgoal_debug;

    if (subgoal == NULL) {
        return "NULL";
    }

    subgoal_debug = MR_lookup_subgoal_debug_addr(subgoal);
    return MR_subgoal_debug_name(subgoal_debug);
}

const char *
MR_subgoal_num_name(int subgoal_index)
{
    MR_SubgoalDebug *subgoal_debug;

    subgoal_debug = MR_lookup_subgoal_debug_num(subgoal_index);
    return MR_subgoal_debug_name(subgoal_debug);
}

const char *
MR_subgoal_status(MR_SubgoalStatus status)
{
    switch (status) {
        case MR_SUBGOAL_INACTIVE:
            return "INACTIVE";

        case MR_SUBGOAL_ACTIVE:
            return "ACTIVE";

        case MR_SUBGOAL_COMPLETE:
            return "COMPLETE";
    }

    return "INVALID";
}

void
MR_print_subgoal_debug(FILE *fp, const MR_Proc_Layout *proc,
    MR_SubgoalDebug *subgoal_debug)
{
    if (subgoal_debug == NULL) {
        fprintf(fp, "NULL subgoal_debug\n");
    } else {
        MR_print_subgoal(fp, proc, subgoal_debug->MR_sgd_subgoal);
    }
}

void
MR_print_subgoal(FILE *fp, const MR_Proc_Layout *proc, MR_Subgoal *subgoal)
{
    MR_SubgoalList  follower;
    MR_ConsumerList consumer;
    MR_AnswerList   answer_list;
    MR_Word         *answer;

    if (subgoal == NULL) {
        fprintf(fp, "NULL subgoal\n");
        return;
    }

#ifdef  MR_TABLE_DEBUG
    if (proc == NULL && subgoal->MR_sg_proc_layout != NULL) {
        proc = subgoal->MR_sg_proc_layout;
    }
#endif

    fprintf(fp, "subgoal %s: status %s, generator frame ",
        MR_subgoal_addr_name(subgoal),
        MR_subgoal_status(subgoal->MR_sg_status));
    MR_print_nondstackptr(fp, subgoal->MR_sg_generator_fr);
    fprintf(fp, "\n");

    if (proc != NULL) {
        fprintf(fp, "proc: ");
        MR_print_proc_id(fp, proc);
        fprintf(fp, "\n");
    }

    fprintf(fp, "leader: %s, ",
        MR_subgoal_addr_name(subgoal->MR_sg_leader));
    fprintf(fp, "followers:");
    for (follower = subgoal->MR_sg_followers;
        follower != NULL; follower = follower->MR_sl_next)
    {
        fprintf(fp, " %s", MR_subgoal_addr_name(follower->MR_sl_item));
    }

    fprintf(fp, "\nconsumers:");
    for (consumer = subgoal->MR_sg_consumer_list;
        consumer != NULL; consumer = consumer->MR_cl_next)
    {
        fprintf(fp, " %s", MR_consumer_addr_name(consumer->MR_cl_item));
    }

    fprintf(fp, "\n");
    fprintf(fp, "answers: %d, committed: %d\n",
        subgoal->MR_sg_num_ans,
        subgoal->MR_sg_num_committed_ans);

    if (proc != NULL) {
        answer_list = subgoal->MR_sg_answer_list;
        while (answer_list != NULL) {
            fprintf(fp, "answer #%d: <", answer_list->MR_aln_answer_num);
            MR_print_answerblock(fp, proc,
                answer_list->MR_aln_answer_data.MR_answerblock);
            fprintf(fp, ">\n");
            answer_list = answer_list->MR_aln_next_answer;
        }
    }
}

void
MR_print_consumer_debug(FILE *fp, const MR_Proc_Layout *proc,
    MR_ConsumerDebug *consumer_debug)
{
    if (consumer_debug == NULL) {
        fprintf(fp, "NULL consumer_debug\n");
    } else {
        MR_print_consumer(fp, proc, consumer_debug->MR_cod_consumer);
    }
}

void
MR_print_consumer(FILE *fp, const MR_Proc_Layout *proc, MR_Consumer *consumer)
{
    if (consumer == NULL) {
        fprintf(fp, "NULL consumer\n");
        return;
    }

    fprintf(fp, "consumer %s", MR_consumer_addr_name(consumer));

#ifdef  MR_TABLE_DEBUG
    fprintf(fp, ", of subgoal %s",
        MR_subgoal_addr_name(consumer->MR_cns_subgoal));
#endif

    fprintf(fp, ", remaining answers %p\n",
        consumer->MR_cns_remaining_answer_list_ptr);
    print_saved_state(fp, &consumer->MR_cns_saved_state);
}

/*---------------------------------------------------------------------------*/

/*
** This part of the file provides the utility functions needed for
** suspensions and resumptions of derivations.
*/

#define SUSPEND_LABEL(name) MR_PASTE3(MR_SUSPEND_ENTRY, _, name)
#define RESUME_LABEL(name)  MR_PASTE3(MR_RESUME_ENTRY, _, name)

/*
** Given pointers to two ordinary frames on the nondet stack, return the
** address of the stack frame of their nearest common ancestor on that stack.
*/

static MR_Word *
nearest_common_ancestor(MR_Word *fr1, MR_Word *fr2)
{
    while (fr1 != fr2) {
  #ifdef MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("common ancestor search: ");
            MR_printnondstackptr(fr1);
            printf(" vs ");
            MR_printnondstackptr(fr2);
            printf("\n");
        }
  #endif

        if (fr1 > fr2) {
            fr1 = MR_succfr_slot(fr1);
        } else {
            fr2 = MR_succfr_slot(fr2);
        }
    }

  #ifdef MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("the common ancestor is ");
        MR_printnondstackptr(fr1);
        printf("\n");
    }
  #endif

    return fr1;
}

/*
** Save the current state of the Mercury abstract machine, so that the
** current computation may be suspended for a while, and restored later.
** The generator_fr argument gives the point from which we need to copy the
** nondet and (indirectly) the det stacks. The parts of those stacks below
** the given points will not change between the suspension and the resumption
** of this state, or if they do, the stack segments in the saved state
** will be extended (via extend_consumer_stacks).
*/

static void
save_state(MR_SavedState *saved_state, MR_Word *generator_fr,
    const char *who, const char *what, const MR_Label_Layout *top_layout)
{
    MR_Word *common_ancestor_fr;
    MR_Word *start_non;
    MR_Word *start_det;

    MR_restore_transient_registers();

    if (MR_not_nearest_flag) {
        /*
        ** This can yield incorrect results, as documented in mday_sld.tex
        ** in papers/tabling2. It is included here only to allow demonstrations
        ** of *why* this is incorrect.
        */
        common_ancestor_fr = generator_fr;
    } else {
        common_ancestor_fr = nearest_common_ancestor(MR_curfr, generator_fr);
    }
    start_non = MR_prevfr_slot(common_ancestor_fr) + 1;
    start_det = MR_table_detfr_slot(common_ancestor_fr) + 1;

    saved_state->MR_ss_succ_ip = MR_succip;
    saved_state->MR_ss_s_p = MR_sp;
    saved_state->MR_ss_cur_fr = MR_curfr;
    saved_state->MR_ss_max_fr = MR_maxfr;
    saved_state->MR_ss_common_ancestor_fr = common_ancestor_fr;
    saved_state->MR_ss_top_layout = top_layout;

    /* we copy from start_non to MR_maxfr, both inclusive */
    saved_state->MR_ss_non_stack_real_start = start_non;
    if (MR_maxfr >= start_non) {
        saved_state->MR_ss_non_stack_block_size = MR_maxfr + 1 - start_non;
        saved_state->MR_ss_non_stack_saved_block = MR_table_allocate_words(
            saved_state->MR_ss_non_stack_block_size);
        MR_table_copy_words(saved_state->MR_ss_non_stack_saved_block,
            saved_state->MR_ss_non_stack_real_start,
            saved_state->MR_ss_non_stack_block_size);
    } else {
        saved_state->MR_ss_non_stack_block_size = 0;
        saved_state->MR_ss_non_stack_saved_block = NULL;
    }

    /* we copy from start_det to MR_sp, both inclusive */
    saved_state->MR_ss_det_stack_real_start = start_det;
    if (MR_sp >= start_det) {
        saved_state->MR_ss_det_stack_block_size = MR_sp + 1 - start_det;
        saved_state->MR_ss_det_stack_saved_block = MR_table_allocate_words(
            saved_state->MR_ss_det_stack_block_size);
        MR_table_copy_words(saved_state->MR_ss_det_stack_saved_block,
            saved_state->MR_ss_det_stack_real_start,
            saved_state->MR_ss_det_stack_block_size);
    } else {
        saved_state->MR_ss_det_stack_block_size = 0;
        saved_state->MR_ss_det_stack_saved_block = NULL;
    }

    saved_state->MR_ss_gen_next = MR_gen_next;
    saved_state->MR_ss_gen_stack_saved_block = MR_table_allocate_structs(
        saved_state->MR_ss_gen_next, MR_GenStackFrame);
    MR_table_copy_structs(saved_state->MR_ss_gen_stack_saved_block,
        MR_gen_stack, saved_state->MR_ss_gen_next, MR_GenStackFrame);

    saved_state->MR_ss_cut_next = MR_cut_next;
    saved_state->MR_ss_cut_stack_saved_block = MR_table_allocate_structs(
        MR_cut_next, MR_CutStackFrame);
    MR_table_copy_structs(saved_state->MR_ss_cut_stack_saved_block,
        MR_cut_stack, saved_state->MR_ss_cut_next, MR_CutStackFrame);

    saved_state->MR_ss_pneg_next = MR_pneg_next;
    saved_state->MR_ss_pneg_stack_saved_block = MR_table_allocate_structs(
        MR_pneg_next, MR_PNegStackFrame);
    MR_table_copy_structs(saved_state->MR_ss_pneg_stack_saved_block,
        MR_pneg_stack, saved_state->MR_ss_pneg_next, MR_PNegStackFrame);

  #ifdef MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("\n%s saves %s stacks\n", who, what);
        print_saved_state(stdout, saved_state);
    }

    if (MR_tablestackdebug) {
        MR_dump_nondet_stack_from_layout(stdout, start_non, 0, MR_maxfr,
            top_layout, MR_sp, MR_curfr);
    }
  #endif /* MR_TABLE_DEBUG */

    MR_save_transient_registers();
}

/*
** Restore the state of the Mercury abstract machine from saved_state.
*/

static void
restore_state(MR_SavedState *saved_state, const char *who, const char *what)
{
    MR_restore_transient_registers();

    MR_succip = saved_state->MR_ss_succ_ip;
    MR_sp = saved_state->MR_ss_s_p;
    MR_curfr = saved_state->MR_ss_cur_fr;
    MR_maxfr = saved_state->MR_ss_max_fr;

    MR_table_copy_words(saved_state->MR_ss_non_stack_real_start,
        saved_state->MR_ss_non_stack_saved_block,
        saved_state->MR_ss_non_stack_block_size);

    MR_table_copy_words(saved_state->MR_ss_det_stack_real_start,
        saved_state->MR_ss_det_stack_saved_block,
        saved_state->MR_ss_det_stack_block_size);

    MR_gen_next = saved_state->MR_ss_gen_next;
    MR_table_copy_structs(MR_gen_stack,
        saved_state->MR_ss_gen_stack_saved_block,
        saved_state->MR_ss_gen_next, MR_GenStackFrame);

    MR_cut_next = saved_state->MR_ss_cut_next;
    MR_table_copy_structs(MR_cut_stack,
        saved_state->MR_ss_cut_stack_saved_block,
        saved_state->MR_ss_cut_next, MR_CutStackFrame);

    MR_pneg_next = saved_state->MR_ss_pneg_next;
    MR_table_copy_structs(MR_pneg_stack,
        saved_state->MR_ss_pneg_stack_saved_block,
        saved_state->MR_ss_pneg_next, MR_PNegStackFrame);

  #ifdef MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("\n%s restores %s stacks\n", who, what);
        print_saved_state(stdout, saved_state);
    }

    if (MR_tablestackdebug) {
        MR_dump_nondet_stack_from_layout(stdout,
            saved_state->MR_ss_non_stack_real_start, 0, MR_maxfr,
            saved_state->MR_ss_top_layout, MR_sp, MR_curfr);
    }
  #endif /* MR_TABLE_DEBUG */

    MR_save_transient_registers();
}

/*
** The saved state of a consumer for a subgoal (say subgoal A) includes
** the stack segments between the tops of the stack at the time that
** A's generator was entered and the time that A's consumer was entered.
** When A becomes a follower of another subgoal B, the responsibility for
** scheduling A's consumers passes to B's generator. Since by definition
** B's nondet stack frame is lower in the stack than A's generator's,
** nca(consumer, B) will in general be lower than nca(consumer, A)
** (where nca = nearest common ancestor). The consumer's saved state
** goes down to nca(consumer, A); we need to extend it to nca(consumer, B).
*/

static void
extend_consumer_stacks(MR_Subgoal *leader, MR_Consumer *consumer)
{
    MR_Word         *arena_block;
    MR_Word         *arena_start;
    MR_Word         arena_size;
    MR_Word         extension_size;
    MR_Word         *old_common_ancestor_fr;
    MR_Word         *new_common_ancestor_fr;
    MR_Word         *saved_fr;
    MR_Word         *real_fr;
    MR_Word         frame_size;
    MR_Word         offset;
    MR_SavedState   *cons_saved_state;

    cons_saved_state = &consumer->MR_cns_saved_state;

    old_common_ancestor_fr = cons_saved_state->MR_ss_common_ancestor_fr;
    new_common_ancestor_fr = nearest_common_ancestor(old_common_ancestor_fr,
        leader->MR_sg_generator_fr);

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("extending saved state of consumer %s for %s\n",
            MR_consumer_addr_name(consumer), MR_subgoal_addr_name(leader));
        printf("common ancestors: old ");
        MR_printnondstackptr(old_common_ancestor_fr);
        printf(", new ");
        MR_printnondstackptr(new_common_ancestor_fr);
        printf("\nold saved state:\n");
        print_saved_state(stdout, cons_saved_state);
    }
#endif  /* MR_TABLE_DEBUG */

    cons_saved_state->MR_ss_common_ancestor_fr = new_common_ancestor_fr;

    arena_start = MR_table_detfr_slot(new_common_ancestor_fr) + 1;
    extension_size = cons_saved_state->MR_ss_det_stack_real_start
        - arena_start;

    if (extension_size > 0) {
        arena_size  = extension_size
            + cons_saved_state->MR_ss_det_stack_block_size;

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("assert: det arena_start ");
            MR_printdetstackptr(arena_start);
            printf(" + %d = ", arena_size);
            MR_printdetstackptr(cons_saved_state->MR_ss_s_p);
            printf(" + 1: diff %d\n",
                (arena_start + arena_size)
                - (cons_saved_state->MR_ss_s_p + 1));
        }
#endif

        if (arena_size != 0) {
            assert(arena_start + arena_size
                == cons_saved_state->MR_ss_s_p + 1);
        }

        arena_block = MR_table_allocate_words(arena_size);

        MR_table_copy_words(arena_block, arena_start, extension_size);
        MR_table_copy_words(arena_block + extension_size,
            cons_saved_state->MR_ss_det_stack_saved_block,
            cons_saved_state->MR_ss_det_stack_block_size);

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("extending det stack of consumer %s for %s\n",
                MR_consumer_addr_name(consumer), MR_subgoal_addr_name(leader));
            printf("start: old %p, new %p\n",
                cons_saved_state->MR_ss_det_stack_real_start, arena_start);
            printf("size:  old %d, new %d\n",
                cons_saved_state->MR_ss_det_stack_block_size, arena_size);
            printf("block: old %p, new %p\n",
                cons_saved_state->MR_ss_det_stack_saved_block, arena_block);
        }
#endif  /* MR_TABLE_DEBUG */

        cons_saved_state->MR_ss_det_stack_saved_block = arena_block;
        cons_saved_state->MR_ss_det_stack_block_size = arena_size;
        cons_saved_state->MR_ss_det_stack_real_start = arena_start;
    }

    arena_start = MR_prevfr_slot(new_common_ancestor_fr) + 1;
    extension_size = cons_saved_state->MR_ss_non_stack_real_start
        - arena_start;
    if (extension_size > 0) {
        arena_size  = extension_size
            + cons_saved_state->MR_ss_non_stack_block_size;

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("assert: non arena_start ");
            MR_printnondstackptr(arena_start);
            printf(" + %d = ", arena_size);
            MR_printnondstackptr(cons_saved_state->MR_ss_max_fr);
            printf(" + 1: diff %d\n",
                (arena_start + arena_size)
                - (cons_saved_state->MR_ss_max_fr + 1));
        }
#endif

        if (arena_size != 0) {
            assert(arena_start + arena_size
                == cons_saved_state->MR_ss_max_fr + 1);
        }

        arena_block = MR_table_allocate_words(arena_size);

        MR_table_copy_words(arena_block, arena_start, extension_size);
        MR_table_copy_words(arena_block + extension_size,
            cons_saved_state->MR_ss_non_stack_saved_block,
            cons_saved_state->MR_ss_non_stack_block_size);

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("extending non stack of suspension %s for %s\n",
                MR_consumer_addr_name(consumer), MR_subgoal_addr_name(leader));
            printf("start: old %p, new %p\n",
                cons_saved_state->MR_ss_non_stack_real_start, arena_start);
            printf("size:  old %d, new %d\n",
                cons_saved_state->MR_ss_non_stack_block_size, arena_size);
            printf("block: old %p, new %p\n",
                cons_saved_state->MR_ss_non_stack_saved_block, arena_block);
        }
#endif  /* MR_TABLE_DEBUG */

        cons_saved_state->MR_ss_non_stack_saved_block = arena_block;
        cons_saved_state->MR_ss_non_stack_block_size = arena_size;
        cons_saved_state->MR_ss_non_stack_real_start = arena_start;
    }

#ifdef  MR_TABLE_DEBUG
    if (MR_tablestackdebug) {
        printf("\nbefore pickling nondet stack\n");
        print_saved_state(stdout, cons_saved_state);
    }
#endif  /* MR_TABLE_DEBUG */

    /* XXX we should be doing this only for the newly added segment */
    saved_fr = cons_saved_state->MR_ss_non_stack_saved_block +
        cons_saved_state->MR_ss_non_stack_block_size - 1;
    real_fr = cons_saved_state->MR_ss_non_stack_real_start +
        cons_saved_state->MR_ss_non_stack_block_size - 1;
    while (saved_fr > cons_saved_state->MR_ss_non_stack_saved_block) {
        frame_size = real_fr - MR_prevfr_slot(saved_fr);

        if (saved_fr - frame_size >
            cons_saved_state->MR_ss_non_stack_saved_block)
        {
            *MR_redoip_addr(saved_fr) = (MR_Word) MR_ENTRY(MR_do_fail);

#ifdef  MR_TABLE_DEBUG
            if (MR_tabledebug) {
                printf("do_fail to redoip at %p (%d)\n",
                    MR_redoip_addr(saved_fr),
                    MR_redoip_addr(saved_fr) -
                        cons_saved_state->MR_ss_non_stack_saved_block);
            }
#endif  /* MR_TABLE_DEBUG */
        } else {
            *MR_redoip_addr(saved_fr) = (MR_Word) MR_ENTRY(MR_RESUME_ENTRY);
#ifdef  MR_TABLE_DEBUG
            if (MR_tabledebug) {
                printf("resume to redoip at %p (%d)\n",
                    MR_redoip_addr(saved_fr),
                    MR_redoip_addr(saved_fr) -
                        cons_saved_state->MR_ss_non_stack_saved_block);
            }
#endif  /* MR_TABLE_DEBUG */
        } /*** else cut_stack XXX */

        saved_fr -= frame_size;
        real_fr -= frame_size;
    }

#ifdef  MR_TABLE_DEBUG
    if (MR_tablestackdebug) {
        printf("\nfinished extending saved consumer stacks\n");
        print_saved_state(stdout, cons_saved_state);
    }
#endif  /* MR_TABLE_DEBUG */
}

/*
** When we discover that two subgoals depend on each other, neither can be
** completed alone. We therefore pass responsibility for completing all
** the subgoals in an SCC to the subgoal whose nondet stack frame is
** lowest in the nondet stack.
*/

static void
make_subgoal_follow_leader(MR_Subgoal *this_follower, MR_Subgoal *leader)
{
    MR_Consumer     *suspension;
    MR_SubgoalList  sub_follower;
    MR_ConsumerList suspend_list;

    MR_restore_transient_registers();

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("\nmaking %s follow %s\n",
            MR_subgoal_addr_name(this_follower), MR_subgoal_addr_name(leader));
    }
#endif  /* MR_TABLE_DEBUG */

    for (sub_follower = this_follower->MR_sg_followers;
        sub_follower != NULL; sub_follower = sub_follower->MR_sl_next)
    {
        for (suspend_list = sub_follower->MR_sl_item->MR_sg_consumer_list;
            suspend_list != NULL;
            suspend_list = suspend_list->MR_cl_next)
        {
            MR_save_transient_registers();
            extend_consumer_stacks(leader, suspend_list->MR_cl_item);
            MR_restore_transient_registers();
            /* suspend_list->item->leader = leader;      XXX */
        }
    }

    /* XXX extend saved state of this_follower */

    this_follower->MR_sg_leader = leader;
    *(leader->MR_sg_followers_tail) = this_follower->MR_sg_followers;
    this_follower->MR_sg_followers = NULL;

    MR_save_transient_registers();
}

static void
print_saved_state(FILE *fp, MR_SavedState *saved_state)
{
    fprintf(fp, "saved state parameters:\n");
    fprintf(fp, "succip:\t");
    MR_printlabel(fp, saved_state->MR_ss_succ_ip);
    fprintf(fp, "sp:\t");
    MR_print_detstackptr(fp, saved_state->MR_ss_s_p);
    fprintf(fp, "\ncurfr:\t");
    MR_print_nondstackptr(fp, saved_state->MR_ss_cur_fr);
    fprintf(fp, "\nmaxfr:\t");
    MR_print_nondstackptr(fp, saved_state->MR_ss_max_fr);
    fprintf(fp, "\n");

    fprintf(fp,
        "slots saved: %d non, %d det, %d generator, %d cut, %d pneg\n",
        saved_state->MR_ss_non_stack_block_size,
        saved_state->MR_ss_det_stack_block_size,
        saved_state->MR_ss_gen_next,
        saved_state->MR_ss_cut_next,
        saved_state->MR_ss_pneg_next);

    if (saved_state->MR_ss_non_stack_block_size > 0) {
        fprintf(fp, "non region from ");
        MR_print_nondstackptr(fp, saved_state->MR_ss_non_stack_real_start);
        fprintf(fp, " to ");
        MR_print_nondstackptr(fp, saved_state->MR_ss_non_stack_real_start +
            saved_state->MR_ss_non_stack_block_size - 1);
        fprintf(fp, " (both inclusive)\n");
    }

  #ifdef MR_TABLE_SEGMENT_DEBUG
    if (saved_state->MR_ss_non_stack_block_size > 0) {
        fprintf(fp, "stored at %p to %p (both inclusive)\n",
            saved_state->MR_ss_non_stack_saved_block,
            saved_state->MR_ss_non_stack_saved_block +
                saved_state->MR_ss_non_stack_block_size - 1);

        fprint_stack_segment(fp, saved_state->MR_ss_non_stack_saved_block,
            saved_state->MR_ss_non_stack_block_size);
    }
  #endif /* MR_TABLE_SEGMENT_DEBUG */

    if (saved_state->MR_ss_det_stack_block_size > 0) {
        fprintf(fp, "det region from ");
        MR_print_detstackptr(fp, saved_state->MR_ss_det_stack_real_start);
        fprintf(fp, " to ");
        MR_print_detstackptr(fp, saved_state->MR_ss_det_stack_real_start +
            saved_state->MR_ss_det_stack_block_size - 1);
        fprintf(fp, " (both inclusive)\n");
    }

  #ifdef MR_TABLE_SEGMENT_DEBUG
    if (saved_state->MR_ss_det_stack_block_size > 0) {
        fprintf(fp, "stored at %p to %p (both inclusive)\n",
            saved_state->MR_ss_det_stack_saved_block,
            saved_state->MR_ss_det_stack_saved_block +
                saved_state->MR_ss_det_stack_block_size - 1);

        print_stack_segment(fp, saved_state->MR_ss_det_stack_saved_block,
            saved_state->MR_ss_det_stack_block_size);
    }
  #endif /* MR_TABLE_SEGMENT_DEBUG */

    MR_print_any_gen_stack(fp, saved_state->MR_ss_gen_next,
        saved_state->MR_ss_gen_stack_saved_block);
    MR_print_any_cut_stack(fp, saved_state->MR_ss_cut_next,
        saved_state->MR_ss_cut_stack_saved_block);
    MR_print_any_pneg_stack(fp, saved_state->MR_ss_pneg_next,
        saved_state->MR_ss_pneg_stack_saved_block);

    fprintf(fp, "\n");
}

static void
print_stack_segment(FILE *fp, MR_Word *segment, MR_Integer size)
{
    int i;

    for (i = 0; i < size; i++) {
        fprintf(fp, "%2d %p: %ld (%lx)\n", i, &segment[i],
            (long) segment[i], (long) segment[i]);
    }
}

#endif  /* MR_USE_MINIMAL_MODEL */

/*---------------------------------------------------------------------------*/

/*
** This part of the file implements the suspension and and resumption
** of derivations.
**
** We need to define stubs for table_nondet_suspend and table_nondet_resume,
** even if MR_USE_MINIMAL_MODEL is not enabled, since they are declared as
** `:- external', and hence for profiling grades the generated code will take
** their address to store in the label table.
**
** We provide three definitions for those two procedures: one for high level
** code (which is incompatible with minimal model tabling), and two for low
** level code. The first of the latter two is for grades without minimal model
** tabling, the second is for grades with minimal model tabling.
*/

#ifdef MR_HIGHLEVEL_CODE

/* Declare them first, to avoid warnings from gcc -Wmissing-decls */
void MR_CALL mercury__table_builtin__table_nondet_resume_1_p_0(
    MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
    MR_Cont cont, void *cont_env_ptr);
void MR_CALL mercury__table_builtin__table_nondet_suspend_2_p_0(
    MR_C_Pointer subgoal_table_node);

void MR_CALL
mercury__table_builtin__table_nondet_resume_1_p_0(
    MR_C_Pointer subgoal_table_node, MR_C_Pointer *answer_block,
    MR_Cont cont, void *cont_env_ptr)
{
    MR_fatal_error("sorry, not implemented: "
        "minimal model tabling with --high-level-code");
}

void MR_CALL
mercury__table_builtin__table_nondet_suspend_2_p_0(
    MR_C_Pointer subgoal_table_node)
{
    MR_fatal_error("sorry, not implemented: "
        "minimal model tabling with --high-level-code");
}

#else   /* ! MR_HIGHLEVEL_CODE */

MR_define_extern_entry(MR_SUSPEND_ENTRY);
MR_define_extern_entry(MR_RESUME_ENTRY);

MR_MAKE_PROC_LAYOUT(MR_SUSPEND_ENTRY, MR_DETISM_NON, 0, -1,
    MR_PREDICATE, "table_builtin", "table_nondet_suspend", 2, 0);
MR_MAKE_PROC_LAYOUT(MR_RESUME_ENTRY, MR_DETISM_NON, 0, -1,
    MR_PREDICATE, "table_builtin", "table_nondet_resume", 1, 0);

#ifndef  MR_USE_MINIMAL_MODEL

MR_BEGIN_MODULE(table_nondet_suspend_resume_module)
    MR_init_entry_sl(MR_SUSPEND_ENTRY);
    MR_init_entry_sl(MR_RESUME_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_SUSPEND_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_RESUME_ENTRY);
MR_BEGIN_CODE

MR_define_entry(MR_SUSPEND_ENTRY);
    MR_fatal_error("call to table_nondet_suspend/2 in a grade "
        "without minimal model tabling");

MR_define_entry(MR_RESUME_ENTRY);
    MR_fatal_error("call to table_nondet_resume/1 in a grade "
        "without minimal model tabling");
MR_END_MODULE

#else   /* MR_USE_MINIMAL_MODEL */

MR_Subgoal      *MR_cur_leader;

MR_define_extern_entry(MR_table_nondet_commit);

MR_declare_entry(MR_do_trace_redo_fail);
MR_declare_entry(MR_table_nondet_commit);

MR_declare_label(RESUME_LABEL(ChangeLoop));
MR_declare_label(RESUME_LABEL(ReachedFixpoint));
MR_declare_label(RESUME_LABEL(LoopOverSubgoals));
MR_declare_label(RESUME_LABEL(LoopOverSuspensions));
MR_declare_label(RESUME_LABEL(ReturnAnswer));
MR_declare_label(RESUME_LABEL(RedoPoint));
MR_declare_label(RESUME_LABEL(RestartPoint));

MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    SUSPEND_LABEL(Call), MR_SUSPEND_ENTRY);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    RESUME_LABEL(ChangeLoop), MR_RESUME_ENTRY);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    RESUME_LABEL(ReachedFixpoint), MR_RESUME_ENTRY);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    RESUME_LABEL(LoopOverSubgoals), MR_RESUME_ENTRY);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    RESUME_LABEL(LoopOverSuspensions), MR_RESUME_ENTRY);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    RESUME_LABEL(ReturnAnswer), MR_RESUME_ENTRY);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    RESUME_LABEL(RedoPoint), MR_RESUME_ENTRY);
MR_MAKE_INTERNAL_LAYOUT_WITH_ENTRY(
    RESUME_LABEL(RestartPoint), MR_RESUME_ENTRY);

MR_BEGIN_MODULE(table_nondet_suspend_resume_module)
    MR_init_entry_sl(MR_SUSPEND_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_SUSPEND_ENTRY);
    MR_init_label_sl(SUSPEND_LABEL(Call));

    MR_init_entry_sl(MR_RESUME_ENTRY);
    MR_INIT_PROC_LAYOUT_ADDR(MR_RESUME_ENTRY);
    MR_init_label_sl(RESUME_LABEL(ChangeLoop));
    MR_init_label_sl(RESUME_LABEL(ReachedFixpoint));
    MR_init_label_sl(RESUME_LABEL(LoopOverSubgoals));
    MR_init_label_sl(RESUME_LABEL(LoopOverSuspensions));
    MR_init_label_sl(RESUME_LABEL(ReturnAnswer));
    MR_init_label_sl(RESUME_LABEL(RedoPoint));
    MR_init_label_sl(RESUME_LABEL(RestartPoint));

    MR_init_entry_an(MR_table_nondet_commit);
MR_BEGIN_CODE

MR_define_entry(MR_SUSPEND_ENTRY);
    /*
    ** The suspend procedure saves the state of the Mercury runtime so that
    ** it may be used in the table_nondet_resume procedure below to return
    ** answers through this saved state. The procedure table_nondet_suspend is
    ** declared as nondet but the code below is obviously of detism failure;
    ** the reason for this is quite simple. Normally when a nondet proc is
    ** called it will first return all of its answers and then fail. In the
    ** case of calls to this procedure this is reversed: first the call will
    ** fail then later on, when the answers are found, answers will be
    ** returned. It is also important to note that the answers are returned
    ** not from the procedure that was originally called (table_nondet_suspend)
    ** but from the procedure table_nondet_resume. So essentially what is
    ** below is the code to do the initial fail; the code to return the
    ** answers is in table_nondet_resume.
    */

    /*
    ** This frame is not used in table_nondet_suspend, but it is copied
    ** to the suspend list as part of the saved nondet stack fragment,
    ** and it *will* be used when table_nondet_resume copies back the
    ** nondet stack fragment. The framevar slot is for use by
    ** table_nondet_resume.
    */
    MR_mkframe(MR_STRINGIFY(MR_SUSPEND_ENTRY), 1, MR_ENTRY(MR_do_fail));

MR_define_label(SUSPEND_LABEL(Call));
{
    MR_SubgoalPtr   subgoal;
    MR_Consumer     *consumer;
    MR_ConsumerList listnode;
    MR_Integer      cur_gen;
    MR_Integer      cur_cut;
    MR_Integer      cur_pneg;
    MR_Word         *fr;
    MR_Word         *prev_fr;
    MR_Word         *stop_addr;
    MR_Word         offset;
    MR_Word         *clobber_addr;

    subgoal = (MR_SubgoalPtr) MR_r1;
    MR_register_suspension(subgoal);
    consumer = MR_table_allocate_struct(MR_Consumer);
    consumer->MR_cns_remaining_answer_list_ptr = &subgoal->MR_sg_answer_list;

#ifdef  MR_TABLE_DEBUG
    consumer->MR_cns_subgoal = subgoal;
    MR_enter_consumer_debug(consumer);

    if (MR_tabledebug) {
        printf("setting up consumer %s\n", MR_consumer_addr_name(consumer));
    }
#endif

    MR_save_transient_registers();
    save_state(&(consumer->MR_cns_saved_state), subgoal->MR_sg_generator_fr,
        "suspension", "consumer",
        (const MR_Label_Layout *) &MR_LAYOUT_FROM_LABEL(SUSPEND_LABEL(Call)));
    MR_restore_transient_registers();

    cur_gen = MR_gen_next - 1;
    cur_cut = MR_cut_next - 1;
    cur_pneg = MR_pneg_next - 1;
    stop_addr = consumer->MR_cns_saved_state.MR_ss_non_stack_real_start;
    for (fr = MR_maxfr; fr > stop_addr; fr = MR_prevfr_slot(fr)) {
        offset = MR_redoip_addr(fr) -
            consumer->MR_cns_saved_state.MR_ss_non_stack_real_start;
        clobber_addr = consumer->MR_cns_saved_state.MR_ss_non_stack_saved_block
            + offset;
#if 0
        if (MR_tablestackdebug) {
            printf("redoip addr ");
            MR_printnondstackptr(MR_redoip_addr(fr));
            printf(", offset %d from start, ", offset);
            printf("saved copy at %p\n", clobber_addr);
        }
#endif

        if (fr == MR_gen_stack[cur_gen].MR_gen_frame) {
            if (MR_gen_stack[cur_gen].MR_gen_subgoal == subgoal) {
                /*
                ** This is the nondet stack frame of the
                ** generator corresponding to this consumer.
                */

  #ifdef  MR_TABLE_DEBUG
                if (MR_tablestackdebug) {
                    printf("completing redoip of frame at ");
                    MR_printnondstackptr(fr);
                    printf(" (in saved copy)\n");
                    printf("  old contents was %p\n",
                        (MR_Word *) *clobber_addr);
                }
  #endif    /* MR_TABLE_DEBUG */
                *clobber_addr = (MR_Word) MR_ENTRY(MR_RESUME_ENTRY);

  #ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                    printf("saved gen_next set to %d from %d\n",
                        cur_gen + 1,
                        consumer->MR_cns_saved_state.MR_ss_gen_next);
                    /* XXX if next assignment is not idempotent */
                    if (consumer->MR_cns_saved_state.MR_ss_gen_next
                        != cur_gen + 1)
                    {
                        MR_print_gen_stack(stdout);
                        MR_print_cut_stack(stdout);
                    }
                }
  #endif    /* MR_TABLE_DEBUG */
                consumer->MR_cns_saved_state.MR_ss_gen_next = cur_gen + 1;
            } else {
                /*
                ** This is the nondet stack frame of some other generator.
                */

  #if 0
                /* reenable XXX */
                assert(MR_prevfr_slot(fr) != (stop_addr - 1));
  #endif

                *clobber_addr = (MR_Word) MR_ENTRY(MR_do_fail);
  #ifdef  MR_TABLE_DEBUG
                if (MR_tablestackdebug) {
                    printf("clobbering redoip of frame at ");
                    MR_printnondstackptr(fr);
                    printf(" (in saved copy)\n");
                }
  #endif    /* MR_TABLE_DEBUG */

                MR_save_transient_registers();
                make_subgoal_follow_leader(MR_gen_stack[cur_gen].
                    MR_gen_subgoal, subgoal);
                MR_restore_transient_registers();
            }

            cur_gen--;
            /* XXX can we be at a generator AND a cut? */
        } else if (cur_cut > 0 && fr == MR_cut_stack[cur_cut].MR_cut_frame) {
            *clobber_addr = (MR_Word) MR_ENTRY(MR_table_nondet_commit);
  #ifdef  MR_TABLE_DEBUG
            if (MR_tablestackdebug) {
                printf("committing redoip of frame at ");
                MR_printnondstackptr(fr);
                printf(" (in saved copy)\n");
            }
  #endif    /* MR_TABLE_DEBUG */

            cur_cut--;
        } else {
            *clobber_addr = (MR_Word) MR_ENTRY(MR_do_fail);
  #ifdef  MR_TABLE_DEBUG
            if (MR_tablestackdebug) {
                printf("clobbering redoip of frame at ");
                MR_printnondstackptr(fr);
                printf(" (in saved copy)\n");
            }
  #endif    /* MR_TABLE_DEBUG */
        }
    }

  #ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("adding suspension node %s to table %s",
                MR_consumer_addr_name(consumer),
                MR_subgoal_addr_name(subgoal));
        printf("\n\tat slot %p\n", subgoal->MR_sg_consumer_list_tail);
    }
  #endif    /* MR_TABLE_DEBUG */

    assert(*(subgoal->MR_sg_consumer_list_tail) == NULL);
    listnode = MR_table_allocate_struct(MR_ConsumerListNode);
    *(subgoal->MR_sg_consumer_list_tail) = listnode;
    subgoal->MR_sg_consumer_list_tail = &(listnode->MR_cl_next);
    listnode->MR_cl_item = consumer;
    listnode->MR_cl_next = NULL;
}
    MR_fail();

MR_define_entry(MR_RESUME_ENTRY);

    /*
    ** The resume procedure restores answers to suspended consumers.
    ** It works by restoring the consumer state saved by the consumer's call
    ** to table_nondet_suspend. By restoring such states and then returning
    ** answers, table_nondet_resume is essentially returning answers out of
    ** the call to table_nondet_suspend, not out of the call to
    ** table_nondet_resume.
    **
    ** The code is arranged as a three level iteration to a fixpoint. The
    ** three levels are: iterating over all subgoals in a connected component,
    ** iterating over all consumers of each of those subgoals, and iterating
    ** over all the answers to be returned to each of those consumers.
    ** Note that returning an answer could lead to further answers for
    ** any of the subgoals in the connected component; it can even lead
    ** to the expansion of the component (i.e. the addition of more subgoals
    ** to it).
    */

    MR_cur_leader = MR_top_generator_table();

    if (MR_cur_leader->MR_sg_leader != NULL) {
        /*
        ** The predicate that called table_nondet_resume is not the leader
        ** of its component. We will leave all answers to be returned
        ** by the leader.
        */

  #ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("non-leader table_nondet_resume fails\n");
        }
  #endif  /* MR_TABLE_DEBUG */

        (void) MR_pop_generator();
        MR_redo();
    }

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("table_nondet_resume enter: current leader is %s\n",
            MR_subgoal_addr_name(MR_cur_leader));
    }
#endif  /* MR_TABLE_DEBUG */

    if (MR_cur_leader->MR_sg_resume_info != NULL) {
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("using existing resume info %p\n",
                MR_cur_leader->MR_sg_resume_info);
        }
#endif  /* MR_TABLE_DEBUG */
    } else {
        MR_cur_leader->MR_sg_resume_info = MR_TABLE_NEW(MR_ResumeInfo);
        MR_cur_leader->MR_sg_resume_info->MR_ri_saved_succip = MR_succip;/*NEW*/

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("resume info succip ");
            MR_printlabel(stdout, MR_succip);
        }
#endif  /* MR_TABLE_DEBUG */

        /*
        ** XXX
        **
        ** We should compute, for all followers, the common ancestor
        ** of the follower and this generator, and save to the deepest
        ** common ancestor.
        **
        ** We should special case the situation where there are no answers
        ** we have not yet returned to consumers.
        */

        MR_save_transient_registers();
        save_state(&(MR_cur_leader->MR_sg_resume_info->MR_ri_leader_state),
            MR_cur_leader->MR_sg_generator_fr, "resumption", "generator",
            NULL);
        MR_restore_transient_registers();

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("creating new resume info %p\n",
                MR_cur_leader->MR_sg_resume_info);
        }
#endif  /* MR_TABLE_DEBUG */
    }

    /* XXX try doing the test at the bottom */
    MR_cur_leader->MR_sg_resume_info->MR_ri_changed = MR_TRUE;

MR_define_label(RESUME_LABEL(ChangeLoop));
{
    MR_ResumeInfo   *resume_info;
    
    resume_info = MR_cur_leader->MR_sg_resume_info;

    if (resume_info->MR_ri_changed) {
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("changed flag set\n");
        }
#endif  /* MR_TABLE_DEBUG */
    } else {
        MR_SubgoalList  table_list;

        /* XXX make sure subgoal_list is initialized early */
        for (table_list = resume_info->MR_ri_subgoal_list;
            table_list != NULL;
            table_list = table_list->MR_sl_next)
        {
            if (table_list->MR_sl_item->MR_sg_num_committed_ans
                != table_list->MR_sl_item->MR_sg_num_ans)
            {
                resume_info->MR_ri_changed = MR_TRUE;
#ifdef  MR_TABLE_DEBUG
                if (MR_tabledebug) {
                    printf("table %s has new answers\n",
                        MR_subgoal_addr_name(table_list->MR_sl_item));
                }
#endif  /* MR_TABLE_DEBUG */
            }
        }
    }

    if (! resume_info->MR_ri_changed) {
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("no more changes\n");
        }
#endif  /* MR_TABLE_DEBUG */

        MR_GOTO_LABEL(RESUME_LABEL(ReachedFixpoint));
    }

    resume_info->MR_ri_subgoal_list = MR_cur_leader->MR_sg_followers;
    if (resume_info->MR_ri_subgoal_list == NULL) {
        resume_info->MR_ri_changed = MR_FALSE; /* XXX NEW */
    }
}

    /* For each of the subgoals on our list of followers */
MR_define_label(RESUME_LABEL(LoopOverSubgoals));
{
    MR_ResumeInfo   *resume_info;
    
    resume_info = MR_cur_leader->MR_sg_resume_info;

    if (resume_info->MR_ri_subgoal_list == NULL) {
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("no more subgoals in the followers list\n");
        }
#endif  /* MR_TABLE_DEBUG */

        MR_GOTO_LABEL(RESUME_LABEL(ChangeLoop));
    }

    resume_info->MR_ri_cur_subgoal =
        resume_info->MR_ri_subgoal_list->MR_sl_item;
    resume_info->MR_ri_subgoal_list =
        resume_info->MR_ri_subgoal_list->MR_sl_next;

    resume_info->MR_ri_consumer_list =
        resume_info->MR_ri_cur_subgoal->MR_sg_consumer_list;

    resume_info->MR_ri_changed = MR_FALSE;
    resume_info->MR_ri_cur_subgoal->MR_sg_num_committed_ans =
        resume_info->MR_ri_cur_subgoal->MR_sg_num_ans;
}

    /* For each of the suspended nodes for cur_subgoal */
MR_define_label(RESUME_LABEL(LoopOverSuspensions));
{
    MR_ResumeInfo   *resume_info;
    
    resume_info = MR_cur_leader->MR_sg_resume_info;

    if (resume_info->MR_ri_consumer_list == NULL) {
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("no more suspensions for current subgoal\n");
        }
#endif  /* MR_TABLE_DEBUG */
        MR_GOTO_LABEL(RESUME_LABEL(LoopOverSubgoals));
    }

    resume_info->MR_ri_cur_consumer =
        resume_info->MR_ri_consumer_list->MR_cl_item;
    resume_info->MR_ri_consumer_list =
        resume_info->MR_ri_consumer_list->MR_cl_next;

    resume_info->MR_ri_cur_consumer_answer_list =
        *(resume_info->MR_ri_cur_consumer->MR_cns_remaining_answer_list_ptr);

    if (resume_info->MR_ri_cur_consumer_answer_list == NULL) {
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("no first answer for this suspension\n");
        }
#endif  /* MR_TABLE_DEBUG */
        MR_GOTO_LABEL(RESUME_LABEL(LoopOverSuspensions));
    }

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("resuming consumer %s from table %s\n",
            MR_consumer_addr_name(resume_info->MR_ri_cur_consumer),
            MR_subgoal_addr_name(resume_info->MR_ri_cur_subgoal));
    }
#endif  /* MR_TABLE_DEBUG */

    MR_save_transient_registers();
    restore_state(&(resume_info->MR_ri_cur_consumer->MR_cns_saved_state),
        "resumption", "consumer");
    MR_restore_transient_registers();

    /* check that there is room for exactly one framevar */
    assert((MR_maxfr - MR_prevfr_slot(MR_maxfr)) ==
        (MR_NONDET_FIXED_SIZE + 1));

    MR_gen_next = resume_info->MR_ri_leader_state.MR_ss_gen_next;
    MR_redoip_slot(MR_maxfr) = MR_LABEL(RESUME_LABEL(RedoPoint));
    MR_redofr_slot(MR_maxfr) = MR_maxfr;
    MR_based_framevar(MR_maxfr, 1) = (MR_Word) MR_cur_leader;
}

MR_define_label(RESUME_LABEL(ReturnAnswer));
{
    MR_ResumeInfo   *resume_info;
    
    resume_info = MR_cur_leader->MR_sg_resume_info;

    /*
    ** Return the next answer in MR_cur_leader->MR_sg_resume_info->
    ** cur_consumer_answer_list to the current consumer. Since we have
    ** already restored the context of the suspended consumer before
    ** we returned the first answer, we don't need to restore it again,
    ** since will not have changed in the meantime.
    */

    MR_r1 = (MR_Word)
        resume_info->MR_ri_cur_consumer_answer_list->MR_aln_answer_data.
        MR_answerblock;
    resume_info->MR_ri_cur_consumer->MR_cns_remaining_answer_list_ptr =
        &(resume_info->MR_ri_cur_consumer_answer_list->MR_aln_next_answer);
    resume_info->MR_ri_cur_consumer_answer_list =
        resume_info->MR_ri_cur_consumer_answer_list->MR_aln_next_answer;

    /*
    ** Return the answer. Since we just restored the state of the
    ** computation that existed when suspend was called, the code
    ** that we return to is the code following the call to suspend.
    */
    MR_succeed();
}

MR_define_label(RESUME_LABEL(RedoPoint));
    MR_update_prof_current_proc(MR_LABEL(MR_RESUME_ENTRY));

    /*
    ** This is where the current consumer suspension will go on
    ** backtracking when it wants the next solution. If there is a solution
    ** we haven't returned to this consumer yet, we do so, otherwise we
    ** remember how many answers we have returned to this consumer so far
    ** and move on to the next suspended consumer of the current subgoal.
    */

    MR_cur_leader = (MR_Subgoal *) MR_based_framevar(MR_maxfr, 1);

MR_define_label(RESUME_LABEL(RestartPoint));
{
    MR_ResumeInfo   *resume_info;
    MR_Subgoal      *cur_subgoal;
    
    resume_info = MR_cur_leader->MR_sg_resume_info;

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("cur_consumer_answer_list: %p\n",
            resume_info->MR_ri_cur_consumer_answer_list);
        printf("*cur_consumer->remaining_answer_list_ptr: %p\n",
            *(resume_info->MR_ri_cur_consumer->
                MR_cns_remaining_answer_list_ptr));
    }
#endif

    if (resume_info->MR_ri_cur_consumer_answer_list != NULL) {
        MR_GOTO_LABEL(RESUME_LABEL(ReturnAnswer));
    }

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("no more unreturned answers for this suspension\n");
    }
#endif  /* MR_TABLE_DEBUG */

    cur_subgoal = resume_info->MR_ri_cur_subgoal;
    if (cur_subgoal->MR_sg_num_committed_ans != cur_subgoal->MR_sg_num_ans) {
        resume_info->MR_ri_changed = MR_TRUE;
    }

    MR_GOTO_LABEL(RESUME_LABEL(LoopOverSuspensions));
}
MR_define_label(RESUME_LABEL(ReachedFixpoint));
{
    MR_SubgoalList  table_list;
    MR_ResumeInfo   *resume_info;

    for (table_list = MR_cur_leader->MR_sg_followers;
        table_list != NULL;
        table_list = table_list->MR_sl_next)
    {
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("marking table %s complete\n",
                MR_subgoal_addr_name(table_list->MR_sl_item));
        }
#endif

        table_list->MR_sl_item->MR_sg_status = MR_SUBGOAL_COMPLETE;
        table_list->MR_sl_item->MR_sg_num_committed_ans = -1;
    }

    resume_info = MR_cur_leader->MR_sg_resume_info;

    /* Restore the state we had when table_nondet_resume was called */
    MR_save_transient_registers();
    restore_state(&(resume_info->MR_ri_leader_state),
        "resumption", "generator");
    MR_restore_transient_registers();

    /* XXX this will go code that does fail() */
    MR_succip = resume_info->MR_ri_saved_succip;

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("using resume info succip ");
        MR_print_label(stdout, MR_succip);
    }
#endif  /* MR_TABLE_DEBUG */

    /* we should free the old resume_info structure */
    MR_cur_leader->MR_sg_resume_info = NULL;

    /* We are done with this generator */
    (void) MR_pop_generator();

    MR_proceed();
}

MR_define_entry(MR_table_nondet_commit);
    MR_commit_cut();
    MR_fail();

MR_END_MODULE

#endif /* MR_USE_MINIMAL_MODEL */
#endif /* MR_HIGHLEVEL_CODE */

/* Ensure that the initialization code for the above modules gets to run. */
/*
INIT mercury_sys_init_table_modules
*/

MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc table_nondet_suspend_resume_module;

/* forward declarations to suppress gcc -Wmissing-decl warnings */
void mercury_sys_init_table_modules_init(void);
void mercury_sys_init_table_modules_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_table_modules_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    table_nondet_suspend_resume_module();
#endif  /* MR_HIGHLEVEL_CODE */
}

void mercury_sys_init_table_modules_init_type_tables(void)
{
    /* no types to register */
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_table_modules_write_out_proc_statics(FILE *fp)
{
    /* no proc_statics to write out */
}
#endif
