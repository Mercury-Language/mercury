// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2009,2012 The University of Melbourne.
// Copyright (C) 2013-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_stack_trace.c - implements stack traces.
//
// Main authors: Tyson Dowd (trd), Zoltan Somogyi (zs).

#include "mercury_imp.h"
#include "mercury_stack_trace.h"
#include "mercury_stack_layout.h"
#include "mercury_debug.h"
#include "mercury_array_macros.h"
#include "mercury_trace_base.h"
#include "mercury_tabling.h"
#include <stdio.h>

static int          MR_compare_proc_layout_ptrs(
                        const void *pl1, const void *pl2);

static  MR_StackWalkStepResult
                    MR_stack_walk_succip_layout(MR_Code *success,
                        const MR_LabelLayout **return_label_layout_ptr,
                        MR_Word **base_sp_ptr, MR_Word **base_curfr_ptr,
                        const char **problem_ptr);

#ifndef MR_HIGHLEVEL_CODE

typedef enum {
    MR_FRAME_ON_MAIN_BRANCH,
    MR_INTERNAL_FRAME_ON_SIDE_BRANCH,
    MR_TOP_FRAME_ON_SIDE_BRANCH,
    MR_TERMINAL_TOP_FRAME_ON_SIDE_BRANCH
} MR_NondetFrameCategory;

typedef struct {
    MR_TraverseNondetFrameFunc      *func;
    void                            *func_data;
} MR_TraverseNondetFrameFuncInfo;

typedef void        MR_DumpOrTraverseNondetFrameFunc(void *user_data,
                        MR_NondetFrameCategory category, MR_Word *top_fr,
                        const MR_LabelLayout *layout, MR_Word *base_sp,
                        MR_Word *base_curfr, int level_number);

// These are two possible functions of type MR_DumpOrTraverseNondetFrameFunc.
static void         MR_dump_nondet_stack_frame(void *fp,
                        MR_NondetFrameCategory category, MR_Word *top_fr,
                        const MR_LabelLayout *top_layout, MR_Word *base_sp,
                        MR_Word *base_curfr, int level_number);
static void         MR_traverse_nondet_stack_frame(void *fp,
                        MR_NondetFrameCategory category, MR_Word *top_fr,
                        const MR_LabelLayout *top_layout, MR_Word *base_sp,
                        MR_Word *base_curfr, int level_number);

static  const char  *MR_step_over_nondet_frame(
                        MR_DumpOrTraverseNondetFrameFunc *func,
                        void *func_data, int level_number, MR_Word *fr);
static  void        MR_init_nondet_branch_infos(MR_Word *base_maxfr,
                        const MR_LabelLayout *top_layout, MR_Word *base_sp,
                        MR_Word *base_curfr);
static  MR_bool     MR_find_matching_branch(MR_Word *fr, int *branch_ptr);
static  void        MR_record_temp_redoip(MR_Word *fr);
static  MR_bool     MR_nofail_ip(MR_Code *ip);
static  MR_Code     *MR_find_nofail_temp_redoip(MR_Word *fr);
static  void        MR_erase_temp_redoip(MR_Word *fr);

#endif  // !MR_HIGHLEVEL_CODE

typedef struct {
    const MR_ProcLayout     *pte_proc_layout;
    int                     pte_first_level;
    int                     pte_last_level;
    int                     pte_num_frames;
    MR_bool                 pte_some_nonseq_frames;
    int                     pte_left;       // negative -> no left subtree
    int                     pte_right;      // negative -> no right subtree
} MR_ProcTableEntry;

static  int         MR_find_proc_in_proc_table(
                        const MR_ProcTableEntry *proc_table,
                        int proc_table_next,
                        const MR_ProcLayout *proc_layout,
                        int *parent_ptr, int *side_ptr);
static  void        MR_add_parent_ptr(MR_ProcTableEntry *proc_table,
                        int parent, int side, int slot);

typedef struct MR_Clique_struct MR_Clique;

struct MR_Clique_struct {
    int                     cl_first_level;
    int                     cl_last_level;
    MR_Clique               *cl_prev_clique;
    MR_Clique               *cl_next_clique;
};

typedef struct {
    const MR_ProcLayout     *ste_proc_layout;
    const MR_LabelLayout    *ste_label_layout;
    MR_Word                 *ste_trace_sp;
    MR_Word                 *ste_trace_curfr;
    MR_Unsigned             ste_reused_frames;
    // int                  ste_last_frame_in_proc;
    int                     ste_proc_table_entry_slot;
} MR_WalkedStackEntry;

typedef struct {
    MR_StackFrameDumpInfo   sdi_prev_frame_dump_info;
    int                     sdi_current_level;
} MR_StackDumpInfo;

typedef struct {
    MR_bool                 sdp_include_trace_data;
    MR_bool                 sdp_include_contexts;
} MR_StackDumpParams;

static  void        MR_init_stack_dump_info(MR_StackDumpInfo *dump_info);
static  int         MR_dump_stack_record_frame(FILE *fp,
                        MR_StackDumpParams *params,
                        MR_StackDumpInfo *dump_info,
                        const MR_LabelLayout *label_layout,
                        MR_Word *base_sp, MR_Word *base_curfr,
                        MR_Unsigned reused_frames,
                        MR_PrintStackRecord print_stack_record,
                        MR_bool at_line_limit);
static  void        MR_dump_stack_record_flush(FILE *fp,
                        MR_StackDumpParams *params,
                        MR_StackDumpInfo *dump_info,
                        MR_PrintStackRecord print_stack_record);

static  void        MR_print_proc_id_internal(FILE *fp,
                        const MR_ProcLayout *proc_layout, MR_bool spec,
                        MR_bool print_mode, MR_bool separate);

static  void        MR_maybe_print_proc_id(FILE *fp, MR_bool print_proc_id,
                        const MR_ProcLayout *proc_layout, const char *path);
static  void        MR_maybe_print_context(FILE *fp, MR_bool print_context,
                        const char *filename, int lineno);
static  void        MR_maybe_print_parent_context(FILE *fp,
                        MR_bool print_parent, MR_bool verbose,
                        const char *filename, int lineno);

static  MR_bool     MR_call_details_are_valid(const MR_ProcLayout *proc_layout,
                        MR_Word *base_sp, MR_Word *base_curfr);
static  MR_bool     MR_call_is_before_event_or_seq(
                        MR_FindFirstCallSeqOrEvent seq_or_event,
                        MR_Unsigned seq_no_or_event_no,
                        const MR_ProcLayout *proc_layout, MR_Word *base_sp,
                        MR_Word *base_curfr);

// See comments in mercury_stack_trace.h.
MR_Code             *MR_stack_trace_bottom_ip;
MR_Word             *MR_nondet_stack_trace_bottom_fr;
#ifdef MR_STACK_SEGMENTS
MR_MemoryZone       *MR_nondet_stack_trace_bottom_zone;
#endif

void
MR_dump_stack(MR_Code *success_pointer, MR_Word *det_stack_pointer,
    MR_Word *current_frame, MR_bool include_trace_data)
{
    const MR_Internal       *label;
    const MR_LabelLayout    *label_layout;
    const char              *result;
    MR_bool                 stack_dump_available;
    char                    *env_suppress;

    env_suppress = getenv("MERCURY_SUPPRESS_STACK_TRACE");
    if (env_suppress != NULL) {
        return;
    }

#ifdef MR_STACK_TRACE
    stack_dump_available = MR_TRUE;
#else
    stack_dump_available = MR_FALSE;
#endif

    if (stack_dump_available) {
        fprintf(stderr, "Stack dump follows:\n");

        MR_do_init_modules();
        label = MR_lookup_internal_by_addr(success_pointer);
        if (label == NULL) {
            fprintf(stderr, "internal label not found\n");
        } else {
            label_layout = label->MR_internal_layout;
            result = MR_dump_stack_from_layout(stderr, label_layout,
                det_stack_pointer, current_frame, include_trace_data,
                MR_TRUE, 0, 0, &MR_dump_stack_record_print);

            if (result != NULL) {
                fprintf(stderr, "%s\n", result);
            }
        }
    } else {
        fprintf(stderr, "Stack dump not available in this grade.\n");
    }
}

const char *
MR_dump_stack_from_layout(FILE *fp, const MR_LabelLayout *label_layout,
    MR_Word *det_stack_pointer, MR_Word *current_frame,
    MR_bool include_trace_data, MR_bool include_contexts,
    MR_FrameLimit frame_limit, MR_SpecLineLimit line_limit,
    MR_PrintStackRecord print_stack_record)
{
    MR_StackWalkStepResult  result;
    MR_StackDumpParams      params;
    MR_StackDumpInfo        dump_info;
    const MR_ProcLayout     *proc_layout;
    const MR_LabelLayout    *cur_label_layout;
    const MR_LabelLayout    *prev_label_layout;
    const char              *problem;
    MR_Word                 *stack_trace_sp;
    MR_Word                 *stack_trace_curfr;
    MR_Word                 *old_trace_sp;
    MR_Word                 *old_trace_curfr;
    MR_FrameLimit           frames_dumped_so_far;
    MR_SpecLineLimit        lines_dumped_so_far;
    MR_Unsigned             reused_frames;

    MR_do_init_modules();

    params.sdp_include_trace_data = include_trace_data;
    params.sdp_include_contexts = include_contexts;
    MR_init_stack_dump_info(&dump_info);

    stack_trace_sp = det_stack_pointer;
    stack_trace_curfr = current_frame;

    cur_label_layout = label_layout;

    if (line_limit == 0) {
        line_limit = MR_UINT_LEAST32_MAX;
    }

    if (frame_limit == 0) {
        frame_limit = MR_UINT_LEAST32_MAX;
    }

    frames_dumped_so_far = 0;
    lines_dumped_so_far = 0;
    do {
        if (frame_limit > 0 && frames_dumped_so_far >= frame_limit) {
            MR_dump_stack_record_flush(fp, &params, &dump_info,
                print_stack_record);
            fprintf(fp, "<more stack frames snipped>\n");
            return NULL;
        }

        if (lines_dumped_so_far >= line_limit) {
            MR_dump_stack_record_flush(fp, &params, &dump_info,
                print_stack_record);
            fprintf(fp, "<more stack frames snipped>\n");
            return NULL;
        }

        proc_layout = cur_label_layout->MR_sll_entry;
        prev_label_layout = cur_label_layout;

        old_trace_sp    = stack_trace_sp;
        old_trace_curfr = stack_trace_curfr;

        result = MR_stack_walk_step(proc_layout, &cur_label_layout,
            &stack_trace_sp, &stack_trace_curfr, &reused_frames, &problem);
        if (result == MR_STEP_ERROR_BEFORE) {
            MR_dump_stack_record_flush(fp, &params, &dump_info,
                print_stack_record);
            return problem;
        } else if (result == MR_STEP_ERROR_AFTER) {
            (void) MR_dump_stack_record_frame(fp, &params, &dump_info,
                prev_label_layout, old_trace_sp, old_trace_curfr,
                reused_frames, print_stack_record, MR_FALSE);

            MR_dump_stack_record_flush(fp, &params, &dump_info,
                print_stack_record);
            return problem;
        } else {
            lines_dumped_so_far += MR_dump_stack_record_frame(fp,
                &params, &dump_info, prev_label_layout,
                old_trace_sp, old_trace_curfr,
                reused_frames, print_stack_record,
                lines_dumped_so_far >= line_limit);
        }

        frames_dumped_so_far++;
    } while (cur_label_layout != NULL);

    MR_dump_stack_record_flush(fp, &params, &dump_info, print_stack_record);
    return NULL;
}

const char *
MR_dump_stack_from_layout_clique(FILE *fp, const MR_LabelLayout *label_layout,
    MR_Word *det_stack_pointer, MR_Word *current_frame,
    MR_bool include_trace_data, MR_bool include_contexts,
    MR_bool detect_cliques, MR_SpecLineLimit clique_line_limit,
    MR_FrameLimit frame_limit, MR_SpecLineLimit line_limit,
    MR_PrintStackRecord print_stack_record)
{
    MR_StackWalkStepResult  result;
    MR_StackDumpParams      params;
    MR_StackDumpInfo        dump_info;
    const MR_ProcLayout     *proc_layout;
    const MR_LabelLayout    *cur_label_layout;
    const MR_LabelLayout    *prev_label_layout;
    const char              *problem;
    MR_bool                 stopped;
    MR_Word                 *stack_trace_sp;
    MR_Word                 *stack_trace_curfr;
    MR_Word                 *old_trace_sp;
    MR_Word                 *old_trace_curfr;
    MR_WalkedStackEntry     *walked_stack;
    MR_FrameLimit           walked_stack_size;
    MR_FrameLimit           walked_stack_next;
    MR_ProcTableEntry       *proc_table;
    int                     proc_table_next;
    MR_Unsigned             reused_frames;
    MR_FrameLimit           level;
    MR_SpecLineLimit        lines_dumped_so_far = 0;
    MR_Clique               *cliques_first;
    MR_Clique               *cliques_last;
    MR_Clique               *cl;
    MR_FrameLimit           rec_first_level;
    MR_FrameLimit           rec_last_level;

    if (clique_line_limit == 0) {
        clique_line_limit = MR_UINT_LEAST32_MAX;
    }

    if (line_limit == 0) {
        line_limit = MR_UINT_LEAST32_MAX;
    }

    if (frame_limit == 0) {
        frame_limit = MR_UINT_LEAST32_MAX;
    }

    MR_do_init_modules();

    stack_trace_sp = det_stack_pointer;
    stack_trace_curfr = current_frame;

    cur_label_layout = label_layout;

    walked_stack_next = 0;
    walked_stack_size = 100;
    walked_stack = MR_malloc(walked_stack_size * sizeof(MR_WalkedStackEntry));

    proc_table = NULL;
    cliques_last = NULL;

    stopped = MR_FALSE;
    problem = NULL;
    do {
        if (frame_limit > 0 && walked_stack_next >= frame_limit) {
            stopped = MR_TRUE;
            break;
        }

        proc_layout = cur_label_layout->MR_sll_entry;
        prev_label_layout = cur_label_layout;

        old_trace_sp    = stack_trace_sp;
        old_trace_curfr = stack_trace_curfr;

        result = MR_stack_walk_step(proc_layout, &cur_label_layout,
            &stack_trace_sp, &stack_trace_curfr, &reused_frames, &problem);

        if (result == MR_STEP_ERROR_BEFORE) {
            break;
        }

        if (walked_stack_next >= walked_stack_size) {
            walked_stack_size = 2 * walked_stack_size;
            walked_stack = MR_realloc(walked_stack,
                walked_stack_size * sizeof(MR_WalkedStackEntry));
        }

        walked_stack[walked_stack_next].ste_proc_layout = proc_layout;
        walked_stack[walked_stack_next].ste_label_layout =
            prev_label_layout;
        walked_stack[walked_stack_next].ste_trace_sp = old_trace_sp;
        walked_stack[walked_stack_next].ste_trace_curfr = old_trace_curfr;
        walked_stack[walked_stack_next].ste_reused_frames = reused_frames;
        walked_stack[walked_stack_next].ste_proc_table_entry_slot = -1;
        walked_stack_next++;

        if (result == MR_STEP_ERROR_AFTER) {
            break;
        }
    } while (cur_label_layout != NULL);

    params.sdp_include_trace_data = include_trace_data;
    params.sdp_include_contexts = include_contexts;
    MR_init_stack_dump_info(&dump_info);

    if (!detect_cliques) {
        for (level = 0; level < walked_stack_next; level++) {
            if (lines_dumped_so_far >= line_limit) {
                fprintf(fp, "<more stack frames snipped>\n");
                problem = NULL;
                goto done;
            }

            lines_dumped_so_far += MR_dump_stack_record_frame(fp,
                &params, &dump_info, walked_stack[level].ste_label_layout,
                walked_stack[level].ste_trace_sp,
                walked_stack[level].ste_trace_curfr,
                walked_stack[level].ste_reused_frames, print_stack_record,
                lines_dumped_so_far >= line_limit);
        }
        MR_dump_stack_record_flush(fp, &params, &dump_info,
            print_stack_record);

        MR_free(walked_stack);
        return problem;
    }

    proc_table = MR_malloc(walked_stack_next * sizeof(MR_ProcTableEntry));
    proc_table_next = 0;
    cliques_first = NULL;
    cliques_last = NULL;

    level = 0;
    rec_first_level = level;
    proc_layout = walked_stack[rec_first_level].ste_proc_layout;
    while (level+1 < walked_stack_next &&
        walked_stack[level+1].ste_proc_layout == proc_layout)
    {
        level++;
    }
    rec_last_level = level;
    level++;

    proc_table[0].pte_proc_layout = proc_layout;
    proc_table[0].pte_first_level = rec_first_level;
    proc_table[0].pte_last_level = rec_last_level;
    proc_table[0].pte_num_frames = rec_last_level + 1 - rec_first_level;
    proc_table[0].pte_some_nonseq_frames = MR_FALSE;
    proc_table[0].pte_left = -1;
    proc_table[0].pte_right = -1;
    proc_table_next = 1;

    while (level < walked_stack_next) {
        MR_bool             has_higher_order_arg;
        int                 slot;
        int                 parent;
        int                 side;

        rec_first_level = level;
        proc_layout = walked_stack[rec_first_level].ste_proc_layout;
        while (level+1 < walked_stack_next &&
            walked_stack[level+1].ste_proc_layout == proc_layout)
        {
            level++;
        }
        rec_last_level = level;
        level++;

        // XXX For higher order predicates like list.map, we should pretend
        // that we have not seen them before.

        slot = MR_find_proc_in_proc_table(proc_table, proc_table_next,
            proc_layout, &parent, &side);

        if (MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc_layout)) {
            has_higher_order_arg = MR_proc_has_higher_order_arg(proc_layout);
        } else {
            // We don't often encounter calls to procedures without debugging
            // info in programs compiled in debug grades. Since we don't know
            // whether the procedure has higher order args, the conservative
            // thing to say is "yes, it does". It will even be true for
            // procedures such as builtin_catch.

            has_higher_order_arg = MR_TRUE;
        }

        if (slot < 0 || has_higher_order_arg) {
            // Either we have not seen this procedure before, or we are
            // pretending that we have not seen it before.
            //
            // The reason for such pretense is that we don't want calls
            // to e.g. list.map in different places in the program
            // to collapse every call between those places into a single
            // clique.

            slot = proc_table_next;
            proc_table[slot].pte_proc_layout =
                walked_stack[rec_first_level].ste_proc_layout;
            proc_table[slot].pte_first_level = rec_first_level;
            proc_table[slot].pte_last_level = rec_last_level;
            proc_table[slot].pte_num_frames =
                rec_last_level + 1 - rec_first_level;
            proc_table[slot].pte_some_nonseq_frames = MR_FALSE;
            proc_table[slot].pte_left = -1;
            proc_table[slot].pte_right = -1;
            proc_table_next++;
            MR_add_parent_ptr(proc_table, parent, side, slot);

            walked_stack[rec_first_level].ste_proc_table_entry_slot = slot;
        } else {
            // We have seen this procedure before.
            proc_table[slot].pte_last_level = rec_last_level;
            proc_table[slot].pte_num_frames +=
                rec_last_level + 1 - rec_first_level;
            proc_table[slot].pte_some_nonseq_frames = MR_TRUE;

            if (cliques_last == NULL) {
                // Add the first clique to the list.

                cl = MR_malloc(sizeof(MR_Clique));
                cl->cl_first_level = proc_table[slot].pte_first_level;
                cl->cl_last_level = rec_last_level;
                cl->cl_prev_clique = NULL;
                cl->cl_next_clique = NULL;
                cliques_first = cl;
                cliques_last = cl;
            } else if (cliques_last->cl_last_level
                < proc_table[slot].pte_first_level)
            {
                // The current clique does not overlap with the last clique,
                // so add a new clique to the list.

                cl = MR_malloc(sizeof(MR_Clique));
                cl->cl_first_level = proc_table[slot].pte_first_level;
                cl->cl_last_level = rec_last_level;
                cl->cl_prev_clique = cliques_last;
                cl->cl_next_clique = NULL;
                cliques_last->cl_next_clique = cl;
                cliques_last = cl;
            } else {
                // The current clique does overlap with the last old clique,
                // and maybe others. Replace all the cliques in the list it
                // overlaps with just one clique. Put this clique in the
                // storage of the clique node that was nearest to
                // cliques_first.

                cl = cliques_last;
                // assert cl != NULL
                while (cl->cl_prev_clique != NULL &&
                    cl->cl_prev_clique->cl_last_level >
                        proc_table[slot].pte_first_level)
                {
                    MR_Clique   *old_cl;

                    old_cl = cl;
                    cl = cl->cl_prev_clique;
                    MR_free(old_cl);
                }

                cl->cl_first_level = MR_min(cl->cl_first_level,
                    proc_table[slot].pte_first_level);
                cl->cl_last_level = rec_last_level;
                cliques_last = cl;
            }
        }
    }

#ifdef  MR_DEBUG_STACK_DUMP_CLIQUE
    for (cl = cliques_first; cl != NULL; cl = cl->cl_next_clique) {
        fprintf(fp, "clique: %d to %d\n",
            cl->cl_first_level, cl->cl_last_level);
    }
#endif

    lines_dumped_so_far = 0;
    level = 0;
    for (cl = cliques_first; cl != NULL; cl = cl->cl_next_clique) {
        MR_SpecLineLimit    lines_dumped_before_clique;

        for (; level < cl->cl_first_level; level++) {
            if (lines_dumped_so_far >= line_limit) {
                MR_dump_stack_record_flush(fp, &params, &dump_info,
                    print_stack_record);
                fprintf(fp, "<more stack frames snipped>\n");
                problem = NULL;
                goto done;
            }

            lines_dumped_so_far += MR_dump_stack_record_frame(fp,
                &params, &dump_info, walked_stack[level].ste_label_layout,
                walked_stack[level].ste_trace_sp,
                walked_stack[level].ste_trace_curfr,
                walked_stack[level].ste_reused_frames, print_stack_record,
                lines_dumped_so_far >= line_limit);
        }
        MR_dump_stack_record_flush(fp, &params, &dump_info,
            print_stack_record);

        fprintf(fp, "<mutually recursive set of stack frames start>\n");
        lines_dumped_before_clique = lines_dumped_so_far;
        for (; level <= cl->cl_last_level; level++) {
            if (lines_dumped_so_far >= line_limit) {
                MR_dump_stack_record_flush(fp, &params, &dump_info,
                    print_stack_record);
                fprintf(fp, "<more stack frames snipped>\n");
                problem = NULL;
                goto done;
            }

            if (lines_dumped_so_far - lines_dumped_before_clique
                >= clique_line_limit)
            {
                MR_dump_stack_record_flush(fp, &params, &dump_info,
                    print_stack_record);
                fprintf(fp, "<more stack frames in clique snipped>\n");
                level = cl->cl_last_level + 1;
                dump_info.sdi_current_level = level;
                break;
            }

            lines_dumped_so_far += MR_dump_stack_record_frame(fp,
                &params, &dump_info, walked_stack[level].ste_label_layout,
                walked_stack[level].ste_trace_sp,
                walked_stack[level].ste_trace_curfr,
                walked_stack[level].ste_reused_frames, print_stack_record,
                lines_dumped_so_far >= line_limit);
        }
        MR_dump_stack_record_flush(fp, &params, &dump_info,
            print_stack_record);
        fprintf(fp, "<mutually recursive set of stack frames end>\n");
    }

    for (; level < walked_stack_next; level++) {
        if (lines_dumped_so_far >= line_limit) {
            MR_dump_stack_record_flush(fp, &params, &dump_info,
                print_stack_record);
            fprintf(fp, "<more stack frames snipped>\n");
            problem = NULL;
            goto done;
        }

        lines_dumped_so_far += MR_dump_stack_record_frame(fp,
            &params, &dump_info, walked_stack[level].ste_label_layout,
            walked_stack[level].ste_trace_sp,
            walked_stack[level].ste_trace_curfr,
            walked_stack[level].ste_reused_frames, print_stack_record,
            lines_dumped_so_far >= line_limit);
    }
    MR_dump_stack_record_flush(fp, &params, &dump_info,
        print_stack_record);

    if (stopped) {
        fprintf(fp, "<more stack frames snipped>\n");
    }

done:
    MR_free(walked_stack);
    MR_free(proc_table);
    cl = cliques_last;
    while (cl != NULL) {
        MR_Clique   *old_cl;

        old_cl = cl;
        cl = cl->cl_prev_clique;
        MR_free(old_cl);
    }

    return problem;
}

static int
MR_find_proc_in_proc_table(const MR_ProcTableEntry *proc_table,
    int proc_table_next, const MR_ProcLayout *proc_layout,
    int *parent_ptr, int *side_ptr)
{
    int cur;
    int parent;
    int side;

    // XXX We don't need proc_table_next for anything else.
    if (proc_table_next == 0) {
        MR_fatal_error("MR_find_proc_in_proc_table: table is empty");
    }

    cur = 0;
    do {
        if (proc_layout == proc_table[cur].pte_proc_layout) {
            return cur;
        } else if (proc_layout < proc_table[cur].pte_proc_layout) {
            parent = cur;
            side = 0;
            cur = proc_table[cur].pte_left;
        } else {
            parent = cur;
            side = 1;
            cur = proc_table[cur].pte_right;
        }
    } while (cur >= 0);

    *parent_ptr = parent;
    *side_ptr = side;
    return -1;
}

static void
MR_add_parent_ptr(MR_ProcTableEntry *proc_table, int parent, int side,
    int slot)
{
    if (side == 0) {
        proc_table[parent].pte_left = slot;
    } else {
        proc_table[parent].pte_right = slot;
    }
}

static int
MR_compare_proc_layout_ptrs(const void *v1, const void *v2)
{
    const MR_ProcLayout *pl1 = * (const MR_ProcLayout **) v1;
    const MR_ProcLayout *pl2 = * (const MR_ProcLayout **) v2;

    if ((MR_Unsigned) pl1 > (MR_Unsigned) pl2) {
        return 1;
    } else if ((MR_Unsigned) pl1 < (MR_Unsigned) pl2) {
        return -1;
    } else {
        return 0;
    }
}

const char *
MR_find_clique_entry(const MR_LabelLayout *label_layout,
    MR_Word *det_stack_pointer, MR_Word *current_frame,
    int *clique_entry_level, int *first_outside_ancestor_level)
{
    MR_StackWalkStepResult  result;
    const MR_LabelLayout    *cur_label_layout;
    const MR_ProcLayout     *cur_proc_layout;
    const char              *problem;
    MR_Word                 *stack_trace_sp;
    MR_Word                 *stack_trace_curfr;
    MR_Word                 *old_trace_sp;
    MR_Word                 *old_trace_curfr;
    MR_Unsigned             reused_frames;

    const MR_ProcLayout     **procs_table;
    int                     procs_table_size;    // allocated
    int                     procs_table_next;    // next free slot
    int                     num_procs_in_clique; // filled in

    int                     highest_level_in_clique;
    int                     ancestor_level;
    MR_bool                 in_clique;
    int                     last_filled;
    int                     i;

    MR_do_init_modules();

    stack_trace_sp = det_stack_pointer;
    stack_trace_curfr = current_frame;

    cur_label_layout = label_layout;
    cur_proc_layout = cur_label_layout->MR_sll_entry;

    // procs_table is an array containing proc_table_size slots.
    // Of these, the slots at index 0 .. num_procs_in_clique-1 contain
    // pointers to the proc layouts of the procedures currently known
    // to be in the same clique as the original top level label_layout.
    // The slots from num_procs_in_clique to procs_table_next contain
    // pointers to the proc_layouts of the other procedures we have
    // encountered so far during our walk of the stack.
    //
    // The slots at 0 .. num_procs_in_clique-1 are sorted, and have no
    // duplicates. The slots at num_procs_in_clique .. procs_table_next
    // are not sorted, and may have duplicates.

    procs_table_size = 256;
    procs_table = MR_malloc(procs_table_size * sizeof(const MR_ProcLayout *));
    procs_table[0] = cur_proc_layout;
    num_procs_in_clique = 1;
    procs_table_next = 1;

#ifdef  MR_DEBUG_FIND_CLIQUE_ENTRY
    printf("INIT %x\n", cur_proc_layout);
#endif

    ancestor_level = 0;
    highest_level_in_clique = 0;
    do {

        old_trace_sp    = stack_trace_sp;
        old_trace_curfr = stack_trace_curfr;

        result = MR_stack_walk_step(cur_proc_layout, &cur_label_layout,
            &stack_trace_sp, &stack_trace_curfr, &reused_frames, &problem);
        if (result == MR_STEP_ERROR_BEFORE) {
            MR_free(procs_table);
            return problem;
        } else if (result == MR_STEP_ERROR_AFTER) {
            MR_free(procs_table);
            return problem;
        }

        if (cur_label_layout == NULL) {
            break;
        }

        cur_proc_layout = cur_label_layout->MR_sll_entry;

        ancestor_level++;
        // Since the part of the procs_table up to num_procs_in_clique
        // is guaranteed to be sorted, we have the option of using either
        // linear search or binary search. We use linear search because
        // we expect the number of procedures in cliques to be small, and
        // linear search is likely to be faster for searching small arrays.

        in_clique = MR_FALSE;
        for (i = 0; i < num_procs_in_clique; i++) {
            if (cur_proc_layout == procs_table[i]) {
                in_clique = MR_TRUE;
                break;
            }
        }

        if (!in_clique) {
#ifdef  MR_DEBUG_FIND_CLIQUE_ENTRY
            printf("NONREC %d %d %x\n",
                num_procs_in_clique, procs_table_next, cur_proc_layout);
#endif

            if (procs_table_next >= procs_table_size) {
                procs_table_size = 2 * procs_table_size;
                procs_table = MR_realloc(procs_table,
                    procs_table_size * sizeof(const MR_ProcLayout *));
            }

            procs_table[procs_table_next] = cur_proc_layout;
            procs_table_next++;

        } else {
#ifdef  MR_DEBUG_FIND_CLIQUE_ENTRY
            printf("REC %d %d %d %d %x\n",
                ancestor_level, highest_level_in_clique,
                num_procs_in_clique, procs_table_next, cur_proc_layout);
#endif

            if (ancestor_level > highest_level_in_clique+1) {
                // There are some slots in the part of procs_table
                // that contains unsorted, possibly duplicate entries,
                // so first sort the whole table ...

                qsort(procs_table, procs_table_next,
                    sizeof(const MR_ProcLayout *),
                    MR_compare_proc_layout_ptrs);

#ifdef  MR_DEBUG_FIND_CLIQUE_ENTRY
                printf("\n");
                for (i = 0; i < procs_table_next; i++) {
                    printf("SORTED %d %x\n", i, procs_table[i]);
                }
#endif
                // ... and then eliminate any duplicates, which are now
                // guaranteed to be consecutive.

                last_filled = 0;
                for (i = 1; i < procs_table_next; i++) {
                    if (procs_table[i] != procs_table[last_filled]) {
                        last_filled++;
                        procs_table[last_filled] = procs_table[i];
                    }
                }

                procs_table_next = last_filled + 1;
                num_procs_in_clique = procs_table_next;

#ifdef  MR_DEBUG_FIND_CLIQUE_ENTRY
                printf("\n");
                for (i = 0; i < procs_table_next; i++) {
                    printf("UNIQ %d %x\n", i, procs_table[i]);
                }
                printf("\n");
#endif
            }

            highest_level_in_clique = ancestor_level;
        }
    } while (MR_TRUE);

    if (clique_entry_level != NULL) {
        *clique_entry_level = highest_level_in_clique;
    }

    if (first_outside_ancestor_level != NULL) {
        if (ancestor_level > highest_level_in_clique) {
            *first_outside_ancestor_level = highest_level_in_clique + 1;
        } else {
            *first_outside_ancestor_level = -1;
        }
    }

    MR_free(procs_table);
    return NULL;
}

const MR_LabelLayout *
MR_find_nth_ancestor(const MR_LabelLayout *label_layout,
    MR_Level ancestor_level, MR_Word **stack_trace_sp,
    MR_Word **stack_trace_curfr, MR_Level *actual_level_ptr,
    const char **problem)
{
    MR_StackWalkStepResult  result;
    const MR_LabelLayout    *return_label_layout;
    MR_Unsigned             level;
    MR_Unsigned             reused_frames;

    MR_do_init_modules();
    *problem = NULL;
    level = 0;
    while (level < ancestor_level && label_layout != NULL) {
        result = MR_stack_walk_step(label_layout->MR_sll_entry,
            &return_label_layout, stack_trace_sp, stack_trace_curfr,
            &reused_frames, problem);

        if (result != MR_STEP_OK) {
            // *problem has already been filled in
            return NULL;
        }

        label_layout = return_label_layout;
        level += 1 + reused_frames;
    }

    if (label_layout == NULL && *problem == NULL) {
        *problem = "not that many ancestors";
    }

    *actual_level_ptr = level;
    return label_layout;
}

MR_StackWalkStepResult
MR_stack_walk_step(const MR_ProcLayout *proc_layout,
    const MR_LabelLayout **return_label_layout_ptr,
    MR_Word **stack_trace_sp_ptr, MR_Word **stack_trace_curfr_ptr,
    MR_Unsigned *reused_frames_ptr, const char **problem_ptr)
{
    MR_LongLval             location;
    MR_LongLvalType         type;
    int                     number;
    int                     determinism;
    MR_Code                 *success;
    MR_Unsigned             reused_frames;

    *return_label_layout_ptr = NULL;

    determinism = proc_layout->MR_sle_detism;
    if (determinism < 0) {
        // This means we have reached some handwritten code that has
        // no further information about the stack frame.

        *problem_ptr = "reached procedure with no stack trace info";
        return MR_STEP_ERROR_BEFORE;
    }

    location = proc_layout->MR_sle_succip_locn;
    if (MR_DETISM_DET_STACK(determinism)) {
        type = MR_LONG_LVAL_TYPE(location);
        number = MR_LONG_LVAL_NUMBER(location);

        if (type != MR_LONG_LVAL_TYPE_STACKVAR) {
            *problem_ptr = "can only handle stackvars";
            return MR_STEP_ERROR_AFTER;
        }

        success = (MR_Code *) MR_based_stackvar(*stack_trace_sp_ptr, number);

        MR_trace_find_reused_frames(proc_layout, *stack_trace_sp_ptr,
            reused_frames);
        *reused_frames_ptr = reused_frames;

        *stack_trace_sp_ptr = *stack_trace_sp_ptr -
            proc_layout->MR_sle_stack_slots;
    } else {
        // Succip is always saved in succip_slot.
        assert(location == -1);
        // Note that curfr always points to an ordinary procedure frame,
        // never to a temp frame, and this property continues to hold
        // while we traverse the nondet stack via the succfr slot.
        // So it is safe to access the succip and succfr slots without checking
        // what kind of frame it is.

        success = MR_succip_slot(*stack_trace_curfr_ptr);
        *reused_frames_ptr = 0;
        *stack_trace_curfr_ptr = MR_succfr_slot(*stack_trace_curfr_ptr);
    }

    return MR_stack_walk_succip_layout(success, return_label_layout_ptr,
        stack_trace_sp_ptr, stack_trace_curfr_ptr, problem_ptr);
}

static MR_StackWalkStepResult
MR_stack_walk_succip_layout(MR_Code *success,
    const MR_LabelLayout **return_label_layout_ptr,
    MR_Word **stack_trace_sp_ptr, MR_Word **stack_trace_curfr_ptr,
    const char **problem_ptr)
{
    MR_Internal             *label;

    if (success == MR_stack_trace_bottom_ip) {
        return MR_STEP_OK;
    }

#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_STACK_SEGMENTS)
    if (success == MR_ENTRY(MR_pop_detstack_segment)) {
        success = (MR_Code *) MR_based_stackvar(*stack_trace_sp_ptr, 2);
        *stack_trace_sp_ptr = (MR_Word *)
            MR_based_stackvar(*stack_trace_sp_ptr, 1);
    }
    if (success == MR_ENTRY(MR_pop_nondetstack_segment)) {
        *problem_ptr = "reached MR_pop_nondetstack_segment";
        return MR_STEP_ERROR_AFTER;
    }
#endif // !MR_HIGHLEVEL_CODE && MR_STACK_SEGMENTS

    label = MR_lookup_internal_by_addr(success);
    if (label == NULL) {
        *problem_ptr = "reached unknown label";
        return MR_STEP_ERROR_AFTER;
    }

    if (label->MR_internal_layout == NULL) {
        *problem_ptr = "reached label with no stack layout info";
        return MR_STEP_ERROR_AFTER;
    }

    *return_label_layout_ptr = label->MR_internal_layout;
    return MR_STEP_OK;
}

////////////////////////////////////////////////////////////////////////////

void
MR_dump_nondet_stack(FILE *fp, MR_FrameLimit frame_limit,
    MR_SpecLineLimit line_limit, MR_Word *base_maxfr)
{
#ifndef MR_HIGHLEVEL_CODE

    MR_dump_nondet_stack_from_layout(fp, frame_limit, line_limit,
        base_maxfr, NULL, NULL, NULL);

#else   // !MR_HIGHLEVEL_CODE

    MR_fatal_error("MR_dump_nondet_stack in high level C grade");

#endif  // !MR_HIGHLEVEL_CODE
}

#ifdef MR_HIGHLEVEL_CODE

void
MR_dump_nondet_stack_from_layout(FILE *fp,
    MR_FrameLimit frame_limit, MR_SpecLineLimit line_limit,
    MR_Word *base_maxfr, const MR_LabelLayout *top_layout,
    MR_Word *base_sp, MR_Word *base_curfr)
{
    MR_fatal_error("MR_dump_nondet_stack_from_layout in high level grade");
}

#else   // !MR_HIGHLEVEL_CODE

// Detailed nondet stack dumps and accurate GC both need to know
// the values of the local variables in each nondet stack frame.
// To find out what variables are live in each frame, we must know
// through what label control will go back to that frame, so we
// can use that label's layout structure.
//
// Control can reach a frame through one of three means.
//
// - It may already be there. This is true only for the frame defined by the
//   arguments of MR_dump_nondet_stack_from_layout, and the layout structure
//   we use is also among the arguments.
//
// - It may get there by backtracking. In this case, the layout structure to
//   use is the one associated with the frame's redoip structure.
//
// - It may get there by returning from a call. In this case, the layout
//   structure to use is the one associated with the return label.
//
// We distinguish the last two cases by keeping an array of nondet stack frames
// that will be returned to from other nondet stack frames higher up, possibly
// via other procedures that live on the det stack. Procedures that live on the
// det stack may occur in the call chain of the currently active procedure, but
// they may not occur in side branches of the search tree: a model_non call may
// leave stack frames only on the nondet stack when it exits.
//
// When we find the top frame of a side branch, we don't know what the value
// of the det stack pointer sp was when execution created that nondet stack
// frame. This means that if that an ancestor of that nondet stack frame lives
// on the det stack, we cannot find the address of the stack frame of the
// ancestor. However, due that above invariant the only such ancestor a nondet
// stack frame on a side branch can have is an ancestor it shares with the
// currently executing call, and for the ancestors of the currently executing
// call we *do* know the values of sp.
//
// The MR_nondet_branch_infos array has one entry for each nondet branch; this
// entry gives the details of the next frame on the nondet stack from that
// branch. The branch_curfr field is valid for all entries and all entries in
// the array have distinct values for this field. The branch_sp field is valid
// only for the entry on the main branch; for all other entries, in contains
// NULL. The branch_layout field gives the address of the layout structure of
// the return address through which control will return to that frame. (Frames
// to which control returns via backtracking never get put into this array,
// only their ancestors do.) The branch_topfr field gives the address of the
// top frame in the branch; we print this because it makes the stack dump
// easier to interpret.
//
// The MR_nondet_branch_infos array grows when we find the tops of new side
// branches and shrinks when we find frames that created side branches.

typedef struct
{
    MR_Word                 *branch_sp;
    MR_Word                 *branch_curfr;
    const MR_LabelLayout    *branch_layout;
    MR_Word                 *branch_topfr;
} MR_NondetBranchInfo;

static MR_NondetBranchInfo  *MR_nondet_branch_infos = NULL;
static int                  MR_nondet_branch_info_next = 0;
static int                  MR_nondet_branch_info_max = 0;

#define MR_INIT_NONDET_BRANCH_ARRAY_SIZE        10

void
MR_dump_nondet_stack_from_layout(FILE *fp,
    MR_FrameLimit frame_limit, MR_SpecLineLimit line_limit,
    MR_Word *base_maxfr, const MR_LabelLayout *top_layout,
    MR_Word *base_sp, MR_Word *base_curfr)
{
    MR_Integer              apparent_frame_size;
    MR_Integer              frame_size;
    int                     level_number;
    MR_bool                 print_vars;
    const char              *problem;
    int                     branch;
    const MR_LabelLayout    *label_layout;
    const MR_ProcLayout     *proc_layout;
    int                     frames_traversed_so_far;
    int                     lines_dumped_so_far;

    MR_do_init_modules();

    MR_nondet_branch_info_next = 0;
    if (top_layout != NULL && base_sp != NULL && base_curfr != NULL
        && MR_address_of_trace_browse_all_on_level != NULL)
    {
        print_vars = MR_TRUE;
        MR_init_nondet_branch_infos(base_maxfr, top_layout, base_sp,
            base_curfr);
    } else {
        print_vars = MR_FALSE;
    }

    // The comparison macro in the condition of the while loop should be
    // the "at_or_above" variant if you want the trace to include the bottom
    // frame created by mercury_wrapper.c (whose redoip/redofr field can be
    // hijacked by other code), and just "above" if you don't want the bottom
    // frame to be included.

    frames_traversed_so_far = 0;
    lines_dumped_so_far = 0;
    level_number = 0;
    while (MR_at_or_above_bottom_nondet_frame(base_maxfr)) {
        if (frame_limit > 0 && frames_traversed_so_far >= frame_limit) {
            fprintf(fp, "<more stack frames snipped>\n");
            return;
        }

        if (line_limit > 0 && lines_dumped_so_far >= line_limit) {
            fprintf(fp, "<more stack frames snipped>\n");
            return;
        }

        // Note that the actual frame size is NOT the apparent frame size
        // for the sentinel frames at the beginnings of the second and
        // later nondet stack segments. However, in such cases, the apparent
        // size will be either negative (if the logically previous segment is
        // at a higher address than the segment that holds the sentinel frame)
        // or a positive number that is least as big as the size of the
        // sentinel frame (if the logically previous segment is at a lower
        // address). Therefore if the apparent size is MR_NONDET_TEMP_SIZE or
        // MR_DET_TEMP_SIZE, that must be the actual size as well.

        apparent_frame_size = base_maxfr - MR_prevfr_slot(base_maxfr);
        if (apparent_frame_size == MR_NONDET_TEMP_SIZE) {
            MR_print_nondetstackptr(fp, base_maxfr);
            fprintf(fp, ": temp\n");
            fprintf(fp, " redoip: ");
            MR_printlabel(fp, MR_redoip_slot(base_maxfr));
            fprintf(fp, " redofr: ");
            MR_print_nondetstackptr(fp, MR_redofr_slot(base_maxfr));
            fprintf(fp, "\n");

            if (print_vars) {
                MR_record_temp_redoip(base_maxfr);
            }

            lines_dumped_so_far += 3;
        } else if (apparent_frame_size == MR_DET_TEMP_SIZE) {
            MR_print_nondetstackptr(fp, base_maxfr);
            fprintf(fp, ": temp\n");
            fprintf(fp, " redoip: ");
            MR_printlabel(fp, MR_redoip_slot(base_maxfr));
            fprintf(fp, " redofr: ");
            MR_print_nondetstackptr(fp, MR_redofr_slot(base_maxfr));
            fprintf(fp, "\n");
            fprintf(fp, " detfr:  ");
            MR_print_detstackptr(fp, MR_tmp_detfr_slot(base_maxfr));
            fprintf(fp, "\n");

            lines_dumped_so_far += 4;
        } else if (MR_redoip_slot(base_maxfr) ==
            MR_ENTRY(MR_pop_nondetstack_segment))
        {
            // For an explanation of sentinel frames, see the comment before
            // MR_new_nondetstack_segment in mercury_stacks.c.

            MR_print_nondetstackptr(fp, base_maxfr);
            fprintf(fp, ": segment sentinel\n");
            fprintf(fp, " orig maxfr: ");
            MR_print_nondetstackptr(fp, MR_prevfr_slot(base_maxfr));
            fprintf(fp, "\n");
            fprintf(fp, " orig curfr: ");
            MR_print_nondetstackptr(fp, MR_succfr_slot(base_maxfr));
            fprintf(fp, "\n");
        } else {
            frame_size = apparent_frame_size;
            MR_print_nondetstackptr(fp, base_maxfr);
            fprintf(fp, ": ordinary, %" MR_INTEGER_LENGTH_MODIFIER "d words",
                frame_size);
            if (print_vars && MR_find_matching_branch(base_maxfr, &branch)) {
                fprintf(fp, ", ");
                label_layout = MR_nondet_branch_infos[branch].branch_layout;
                MR_print_proc_id(fp, label_layout->MR_sll_entry);
                fprintf(fp, " <%s>", MR_label_goal_path(label_layout));
            }
            fprintf(fp, "\n");
            fprintf(fp, " redoip: ");
            MR_printlabel(fp, MR_redoip_slot(base_maxfr));
            fprintf(fp, " redofr: ");
            MR_print_nondetstackptr(fp, MR_redofr_slot(base_maxfr));
            fprintf(fp, "\n");
            fprintf(fp, " succip: ");
            MR_printlabel(fp, MR_succip_slot(base_maxfr));
            fprintf(fp, " succfr: ");
            MR_print_nondetstackptr(fp, MR_succfr_slot(base_maxfr));
            fprintf(fp, "\n");
            lines_dumped_so_far += 5;
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
            fprintf(fp, " detfr:  ");
            MR_print_detstackptr(fp, MR_table_detfr_slot(base_maxfr));
            fprintf(fp, "\n");
            lines_dumped_so_far += 1;
#endif
            if (print_vars && MR_find_matching_branch(base_maxfr, &branch)) {
                label_layout = MR_nondet_branch_infos[branch].branch_layout;
                proc_layout = label_layout->MR_sll_entry;
                if (MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc_layout)
                    && MR_debug_slots_flag)
                {
                    fprintf(fp, " debug:  ");
                    fprintf(fp, "call event ");
                    MR_print_nondetstackptr(fp,
                        &MR_event_num_framevar(base_maxfr));
                    fprintf(fp, " => %" MR_INTEGER_LENGTH_MODIFIER "d, ",
                        MR_event_num_framevar(base_maxfr) + 1);
                    fprintf(fp, "call seq ");
                    MR_print_nondetstackptr(fp,
                        &MR_call_num_framevar(base_maxfr));
                    fprintf(fp, " => %" MR_INTEGER_LENGTH_MODIFIER "d, ",
                        MR_call_num_framevar(base_maxfr)),
                    fprintf(fp, "depth ");
                    MR_print_nondetstackptr(fp,
                        &MR_call_depth_framevar(base_maxfr));
                    fprintf(fp, " => %" MR_INTEGER_LENGTH_MODIFIER "d\n",
                        MR_call_depth_framevar(base_maxfr));

                    lines_dumped_so_far += 1;
                }
            }

            level_number++;
            if (print_vars && MR_above_bottom_nondet_frame(base_maxfr)) {
                problem = MR_step_over_nondet_frame(MR_dump_nondet_stack_frame,
                    fp, level_number, base_maxfr);
                if (problem != NULL) {
                    fprintf(fp, "%s\n", problem);
                    return;
                }
            }
        }

        base_maxfr = MR_prevfr_slot(base_maxfr);
        frames_traversed_so_far++;
    }
}

static void
MR_dump_nondet_stack_frame(void *fp, MR_NondetFrameCategory category,
    MR_Word *top_fr, const MR_LabelLayout *top_layout, MR_Word *base_sp,
    MR_Word *base_curfr, int level_number)
{
    FILE *dump_fp = fp;

    switch (category) {
        case MR_INTERNAL_FRAME_ON_SIDE_BRANCH:
            fprintf(dump_fp, " internal frame on nondet side branch ");
            MR_print_nondetstackptr(dump_fp, top_fr);
            fprintf(dump_fp, "\n");
            break;
        case MR_FRAME_ON_MAIN_BRANCH:
            fprintf(dump_fp, " on main nondet branch ");
            MR_print_nondetstackptr(dump_fp, top_fr);
            fprintf(dump_fp, "\n");
            break;
        case MR_TERMINAL_TOP_FRAME_ON_SIDE_BRANCH:
            fprintf(dump_fp, " terminal top frame of a nondet side branch ");
            MR_print_nondetstackptr(dump_fp, base_curfr);
            fprintf(dump_fp, "\n");
            break;
        case MR_TOP_FRAME_ON_SIDE_BRANCH:
            fprintf(dump_fp, " top frame of a nondet side branch ");
            MR_print_nondetstackptr(dump_fp, base_curfr);
            fprintf(dump_fp, "\n");
            break;
        default:
            MR_fatal_error("invalid MR_NondetFrameCategory");
    }

    if (category != MR_TERMINAL_TOP_FRAME_ON_SIDE_BRANCH) {
        // The browsing code is in Mercury, so we need to disable debugger
        // events and diagnostics inside.

        MR_SavedDebugState  saved_debug_state;

        MR_turn_off_debug(&saved_debug_state, MR_TRUE);
        // XXX We ignore the return value.
        (void) (*MR_address_of_trace_browse_all_on_level)(dump_fp, top_layout,
            base_sp, base_curfr, level_number, MR_TRUE);
        MR_turn_debug_back_on(&saved_debug_state);
    }
}

void
MR_traverse_nondet_stack_from_layout(MR_Word *base_maxfr,
    const MR_LabelLayout *top_layout, MR_Word *base_sp, MR_Word *base_curfr,
    MR_TraverseNondetFrameFunc *func, void *func_data)
{
    int                             frame_size;
    int                             level_number;
    const char                      *problem;
    int                             frames_traversed_so_far;
    MR_TraverseNondetFrameFuncInfo  func_info;

    assert(top_layout != NULL && base_sp != NULL && base_curfr != NULL);

    MR_do_init_modules();

    MR_init_nondet_branch_infos(base_maxfr, top_layout, base_sp, base_curfr);

    frames_traversed_so_far = 0;
    level_number = 0;
    func_info.func = func;
    func_info.func_data = func_data;

    // The comparison macro in the condition of the while loop should be
    // the "at_or_above" variant if you want the trace to include the bottom
    // frame created by mercury_wrapper.c (whose redoip/redofr field can be
    // hijacked by other code), and the "above" variant if you don't want
    // the bottom frame to be included.

    while (MR_at_or_above_bottom_nondet_frame(base_maxfr)) {
        frame_size = base_maxfr - MR_prevfr_slot(base_maxfr);
        if (frame_size == MR_NONDET_TEMP_SIZE) {
            MR_record_temp_redoip(base_maxfr);
        } else if (frame_size == MR_DET_TEMP_SIZE) {
            // do nothing
        } else {
            level_number++;
            if (MR_above_bottom_nondet_frame(base_maxfr)) {
                problem = MR_step_over_nondet_frame(
                    MR_traverse_nondet_stack_frame, &func_info,
                    level_number, base_maxfr);
                if (problem != NULL) {
                    MR_fatal_error(problem);
                }
            }
        }

        base_maxfr = MR_prevfr_slot(base_maxfr);
        frames_traversed_so_far++;
    }
}

static void
MR_traverse_nondet_stack_frame(void *info, MR_NondetFrameCategory category,
    MR_Word *top_fr, const MR_LabelLayout *top_layout, MR_Word *base_sp,
    MR_Word *base_curfr, int level_number)
{
    MR_TraverseNondetFrameFuncInfo *func_info;

    func_info = (MR_TraverseNondetFrameFuncInfo *) info;
    if (category != MR_TERMINAL_TOP_FRAME_ON_SIDE_BRANCH) {
        func_info->func(func_info->func_data, top_layout, base_sp, base_curfr);
    }
}

static void
MR_init_nondet_branch_infos(MR_Word *base_maxfr,
    const MR_LabelLayout *top_layout, MR_Word *base_sp, MR_Word *base_curfr)
{
    const MR_LabelLayout        *label_layout;
    const MR_ProcLayout         *proc_layout;
    MR_Word                     *stack_pointer;
    MR_Word                     *current_frame;
    MR_StackWalkStepResult      result;
    const char                  *problem;
    MR_Unsigned                 reused_frames;

    label_layout = top_layout;
    stack_pointer = base_sp;
    current_frame = base_curfr;

    MR_nondet_branch_info_next = 0;

    // Skip past any model_det frames.
    do {
        proc_layout = label_layout->MR_sll_entry;
        if (!MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
            break;
        }
        result = MR_stack_walk_step(proc_layout, &label_layout,
            &stack_pointer, &current_frame, &reused_frames, &problem);
        if (result == MR_STEP_ERROR_BEFORE || result == MR_STEP_ERROR_AFTER) {
            MR_fatal_error(problem);
        }

    } while (label_layout != NULL);

    // Double-check that we didn't skip any model_non frames.
    assert(current_frame == base_curfr);

    if (label_layout != NULL) {
        MR_ensure_room_for_next(MR_nondet_branch_info, MR_NondetBranchInfo,
            MR_INIT_NONDET_BRANCH_ARRAY_SIZE);
        MR_nondet_branch_infos[0].branch_sp = stack_pointer;
        MR_nondet_branch_infos[0].branch_curfr = current_frame;
        MR_nondet_branch_infos[0].branch_layout = label_layout;
        MR_nondet_branch_infos[0].branch_topfr = base_curfr;
        MR_nondet_branch_info_next++;
    }
}

static const char *
MR_step_over_nondet_frame(MR_DumpOrTraverseNondetFrameFunc *func,
    void *func_data, int level_number, MR_Word *fr)
{
    MR_StackWalkStepResult      result;
    MR_Determinism              determinism;
    const MR_Internal           *internal;
    int                         branch;
    int                         last;
    MR_Word                     *base_sp;
    MR_Word                     *base_curfr;
    MR_Word                     *topfr;
    const MR_LabelLayout        *label_layout;
    const MR_ProcLayout         *proc_layout;
    MR_Code                     *redoip;
    MR_Code                     *success;
    const char                  *problem;
    MR_NondetFrameCategory      category;
    MR_Unsigned                 reused_frames;

    if (MR_find_matching_branch(fr, &branch)) {
        base_sp = MR_nondet_branch_infos[branch].branch_sp;
        base_curfr = MR_nondet_branch_infos[branch].branch_curfr;
        label_layout = MR_nondet_branch_infos[branch].branch_layout;
        topfr = MR_nondet_branch_infos[branch].branch_topfr;
        if (base_sp == NULL) {
            category = MR_INTERNAL_FRAME_ON_SIDE_BRANCH;
        } else {
            category = MR_FRAME_ON_MAIN_BRANCH;
        }
        (*func)(func_data, category, topfr, label_layout, base_sp, base_curfr,
            level_number);
        MR_erase_temp_redoip(fr);
        proc_layout = label_layout->MR_sll_entry;

        // Step past all other detstack-living ancestors on the main branch.

        while (MR_TRUE) {
            result = MR_stack_walk_step(proc_layout, &label_layout,
                &base_sp, &base_curfr, &reused_frames, &problem);

            if (result != MR_STEP_OK) {
                return problem;
            }

            if (label_layout == NULL) {
                return NULL;
            }

            proc_layout = label_layout->MR_sll_entry;
            determinism = proc_layout->MR_sle_detism;

            if (! MR_DETISM_DET_STACK(determinism)) {
                // We will handle this call to a model_non procedure when the
                // sweep in MR_traverse_nondet_stack_from_layout reaches it.
                // For now, we only put it into the table.

                break;
            } else if (base_sp == NULL) {
                // We are on a side branch, and we must have arrived at
                // the common ancestor of the side branch and the main branch.

                return NULL;
            }
        }

        last = MR_nondet_branch_info_next - 1;
        MR_assign_structure(MR_nondet_branch_infos[branch],
            MR_nondet_branch_infos[last]);
        MR_nondet_branch_info_next--;
    } else {
        redoip = MR_find_nofail_temp_redoip(fr);
        if (redoip == NULL && MR_nofail_ip(MR_redoip_slot(fr))) {
            redoip = MR_redoip_slot(fr);
        }

        if (redoip == NULL) {
            (*func)(func_data, MR_TERMINAL_TOP_FRAME_ON_SIDE_BRANCH,
                NULL, NULL, NULL, fr, level_number);
            MR_erase_temp_redoip(fr);

            success = MR_succip_slot(fr);
            base_sp = NULL;
            base_curfr = MR_succfr_slot(fr);
            topfr = fr;
            result = MR_stack_walk_succip_layout(success, &label_layout,
                &base_sp, &base_curfr, &problem);
        } else {
            internal = MR_lookup_internal_by_addr(redoip);
            if (internal == NULL || internal->MR_internal_layout == NULL) {
                return "cannot find redoip label's layout structure";
            }

            label_layout = internal->MR_internal_layout;
            (*func)(func_data, MR_TOP_FRAME_ON_SIDE_BRANCH, NULL, label_layout,
                NULL, fr, level_number);
            MR_erase_temp_redoip(fr);

            // Passing a NULL base_sp to MR_stack_walk_step is OK because
            // the procedure whose stack frame we are now looking at uses
            // the nondet stack. Putting a NULL base_sp into the table
            // is OK because all the ancestors of this procedure that are
            // not also ancestors of the call currently being executed
            // must also use the nondet stack. This is a consequence of the
            // invariant that model_non calls leave the det stack unchanged.

            base_sp = NULL;
            base_curfr = fr;
            proc_layout = label_layout->MR_sll_entry;
            topfr = fr;
            result = MR_stack_walk_step(proc_layout, &label_layout,
                &base_sp, &base_curfr, &reused_frames, &problem);
        }

        if (result != MR_STEP_OK) {
            return problem;
        }

        if (label_layout == NULL) {
            return NULL;
        }

        proc_layout = label_layout->MR_sll_entry;
        determinism = proc_layout->MR_sle_detism;
        if (MR_DETISM_DET_STACK(determinism)) {
            // We must have found the common ancestor of the procedure call
            // whose variables we just printed and the call currently being
            // executed. While this common ancestor must include model_non
            // code, this may be inside a commit in a procedure that lives
            // on the det stack. If that is the case, the common ancestor
            // must not be put into MR_nondet_branch_info.

            return NULL;
        }
    }

    if (! MR_find_matching_branch(base_curfr, &branch)) {
        MR_ensure_room_for_next(MR_nondet_branch_info, MR_NondetBranchInfo,
            MR_INIT_NONDET_BRANCH_ARRAY_SIZE);
        last = MR_nondet_branch_info_next;
        MR_nondet_branch_infos[last].branch_layout = label_layout;
        MR_nondet_branch_infos[last].branch_sp = base_sp;
        MR_nondet_branch_infos[last].branch_curfr = base_curfr;
        MR_nondet_branch_infos[last].branch_topfr = topfr;
        MR_nondet_branch_info_next++;
    } else if (base_sp != NULL &&
        MR_nondet_branch_infos[last].branch_sp == NULL)
    {
        MR_fatal_error("common ancestor reached from non-main branch first");
    }

    return NULL;
}

static MR_bool
MR_find_matching_branch(MR_Word *fr, int *branch_ptr)
{
    int     branch;

    for (branch = 0; branch < MR_nondet_branch_info_next; branch++) {
        if (MR_nondet_branch_infos[branch].branch_curfr == fr) {
            *branch_ptr = branch;
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

// The contents of a nondet stack frame which control will enter via
// backtracking is described by the layout structure of the label at which
// execution will resume inside the procedure. This need not be the label in
// the redoip slot in the procedure's ordinary stack frame; if the procedure
// created any temporary nondet stack frames, it will be the label in the
// redoip slot of the top temporary nondet stack frame created by the
// procedure.
//
// We record the contents of topmost temp frames as go past them, and erase the
// records as we go past the ordinary frames to which they refer.

typedef struct
{
    MR_Word         *temp_redofr;
    MR_Code         *temp_redoip;
} MR_Temp_Redoip;

static MR_Temp_Redoip   *MR_temp_frame_infos = NULL;
static int              MR_temp_frame_info_next = 0;
static int              MR_temp_frame_info_max = 0;

#define MR_INIT_TEMP_REDOIP_ARRAY_SIZE  10

static void
MR_record_temp_redoip(MR_Word *fr)
{
    int     slot;

    for (slot = 0; slot < MR_temp_frame_info_next; slot++) {
        if (fr == MR_temp_frame_infos[slot].temp_redofr) {
            // This is not the top temp frame for this call.
            return;
        }
    }

    MR_ensure_room_for_next(MR_temp_frame_info, MR_Temp_Redoip,
            MR_INIT_TEMP_REDOIP_ARRAY_SIZE);
    slot = MR_temp_frame_info_next;
    MR_temp_frame_infos[slot].temp_redofr = fr;
    MR_temp_frame_infos[slot].temp_redoip = MR_redoip_slot(fr);
    MR_temp_frame_info_next++;
}

// Return false iff the given label effectively implements the predicate "fail"
// and true otherwise.

static MR_bool
MR_nofail_ip(MR_Code *ip)
{
    if (ip == MR_ENTRY(MR_do_fail)) {
        return MR_FALSE;
    }
    if (ip == MR_ENTRY(MR_do_trace_redo_fail_shallow)) {
        return MR_FALSE;
    }
    if (ip == MR_ENTRY(MR_do_trace_redo_fail_deep)) {
        return MR_FALSE;
    }
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
    if (ip == MR_ENTRY(MR_MMSC_COMPLETION_ENTRY)) {
        return MR_FALSE;
    }
#endif

    return MR_TRUE;
}

static MR_Code *
MR_find_nofail_temp_redoip(MR_Word *fr)
{
    int     slot;

    for (slot = 0; slot < MR_temp_frame_info_next; slot++) {
        if (fr == MR_temp_frame_infos[slot].temp_redofr &&
            MR_nofail_ip(MR_temp_frame_infos[slot].temp_redoip))
        {
            return MR_temp_frame_infos[slot].temp_redoip;
        }
    }

    return NULL;
}

static void
MR_erase_temp_redoip(MR_Word *fr)
{
    int     slot;
    int     last;

    for (slot = 0; slot < MR_temp_frame_info_next; slot++) {
        if (fr == MR_temp_frame_infos[slot].temp_redofr) {
            last = MR_temp_frame_info_next - 1;
            MR_temp_frame_infos[slot].temp_redofr =
                MR_temp_frame_infos[last].temp_redofr;
            MR_temp_frame_infos[slot].temp_redoip =
                MR_temp_frame_infos[last].temp_redoip;
            MR_temp_frame_info_next--;
            return;
        }
    }
}

#endif  // !MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////

static void
MR_init_stack_dump_info(MR_StackDumpInfo *dump_info)
{
    dump_info->sdi_prev_frame_dump_info.MR_sdi_proc_layout = NULL;
    dump_info->sdi_current_level = 0;
}

static int
MR_dump_stack_record_frame(FILE *fp, MR_StackDumpParams *params,
    MR_StackDumpInfo *dump_info, const MR_LabelLayout *label_layout,
    MR_Word *base_sp, MR_Word *base_curfr, MR_Unsigned reused_frames,
    MR_PrintStackRecord print_stack_record, MR_bool at_line_limit)
{
    const MR_ProcLayout     *proc_layout;
    const char              *filename;
    int                     linenumber;
    MR_bool                 must_flush;
    int                     lines_printed;

    proc_layout = label_layout->MR_sll_entry;
    if (! MR_find_context(label_layout, &filename, &linenumber)
        || ! params->sdp_include_contexts)
    {
        filename = "";
        linenumber = 0;
    }

    // We cannot merge two calls if they are to different procedures.
    //
    // We cannot merge two calls even to the same procedure if we are printing
    // trace data, since this will differ between the calls.
    //
    // Note that it is not possible for two calls to the same procedure
    // to differ on whether the procedure has trace layout data or not.

    must_flush =
        (proc_layout != dump_info->sdi_prev_frame_dump_info.MR_sdi_proc_layout)
        || params->sdp_include_trace_data;

    if (must_flush) {
        if (! at_line_limit) {
            MR_dump_stack_record_flush(fp, params, dump_info,
                print_stack_record);
        }

        dump_info->sdi_prev_frame_dump_info.MR_sdi_proc_layout = proc_layout;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_num_frames = 1;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_min_level =
            dump_info->sdi_current_level;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_max_level =
            dump_info->sdi_current_level + reused_frames;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_filename = filename;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_linenumber = linenumber;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_context_mismatch = MR_FALSE;

        dump_info->sdi_prev_frame_dump_info.MR_sdi_base_sp = base_sp;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_base_curfr = base_curfr;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_goal_path =
            MR_label_goal_path(label_layout);

        lines_printed = 1;
    } else {
        dump_info->sdi_prev_frame_dump_info.MR_sdi_num_frames++;
        dump_info->sdi_prev_frame_dump_info.MR_sdi_max_level =
            dump_info->sdi_current_level + reused_frames;
        if (dump_info->sdi_prev_frame_dump_info.MR_sdi_filename != filename
            || dump_info->sdi_prev_frame_dump_info.MR_sdi_linenumber
                != linenumber)
        {
            dump_info->sdi_prev_frame_dump_info.MR_sdi_context_mismatch =
                MR_TRUE;
        }

        lines_printed = 0;
    }

    dump_info->sdi_current_level += 1 + reused_frames;
    return lines_printed;
}

static void
MR_dump_stack_record_flush(FILE *fp, MR_StackDumpParams *params,
    MR_StackDumpInfo *dump_info, MR_PrintStackRecord print_stack_record)
{
    if (dump_info->sdi_prev_frame_dump_info.MR_sdi_proc_layout != NULL) {
        (*print_stack_record)(fp, params->sdp_include_trace_data,
            &(dump_info->sdi_prev_frame_dump_info));
    }
    dump_info->sdi_prev_frame_dump_info.MR_sdi_proc_layout = NULL;
}

void
MR_dump_stack_record_print(FILE *fp, MR_bool include_trace_data,
    const MR_StackFrameDumpInfo *frame_dump_info)
{
    MR_Level    num_levels;

    num_levels = frame_dump_info->MR_sdi_max_level + 1
        - frame_dump_info->MR_sdi_min_level;
    fprintf(fp, "%4" MR_INTEGER_LENGTH_MODIFIER "d ",
        frame_dump_info->MR_sdi_min_level);

    // If we are printing trace data, we need all the horizontal room
    // we can get, and there will not be any repeated lines, so we do not
    // reserve space for the repeat counts.

    if (! include_trace_data) {
        if (num_levels > 1) {
            if (num_levels != frame_dump_info->MR_sdi_num_frames) {
                fprintf(fp, " %3" MR_INTEGER_LENGTH_MODIFIER "ux ",
                    num_levels);
            } else {
                fprintf(fp, " %3" MR_INTEGER_LENGTH_MODIFIER "u* ",
                    num_levels);
            }
        } else {
            fprintf(fp, "%5s ", "");
        }
    }

    MR_maybe_print_call_trace_info(fp, include_trace_data,
        frame_dump_info->MR_sdi_proc_layout,
        frame_dump_info->MR_sdi_base_sp, frame_dump_info->MR_sdi_base_curfr);
    MR_print_proc_id(fp, frame_dump_info->MR_sdi_proc_layout);
    if (MR_strdiff(frame_dump_info->MR_sdi_filename, "")
        && frame_dump_info->MR_sdi_linenumber > 0)
    {
        fprintf(fp, " (%s:%d%s)",
            frame_dump_info->MR_sdi_filename,
            frame_dump_info->MR_sdi_linenumber,
            frame_dump_info->MR_sdi_context_mismatch ? " and others" : "");
    }

    if (include_trace_data) {
        if (MR_strdiff(frame_dump_info->MR_sdi_goal_path, "")) {
            fprintf(fp, " %s", frame_dump_info->MR_sdi_goal_path);
        } else {
            fprintf(fp, " (empty)");
        }
    }

    fprintf(fp, "\n");
}

MR_bool
MR_find_context(const MR_LabelLayout *label, const char **fileptr,
    int *lineptr)
{
    const MR_ProcLayout             *proc;
    const MR_ModuleLayout           *module;
    const MR_ModuleFileLayout       *file_layout;
    int                             i, j;

    proc = label->MR_sll_entry;
    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc)) {
        return MR_FALSE;
    }

    module = proc->MR_sle_module_layout;
    for (i = 0; i < module->MR_ml_filename_count; i++) {
        file_layout = module->MR_ml_module_file_layout[i];
        for (j = 0; j < file_layout->MR_mfl_label_count; j++) {
            if (file_layout->MR_mfl_label_layout[j] == label) {
                *fileptr = file_layout->MR_mfl_filename;
                *lineptr = file_layout->MR_mfl_label_lineno[j];
                return MR_TRUE;
            }
        }
    }

    return MR_FALSE;
}

void
MR_maybe_print_call_trace_info(FILE *fp, MR_bool include_trace_data,
    const MR_ProcLayout *proc_layout, MR_Word *base_sp, MR_Word *base_curfr)
{
    if (include_trace_data) {
        MR_print_call_trace_info(fp, proc_layout, base_sp, base_curfr);
    }
}

// Note that MR_print_call_trace_info is more permissive than its documentation
// in the header file.

void
MR_print_call_trace_info(FILE *fp, const MR_ProcLayout *proc_layout,
    MR_Word *base_sp, MR_Word *base_curfr)
{
    MR_bool             print_details;

    if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
        if (base_sp == NULL) {
            return;
        }
    } else {
        if (base_curfr == NULL) {
            return;
        }
    }

    print_details =
        MR_call_details_are_valid(proc_layout, base_sp, base_curfr);

    if (print_details) {
        unsigned long   event_num;
        unsigned long   call_num;
        unsigned long   depth;

        if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
            event_num = MR_event_num_stackvar(base_sp) + 1;
            call_num = MR_call_num_stackvar(base_sp);
            depth = MR_call_depth_stackvar(base_sp);
        } else {
            event_num = MR_event_num_framevar(base_curfr) + 1;
            call_num = MR_call_num_framevar(base_curfr);
            depth = MR_call_depth_framevar(base_curfr);
        }

        // The code below does has a job that is very similar to the job
        // of the function MR_trace_event_print_internal_report in
        // trace/mercury_trace_internal.c. Any changes here will probably
        // require similar changes there.

        if (MR_standardize_event_details) {
            char    buf[64];    // Plenty big enough.

            // Do not print the context id, since it is not standardized.
            event_num = MR_standardize_event_num(event_num);
            call_num = MR_standardize_call_num(call_num);
            MR_snprintf(buf, 64, "E%lu", event_num);
            fprintf(fp, "%7s ", buf);
            MR_snprintf(buf, 64, "C%lu", call_num);
            fprintf(fp, "%7s ", buf);
            fprintf(fp, "%4lu ", depth);
        } else {
            // Do not print the context id, since it is the same for
            // all the calls in the stack.

            fprintf(fp, "%7lu %7lu %4lu ", event_num, call_num, depth);
        }
    } else {
        // Ensure that the remaining columns line up.
        fprintf(fp, "%21s", "");
    }

#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_TABLE_DEBUG)
  #if 0
    // reenable this code if you need to
    if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
        MR_print_detstackptr(fp, base_sp);
    } else {
        MR_print_nondetstackptr(fp, base_curfr);
    }

    fprintf(fp, " ");
  #endif
#endif
}

void
MR_print_proc_id(FILE *fp, const MR_ProcLayout *entry)
{
    MR_print_proc_id_internal(fp, entry, MR_FALSE, MR_TRUE, MR_FALSE);
}

void
MR_print_pred_id(FILE *fp, const MR_ProcLayout *entry)
{
    MR_print_proc_id_internal(fp, entry, MR_FALSE, MR_FALSE, MR_FALSE);
}

void
MR_print_proc_spec(FILE *fp, const MR_ProcLayout *entry)
{
    MR_print_proc_id_internal(fp, entry, MR_TRUE, MR_TRUE, MR_FALSE);
}

void
MR_print_proc_separate(FILE *fp, const MR_ProcLayout *entry)
{
    MR_print_proc_id_internal(fp, entry, MR_TRUE, MR_TRUE, MR_TRUE);
}

static void
MR_print_proc_id_internal(FILE *fp, const MR_ProcLayout *entry, MR_bool spec,
    MR_bool print_mode, MR_bool separate)
{
    const MR_UserProcId *user;
    const MR_UCIProcId  *uci;

    if (! MR_PROC_LAYOUT_HAS_PROC_ID(entry)) {
        MR_fatal_error("cannot print procedure id without layout");
    }

    if (MR_PROC_LAYOUT_IS_UCI(entry)) {
        uci = &entry->MR_sle_uci;

        if (spec) {
            if (MR_streq(uci->MR_uci_pred_name, "__Unify__")) {
                fprintf(fp, "unif");
            } else if (MR_streq(uci->MR_uci_pred_name, "__Compare__")) {
                fprintf(fp, "comp");
            } else if (MR_streq(uci->MR_uci_pred_name, "__Index__")) {
                fprintf(fp, "indx");
            } else {
                MR_fatal_error("uci procedure is not "
                    "unify, compare or index");
            }

            if (separate) {
                fprintf(fp, " %s %s %ld",
                    uci->MR_uci_type_module,
                    uci->MR_uci_type_name,
                    (long) uci->MR_uci_type_arity);
            } else {
                fprintf(fp, "*%s.%s/%ld",
                    uci->MR_uci_type_module,
                    uci->MR_uci_type_name,
                    (long) uci->MR_uci_type_arity);
            }
        } else {
            fprintf(fp, "%s for %s.%s/%ld",
                uci->MR_uci_pred_name,
                uci->MR_uci_type_module,
                uci->MR_uci_type_name,
                (long) uci->MR_uci_type_arity);
        }

        if (print_mode) {
            if (separate) {
                fprintf(fp, " %ld", (long) uci->MR_uci_mode);
            } else {
                fprintf(fp, "-%ld", (long) uci->MR_uci_mode);
            }
        }

        if (strcmp(uci->MR_uci_type_module,
            uci->MR_uci_def_module) != 0)
        {
            fprintf(fp, " {%s}", uci->MR_uci_def_module);
        }
    } else {
        user = &entry->MR_sle_user;

        if (user->MR_user_pred_or_func == MR_PREDICATE) {
            fprintf(fp, "pred");
        } else if (user->MR_user_pred_or_func == MR_FUNCTION) {
            fprintf(fp, "func");
        } else {
            MR_fatal_error("procedure is not pred or func");
        }

        if (separate) {
            fprintf(fp, " ");
        } else if (spec) {
            fprintf(fp, "*");
        } else {
            fprintf(fp, " ");
        }

        if (separate) {
            fprintf(fp, "%s %s %ld",
                user->MR_user_decl_module,
                user->MR_user_name,
                (long) MR_sle_user_adjusted_arity(entry));
        } else {
            fprintf(fp, "%s.%s/%ld",
                user->MR_user_decl_module,
                user->MR_user_name,
                (long) MR_sle_user_adjusted_arity(entry));
        }

        if (print_mode) {
            if (separate) {
                fprintf(fp, " %ld", (long) user->MR_user_mode);
            } else {
                fprintf(fp, "-%ld", (long) user->MR_user_mode);
            }
        }

        if (!spec && strcmp(user->MR_user_decl_module,
            user->MR_user_def_module) != 0)
        {
            fprintf(fp, " {%s}", user->MR_user_def_module);
        }
    }

    if (! spec && print_mode) {
        fprintf(fp, " (%s)", MR_detism_names[entry->MR_sle_detism]);
    }
}

void
MR_print_proc_id_trace_and_context(FILE *fp, MR_bool include_trace_data,
    MR_ContextPosition pos, MR_UserEventContext user_event_context,
    const MR_ProcLayout *proc_layout, const char *maybe_user_event_name,
    MR_Word *base_sp, MR_Word *base_curfr,
    const char *path, const char *filename, int lineno, MR_bool print_parent,
    const char *parent_filename, int parent_lineno, int indent)
{
    MR_bool             print_context;
    MR_bool             print_proc_id;

    if (maybe_user_event_name != NULL) {
        switch (user_event_context) {
            case MR_USER_EVENT_CONTEXT_NONE:
                print_context = MR_FALSE;
                print_proc_id = MR_FALSE;
                break;

            case MR_USER_EVENT_CONTEXT_FILE:
                print_context = MR_TRUE;
                print_proc_id = MR_FALSE;
                break;

            case MR_USER_EVENT_CONTEXT_PROC:
                print_context = MR_FALSE;
                print_proc_id = MR_TRUE;
                break;

            case MR_USER_EVENT_CONTEXT_FULL:
            default:
                print_context = MR_TRUE;
                print_proc_id = MR_TRUE;
                break;
        }

        print_parent = MR_FALSE;
    } else {
        print_context = MR_TRUE;
        print_proc_id = MR_TRUE;
    }

    switch (pos) {
        case MR_CONTEXT_NOWHERE:
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data,
                proc_layout, base_sp, base_curfr);
            MR_maybe_print_proc_id(fp, print_proc_id, proc_layout, path);
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_BEFORE:
            MR_maybe_print_context(fp, print_context, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_FALSE,
                parent_filename, parent_lineno);
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data,
                proc_layout, base_sp, base_curfr);
            MR_maybe_print_proc_id(fp, print_proc_id, proc_layout, path);
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_AFTER:
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data,
                proc_layout, base_sp, base_curfr);
            MR_maybe_print_proc_id(fp, print_proc_id, proc_layout, path);
            MR_maybe_print_context(fp, print_context, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_FALSE,
                parent_filename, parent_lineno);
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_PREVLINE:
            MR_maybe_print_context(fp, print_context, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_TRUE,
                parent_filename, parent_lineno);
            fprintf(fp, "\n%*s ", indent, "");
            MR_maybe_print_call_trace_info(fp, include_trace_data,
                proc_layout, base_sp, base_curfr);
            MR_maybe_print_proc_id(fp, print_proc_id, proc_layout, path);
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_NEXTLINE:
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data,
                proc_layout, base_sp, base_curfr);
            MR_maybe_print_proc_id(fp, print_proc_id, proc_layout, path);
            fprintf(fp, "\n%*s", indent, "");
            MR_maybe_print_context(fp, print_context, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_TRUE,
                parent_filename, parent_lineno);
            fprintf(fp, "\n");
            break;

        default:
            MR_fatal_error("invalid MR_ContextPosition");
    }
}

static void
MR_maybe_print_proc_id(FILE *fp, MR_bool print_proc_id,
    const MR_ProcLayout *proc_layout, const char *path)
{
    if (print_proc_id) {
        MR_print_proc_id(fp, proc_layout);
        if (strlen(path) > 0) {
            fprintf(fp, " %s", path);
        }
    }
}

static void
MR_maybe_print_context(FILE *fp, MR_bool print_context, const char *filename,
    int lineno)
{
    if (print_context && MR_strdiff(filename, "") && lineno != 0) {
        fprintf(fp, " %s:%d", filename, lineno);
    }
}

static void
MR_maybe_print_parent_context(FILE *fp, MR_bool print_parent, MR_bool verbose,
    const char *filename, int lineno)
{
    if (print_parent && MR_strdiff(filename, "") && lineno != 0) {
        if (verbose) {
            fprintf(fp, " (from %s:%d)", filename, lineno);
        } else {
            fprintf(fp, " (%s:%d)", filename, lineno);
        }
    }
}

static  MR_bool
MR_call_details_are_valid(const MR_ProcLayout *proc_layout, MR_Word *base_sp,
    MR_Word *base_curfr)
{
    if (MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc_layout)) {
        MR_Integer maybe_from_full = proc_layout->MR_sle_maybe_from_full;
        if (maybe_from_full > 0) {
            // For procedures compiled with shallow tracing, the details
            // will be valid only if the value of MR_from_full saved in
            // the appropriate stack slot was MR_TRUE.

            if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
                return MR_based_stackvar(base_sp, maybe_from_full);
            } else {
                return MR_based_framevar(base_curfr, maybe_from_full);
            }
        } else {
            return MR_TRUE;
        }
    } else {
        return MR_FALSE;
    }
}

static MR_bool
MR_call_is_before_event_or_seq(MR_FindFirstCallSeqOrEvent seq_or_event,
    MR_Unsigned seq_no_or_event_no,
    const MR_ProcLayout *proc_layout, MR_Word *base_sp, MR_Word *base_curfr)
{
    MR_Unsigned     call_event_num;
    MR_Unsigned     call_seq_num;

    if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
        if (base_sp == NULL) {
            return MR_FALSE;
        }
    } else {
        if (base_curfr == NULL) {
            return MR_FALSE;
        }
    }

    if (MR_call_details_are_valid(proc_layout, base_sp, base_curfr)) {
        if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
            call_event_num = MR_event_num_stackvar(base_sp) + 1;
            call_seq_num = MR_call_num_stackvar(base_sp);
        } else {
            call_event_num = MR_event_num_framevar(base_curfr) + 1;
            call_seq_num = MR_call_num_framevar(base_curfr);
        }
        if (seq_or_event == MR_FIND_FIRST_CALL_BEFORE_EVENT) {
            return call_event_num <= seq_no_or_event_no;
        } else if (seq_or_event == MR_FIND_FIRST_CALL_BEFORE_SEQ) {
            return call_seq_num <= seq_no_or_event_no;
        } else {
            MR_fatal_error("Unknown MR_FindFirstCallSeqOrEvent");
        }
    } else {
        return MR_FALSE;
    }
}

int
MR_find_first_call_less_eq_seq_or_event(
    MR_FindFirstCallSeqOrEvent seq_or_event,
    MR_Unsigned seq_no_or_event_no, const MR_LabelLayout *label_layout,
    MR_Word *det_stack_pointer, MR_Word *current_frame, const char **problem)
{
    MR_StackWalkStepResult  result;
    const MR_LabelLayout    *cur_label_layout;
    MR_Word                 *stack_trace_sp;
    MR_Word                 *stack_trace_curfr;
    int                     ancestor_level;
    MR_Unsigned             reused_frames;

    MR_do_init_modules();

    stack_trace_sp = det_stack_pointer;
    stack_trace_curfr = current_frame;

    cur_label_layout = label_layout;

    ancestor_level = 0;
    while (cur_label_layout != NULL) {
        if (MR_call_is_before_event_or_seq(seq_or_event, seq_no_or_event_no,
                cur_label_layout->MR_sll_entry, stack_trace_sp,
                stack_trace_curfr))
        {
            return ancestor_level;
        }

        result = MR_stack_walk_step(cur_label_layout->MR_sll_entry,
            &cur_label_layout, &stack_trace_sp, &stack_trace_curfr,
            &reused_frames, problem);

        if (result != MR_STEP_OK) {
            return -1;
        }

        ancestor_level += 1 + reused_frames;
    } while (cur_label_layout != NULL);

    *problem = "no more stack";
    return -1;
}

// The different Mercury determinisms are internally represented by integers.
// This array gives the correspondence with the internal representation and
// the names that are usually used to denote determinisms.

const char * MR_detism_names[] = {
    "failure",      // 0
    "",             // 1
    "semidet",      // 2
    "nondet",       // 3
    "erroneous",    // 4
    "",             // 5
    "det",          // 6
    "multi",        // 7
    "",             // 8
    "",             // 9
    "cc_nondet",    // 10
    "",             // 11
    "",             // 12
    "",             // 13
    "cc_multi"      // 14
};
