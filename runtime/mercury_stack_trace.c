/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_stack_trace.c - implements stack traces.
**
** Main authors: Tyson Dowd (trd), Zoltan Somogyi (zs).
*/

#include "mercury_imp.h"
#include "mercury_stack_trace.h"
#include "mercury_stack_layout.h"
#include "mercury_debug.h"
#include "mercury_array_macros.h"
#include <stdio.h>

#ifndef MR_HIGHLEVEL_CODE

static  const char  *MR_step_over_nondet_frame(FILE *fp,
                        int level_number, MR_Word *fr);
static  MR_bool     MR_find_matching_branch(MR_Word *fr, int *branch_ptr);
static  void        MR_record_temp_redoip(MR_Word *fr);
static  MR_Code     *MR_find_temp_redoip(MR_Word *fr);
static  void        MR_erase_temp_redoip(MR_Word *fr);

#endif  /* !MR_HIGHLEVEL_CODE */

static  void        MR_dump_stack_record_init(MR_bool include_trace_data,
                        MR_bool include_contexts);
static  void        MR_dump_stack_record_frame(FILE *fp,
                        const MR_Label_Layout *label_layout,
                        MR_Word *base_sp, MR_Word *base_curfr, 
                        MR_Print_Stack_Record print_stack_record);
static  void        MR_dump_stack_record_flush(FILE *fp, 
                        MR_Print_Stack_Record print_stack_record);

static  void        MR_print_proc_id_internal(FILE *fp,
                        const MR_Proc_Layout *entry, MR_bool spec);

static  void        MR_maybe_print_context(FILE *fp,
                        const char *filename, int lineno);
static  void        MR_maybe_print_parent_context(FILE *fp,
                        MR_bool print_parent, MR_bool verbose,
                        const char *filename, int lineno);

/* see comments in mercury_stack_trace.h */
MR_Code *MR_stack_trace_bottom;
MR_Word *MR_nondet_stack_trace_bottom;

void
MR_dump_stack(MR_Code *success_pointer, MR_Word *det_stack_pointer,
    MR_Word *current_frame, MR_bool include_trace_data)
{
#ifndef MR_STACK_TRACE
    fprintf(stderr, "Stack dump not available in this grade.\n");
#else

    const MR_Internal       *label;
    const MR_Label_Layout   *layout;
    const char              *result;

    fprintf(stderr, "Stack dump follows:\n");

    MR_do_init_modules();
    label = MR_lookup_internal_by_addr(success_pointer);
    if (label == NULL) {
        fprintf(stderr, "internal label not found\n");
    } else {
        layout = label->i_layout;
        result = MR_dump_stack_from_layout(stderr, layout,
            det_stack_pointer, current_frame, include_trace_data,
            MR_TRUE, &MR_dump_stack_record_print);

        if (result != NULL) {
            fprintf(stderr, "%s\n", result);
        }
    }
#endif
}

const char *
MR_dump_stack_from_layout(FILE *fp, const MR_Label_Layout *label_layout,
    MR_Word *det_stack_pointer, MR_Word *current_frame,
    MR_bool include_trace_data, MR_bool include_contexts,
    MR_Print_Stack_Record print_stack_record)
{
    MR_Stack_Walk_Step_Result       result;
    const MR_Proc_Layout            *entry_layout;
    const MR_Label_Layout           *cur_label_layout;
    const MR_Label_Layout           *prev_label_layout;
    const char                      *problem;
    MR_Word                         *stack_trace_sp;
    MR_Word                         *stack_trace_curfr;
    MR_Word                         *old_trace_sp;
    MR_Word                         *old_trace_curfr;

    MR_do_init_modules();
    MR_dump_stack_record_init(include_trace_data, include_contexts);

    stack_trace_sp = det_stack_pointer;
    stack_trace_curfr = current_frame;

    cur_label_layout = label_layout;

    do {
        entry_layout = cur_label_layout->MR_sll_entry;
        prev_label_layout = cur_label_layout;

        old_trace_sp    = stack_trace_sp;
        old_trace_curfr = stack_trace_curfr;

        result = MR_stack_walk_step(entry_layout, &cur_label_layout,
                        &stack_trace_sp, &stack_trace_curfr, &problem);
        if (result == MR_STEP_ERROR_BEFORE) {
            MR_dump_stack_record_flush(fp, print_stack_record);
            return problem;
        } else if (result == MR_STEP_ERROR_AFTER) {
            MR_dump_stack_record_frame(fp, prev_label_layout,
                old_trace_sp, old_trace_curfr, print_stack_record);

                MR_dump_stack_record_flush(fp, print_stack_record);
                return problem;
        } else {
                MR_dump_stack_record_frame(fp, prev_label_layout,
                    old_trace_sp, old_trace_curfr, print_stack_record);
        }
    } while (cur_label_layout != NULL);

    MR_dump_stack_record_flush(fp, print_stack_record);
    return NULL;
}

const MR_Label_Layout *
MR_find_nth_ancestor(const MR_Label_Layout *label_layout, int ancestor_level,
    MR_Word **stack_trace_sp, MR_Word **stack_trace_curfr,
    const char **problem)
{
    MR_Stack_Walk_Step_Result       result;
    const MR_Label_Layout           *return_label_layout;
    int                             i;

    if (ancestor_level < 0) {
        *problem = "no such stack frame";
        return NULL;
    }

    MR_do_init_modules();
    *problem = NULL;
    for (i = 0; i < ancestor_level && label_layout != NULL; i++) {
        result = MR_stack_walk_step(label_layout->MR_sll_entry,
                        &return_label_layout, stack_trace_sp,
                        stack_trace_curfr, problem);

        if (result != MR_STEP_OK) {
            /* *problem has already been filled in */
            return NULL;
        }

        label_layout = return_label_layout;
    }

    if (label_layout == NULL && *problem == NULL) {
        *problem = "not that many ancestors";
    }

    return label_layout;
}

MR_Stack_Walk_Step_Result
MR_stack_walk_step(const MR_Proc_Layout *entry_layout,
    const MR_Label_Layout **return_label_layout,
    MR_Word **stack_trace_sp_ptr, MR_Word **stack_trace_curfr_ptr,
    const char **problem_ptr)
{
    MR_Internal             *label;
    MR_Long_Lval            location;
    MR_Long_Lval_Type       type;
    int                     number;
    int                     determinism;
    MR_Code                 *success;

    *return_label_layout = NULL;

    determinism = entry_layout->MR_sle_detism;
    if (determinism < 0) {
        /*
        ** This means we have reached some handwritten code that has
        ** no further information about the stack frame.
        */

        *problem_ptr = "reached procedure with no stack trace info";
        return MR_STEP_ERROR_BEFORE;
    }

    if (MR_DETISM_DET_STACK(determinism)) {
        location = entry_layout->MR_sle_succip_locn;
        type = MR_LONG_LVAL_TYPE(location);
        number = MR_LONG_LVAL_NUMBER(location);

        if (type != MR_LONG_LVAL_TYPE_STACKVAR) {
            *problem_ptr = "can only handle stackvars";
            return MR_STEP_ERROR_AFTER;
        }

        success = (MR_Code *) MR_based_stackvar(*stack_trace_sp_ptr, number);
        *stack_trace_sp_ptr = *stack_trace_sp_ptr -
            entry_layout->MR_sle_stack_slots;
    } else {
            success = MR_succip_slot(*stack_trace_curfr_ptr);
            *stack_trace_curfr_ptr = MR_succfr_slot(*stack_trace_curfr_ptr);
    }

    if (success == MR_stack_trace_bottom) {
        return MR_STEP_OK;
    }

    label = MR_lookup_internal_by_addr(success);
    if (label == NULL) {
        *problem_ptr = "reached unknown label";
        return MR_STEP_ERROR_AFTER;
    }

    if (label->i_layout == NULL) {
        *problem_ptr = "reached label with no stack layout info";
        return MR_STEP_ERROR_AFTER;
    }

    *return_label_layout = label->i_layout;
    return MR_STEP_OK;
}

/**************************************************************************/

void
MR_dump_nondet_stack(FILE *fp, MR_Word *base_maxfr)
{
#ifndef MR_HIGHLEVEL_CODE

    MR_dump_nondet_stack_from_layout(fp, base_maxfr, NULL, NULL, NULL);

#else   /* !MR_HIGHLEVEL_CODE */

    MR_fatal_error("MR_dump_nondet_stack in high level C grade");

#endif  /* !MR_HIGHLEVEL_CODE */
}

#ifdef MR_HIGHLEVEL_CODE

void
MR_dump_nondet_stack_from_layout(FILE *fp, MR_Word *base_maxfr,
    const MR_Label_Layout *top_layout, MR_Word *base_sp, MR_Word *base_curfr)
{
    MR_fatal_error("MR_dump_nondet_stack_from_layout in high level grade");
}

#else   /* !MR_HIGHLEVEL_CODE */

/*
** Detailed nondet stack dumps include the values of the local variables in
** each nondet stack frame. To find out what variables are live in each frame,
** we must know through what label control will go back to that frame, so we
** can use that label's layout structure.
**
** Control can reach a frame through one of three means.
**
** - It may already be there. This is true only for the frame defined by the
**   arguments of MR_dump_nondet_stack_from_layout, and the layout structure
**   we use is also among the arguments.
**
** - It may get there by backtracking. In this case, the layout structure to
**   use is the one associated with the frame's redoip structure.
**
** - It may get there by returning from a call. In this case, the layout
**   structure to use is the one associated with the return label.
**
** We distinguish the last two cases by keeping an array of nondet stack frames
** that will be returned to from other nondet stack frames higher up, possibly
** via other procedures that live on the det stack. Procedures that live on the
** det stack may occur in the call chain of the currently active procedure, but
** they may not occur in side branches of the search tree: a model_non call may
** leave stack frames only on the nondet stack when it exits.
**
** When we find the top frame of a side branch, we don't know what the value
** of the det stack pointer sp was when execution created that nondet stack
** frame. This means that if that an ancestor of that nondet stack frame lives
** on the det stack, we cannot find the address of the stack frame of the
** ancestor. However, due that above invariant the only such ancestor a nondet
** stack frame on a side branch can have is an ancestor it shares with the
** currently executing call, and for the ancestors of the currently executing
** call we *do* know the values of sp.
**
** The MR_nondet_branch_infos array has one entry for each nondet branch; this
** entry gives the details of the next frame on the nondet stack from that
** branch. The branch_curfr field is valid for all entries and all entries in
** the array have distinct values for this field. The branch_sp field is valid
** only for the entry on the main branch; for all other entries, in contains
** NULL. The branch_layout field gives the address of the layout structure of
** the return address through which control will return to that frame. (Frames
** to which control returns via backtracking never get put into this array,
** only their ancestors do.) The branch_topfr field gives the address of the
** top frame in the branch; we print this because it makes the stack dump
** easier to interpret.
**
** The MR_nondet_branch_infos array grows when we find the tops of new side
** branches and shrinks when we find frames that created side branches.
*/

typedef struct
{
    MR_Word                 *branch_sp;
    MR_Word                 *branch_curfr;
    const MR_Label_Layout   *branch_layout;
    MR_Word                 *branch_topfr;
} MR_Nondet_Branch_Info;

static MR_Nondet_Branch_Info    *MR_nondet_branch_infos = NULL;
static int                      MR_nondet_branch_info_next = 0;
static int                      MR_nondet_branch_info_max = 0;

#define MR_INIT_NONDET_BRANCH_ARRAY_SIZE        10

void
MR_dump_nondet_stack_from_layout(FILE *fp, MR_Word *base_maxfr,
    const MR_Label_Layout *top_layout, MR_Word *base_sp, MR_Word *base_curfr)
{
    int             frame_size;
    int             level_number;
    MR_bool         print_vars;
    const char      *problem;

    MR_do_init_modules();

    MR_nondet_branch_info_next = 0;
    if (top_layout != NULL && base_sp != NULL && base_curfr != NULL
        && MR_address_of_trace_browse_all_on_level != NULL)
    {
        print_vars = MR_TRUE;
        MR_ensure_room_for_next(MR_nondet_branch_info, MR_Nondet_Branch_Info,
                MR_INIT_NONDET_BRANCH_ARRAY_SIZE);
        MR_nondet_branch_infos[0].branch_sp = base_sp;
        MR_nondet_branch_infos[0].branch_curfr = base_curfr;
        MR_nondet_branch_infos[0].branch_layout = top_layout;
        MR_nondet_branch_infos[0].branch_topfr = base_curfr;
        MR_nondet_branch_info_next++;
    } else {
        print_vars = MR_FALSE;
    }

    /*
    ** The comparison operator in the condition of the while loop
    ** should be >= if you want the trace to include the bottom frame
    ** created by mercury_wrapper.c (whose redoip/redofr field can be
    ** hijacked by other code), and > if you don't want the bottom
    ** frame to be included.
    */

    level_number = 0;
    while (base_maxfr >= MR_nondet_stack_trace_bottom) {
        frame_size = base_maxfr - MR_prevfr_slot(base_maxfr);
        if (frame_size == MR_NONDET_TEMP_SIZE) {
            MR_print_nondstackptr(fp, base_maxfr);
            fprintf(fp, ": temp\n");
            fprintf(fp, " redoip: ");
            MR_printlabel(fp, MR_redoip_slot(base_maxfr));
            fprintf(fp, " redofr: ");
            MR_print_nondstackptr(fp, MR_redofr_slot(base_maxfr));
            fprintf(fp, " \n");

            if (print_vars) {
                MR_record_temp_redoip(base_maxfr);
            }
        } else if (frame_size == MR_DET_TEMP_SIZE) {
            MR_print_nondstackptr(fp, base_maxfr);
            fprintf(fp, ": temp\n");
            fprintf(fp, " redoip: ");
            MR_printlabel(fp, MR_redoip_slot(base_maxfr));
            fprintf(fp, " redofr: ");
            MR_print_nondstackptr(fp, MR_redofr_slot(base_maxfr));
            fprintf(fp, " \n");
            fprintf(fp, " detfr: ");
            MR_print_detstackptr(fp, MR_tmp_detfr_slot(base_maxfr));
            fprintf(fp, " \n");
        } else {
            MR_print_nondstackptr(fp, base_maxfr);
            fprintf(fp, ": ordinary, %d words\n", frame_size);
            fprintf(fp, " redoip: ");
            MR_printlabel(fp, MR_redoip_slot(base_maxfr));
            fprintf(fp, " redofr: ");
            MR_print_nondstackptr(fp, MR_redofr_slot(base_maxfr));
            fprintf(fp, " \n");
            fprintf(fp, " succip: ");
            MR_printlabel(fp, MR_succip_slot(base_maxfr));
            fprintf(fp, " succfr: ");
            MR_print_nondstackptr(fp, MR_succfr_slot(base_maxfr));
            fprintf(fp, " \n");

            level_number++;
            if (print_vars && base_maxfr > MR_nondet_stack_trace_bottom) {
                problem = MR_step_over_nondet_frame(fp, level_number,
                            base_maxfr);
                if (problem != NULL) {
                    fprintf(fp, "%s\n", problem);
                    return;
                }
            }
        }

        base_maxfr = MR_prevfr_slot(base_maxfr);
    }
}

static const char *
MR_step_over_nondet_frame(FILE *fp, int level_number, MR_Word *fr)
{
    MR_Stack_Walk_Step_Result       result;
    MR_Determinism                      determinism;
    const MR_Internal               *internal;
    int                             branch;
    int                             last;
    MR_Word                         *base_sp;
    MR_Word                         *base_curfr;
    MR_Word                         *topfr;
    const MR_Label_Layout           *label_layout;
    const MR_Proc_Layout            *proc_layout;
    const char                      *problem;

    if (MR_find_matching_branch(fr, &branch)) {
        base_sp = MR_nondet_branch_infos[branch].branch_sp;
        base_curfr = MR_nondet_branch_infos[branch].branch_curfr;
        label_layout = MR_nondet_branch_infos[branch].branch_layout;
        topfr = MR_nondet_branch_infos[branch].branch_topfr;
        if (base_sp == NULL) {
            fprintf(fp, " internal frame on nondet side branch ");
            MR_printnondstackptr(topfr);
            fprintf(fp, "\n");
        } else {
            fprintf(fp, " on main nondet branch ");
            MR_printnondstackptr(topfr);
            fprintf(fp, "\n");
        }
        (*MR_address_of_trace_browse_all_on_level)(fp, label_layout,
                base_sp, base_curfr, level_number, MR_TRUE);
        MR_erase_temp_redoip(fr);
        proc_layout = label_layout->MR_sll_entry;

        /*
        ** Step past all other detstack-living
        ** ancestors on the main branch.
        */
        while (MR_TRUE) {
            result = MR_stack_walk_step(proc_layout, &label_layout,
                            &base_sp, &base_curfr, &problem);

            if (result != MR_STEP_OK) {
                return problem;
            }

            if (label_layout == NULL) {
                return NULL;
            }

            proc_layout = label_layout->MR_sll_entry;
            determinism = proc_layout->MR_sle_detism;

            if (! MR_DETISM_DET_STACK(determinism)) {
                /*
                ** We will handle this call to a model_non
                ** procedure when the sweep in
                ** MR_dump_nondet_stack_from_layout reaches it.
                ** For now, we only put into the table.
                */
                break;
            } else if (base_sp == NULL) {
                /*
                ** We are on a side branch, and we must have
                ** arrived at the common ancestor of the side
                ** branch and the main branch.
                */
                return NULL;
            }
        }

        last = MR_nondet_branch_info_next - 1;
        MR_nondet_branch_infos[branch].branch_layout =
                MR_nondet_branch_infos[last].branch_layout;
        MR_nondet_branch_infos[branch].branch_sp =
                MR_nondet_branch_infos[last].branch_sp;
        MR_nondet_branch_infos[branch].branch_curfr =
                MR_nondet_branch_infos[last].branch_curfr;
        MR_nondet_branch_infos[branch].branch_topfr =
                MR_nondet_branch_infos[last].branch_topfr;
        MR_nondet_branch_info_next--;
    } else {
        MR_Code *redoip;

        redoip = MR_find_temp_redoip(fr);
        if (redoip == NULL) {
            redoip = MR_redoip_slot(fr);
        }

        internal = MR_lookup_internal_by_addr(redoip);
        if (internal == NULL || internal->i_layout == NULL) {
            return "cannot find redoip label's layout structure";
        }

        label_layout = internal->i_layout;
        fprintf(fp, " top frame of a nondet side branch ");
        MR_printnondstackptr(fr);
        fprintf(fp, "\n");
        (*MR_address_of_trace_browse_all_on_level)(fp, label_layout,
                NULL, fr, level_number, MR_TRUE);
        MR_erase_temp_redoip(fr);

        /*
        ** Passing a NULL base_sp to MR_stack_walk_step is OK because
        ** the procedure whose stack frame we are now looking at uses
        ** the nondet stack. Putting a NULL base_sp into the table
        ** is OK because all the ancestors of this procedure that are
        ** not also ancestors of the call currently being executed
        ** must also use the nondet stack. This is a consequence of the
        ** invariant that model_non calls leave the det stack
        ** unchanged.
        */

        base_sp = NULL;
        base_curfr = fr;
        proc_layout = label_layout->MR_sll_entry;
        topfr = fr;
        result = MR_stack_walk_step(proc_layout, &label_layout,
                        &base_sp, &base_curfr, &problem);

        if (result != MR_STEP_OK) {
            return problem;
        }

        if (label_layout == NULL) {
            return NULL;
        }

        proc_layout = label_layout->MR_sll_entry;
        determinism = proc_layout->MR_sle_detism;
        if (MR_DETISM_DET_STACK(determinism)) {
            /*
            ** We must have found the common ancestor of the
            ** procedure call whose variables we just printed
            ** and the call currently being executed. While this
            ** common ancestor must include model_non code, this
            ** may be inside a commit in a procedure that lives on
            ** the det stack. If that is the case, the common
            ** ancestor must not be put into MR_nondet_branch_info.
            */

            return NULL;
        }
    }

    if (! MR_find_matching_branch(base_curfr, &branch)) {
        MR_ensure_room_for_next(MR_nondet_branch_info, MR_Nondet_Branch_Info,
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

/*
** The contents of a nondet stack frame which control will enter via
** backtracking is described by the layout structure of the label at which
** execution will resume inside the procedure. This need not be the label in
** the redoip slot in the procedure's ordinary stack frame; if the procedure
** created any temporary nondet stack frames, it will be the label in the
** redoip slot of the top temporary nondet stack frame created by the
** procedure.
**
** We record the contents of topmost temp frames as go past them, and erase the
** records as we go past the ordinary frames to which they refer.
*/

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
            /* this is not the top temp frame for this call */
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

static MR_Code *
MR_find_temp_redoip(MR_Word *fr)
{
    int     slot;

    for (slot = 0; slot < MR_temp_frame_info_next; slot++) {
        if (fr == MR_temp_frame_infos[slot].temp_redofr) {
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

#endif  /* !MR_HIGHLEVEL_CODE */

/**************************************************************************/

static  const MR_Proc_Layout    *prev_entry_layout;
static  int                     prev_entry_layout_count;
static  int                     prev_entry_start_level;
static  MR_Word                 *prev_entry_base_sp;
static  MR_Word                 *prev_entry_base_curfr;
static  const char              *prev_entry_filename;
static  int                     prev_entry_linenumber;
static  const char              *prev_entry_goal_path;
static  MR_bool                 prev_entry_context_mismatch;
static  int                     current_level;
static  MR_bool                 trace_data_enabled;
static  MR_bool                 contexts_enabled;

static void
MR_dump_stack_record_init(MR_bool include_trace_data, MR_bool include_contexts)
{
    prev_entry_layout = NULL;
    prev_entry_layout_count = 0;
    prev_entry_start_level = 0;
    current_level = 0;
    contexts_enabled = include_contexts;
    trace_data_enabled = include_trace_data;
}

static void
MR_dump_stack_record_frame(FILE *fp, const MR_Label_Layout *label_layout,
    MR_Word *base_sp, MR_Word *base_curfr,
    MR_Print_Stack_Record print_stack_record)
{
    const MR_Proc_Layout    *entry_layout;
    const char              *filename;
    int                     linenumber;
    MR_bool                 must_flush;

    entry_layout = label_layout->MR_sll_entry;
    if (! MR_find_context(label_layout, &filename, &linenumber)
        || ! contexts_enabled)
    {
        filename = "";
        linenumber = 0;
    }

    /*
    ** We cannot merge two calls if they are to different
    ** procedures.
    **
    ** We cannot merge two calls even to the same procedure
    ** if we are printing trace data, since this will differ
    ** between the calls.
    **
    ** Note that it is not possible for two calls to the same
    ** procedure to differ on whether the procedure has trace
    ** layout data or not.
    */
    must_flush = (entry_layout != prev_entry_layout) || trace_data_enabled;

    if (must_flush) {
        MR_dump_stack_record_flush(fp, print_stack_record);

        prev_entry_layout = entry_layout;
        prev_entry_layout_count = 1;
        prev_entry_start_level = current_level;
        prev_entry_base_sp = base_sp;
        prev_entry_base_curfr = base_curfr;
        prev_entry_filename = filename;
        prev_entry_linenumber = linenumber;
        prev_entry_goal_path = MR_label_goal_path(label_layout);
        prev_entry_context_mismatch = MR_FALSE;
    } else {
        prev_entry_layout_count++;
        if (prev_entry_filename != filename
            || prev_entry_linenumber != linenumber)
        {
            prev_entry_context_mismatch = MR_TRUE;
        }
    }

    current_level++;
}

static void
MR_dump_stack_record_flush(FILE *fp, MR_Print_Stack_Record print_stack_record)
{
    if (prev_entry_layout != NULL) {
        print_stack_record(fp, prev_entry_layout,
            prev_entry_layout_count, prev_entry_start_level,
            prev_entry_base_sp, prev_entry_base_curfr,
            prev_entry_filename, prev_entry_linenumber,
            prev_entry_goal_path, prev_entry_context_mismatch);
    }
}

void
MR_dump_stack_record_print(FILE *fp, const MR_Proc_Layout *entry_layout,
    int count, int start_level, MR_Word *base_sp, MR_Word *base_curfr,
    const char *filename, int linenumber, const char *goal_path,
    MR_bool context_mismatch)
{
    fprintf(fp, "%4d ", start_level);

    if (count > 1) {
        fprintf(fp, " %3d* ", count);
    } else if (! trace_data_enabled) {
        fprintf(fp, "%5s ", "");
    } else {
        /*
        ** If we are printing trace data, we need all the horizonal
        ** room we can get, and there will not be any repeated lines,
        ** so we don't reserve space for the repeat counts.
        */
    }

    MR_maybe_print_call_trace_info(fp, trace_data_enabled, entry_layout,
            base_sp, base_curfr);
    MR_print_proc_id(fp, entry_layout);
    if (MR_strdiff(filename, "") && linenumber > 0) {
        fprintf(fp, " (%s:%d%s)", filename, linenumber,
            context_mismatch ? " and others" : "");
    }

    if (trace_data_enabled) {
        if (MR_strdiff(goal_path, "")) {
            fprintf(fp, " %s", goal_path);
        } else {
            fprintf(fp, " (empty)");
        }
    }

    fprintf(fp, "\n");
}

MR_bool
MR_find_context(const MR_Label_Layout *label, const char **fileptr,
    int *lineptr)
{
    const MR_Proc_Layout            *proc;
    const MR_Module_Layout          *module;
    const MR_Module_File_Layout     *file_layout;
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
    const MR_Proc_Layout *entry, MR_Word *base_sp, MR_Word *base_curfr)
{
    if (include_trace_data) {
        MR_print_call_trace_info(fp, entry, base_sp, base_curfr);
    }
}

/*
** Note that MR_print_call_trace_info is more permissive than its documentation
** in the header file.
*/

void
MR_print_call_trace_info(FILE *fp, const MR_Proc_Layout *entry,
    MR_Word *base_sp, MR_Word *base_curfr)
{
    MR_bool print_details;

    if (base_sp == NULL || base_curfr == NULL) {
        return;
    }

    if (MR_PROC_LAYOUT_HAS_EXEC_TRACE(entry)) {
        MR_Integer maybe_from_full = entry->MR_sle_maybe_from_full;
        if (maybe_from_full > 0) {
            /*
            ** For procedures compiled with shallow
            ** tracing, the details will be valid only
            ** if the value of MR_from_full saved in
            ** the appropriate stack slot was MR_TRUE.
            */
            if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
                print_details = MR_based_stackvar(base_sp, maybe_from_full);
            } else {
                print_details = MR_based_framevar(base_curfr, maybe_from_full);
            }
        } else {
            /*
            ** For procedures compiled with full tracing,
            ** we can always print out the details.
            */
            print_details = MR_TRUE;
        }
    } else {
        print_details = MR_FALSE;
    }

    if (print_details) {
        if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
            fprintf(fp, "%7lu %7lu %4lu ",
                (unsigned long) MR_event_num_stackvar(base_sp) + 1,
                (unsigned long) MR_call_num_stackvar(base_sp),
                (unsigned long) MR_call_depth_stackvar(base_sp));
        } else {
            fprintf(fp, "%7lu %7lu %4lu ",
                (unsigned long) MR_event_num_framevar(base_curfr) + 1,
                (unsigned long) MR_call_num_framevar(base_curfr),
                (unsigned long) MR_call_depth_framevar(base_curfr));
        }
    } else {
        /* ensure that the remaining columns line up */
        fprintf(fp, "%21s", "");
    }
}

void
MR_print_proc_id(FILE *fp, const MR_Proc_Layout *entry)
{
    MR_print_proc_id_internal(fp, entry, MR_FALSE);
}

void
MR_print_proc_spec(FILE *fp, const MR_Proc_Layout *entry)
{
    MR_print_proc_id_internal(fp, entry, MR_TRUE);
}

static void
MR_print_proc_id_internal(FILE *fp, const MR_Proc_Layout *entry, MR_bool spec)
{
    if (! MR_PROC_LAYOUT_HAS_PROC_ID(entry)) {
        MR_fatal_error("cannot print procedure id without layout");
    }

    if (MR_PROC_LAYOUT_COMPILER_GENERATED(entry)) {
        if (spec) {
            MR_fatal_error("cannot generate specifications "
                "for compiler generated procedures");
        }

        fprintf(fp, "%s for %s:%s/%ld-%ld",
            entry->MR_sle_comp.MR_comp_pred_name,
            entry->MR_sle_comp.MR_comp_type_module,
            entry->MR_sle_comp.MR_comp_type_name,
            (long) entry->MR_sle_comp.MR_comp_arity,
            (long) entry->MR_sle_comp.MR_comp_mode);

        if (strcmp(entry->MR_sle_comp.MR_comp_type_module,
            entry->MR_sle_comp.MR_comp_def_module) != 0)
        {
            fprintf(fp, " {%s}", entry->MR_sle_comp.MR_comp_def_module);
        }
    } else {
        if (entry->MR_sle_user.MR_user_pred_or_func == MR_PREDICATE) {
            fprintf(fp, "pred");
        } else if (entry->MR_sle_user.MR_user_pred_or_func == MR_FUNCTION) {
            fprintf(fp, "func");
        } else {
            MR_fatal_error("procedure is not pred or func");
        }

        if (spec) {
            fprintf(fp, "*");
        } else {
            fprintf(fp, " ");
        }

        fprintf(fp, "%s:%s/%ld-%ld",
            entry->MR_sle_user.MR_user_decl_module,
            entry->MR_sle_user.MR_user_name,
            (long) MR_sle_user_adjusted_arity(entry),
            (long) entry->MR_sle_user.MR_user_mode);

        if (!spec && strcmp(entry->MR_sle_user.MR_user_decl_module,
            entry->MR_sle_user.MR_user_def_module) != 0)
        {
            fprintf(fp, " {%s}", entry->MR_sle_user.MR_user_def_module);
        }
    }

    if (! spec) {
        fprintf(fp, " (%s)", MR_detism_names[entry->MR_sle_detism]);
    }
}

void
MR_print_proc_id_trace_and_context(FILE *fp, MR_bool include_trace_data,
    MR_Context_Position pos, const MR_Proc_Layout *entry, MR_Word *base_sp,
    MR_Word *base_curfr, const char *path, const char *filename, int lineno,
    MR_bool print_parent, const char *parent_filename, int parent_lineno,
    int indent)
{
    switch (pos) {
        case MR_CONTEXT_NOWHERE:
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data, entry,
                base_sp, base_curfr);
            MR_print_proc_id(fp, entry);
            if (strlen(path) > 0) {
                fprintf(fp, " %s", path);
            }
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_BEFORE:
            MR_maybe_print_context(fp, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_FALSE,
                parent_filename, parent_lineno);
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data,
                entry, base_sp, base_curfr);
            MR_print_proc_id(fp, entry);
            if (strlen(path) > 0) {
                fprintf(fp, " %s", path);
            }
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_AFTER:
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data,
                entry, base_sp, base_curfr);
            MR_print_proc_id(fp, entry);
            if (strlen(path) > 0) {
                fprintf(fp, " %s", path);
            }
            MR_maybe_print_context(fp, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_FALSE,
                parent_filename, parent_lineno);
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_PREVLINE:
            MR_maybe_print_context(fp, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_TRUE,
                parent_filename, parent_lineno);
            fprintf(fp, "\n%*s ", indent, "");
            MR_maybe_print_call_trace_info(fp, include_trace_data, entry,
                base_sp, base_curfr);
            MR_print_proc_id(fp, entry);
            if (strlen(path) > 0) {
                fprintf(fp, " %s", path);
            }
            fprintf(fp, "\n");
            break;

        case MR_CONTEXT_NEXTLINE:
            fprintf(fp, " ");
            MR_maybe_print_call_trace_info(fp, include_trace_data, entry,
                base_sp, base_curfr);
            MR_print_proc_id(fp, entry);
            if (strlen(path) > 0) {
                fprintf(fp, " %s", path);
            }
            fprintf(fp, "\n%*s", indent, "");
            MR_maybe_print_context(fp, filename, lineno);
            MR_maybe_print_parent_context(fp, print_parent, MR_TRUE,
                parent_filename, parent_lineno);
            fprintf(fp, "\n");
            break;

        default:
            MR_fatal_error("invalid MR_Context_Position");
    }
}

static void
MR_maybe_print_context(FILE *fp, const char *filename, int lineno)
{
    if (MR_strdiff(filename, "") && lineno != 0) {
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

/*
** The different Mercury determinisms are internally represented by integers. 
** This array gives the correspondance with the internal representation and 
** the names that are usually used to denote determinisms.
*/

const char * MR_detism_names[] = {
    "failure",      /* 0 */
    "",             /* 1 */
    "semidet",      /* 2 */
    "nondet",       /* 3 */
    "erroneous",    /* 4 */
    "",             /* 5 */
    "det",          /* 6 */
    "multi",        /* 7 */
    "",             /* 8 */
    "",             /* 9 */
    "cc_nondet",    /* 10 */
    "",             /* 11 */
    "",             /* 12 */
    "",             /* 13 */
    "cc_multi"      /* 14 */
};
