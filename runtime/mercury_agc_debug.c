// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007, 2009, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Debugging support for the accurate garbage collector.

#include "mercury_imp.h"
#include "mercury_layout_util.h"
#include "mercury_deep_copy.h"
#include "mercury_agc_debug.h"

#ifdef MR_NATIVE_GC

// Function prototypes.

static  void    MR_dump_long_value(MR_LongLval locn, MR_MemoryZone *heap_zone,
                    MR_Word * stack_pointer, MR_Word *current_frame,
                    MR_bool do_regs);
static  void    MR_dump_short_value(MR_ShortLval locn, MR_MemoryZone *heap_zone,
                    MR_Word * stack_pointer, MR_Word *current_frame,
                    MR_bool do_regs);
static  void    MR_dump_live_variables(const MR_LabelLayout *layout,
                    MR_MemoryZone *heap_zone, MR_bool top_frame,
                    MR_Word *stack_pointer, MR_Word *current_frame);

////////////////////////////////////////////////////////////////////////////

// Currently this variable is never modified by the Mercury runtime code,
// but it can be modified manually in gdb.
// XXX It would be nicer to set it based on a runtime option setting.

#ifdef MR_DEBUG_AGC_PRINT_VARS
  static MR_bool MR_debug_agc_print_vars = MR_TRUE;
#else
  static MR_bool MR_debug_agc_print_vars = MR_FALSE;
#endif

void
MR_agc_dump_roots(MR_RootList roots)
{
#ifndef MR_HIGHLEVEL_CODE
    MR_Word saved_regs[MR_MAX_FAKE_REG];
    MR_Float saved_f_regs[MR_MAX_VIRTUAL_F_REG];
#endif

    fflush(NULL);
    fprintf(stderr, "Dumping roots\n");

    if (MR_debug_agc_print_vars) {
        while (roots != NULL) {
#ifndef MR_HIGHLEVEL_CODE
            // Restore the registers, because we need to save them to a more
            // permanent backing store (we are going to call Mercury soon,
            // and we don't want it messing with the saved registers).

            MR_restore_registers();
            MR_copy_regs_to_saved_regs(MR_MAX_FAKE_REG - 1, saved_regs,
                MR_MAX_VIRTUAL_F_REG - 1, saved_f_regs);

            MR_hp_word = MR_ENGINE(MR_eng_debug_heap_zone->MR_zone_min);
            MR_virtual_hp_word = MR_ENGINE(MR_eng_debug_heap_zone->MR_zone_min);
#endif // !MR_HIGHLEVEL_CODE

            fflush(NULL);
            MR_write_variable(roots->type_info, *roots->root);
            fflush(NULL);
            fprintf(stderr, "\n");

#ifndef MR_HIGHLEVEL_CODE
            MR_copy_saved_regs_to_regs(MR_MAX_FAKE_REG - 1, saved_regs,
                MR_MAX_VIRTUAL_F_REG, saved_f_regs);
            MR_save_registers();
#endif // !MR_HIGHLEVEL_CODE
            roots = roots->next;
        }
    }
}

#ifndef MR_HIGHLEVEL_CODE
void
MR_agc_dump_nondet_stack_frames(MR_Internal *label, MR_MemoryZone *heap_zone,
    MR_Word *stack_pointer, MR_Word *current_frame, MR_Word *max_frame)
{
    MR_Code     *success_ip;
    int         frame_size;
    MR_bool     registers_valid;

    while (max_frame > MR_nondet_stack_trace_bottom_fr) {
        registers_valid = (max_frame == current_frame);

        frame_size = max_frame - MR_prevfr_slot(max_frame);
        if (frame_size == MR_NONDET_TEMP_SIZE) {
            fprintf(stderr, "%p: nondet temp\n", max_frame);
            fprintf(stderr, " redoip: ");
            fflush(NULL);
            MR_printlabel(stderr, MR_redoip_slot(max_frame));
            fflush(NULL);
            fprintf(stderr, " redofr: %p\n", MR_redofr_slot(max_frame));

            label = MR_lookup_internal_by_addr(MR_redoip_slot(max_frame));

            if (label && label->MR_internal_layout) {
                MR_dump_live_variables(label->MR_internal_layout, heap_zone,
                    registers_valid, stack_pointer, MR_redofr_slot(max_frame));
            }

        } else if (frame_size == MR_DET_TEMP_SIZE) {
            fprintf(stderr, "%p: nondet temp\n", max_frame);
            fprintf(stderr, " redoip: ");
            fflush(NULL);
            MR_printlabel(stderr, MR_redoip_slot(max_frame));
            fflush(NULL);
            fprintf(stderr, " redofr: %p\n", MR_redofr_slot(max_frame));
            fprintf(stderr, " detfr:  %p\n", MR_tmp_detfr_slot(max_frame));

            label = MR_lookup_internal_by_addr(MR_redoip_slot(max_frame));

            if (label && label->MR_internal_layout) {
                MR_dump_live_variables(label->MR_internal_layout, heap_zone,
                    registers_valid, MR_tmp_detfr_slot(max_frame), max_frame);
                // XXX Should max_frame above be
                // MR_redoip_slot(max_frame) instead?

            }

        } else {
            fprintf(stderr, "%p: nondet ordinary\n", max_frame);
            fprintf(stderr, " redoip: ");
            fflush(NULL);
            MR_printlabel(stderr, MR_redoip_slot(max_frame));
            fflush(NULL);
            fprintf(stderr, " redofr: %p\n", MR_redofr_slot(max_frame));
            fprintf(stderr, " succip: ");
            fflush(NULL);
            MR_printlabel(stderr, MR_succip_slot(max_frame));
            fflush(NULL);
            fprintf(stderr, " succfr: %p\n", MR_succfr_slot(max_frame));

            // XXX ???
            label = MR_lookup_internal_by_addr(MR_redoip_slot(max_frame));

            if (label != NULL && label->MR_internal_layout) {
                MR_dump_live_variables(label->MR_internal_layout, heap_zone,
                    registers_valid, stack_pointer, MR_redofr_slot(max_frame));
                fprintf(stderr, " this label: %s\n", label->MR_internal_name);
            }
        }

        max_frame = MR_prevfr_slot(max_frame);
    }

    // XXX Lookup the address (redoip?) and dump the variables.

    fflush(NULL);
}

void
MR_agc_dump_stack_frames(MR_Internal *label, MR_MemoryZone *heap_zone,
    MR_Word *stack_pointer, MR_Word *current_frame)
{
    MR_Word                 saved_regs[MR_MAX_FAKE_REG];
    int                     i;
    int                     short_var_count;
    int                     long_var_count;
    MR_Word                 *type_params;
    MR_TypeInfo             type_info;
    MR_Word                 value;
    const MR_ProcLayout     *entry_layout;
    const MR_LabelLayout    *layout;
    const MR_Code           *success_ip;
    MR_bool                 top_frame;

    layout = label->MR_internal_layout;
    success_ip = label->MR_internal_addr;
    entry_layout = layout->MR_sll_entry;

    // For each stack frame...

    top_frame = MR_TRUE;
    while (MR_DETISM_DET_STACK(entry_layout->MR_sle_detism)) {
        if (label->MR_internal_name != NULL) {
            fprintf(stderr, "    label: %s\n", label->MR_internal_name);
        } else {
            fprintf(stderr, "    label: %p\n", label->MR_internal_addr);
        }

        if (success_ip == MR_stack_trace_bottom_ip) {
            break;
        }

        MR_dump_live_variables(layout, heap_zone, top_frame,
            stack_pointer, current_frame);
        // Move to the next stack frame.

        {
            MR_LongLval     location;
            MR_LongLvalType type;
            int             number;

            location = entry_layout->MR_sle_succip_locn;
            type = MR_LONG_LVAL_TYPE(location);
            number = MR_LONG_LVAL_NUMBER(location);
            if (type != MR_LONG_LVAL_TYPE_STACKVAR) {
                MR_fatal_error("can only handle stackvars");
            }

            success_ip = (MR_Code *) MR_based_stackvar(stack_pointer, number);
            stack_pointer = stack_pointer - entry_layout->MR_sle_stack_slots;
            label = MR_lookup_internal_by_addr(success_ip);
        }

        top_frame = MR_FALSE;
        layout = label->MR_internal_layout;

        if (layout != NULL) {
            entry_layout = layout->MR_sll_entry;
        }
    }
}

static void
MR_dump_live_variables(const MR_LabelLayout *label_layout,
    MR_MemoryZone *heap_zone, MR_bool top_frame,
    MR_Word *stack_pointer, MR_Word *current_frame)
{
    int                 short_var_count;
    int                 long_var_count;
    int                 i;
    MR_TypeInfo         type_info;
    MR_Word             value;
    MR_TypeInfoParams   type_params;
    MR_Word             saved_regs[MR_MAX_FAKE_REG];
    MR_Float            saved_f_regs[MR_MAX_VIRTUAL_F_REG];
    MR_Word             *current_regs;
    MR_Float            *current_f_regs;

    long_var_count = MR_long_desc_var_count(label_layout);
    short_var_count = MR_short_desc_var_count(label_layout);

    // For the top stack frame, we should pass a pointer to a filled-in
    // saved_regs instead of NULL. For other stack frames, passing NULL
    // is fine, since output arguments are not live yet for any call
    // except the top one.

    MR_restore_registers();
    MR_copy_regs_to_saved_regs(MR_MAX_FAKE_REG - 1, saved_regs,
        MR_MAX_VIRTUAL_F_REG - 1, saved_f_regs);
    if (top_frame) {
        current_regs = saved_regs;
        current_f_regs = saved_f_regs;
    } else {
        current_regs = NULL;
        current_f_regs = NULL;
    }
    type_params = MR_materialize_type_params_base(label_layout,
        current_regs, stack_pointer, current_frame);

    for (i = 0; i < long_var_count; i++) {
        fprintf(stderr, "%-12s\t", "");
        if (MR_PROC_LAYOUT_HAS_PROC_ID(label_layout->MR_sll_entry)) {
            MR_print_proc_id(stderr, label_layout->MR_sll_entry);
        }

        MR_dump_long_value(MR_long_desc_var_locn(label_layout, i),
            heap_zone, stack_pointer, current_frame, top_frame);
        fprintf(stderr, "\n");
        fflush(NULL);

        if (MR_debug_agc_print_vars) {
            // Call Mercury but use the debugging heap.

            MR_hp_word = MR_ENGINE(MR_eng_debug_heap_zone->MR_zone_min);
            MR_virtual_hp_word = MR_ENGINE(MR_eng_debug_heap_zone->MR_zone_min);

            if (MR_get_type_and_value_base(label_layout, i,
                    current_regs, stack_pointer, current_frame, current_f_regs,
                    type_params, &type_info, &value)) {
                printf("\t");
                MR_write_variable(type_info, value);
                printf("\n");
            }

            fflush(NULL);
        }
    }

    for (i = 0; i < short_var_count; i++) {
        fprintf(stderr, "%-12s\t", "");
        if (MR_PROC_LAYOUT_HAS_PROC_ID(label_layout->MR_sll_entry)) {
            MR_print_proc_id(stderr, label_layout->MR_sll_entry);
        }

        MR_dump_short_value(MR_short_desc_var_locn(label_layout, i),
            heap_zone, stack_pointer, current_frame, top_frame);
        fprintf(stderr, "\n");
        fflush(NULL);

        if (MR_debug_agc_print_vars) {
            // Call Mercury but use the debugging heap.

            MR_hp_word = MR_ENGINE(MR_eng_debug_heap_zone->MR_zone_min);
            MR_virtual_hp_word = MR_ENGINE(MR_eng_debug_heap_zone->MR_zone_min);

            if (MR_get_type_and_value_base(label_layout, i,
                    current_regs, stack_pointer, current_frame, current_f_regs,
                    type_params, &type_info, &value)) {
                printf("\t");
                MR_write_variable(type_info, value);
                printf("\n");
            }

            fflush(NULL);
        }
    }

    MR_copy_saved_regs_to_regs(MR_MAX_FAKE_REG - 1, saved_regs,
        MR_MAX_VIRTUAL_F_REG - 1, saved_f_regs);
    MR_save_registers();
    MR_free(type_params);
}

static void
MR_dump_long_value(MR_LongLval locn, MR_MemoryZone *heap_zone,
    MR_Word *stack_pointer, MR_Word *current_frame, MR_bool do_regs)
{
    int     locn_num;
    MR_Word value;
    int     difference;
    MR_bool have_value;

    value = 0;
    have_value = MR_FALSE;

    locn_num = MR_LONG_LVAL_NUMBER(locn);
    switch (MR_LONG_LVAL_TYPE(locn)) {
        case MR_LONG_LVAL_TYPE_R:
            if (do_regs) {
                value = MR_virtual_reg_value(locn_num);
                have_value = MR_TRUE;
                fprintf(stderr, "r%d\t", locn_num);
            } else {
                fprintf(stderr, "r%d (invalid)\t", locn_num);
            }
            break;

        case MR_LONG_LVAL_TYPE_F:
            fprintf(stderr, "f%d\t", locn_num);
            break;

        case MR_LONG_LVAL_TYPE_STACKVAR:
            value = MR_based_stackvar(stack_pointer, locn_num);
            have_value = MR_TRUE;
            fprintf(stderr, "stackvar%d", locn_num);
            break;

        case MR_LONG_LVAL_TYPE_FRAMEVAR:
            value = MR_based_framevar(current_frame, locn_num);
            have_value = MR_TRUE;
            fprintf(stderr, "framevar%d", locn_num);
            break;

        case MR_LONG_LVAL_TYPE_SUCCIP:
            fprintf(stderr, "succip");
            break;

        case MR_LONG_LVAL_TYPE_MAXFR:
            fprintf(stderr, "maxfr");
            break;

        case MR_LONG_LVAL_TYPE_CURFR:
            fprintf(stderr, "curfr");
            break;

        case MR_LONG_LVAL_TYPE_HP:
            fprintf(stderr, "hp");
            break;

        case MR_LONG_LVAL_TYPE_SP:
            fprintf(stderr, "sp");
            break;

        case MR_LONG_LVAL_TYPE_INDIRECT:
            fprintf(stderr, "offset %d from ",
                MR_LONG_LVAL_INDIRECT_OFFSET(locn_num));
            // XXX Tyson will have to complete this
            // based on what he wants this function to do.

        case MR_LONG_LVAL_TYPE_UNKNOWN:
            fprintf(stderr, "unknown");
            break;

        default:
            fprintf(stderr, "LONG DEFAULT");
            break;
    }
    if (have_value) {
        if (value >= (MR_Word) heap_zone->MR_zone_min &&
                value < (MR_Word) heap_zone->MR_zone_hardmax)
        {
            difference = (MR_Word *) value - (MR_Word *) heap_zone->MR_zone_min;
            fprintf(stderr, "\thp[%d]\t(%lx)", difference, (long) value);
        } else {
            fprintf(stderr, "\t       \t(%lx)", (long) value);
        }
    }
}

static void
MR_dump_short_value(MR_ShortLval locn, MR_MemoryZone *heap_zone,
    MR_Word *stack_pointer, MR_Word *current_frame, MR_bool do_regs)
{
    int     locn_num;
    MR_Word value;
    int     difference;
    MR_bool have_value;

    value = 0;
    have_value = MR_FALSE;
    locn_num = (int) locn >> MR_SHORT_LVAL_TAGBITS;
    switch (MR_SHORT_LVAL_TYPE(locn)) {
        case MR_SHORT_LVAL_TYPE_R:
            if (do_regs) {
                value = MR_virtual_reg_value(locn_num);
                have_value = MR_TRUE;
                fprintf(stderr, "r%d\t", locn_num);
            } else {
                fprintf(stderr, "r%d (invalid)\t", locn_num);
            }
            break;

        case MR_SHORT_LVAL_TYPE_STACKVAR:
            value = MR_based_stackvar(stack_pointer, locn_num);
            have_value = MR_TRUE;
            fprintf(stderr, "stackvar%d", locn_num);
            break;

        case MR_SHORT_LVAL_TYPE_FRAMEVAR:
            value = MR_based_framevar(current_frame, locn_num);
            have_value = MR_TRUE;
            fprintf(stderr, "framevar%d", locn_num);
            break;

        case MR_SHORT_LVAL_TYPE_SPECIAL:
            switch (locn_num) {
                case MR_LONG_LVAL_TYPE_SUCCIP:
                    fprintf(stderr, "succip");
                    break;
                case MR_LONG_LVAL_TYPE_MAXFR:
                    fprintf(stderr, "succip");
                    break;
                case MR_LONG_LVAL_TYPE_CURFR:
                    fprintf(stderr, "curfr");
                    break;
                case MR_LONG_LVAL_TYPE_HP:
                    fprintf(stderr, "hp");
                    break;
                case MR_LONG_LVAL_TYPE_SP:
                    fprintf(stderr, "sp");
                    break;
                default:
                    fprintf(stderr, "SPECIAL DEFAULT");
                    break;
            }
            break;

        default:
            fprintf(stderr, "SHORT DEFAULT");
            break;
    }
    if (have_value) {
        if (value >= (MR_Word) heap_zone->MR_zone_min &&
                value < (MR_Word) heap_zone->MR_zone_hardmax)
        {
            difference = (MR_Word *) value - (MR_Word *) heap_zone->MR_zone_min;
            fprintf(stderr, "\thp[%d]\t(%lx)", difference, (long) value);
        } else {
            fprintf(stderr, "\t       \t(%lx)", (long) value);
        }
    }
}

#endif // !MR_HIGHLEVEL_CODE

#endif // MR_NATIVE_GC
