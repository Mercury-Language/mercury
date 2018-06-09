// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007, 2009, 2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module contains the accurate garbage collector.
//
// Currently we are just using a simple copying collector.
//
// For the LLDS back-end, we detect heap overflow
// using the virtual memory system; the page fault
// handler will set the return address to call the
// garbage collector. See also compiler/notes/gc_and_c_code.html.
//
// XXX This may cause problems for tight loops that allocate memory
// and don't have any procedure returns in them... either because they
// loop via tail calls and/or retries (failure in nondet code).
//
// For the MLDS back-end, we can't use this technique
// of overwriting the return address, so instead we
// just explicitly call MR_GC_check() to check for
// heap overflow before each heap allocation.
//
// For documentation on accurate collection in the MLDS back-end,
// see compiler/ml_elim_nested.m.
//
// TODO for the LLDS back-end:
//  - add code to support tracing the stack frames left by builtin__catch;
//  - fix issue with tight loops via tail calls (see XXX above);
//  - fix issue with tight loops via retries (see XXX above);
//  - handle semidet existentially typed procedures properly (see XXX below);
//  - use write() rather than fprintf() in signal handler (see XXX below).

#include "mercury_imp.h"

#ifdef MR_NATIVE_GC

#include "mercury_deep_copy.h"
#include "mercury_layout_util.h"
#include "mercury_agc_debug.h"

////////////////////////////////////////////////////////////////////////////
// Function prototypes.

#ifdef MR_HIGHLEVEL_CODE

  void          MR_garbage_collect(void);
  static void   traverse_stack(struct MR_StackChain *top);
  static void   resize_and_reset_gc_threshold(MR_MemoryZone *old_heap,
                    MR_MemoryZone *new_heap);

#else // !MR_HIGHLEVEL_CODE

  MR_define_extern_entry(mercury__garbage_collect_0_0);

  static void   MR_LLDS_garbage_collect(MR_Code *saved_success,
                        MR_bool callee_model_semi, MR_Word *stack_pointer,
                        MR_Word *max_frame, MR_Word *current_frame);
  static void   traverse_det_stack(const MR_LabelLayout *label_layout,
                        MR_bool callee_model_semi,
                        MR_Word *stack_pointer, MR_Word *current_frame);
  static void   traverse_nondet_stack(const MR_LabelLayout *label_layout,
                        MR_bool callee_model_semi, MR_Word *stack_pointer,
                        MR_Word *max_frame, MR_Word *current_frame);
  static void   traverse_nondet_frame(void *user_data,
                        const MR_LabelLayout *label_layout,
                        MR_Word *stack_pointer, MR_Word *current_frame);
  static MR_bool are_registers_live(MR_bool is_first_frame,
                        MR_bool callee_model_semi);
  static void   traverse_frame(MR_bool registers_live,
                        const MR_LabelLayout *label_layout,
                        MR_Word *stack_pointer, MR_Word *current_frame);
  static void   resize_and_reset_redzone(MR_MemoryZone *old_heap,
                        MR_MemoryZone *new_heap);
  static void   copy_long_value(MR_LongLval locn, MR_TypeInfo type_info,
                        MR_bool registers_live, MR_Word *stack_pointer,
                        MR_Word *current_frame);
  static void   copy_short_value(MR_ShortLval locn, MR_TypeInfo type_info,
                        MR_bool registers_live, MR_Word *stack_pointer,
                        MR_Word *current_frame);

#endif

static void     notify_gc_start(const MR_MemoryZone *old_heap,
                    const MR_MemoryZone *new_heap);
static void     notify_gc_end(const MR_MemoryZone *old_heap,
                    const MR_MemoryZone *new_heap, const MR_Word *old_hp);
static void     init_forwarding_pointer_bitmap(const MR_MemoryZone *old_heap,
                    MR_Word *old_hp);
static void     swap_heaps(void);
static void     traverse_extra_roots(void);
static void     maybe_clear_old_heap(MR_MemoryZone *old_heap, MR_Word *old_hp);

////////////////////////////////////////////////////////////////////////////
// Global variables (only used in this module, however).

#ifndef MR_HIGHLEVEL_CODE
static MR_Code  *saved_success = NULL;
static MR_Code  **saved_success_location = NULL;
static MR_bool  callee_was_model_semi = MR_FALSE;
static MR_bool  gc_scheduled = MR_FALSE;
static MR_bool  gc_running = MR_FALSE;
#endif

// The list of roots.
static MR_RootList root_list = NULL;

// The last root on the list.
static MR_RootList last_root = NULL;

////////////////////////////////////////////////////////////////////////////
// The MLDS version of the collector.

#ifdef MR_HIGHLEVEL_CODE

// Perform a garbage collection:
//
//  swap the two heaps;
//  traverse the roots, copying data from the old heap to the new heap;
//  reset the old heap;
//
// This is the version for the MLDS back-end. Beware that there is some
// code duplication with the version for the LLDS back-end, which is below.

void
MR_garbage_collect(void)
{
    MR_MemoryZone       *old_heap, *new_heap;
    MR_Word             *old_hp;

    old_heap = MR_ENGINE(MR_eng_heap_zone);
    new_heap = MR_ENGINE(MR_eng_heap_zone2);
    old_hp = MR_virtual_hp;

    // Print some debugging messages.

    notify_gc_start(old_heap, new_heap);

    // Initialize the forwarding pointer bitmap.

    init_forwarding_pointer_bitmap(old_heap, old_hp);

    // Swap the two heaps.
    // The new heap pointer starts at the bottom of the new heap.

    swap_heaps();
    MR_virtual_hp_word = (MR_Word) new_heap->MR_zone_min;

    // Copy any roots on the stack.

    traverse_stack(mercury__private_builtin__stack_chain);

    // Copy any roots that are not on the stack.

    traverse_extra_roots();

    // Clear out the old heap.

    maybe_clear_old_heap(old_heap, old_hp);

    // Compute the new size at which to GC,
    // and reset the gc_threshold on the new heap.

    resize_and_reset_gc_threshold(old_heap, new_heap);

    // Print some more debugging messages,

    if (MR_agc_debug) {
        fprintf(stderr, "AFTER:\n");
    }
    notify_gc_end(old_heap, new_heap, old_hp);
}

static void
traverse_stack(struct MR_StackChain *stack_chain)
{
    // The trace() routines themselves should not allocate heap space;
    // they should use stack allocation.

    while (stack_chain != NULL) {
        (*stack_chain->trace)(stack_chain);
        stack_chain = stack_chain->prev;
    }
}

// Compute the new size at which to GC,
// and reset the redzone on the new heap.

static void
resize_and_reset_gc_threshold(MR_MemoryZone *old_heap, MR_MemoryZone *new_heap)
{
    // These counts include some wasted space between
    // ->MR_zone_min and ->MR_zone_bottom.

    size_t old_heap_space =
        (char *) old_heap->MR_zone_gc_threshold -
        (char *) old_heap->MR_zone_bottom;
    size_t new_heap_usage =
        (char *) MR_virtual_hp - (char *) new_heap->MR_zone_bottom;
    size_t gc_heap_size;

    // Set the size at which to GC to be MR_heap_expansion_factor
    // (which defaults to two) times the current usage,
    // or the size at which we GC'd last time, whichever is larger.

    gc_heap_size = MR_round_up(
        (size_t) (MR_heap_expansion_factor * new_heap_usage),
        MR_unit);
    if (gc_heap_size < old_heap_space) {
        gc_heap_size = old_heap_space;
    }
    old_heap->MR_zone_gc_threshold =
        ((char *) old_heap->MR_zone_bottom + gc_heap_size);
}

////////////////////////////////////////////////////////////////////////////
// The LLDS version of the collector.

#else // !MR_HIGHLEVEL_CODE

// MR_schedule_agc:
//
// Schedule garbage collection.
//
// We do this by replacing the succip that is saved in the current procedure's
// stack frame with the address of the garbage collector. When the current
// procedure returns, it will call the garbage collector.
//
// (We go to this trouble because then the stacks will be in a known state
// -- each stack frame is described by information associated with the
// continuation label that the code will return to).

// XXX We should use write() rather than fprintf() here, since this code
// is called from a signal handler, and stdio is not guaranteed
// to be reentrant.

void
MR_schedule_agc(MR_Code *pc_at_signal, MR_Word *sp_at_signal,
    MR_Word *curfr_at_signal)
{
    const MR_ProcLayout     *proc_layout;
    MR_LongLvalType         type;
    MR_LongLval             location;
    MR_Entry                *entry_label = NULL;
    int                     number;
    MR_Determinism          determinism;

    if (gc_running) {
        // This is bad news, but it can happen if you don't collect
        // any garbage. We should try to avoid it by resizing the heaps
        // so they don't become too full.
        //
        // It might also be worthwhile eventually turning off the redzone
        // in the destination heap (but only when the large problem of
        // handling collections with little garbage has been solved).

        fprintf(stderr, "Mercury runtime: Garbage collection scheduled"
            " while collector is already running\n");
        fprintf(stderr, "Mercury_runtime: Trying to continue...\n");
        return;
    }
#ifdef MR_DEBUG_AGC_SCHEDULING
    fprintf(stderr, "PC at signal: %ld (%lx)\n",
        (long) pc_at_signal, (long) pc_at_signal);
    fprintf(stderr, "SP at signal: %ld (%lx)\n",
        (long) sp_at_signal, (long) sp_at_signal);
    fprintf(stderr, "curfr at signal: %ld (%lx)\n",
        (long) curfr_at_signal, (long) curfr_at_signal);
    fflush(NULL);
#endif

    // Search for the entry label.

    entry_label = MR_prev_entry_by_addr(pc_at_signal);
    proc_layout = (entry_label != NULL ? entry_label->MR_entry_layout : NULL);

    determinism = (proc_layout != NULL ? proc_layout->MR_sle_detism : -1);

    if (determinism < 0) {
        // This means we have reached some handwritten code that has
        // no further information about the stack frame.

        fprintf(stderr, "Mercury runtime: "
            "attempt to schedule garbage collection failed\n");
        if (entry_label != NULL) {
            fprintf(stderr, "Mercury runtime: the label ");
            if (entry_label->MR_entry_name != NULL) {
                fprintf(stderr, "%s has no stack layout info\n",
                        entry_label->MR_entry_name);
            } else {
                fprintf(stderr, "at address %p "
                    "has no stack layout info\n", entry_label->MR_entry_addr);
            }
            fprintf(stderr, "Mercury runtime: PC address = %p\n", pc_at_signal);
            fprintf(stderr, "Mercury runtime: PC = label + 0x%lx\n",
                (long) ((char *) pc_at_signal -
                    (char *)entry_label->MR_entry_addr));
        } else {
            fprintf(stderr, "Mercury runtime: no entry label ");
            fprintf(stderr, "for PC address %p\n", pc_at_signal);
        }

        fprintf(stderr, "Mercury runtime: Trying to continue...\n");
        return;
    }
#ifdef MR_DEBUG_AGC_SCHEDULING
    if (entry_label->MR_entry_name != NULL) {
        fprintf(stderr, "scheduling called at: %s (%ld %lx)\n",
            entry_label->MR_entry_name,
            (long) entry_label->MR_entry_addr,
            (long) entry_label->MR_entry_addr);
    } else {
        fprintf(stderr, "scheduling called at: (%ld %lx)\n",
            (long) entry_label->MR_entry_addr,
            (long) entry_label->MR_entry_addr);
    }
    fflush(NULL);
#endif

    // If we have already scheduled a garbage collection, undo the last change,
    // and do a new one.

    if (gc_scheduled) {
#ifdef MR_DEBUG_AGC_SCHEDULING
        fprintf(stderr, "GC scheduled again. Replacing old scheduling,"
            " and trying to schedule again.\n");
#endif
        *saved_success_location = saved_success;
    }
    gc_scheduled = MR_TRUE;

    location = proc_layout->MR_sle_succip_locn;
    type = MR_LONG_LVAL_TYPE(location);
    number = MR_LONG_LVAL_NUMBER(location);
    if (MR_DETISM_DET_STACK(determinism)) {
        callee_was_model_semi = MR_DETISM_CAN_FAIL(determinism);

        if (type != MR_LONG_LVAL_TYPE_STACKVAR) {
            MR_fatal_error("can only handle stackvars");
        }

        // Save the old succip and its location.

        saved_success_location = (MR_Code **)
                & MR_based_stackvar(sp_at_signal, number);
        saved_success = *saved_success_location;
    } else {
        callee_was_model_semi = MR_FALSE;
        // XXX We ought to also overwrite the redoip, otherwise we might miss
        // failure-driven loops which don't contain any returns.

        // Save the old succip and its location.
        //
        // Note that curfr always points to an ordinary procedure frame,
        // never to a temp frame, so it is safe to access the succip slot
        // of that frame without checking what kind of frame it is.

        assert(location == -1);
        // succip is saved in succip_slot
        saved_success_location = MR_succip_slot_addr(curfr_at_signal);
        saved_success = *saved_success_location;
    }

#ifdef MR_DEBUG_AGC_SCHEDULING
    fprintf(stderr, "old succip: %ld (%lx) new: %ld (%lx)\n",
        (long) saved_success, (long) saved_success,
        (long) MR_ENTRY(mercury__garbage_collect_0_0),
        (long) MR_ENTRY(mercury__garbage_collect_0_0));
#endif

    // Replace the old succip with the address of the garbage collector.

    *saved_success_location = MR_ENTRY(mercury__garbage_collect_0_0);

#ifdef MR_DEBUG_AGC_SCHEDULING
    fprintf(stderr, "Accurate GC scheduled.\n");
#endif
}

MR_BEGIN_MODULE(native_gc)
MR_BEGIN_CODE

// Our garbage collection entry label.
//
// It saves the registers -- we use the saved registers
// for garbage collection and leave the real ones alone.

MR_define_entry(mercury__garbage_collect_0_0);

    // Record that the collector is running.
    gc_running = MR_TRUE;

    // Restore the original contents of the saved success location.
    // This is needed in case it was on the nondet stack; in that case,
    // it might be used again after backtracking.

    *saved_success_location = saved_success;

    MR_save_registers();
    MR_LLDS_garbage_collect(saved_success, callee_was_model_semi,
        MR_sp, MR_maxfr, MR_curfr);
    MR_restore_registers();
    gc_scheduled = MR_FALSE;
    gc_running = MR_FALSE;

    MR_succip_word = (MR_Word) saved_success;
    MR_proceed();
    MR_fatal_error("Unreachable code reached");

MR_END_MODULE

// MR_LLDS_garbage_collect:
//
// The main garbage collection routine.
//
// This is the version for the LLDS back-end. Beware that there is some
// code duplication with the version for the MLDS back-end, which is above.

static void
MR_LLDS_garbage_collect(MR_Code *success_ip, MR_bool callee_model_semi,
    MR_Word *stack_pointer, MR_Word *max_frame, MR_Word *current_frame)
{
    MR_MemoryZone                   *old_heap, *new_heap;
    MR_Word                         *old_hp;
    MR_Internal                     *label;
    const MR_LabelLayout            *label_layout;

    old_heap = MR_ENGINE(MR_eng_heap_zone);
    new_heap = MR_ENGINE(MR_eng_heap_zone2);
    old_hp = MR_virtual_hp;

    // Print some debugging messages.

    notify_gc_start(old_heap, new_heap);

    // Initialize the forwarding pointer bitmap.

    init_forwarding_pointer_bitmap(old_heap, old_hp);

    // Swap the two heaps.
    // The new heap pointer starts at the bottom of the new heap.

    swap_heaps();
    MR_virtual_hp_word = (MR_Word) new_heap->MR_zone_min;

    label = MR_lookup_internal_by_addr(success_ip);
    label_layout = label->MR_internal_layout;

    if (MR_agc_debug) {
        fprintf(stderr, "BEFORE:\n");

        MR_agc_dump_stack_frames(label, old_heap, stack_pointer,
            current_frame);
        MR_agc_dump_nondet_stack_frames(label, old_heap, stack_pointer,
            current_frame, max_frame);
        MR_agc_dump_roots(root_list);

        // XXX The debug code seems to be messing up the heap pointer.
        // Easier to reset it than debug the problem at the moment.

        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
        MR_virtual_hp_word = (MR_Word) new_heap->MR_zone_min;
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
    }

    // Traverse the stacks, copying the live data in the old heap to the
    // new heap.

    traverse_det_stack(label_layout, callee_model_semi,
        stack_pointer, current_frame);
    traverse_nondet_stack(label_layout, callee_model_semi,
        stack_pointer, max_frame, current_frame);

    // Copy any roots that are not on the stack.

    traverse_extra_roots();

    // Clear out the old heap. This needs to be done _before_
    // we reset the redzone.

    maybe_clear_old_heap(old_heap, old_hp);

    // Compute the new size at which to GC,
    // and reset the redzone on the new heap.

    resize_and_reset_redzone(old_heap, new_heap);

    // Print some more debugging messages

    if (MR_agc_debug) {
        MR_Word *new_hp;

        fprintf(stderr, "AFTER:\n");

        // XXX Save this, it appears to get clobbered.
        new_hp = MR_virtual_hp;

        MR_agc_dump_stack_frames(label, new_heap, stack_pointer,
            current_frame);
        MR_agc_dump_nondet_stack_frames(label, new_heap,
            stack_pointer, current_frame, max_frame);

        // XXX Restore this, it appears to get clobbered.
        MR_virtual_hp_word = (MR_Word) new_hp;
    }

    notify_gc_end(old_heap, new_heap, old_hp);
}

// Traverse the det stack. In order to do this, we need to traverse
// the call stack, which includes all of the det stack _plus_ the
// success continuation frames on the nondet stack (but not the failure
// continuation frames on the nondet stack). But we skip all the nondet
// frames.

static void
traverse_det_stack(const MR_LabelLayout *label_layout,
    MR_bool callee_model_semi, MR_Word *stack_pointer, MR_Word *current_frame)
{
    // Record whether this is the first frame on the call stack.
    // The very first frame on the call stack is special because it may have
    // live registers (the other frames should only have live stack slots).

    MR_bool   is_first_frame = MR_TRUE;

    // For each stack frame ...

    do {
        const MR_ProcLayout             *proc_layout;
        MR_StackWalkStepResult          result;
        MR_Unsigned                     reused_frames;
        const char                      *problem;

        // Traverse this stack frame, if it is a det frame.

        proc_layout = label_layout->MR_sll_entry;
        if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
            MR_bool registers_live = are_registers_live(is_first_frame,
                callee_model_semi);
            traverse_frame(registers_live, label_layout, stack_pointer,
                current_frame);
        }

        // Get the next stack frame.

        result = MR_stack_walk_step(proc_layout, &label_layout,
            &stack_pointer, &current_frame, &reused_frames, &problem);
        if (result == MR_STEP_ERROR_BEFORE || result == MR_STEP_ERROR_AFTER) {
            MR_fatal_error(problem);
        }

        is_first_frame = MR_FALSE;

    } while (label_layout != NULL); // end for each stack frame...
}

static MR_bool
are_registers_live(MR_bool is_first_frame, MR_bool callee_model_semi)
{
    // The registers are live if this is the first (top) frame.
    // However, as an exception, if the called procedure was semidet,
    // and the success indicator (MR_r1) is false, then the registers
    // are not live (only MR_r1 is live, and we don't need to traverse that).

    MR_bool registers_live = is_first_frame;
    if (callee_model_semi && !MR_virtual_reg_value(1)) {
        registers_live = MR_FALSE;
    }
    return registers_live;
}

// Traverse the whole of the nondet stack.

struct first_frame_data {
    const MR_LabelLayout    *first_frame_layout;
    MR_Word                 *first_frame_curfr;
    MR_bool                 first_frame_callee_model_semi;
};

static void
traverse_nondet_stack(const MR_LabelLayout *first_frame_layout,
    MR_bool callee_model_semi, MR_Word *stack_pointer,
    MR_Word *max_frame, MR_Word *current_frame)
{
    struct first_frame_data data;
    data.first_frame_layout = first_frame_layout;
    data.first_frame_curfr = current_frame;
    data.first_frame_callee_model_semi = callee_model_semi;
    MR_traverse_nondet_stack_from_layout(max_frame, first_frame_layout,
        stack_pointer, current_frame, traverse_nondet_frame, &data);
}

static void
traverse_nondet_frame(void *user_data, const MR_LabelLayout *label_layout,
    MR_Word *stack_pointer, MR_Word *current_frame)
{
    MR_bool                 is_first_frame;
    MR_bool                 registers_live;
    struct first_frame_data *data = user_data;

    // Determine whether this is the first frame on the call stack.
    // The very first frame on the call stack is special because it may have
    // live registers (the other frames should only have live stack slots).

    is_first_frame = (current_frame == data->first_frame_curfr
        && !MR_DETISM_DET_STACK(
            data->first_frame_layout->MR_sll_entry->MR_sle_detism));

    registers_live = are_registers_live(is_first_frame,
        data->first_frame_callee_model_semi);

    traverse_frame(registers_live, label_layout, stack_pointer, current_frame);
}

// Traverse a stack frame (it could be either a det frame or a nondet frame).

static void
traverse_frame(MR_bool registers_live, const MR_LabelLayout *label_layout,
    MR_Word *stack_pointer, MR_Word *current_frame)
{
    int                             short_var_count;
    int                             long_var_count;
    int                             i;
    MR_MemoryList                   allocated_memory_cells;
    MR_TypeInfoParams               type_params;
    MR_ShortLval                    locn;
    MR_PseudoTypeInfo               pseudo_type_info;
    MR_TypeInfo                     type_info;

    allocated_memory_cells = NULL;

    if (MR_agc_debug) {
        // XXX We used to print the label name here, but that is
        // not available anymore.

        printf("traverse_frame: traversing frame with label layout %p\n",
            (const void *) label_layout);
        fflush(NULL);
    }

    long_var_count = MR_long_desc_var_count(label_layout);
    short_var_count = MR_short_desc_var_count(label_layout);

    // Get the type parameters from the stack frame.

    // For frames other than the first frame, we must pass NULL for the
    // registers here, since the registers have not been saved;
    // This should be OK, because none of the values used by a procedure
    // will be stored in registers across a call, since we have no
    // caller-save registers (they are all callee-save).

    // XXX This won't handle calls to semidet existentially typed procedures.
    // We will try to dereference the type_infos for the existential type vars
    // here, even if the success indicator is false and hence the registers
    // are not live.

    type_params = MR_materialize_type_params_base(label_layout,
        (registers_live ? MR_fake_reg : NULL), stack_pointer, current_frame);

    // Copy each live variable.

    for (i = 0; i < long_var_count; i++) {
        locn = MR_long_desc_var_locn(label_layout, i);
        pseudo_type_info = MR_var_pti(label_layout, i);

        type_info = MR_make_type_info(type_params, pseudo_type_info,
            &allocated_memory_cells);
        copy_long_value(locn, type_info, registers_live,
            stack_pointer, current_frame);
        MR_deallocate(allocated_memory_cells);
        allocated_memory_cells = NULL;
    }

    for (i = 0; i < short_var_count; i++) {
        locn = MR_short_desc_var_locn(label_layout, i);
        pseudo_type_info = MR_var_pti(label_layout, i);

        type_info = MR_make_type_info(type_params, pseudo_type_info,
            &allocated_memory_cells);
        copy_short_value(locn, type_info, registers_live,
            stack_pointer, current_frame);
        MR_deallocate(allocated_memory_cells);
        allocated_memory_cells = NULL;
    }

    MR_free(type_params);
}

// copy_long_value:
//
// Copies a value in a register or stack frame, replacing the original
// with the new copy.
//
// The copying is done using MR_agc_deep_copy, which is the accurate GC version
// of MR_deep_copy (it leaves forwarding pointers in the old copy of the data,
// if it is on the old heap).

static void
copy_long_value(MR_LongLval locn, MR_TypeInfo type_info,
    MR_bool registers_live, MR_Word *stack_pointer, MR_Word *current_frame)
{
    int     locn_num;
    MR_Word copy;

    locn_num = MR_LONG_LVAL_NUMBER(locn);
    switch (MR_LONG_LVAL_TYPE(locn)) {
    case MR_LONG_LVAL_TYPE_R:
        if (registers_live) {
            copy = MR_agc_deep_copy(MR_virtual_reg_value(locn_num), type_info,
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
            MR_virtual_reg_assign(locn_num, copy);
        }
        break;

    case MR_LONG_LVAL_TYPE_F:
        MR_fatal_error("copy_long_value: MR_LONG_LVAL_TYPE_F");
        break;

    case MR_LONG_LVAL_TYPE_STACKVAR:
        MR_based_stackvar(stack_pointer, locn_num) =
            MR_agc_deep_copy(MR_based_stackvar(stack_pointer, locn_num),
                type_info,
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
        break;

    case MR_LONG_LVAL_TYPE_FRAMEVAR:
        MR_based_framevar(current_frame, locn_num) =
            MR_agc_deep_copy(MR_based_framevar(current_frame, locn_num),
                type_info,
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
        break;

    case MR_LONG_LVAL_TYPE_DOUBLE_STACKVAR:
        MR_fatal_error("copy_long_value: MR_LONG_LVAL_TYPE_DOUBLE_STACKVAR");
        break;

    case MR_LONG_LVAL_TYPE_DOUBLE_FRAMEVAR:
        MR_fatal_error("copy_long_value: MR_LONG_LVAL_TYPE_DOUBLE_FRAMEVAR");
        break;

    case MR_LONG_LVAL_TYPE_SUCCIP:
        break;

    case MR_LONG_LVAL_TYPE_MAXFR:
        break;

    case MR_LONG_LVAL_TYPE_CURFR:
        break;

    case MR_LONG_LVAL_TYPE_HP:
        // Currently we don't support heap reclamation on failure
        // with accurate GC, so we shouldn't get any saved HP values.
        // XXX To support this, saved heap pointer values would need to be
        // updated to point to the new heap... but this is tricky -- see the
        // CVS log message for revision 1.21 of runtime/mercury_accurate_gc.c.

        MR_fatal_error("copy_long_value: MR_LONG_LVAL_TYPE_HP");
        break;

    case MR_LONG_LVAL_TYPE_SP:
        break;

    case MR_LONG_LVAL_TYPE_INDIRECT:
        // XXX Tyson will have to write the code for this.
        MR_fatal_error("NYI: copy_long_value on MR_LONG_LVAL_TYPE_INDIRECT");
        break;

    case MR_LONG_LVAL_TYPE_UNKNOWN:
        MR_fatal_error("copy_long_value: MR_LONG_LVAL_TYPE_UNKNOWN");
        break;

    default:
        MR_fatal_error("Unknown MR_LongLvalType in copy_long_value");
        break;
    }
}

static void
copy_short_value(MR_ShortLval locn, MR_TypeInfo type_info,
     MR_bool registers_live, MR_Word *stack_pointer, MR_Word *current_frame)
{
    int     locn_num;
    MR_Word copy;

    switch (MR_SHORT_LVAL_TYPE(locn)) {
    case MR_SHORT_LVAL_TYPE_R:
        if (registers_live) {
            locn_num = MR_SHORT_LVAL_NUMBER(locn);
            copy = MR_agc_deep_copy(MR_virtual_reg_value(locn_num), type_info,
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
                MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
            MR_virtual_reg_assign(locn_num, copy);
        }
        break;

    case MR_SHORT_LVAL_TYPE_STACKVAR:
        locn_num = MR_SHORT_LVAL_NUMBER(locn);
        MR_based_stackvar(stack_pointer, locn_num) = MR_agc_deep_copy(
            MR_based_stackvar(stack_pointer, locn_num), type_info,
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
        break;

    case MR_SHORT_LVAL_TYPE_FRAMEVAR:
        locn_num = MR_SHORT_LVAL_NUMBER(locn);
        MR_based_framevar(current_frame, locn_num) = MR_agc_deep_copy(
            MR_based_framevar(current_frame, locn_num), type_info,
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
        break;

    default:
        MR_fatal_error("Unknown MR_ShortLval_Type in copy_short_value");
        break;
    }
}

// Compute the new size at which to GC,
// and reset the redzone on the new heap.

static void
resize_and_reset_redzone(MR_MemoryZone *old_heap, MR_MemoryZone *new_heap)
{
    // These counts include some wasted space between
    // ->MR_zone_min and ->MR_zone_bottom.

    size_t old_heap_space =
        (char *) old_heap->MR_zone_redzone_base -
            (char *) old_heap->MR_zone_bottom;
    size_t new_heap_usage =
        (char *) MR_virtual_hp - (char *) new_heap->MR_zone_bottom;
    size_t gc_heap_size;

    // Set the size at which to GC to be MR_heap_expansion_factor
    // (which defaults to two) times the current usage,
    // or the size at which we GC'd last time, whichever is larger.

    gc_heap_size = MR_round_up(
        (size_t) (MR_heap_expansion_factor * new_heap_usage),
        MR_unit);
    if (gc_heap_size < old_heap_space) {
        gc_heap_size = old_heap_space;
    }

    // Reset the redzone on the new heap
    old_heap->MR_zone_redzone_base = (MR_Word *)
        ((char *) old_heap->MR_zone_bottom + gc_heap_size);
    MR_reset_redzone(old_heap);
}

#endif // !MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
// Subroutines shared between both the LLDS and MLDS versions of the collector.

// Print debugging messages at the start of a collection.

static void
notify_gc_start(const MR_MemoryZone *old_heap, const MR_MemoryZone *new_heap)
{
    if (MR_agc_debug) {
        fprintf(stderr, "\nGarbage collection started.\n");

        fprintf(stderr, "old_heap->min:  %lx \t old_heap->hardmax:  %lx\n",
            (long) old_heap->MR_zone_min, (long) old_heap->MR_zone_hardmax);
        fprintf(stderr, "new_heap->min: %lx \t new_heap->hardmax: %lx\n",
            (long) new_heap->MR_zone_min, (long) new_heap->MR_zone_hardmax);

        fprintf(stderr, "MR_virtual_hp:  %lx\n", (long) MR_virtual_hp);
    }
}

// Print debugging messages at the end of a collection.

static void
notify_gc_end(const MR_MemoryZone *old_heap, const MR_MemoryZone *new_heap,
    const MR_Word *old_hp)
{
    if (MR_agc_debug) {
        MR_Word *new_hp;

        // XXX Save this, it appears to get clobbered.
        new_hp = MR_virtual_hp;

        MR_agc_dump_roots(root_list);

        // XXX Restore this, it appears to get clobbered.
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
        MR_virtual_hp_word = (MR_Word) new_hp;
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);

        fprintf(stderr, "old heap: %ld bytes, new heap: %ld bytes\n",
            (long) ((char *) old_hp - (char *) old_heap->MR_zone_min),
            (long) ((char *) MR_virtual_hp - (char *) new_heap->MR_zone_min));
        fprintf(stderr, "%ld bytes recovered\n",
            (long) ((char *) old_hp - (char *) old_heap->MR_zone_min) -
            ((char *) MR_virtual_hp - (char *) new_heap->MR_zone_min));

        fprintf(stderr, "Garbage collection done.\n\n");
    }
}

// Initialize the forwarding pointer bitmap (MR_has_forwarding_pointer[])
// with all bits unset (zero).

static void
init_forwarding_pointer_bitmap(const MR_MemoryZone *old_heap, MR_Word *old_hp)
{
    size_t              heap_size_in_words;
    size_t              num_words_for_bitmap;
    size_t              num_bytes_for_bitmap;
    static size_t       prev_num_bytes_for_bitmap;

    heap_size_in_words = old_hp - old_heap->MR_zone_min;
    num_words_for_bitmap = (heap_size_in_words + MR_WORDBITS - 1) / MR_WORDBITS;
    num_bytes_for_bitmap = num_words_for_bitmap * sizeof(MR_Word);
    if (MR_has_forwarding_pointer == NULL
        || num_bytes_for_bitmap > prev_num_bytes_for_bitmap)
    {
        MR_has_forwarding_pointer =
            MR_realloc(MR_has_forwarding_pointer, num_bytes_for_bitmap);
        prev_num_bytes_for_bitmap = num_bytes_for_bitmap;
    }
    MR_memset(MR_has_forwarding_pointer, 0, num_bytes_for_bitmap);
}

// Swap the two heaps.

static void
swap_heaps(void)
{
    MR_MemoryZone *tmp;

    tmp = MR_ENGINE(MR_eng_heap_zone2);
    MR_ENGINE(MR_eng_heap_zone2) = MR_ENGINE(MR_eng_heap_zone);
    MR_ENGINE(MR_eng_heap_zone) = tmp;

    if (MR_agc_debug) {
        fprintf(stderr, "Swapped heaps\n");
        fprintf(stderr, "MR_virtual_hp: %lx\n", (long) MR_virtual_hp);
    }
}

// traverse_extra_roots:
//
// Copies the extra roots. The roots are overwritten with the new data.

static void
traverse_extra_roots(void)
{
    MR_RootList current = root_list;

    while (current != NULL) {
        *current->root = MR_agc_deep_copy(*current->root, current->type_info,
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_min),
            MR_ENGINE(MR_eng_heap_zone2->MR_zone_hardmax));
        current = current->next;
    }
}

// If we are debugging, wipe out the contents of the old heap.
// This helps ensure that any bugs in the collector show up sooner.

static void
maybe_clear_old_heap(MR_MemoryZone *old_heap, MR_Word *old_hp)
{
    MR_Word *tmp_hp;

    if (MR_agc_debug) {
        fprintf(stderr, "Clearing old heap:\n");
        for (tmp_hp = old_heap->MR_zone_min; tmp_hp < old_hp; tmp_hp++) {
            *tmp_hp = 0xDEADBEAF;
        }
    }
}

// MR_agc_add_root:
//
// Adds a new root to the set of extra roots.

void
MR_agc_add_root(MR_Word *root_addr, MR_TypeInfo type_info)
{
    MR_RootList node;

    node = MR_malloc(sizeof(*node));
    node->root = root_addr;
    node->type_info = type_info;
    node->next = NULL;

    if (root_list == NULL) {
        root_list = node;
        last_root = node;
    } else {
        last_root->next = node;
        last_root = node;
    }
}

#endif // MR_NATIVE_GC
