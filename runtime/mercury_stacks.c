// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2001, 2003-2006, 2008, 2010-2011 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains code for printing statistics about stack frame sizes,
// and for manipulating the generator stack, the cut stack and the pneg stack.
//
// The generator stack has one entry for each call to a minimal model tabled
// procedure that is (a) acting as the generator for its subgoal and (b) is
// in the active state. In systems such as XSB, each choice point has a flag
// saying whether it is an active generator or not, and if yes, where its
// subgoal's tabling information is stored. We achieve the same effect by
// checking whether a nondet stack frame at a given offset has an entry in
// the generator stack, an approach that minimizes the performance impact
// of tabling on non-tabled procedures.
//
// The cut stack has one entry for each commit goal that execution has entered
// but not yet exited. Each commit stack entry has a list of all the generators
// that have been started inside the corresponding commit goal. When the commit
// goal is exited, it is possible that some of these generators are left
// incomplete; due to the commit, they will in fact never be completed.
// The purpose of the cut stack is to enable us to reset the call table
// entries of such generators to inactive.
//
// All the functions in this file that take MR_TrieNode arguments use
// only the subgoal member of the union.

/*
INIT mercury_sys_init_stacks
ENDINIT
*/

#include "mercury_imp.h"
#include "mercury_runtime_util.h"
#include "mercury_memory_handlers.h"    // for MR_default_handler
#include "mercury_context.h"

#include <stdio.h>

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_STACK_FRAME_STATS

#include "mercury_dword.h"

MR_Dword    MR_det_frame_count;
MR_Dword    MR_det_frame_total_size;
MR_Word     *MR_det_frame_max;
MR_Dword    MR_non_frame_count;
MR_Dword    MR_non_frame_total_size;
MR_Word     *MR_non_frame_max;

MR_uint_least32_t MR_old_low_tmp;

void
MR_init_stack_frame_stats(void)
{
    MR_zero_dword(MR_det_frame_count);
    MR_zero_dword(MR_det_frame_total_size);
    MR_zero_dword(MR_non_frame_count);
    MR_zero_dword(MR_non_frame_total_size);

    // We cannot initialize these to the starts of the their respective
    // memory areas, since those areas may not have been initialized yet.

    MR_det_frame_max = NULL;
    MR_non_frame_max = NULL;
}

void
MR_print_stack_frame_stats(void)
{
    FILE    *fp;
    double  det_frame_count;
    double  det_frame_total_size;
    double  non_frame_count;
    double  non_frame_total_size;

    fp = MR_checked_fopen(MR_STACK_FRAME_STATS, "open", "a");

    MR_convert_dword_to_double(MR_det_frame_count, det_frame_count);
    MR_convert_dword_to_double(MR_det_frame_total_size, det_frame_total_size);
    MR_convert_dword_to_double(MR_non_frame_count, non_frame_count);
    MR_convert_dword_to_double(MR_non_frame_total_size, non_frame_total_size);

    fprintf(fp, "number of det stack frames created:  %.0f\n",
        det_frame_count);
    fprintf(fp, "number of words in det stack frames: %.0f\n",
        det_frame_total_size);
    fprintf(fp, "average size of a det stack frame:   %.3f\n",
        det_frame_total_size / det_frame_count);
    fprintf(fp, "max size of det stack:               %ld\n",
        (long) (MR_det_frame_max - MR_CONTEXT(MR_ctxt_detstack_zone)->min));
    fprintf(fp, "\n");

    fprintf(fp, "number of non stack frames created:  %.0f\n",
        non_frame_count);
    fprintf(fp, "number of words in non stack frames: %.0f\n",
        non_frame_total_size);
    fprintf(fp, "average size of a non stack frame:   %.3f\n",
        non_frame_total_size / non_frame_count);
    fprintf(fp, "max size of non stack:               %ld\n",
        (long) (MR_non_frame_max - MR_CONTEXT(MR_ctxt_nondetstack_zone)->min));
    fprintf(fp, "-------------------------------------------\n");

    MR_checked_fclose(fp, MR_STACK_FRAME_STATS);
}

#endif  // MR_STACK_FRAME_STATS

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_EXTEND_STACKS_WHEN_NEEDED

static  void    MR_debug_zone_extend(FILE *fp, const char *when,
                    const char *stackname, MR_MemoryZone *zone);

void
MR_extend_detstack(void)
{
    MR_MemoryZone   *zone;
    MR_Unsigned     old_size;
    MR_Unsigned     new_size;
    FILE            *debug_fp;

    zone = MR_CONTEXT(MR_ctxt_detstack_zone);
    old_size = zone->MR_zone_desired_size;
    new_size = old_size * 2;

    debug_fp = NULL;
#ifdef  MR_STACK_EXTEND_DEBUG
    if (MR_stack_extend_debug) {
        debug_fp = fopen(".extend_stacks", "a");
    }
#endif

    if (debug_fp != NULL) {
        MR_debug_zone_extend(debug_fp, "before", "detstack", zone);
    }

    (void) MR_extend_zone(zone, new_size);

    if (debug_fp != NULL) {
        MR_debug_zone_extend(debug_fp, "after", "detstack", zone);
    }
}

void
MR_extend_nondetstack(void)
{
    MR_MemoryZone   *zone;
    MR_Unsigned     old_size;
    MR_Unsigned     new_size;
    MR_Integer      base_incr;
    FILE            *debug_fp;

    zone = MR_CONTEXT(MR_ctxt_nondetstack_zone);
    old_size = zone->MR_zone_desired_size;
    new_size = old_size * 2;

    debug_fp = NULL;
#ifdef  MR_STACK_EXTEND_DEBUG
    if (MR_stack_extend_debug) {
        debug_fp = fopen(".extend_stacks", "a");
    }
#endif

    if (debug_fp != NULL) {
        MR_debug_zone_extend(debug_fp, "before", "nondetstack", zone);
    }

    base_incr = MR_extend_zone(zone, new_size);
    // XXX add code to adjust all the links in the nondet stack

    if (debug_fp != NULL) {
        MR_debug_zone_extend(debug_fp, "after", "nondetstack", zone);
    }
}

static void
MR_debug_zone_extend(FILE *fp, const char *when, const char *stackname,
    MR_MemoryZone *zone)
{
    fprintf(fp, "----------------\n");
    fprintf(fp, "%s extending %s\n\n", when, stackname);
    MR_debug_memory_zone(fp, zone);
}

#endif

////////////////////////////////////////////////////////////////////////////

#ifndef  MR_HIGHLEVEL_CODE

#ifdef  MR_STACK_SEGMENTS

MR_declare_entry(MR_pop_detstack_segment);
MR_declare_entry(MR_pop_nondetstack_segment);

MR_Word *
MR_new_detstack_segment(MR_Word *sp, int n)
{
    MR_Word         *old_sp;
    MR_MemoryZones  *list;
    MR_MemoryZone   *new_zone;

    old_sp = sp;

    // We perform explicit overflow checks so redzones just waste space.
    new_zone = MR_create_or_reuse_zone("detstack_segment",
        MR_detstack_size, 0, 0, MR_default_handler);

    list = MR_GC_malloc_uncollectable_attrib(sizeof(MR_MemoryZones),
        MR_ALLOC_SITE_RUNTIME);

#ifdef  MR_DEBUG_STACK_SEGMENTS
    // If you ever need to debug this again, you will probably want to
    // change this debugging code to include the information printed out
    // by MR_new_nondetstack_segment() below.

    MR_debug_log_message(
        "create new det segment: old zone: %p, old sp %p, old succip %p",
        MR_CONTEXT(MR_ctxt_detstack_zone), old_sp, MR_succip);
#endif

    list->MR_zones_head = MR_CONTEXT(MR_ctxt_detstack_zone);
    list->MR_zones_tail = MR_CONTEXT(MR_ctxt_prev_detstack_zones);
    MR_CONTEXT(MR_ctxt_prev_detstack_zones) = list;
    MR_CONTEXT(MR_ctxt_detstack_zone) = new_zone;
    MR_CONTEXT(MR_ctxt_sp) = MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_min;

    MR_sp_word = (MR_Word) MR_CONTEXT(MR_ctxt_sp);

    MR_incr_sp_leaf(2);
    MR_stackvar(1) = (MR_Word) old_sp;
    MR_stackvar(2) = (MR_Word) MR_succip;

    // This may not be for a leaf procedure; we abuse the macro to avoid
    // a check for whether we have run out of the new detstack segment.
    //
    // XXX It is *theoretically* possible for a single stack frame to need
    // more memory than is available in the whole of the new segment.
    // However, if this is true, then the program is screwed anyway.
    // We cannot save it, though we *could* give a meaningful error message
    // instead of just leaving the program to crash.

    MR_incr_sp_leaf(n);

#ifdef  MR_DEBUG_STACK_SEGMENTS
    MR_debug_log_message(
        "create new det segment: new zone: %p, new sp %p new succip: %p",
        MR_CONTEXT(MR_ctxt_detstack_zone), MR_sp,
        MR_ENTRY(MR_pop_detstack_segment));
#endif

    return MR_sp;
}

// There are two ways that normal execution can remove a nondet stack frame.
//
// - backward execution can remove the currently top nondet stack frame
//   by invoking MR_fail(), and
// - forward execution can remove a sequence of nondet stack frames
//   at the top of the nondet stack when it commits to a solution.
//
// We implement commits by taking a snapshot of maxfr and later restoring it.
// Since the nondet stack frames cut away by such a restoration of maxfr
// do not get any control at commits, freeing nondet stack segments only
// when control reaches the frame at the bottom of such segments is clearly
// not sufficient on its own to eventually recover all nondet stack segments.
//
// We could make commits free all nondet stack segments beyond the one
// containing the restored maxfr. However, that solution has three problems.
//
// - It requires stack-segment-specific code at commits.
// - It requires more code at commits, slowing them down.
// - It is likely that the freed segment will be needed quite soon. Freeing a
//   zone and then allocating it again for the same purpose is probably
//   slower than simply keeping and reusing it.
//
// We therefore adopt the following technique:
//
// 1 When we fail back to the last frame of a nondet stack segment, we
//   KEEP that segment, but free any other segments beyond this.
//
// 2 When we run out of the current nondet stack segment, we check whether
//   we already have allocated the next segment. If we haven't, we allocate
//   a new one. If we have, we keep that segment, but free any segments
//   beyond it.
//
// 3 We do not recover nondet stack stack segments at commits.
//
// Parts 1 and 2 above each limit the amount of allocated but not currently
// used memory to about one segment. It is possible for the amount of
// allocated but not currently used nondet stack space to exceed twice the
// size of a segment, possibly by a lot, but in only one circumstance:
// *after* a commit cuts away several segments of nondet stack, and *before*
// the next time the program reaches either end (min or max) of the current
// segment of the nondet stack. If the program uses the nondet stack at all
// intensively, and if nondet stack segments are small, then this period of
// time *should* be acceptably small. The gain we get from accepting this
// downside is that we don't have to deal with segments except when we are
// at a segment boundary.

static  MR_MemoryZone   *MR_rewind_nondetstack_segments(MR_Word *maxfr);

MR_Word *
MR_new_nondetstack_segment(MR_Word *maxfr, int incr)
{
    MR_Word         *sentinel_maxfr;
    MR_Word         *old_maxfr;
    MR_Word         *old_curfr;
    MR_MemoryZone   *new_cur_zone;
    MR_MemoryZones  *new_prev_zones;

    old_maxfr = maxfr;
    old_curfr = MR_curfr;

#ifdef  MR_DEBUG_STACK_SEGMENTS
    printf("\nadding new nondet stack segment");
    printf("\ncontext: %p", &MR_ENGINE(MR_eng_context));
    printf("\nold maxfr: ");
    MR_printnondetstack(stdout, old_maxfr);
    printf("\nold curfr: ");
    MR_printnondetstack(stdout, old_curfr);
    printf("\n");
#endif

    new_cur_zone = MR_rewind_nondetstack_segments(maxfr);
    if (new_cur_zone == NULL) {
        // There is no old segment to reuse in the nondet stack itself,
        // so allocate a new one (possibly one that was freed earlier).
        //
        // Note that we perform explicit overflow checks, so redzones
        // would just waste space.

        new_cur_zone = MR_create_or_reuse_zone("nondetstack_segment",
            MR_nondetstack_size, 0, 0, MR_default_handler);
    }

#ifdef  MR_DEBUG_STACK_SEGMENTS
    printf("\nbefore creating new nondet segment:\n");
    MR_print_zone(stdout, MR_CONTEXT(MR_ctxt_nondetstack_zone));
    printf("\n");
#endif

    new_prev_zones = MR_GC_malloc_uncollectable_attrib(sizeof(MR_MemoryZones),
        MR_ALLOC_SITE_RUNTIME);
    new_prev_zones->MR_zones_head = MR_CONTEXT(MR_ctxt_nondetstack_zone);
    new_prev_zones->MR_zones_tail = MR_CONTEXT(MR_ctxt_prev_nondetstack_zones);
    MR_CONTEXT(MR_ctxt_prev_nondetstack_zones) = new_prev_zones;
    MR_CONTEXT(MR_ctxt_nondetstack_zone) = new_cur_zone;
    MR_CONTEXT(MR_ctxt_maxfr) = new_cur_zone->MR_zone_min;

    MR_maxfr_word = (MR_Word) MR_CONTEXT(MR_ctxt_maxfr);

#ifdef  MR_DEBUG_STACK_SEGMENTS
    printf("\nafter creating new nondet segment\n");
    printf("new maxfr: ");
    MR_printnondetstack(stdout, MR_maxfr);
    printf("\nnew cur zone:\n");
    MR_print_zone(stdout, MR_CONTEXT(MR_ctxt_nondetstack_zone));
    printf("new prev zones:\n");
    MR_print_zones(stdout, MR_CONTEXT(MR_ctxt_prev_nondetstack_zones));
    printf("\n");
    fflush(stdout);
#endif

    // The stack trace tracing code needs to know the size of each nondet
    // stack frame, since it uses the size to classify frames as temp or
    // ordinary. The size is given by the difference in address between
    // the address of the frame and the address of the previous frame.
    // This difference would yield an incorrect size and hence an incorrect
    // frame classification if a temp frame were allowed to have a frame
    // on a different segment as its immediate predecessor.
    //
    // We prevent this by putting an ordinary (i.e. non-temp) frame at the
    // bottom of every new nondet stack segment as a sentinel. We hand-build
    // this frame, since it is not an "ordinary" ordinary frame. It is not
    // created by a call, so it has no meaningful success continuation,
    // and since it does not make any calls, no other frame's success
    // continuation can point to it either.
    //
    // We store three pieces of information in the sentinel frame.
    //
    // - The maxfr at the time the sentinel frame was created, which we store
    //   in the prevfr slot. This is actually the address of the logically
    //   previous frame, so we are using the slot for its intended purpose,
    //   but the difference between the addresses of the two frames is NOT
    //   the size of the sentinel frame.
    //
    // - The curfr at the time the sentinel frame was created, which we store
    //   in the succfr slot. This is NOT actually the frame of the success
    //   continuation; we can store it there because this frame HAS no
    //   meaningful success continuation, so the slot is not needed for its
    //   intended purpose.
    //
    // - The address of the MR_MemoryZone structure of the zone containing
    //   the sentinel frame, which we store in framevar 1. This is used by
    //   the code of MR_pop_nondetstack_segment.

    sentinel_maxfr = MR_maxfr + (MR_NONDET_FIXED_SIZE + 1);
    MR_prevfr_slot_word(sentinel_maxfr) = (MR_Word) old_maxfr;
    MR_succfr_slot_word(sentinel_maxfr) = (MR_Word) old_curfr;
    MR_succip_slot_word(sentinel_maxfr) =
        (MR_Word) MR_ENTRY(MR_do_not_reached);
    MR_redofr_slot_word(sentinel_maxfr) = (MR_Word) sentinel_maxfr;
    MR_redoip_slot_word(sentinel_maxfr) =
        (MR_Word) MR_ENTRY(MR_pop_nondetstack_segment);
    MR_based_framevar(sentinel_maxfr, 1) = (MR_Word) new_cur_zone;

#ifdef  MR_DEBUG_STACK_SEGMENTS
    printf("creating sentinel frame:\n");
    printf("sentinel_maxfr: ");
    MR_printnondetstack(stdout, sentinel_maxfr);
    printf("\nsentinel frame's prevfr slot: ");
    MR_printnondetstack(stdout, MR_prevfr_slot(sentinel_maxfr));
    printf("\nsentinel frame's succfr slot: ");
    MR_printnondetstack(stdout, MR_succfr_slot(sentinel_maxfr));
    printf("\nsentinel frame's redofr slot: ");
    MR_printnondetstack(stdout, MR_redofr_slot(sentinel_maxfr));
    printf("\n");
#endif

    // Reserve space for the new nondet stack frame on top of the
    // sentinel frame.

    MR_maxfr_word = (MR_Word) (sentinel_maxfr + incr);

#ifdef  MR_DEBUG_STACK_SEGMENTS
    printf("after creating sentinel frame and reserving %d words:\n", incr);
    printf("new maxfr: ");
    MR_printnondetstack(stdout, MR_maxfr);
    printf("\n");
    fflush(stdout);
#endif

    return MR_maxfr;
}

static MR_MemoryZone *
MR_rewind_nondetstack_segments(MR_Word *maxfr)
{
    MR_MemoryZone   *zone_to_reuse;
    MR_MemoryZone   *zone;
    MR_Word         *limit;
    MR_MemoryZones  *list;

    zone_to_reuse = NULL;

    for (;;) {
        zone = MR_CONTEXT(MR_ctxt_nondetstack_zone);
        limit = (MR_Word *) zone->MR_zone_end;
        if (maxfr >= zone->MR_zone_min && maxfr < limit) {
            break;
        }

#ifdef  MR_DEBUG_STACK_SEGMENTS
        printf("\nfreeing zone\n");
        MR_print_zone(stdout, zone);
#endif

        // If there are several currently unneeded segments, this algorithm
        // reuses the zone of the topmost segment (the first segment in the
        // list from the top), since its contents are more likely to have been
        // recently referred to, and thus more likely to be in the cache.
        //
        // However, reusing the zone of the bottom-most unneeded segment
        // would look conceptually a bit neater in that it would preserve
        // the follows/precedes relationship between the zones.

        if (zone_to_reuse == NULL) {
            zone_to_reuse = zone;
        } else {
            MR_release_zone(zone);
        }

        list = MR_CONTEXT(MR_ctxt_prev_nondetstack_zones);
        assert(list != NULL);
        MR_CONTEXT(MR_ctxt_nondetstack_zone) = list->MR_zones_head;
        MR_CONTEXT(MR_ctxt_prev_nondetstack_zones) = list->MR_zones_tail;
        MR_GC_free_attrib(list);
    }

#ifdef  MR_DEBUG_STACK_SEGMENTS
    if (zone_to_reuse == NULL) {
        printf("\nno old nondet segment zone available for reuse\n");
    } else {
        printf("\nreturning zone of old nondet segment for reuse: %p\n",
            zone_to_reuse);
    }
#endif

    return zone_to_reuse;
}

// Needed for bootstrapping.

extern void
MR_nondetstack_segment_extend_slow_path(MR_Word *old_maxfr, int incr);

void
MR_nondetstack_segment_extend_slow_path(MR_Word *old_maxfr, int incr)
{
}

#endif  // MR_STACK_SEGMENTS

MR_BEGIN_MODULE(stack_segment_module)
    MR_init_entry_an(MR_pop_detstack_segment);
    MR_init_entry_an(MR_pop_nondetstack_segment);
MR_BEGIN_CODE

MR_define_entry(MR_pop_detstack_segment);
#ifdef MR_STACK_SEGMENTS
{
    MR_MemoryZones  *list;
    MR_Word         *orig_sp;
    MR_Code         *orig_succip;

    orig_sp = (MR_Word *) MR_stackvar(1);
    orig_succip = (MR_Code *) MR_stackvar(2);

  #ifdef MR_DEBUG_STACK_SEGMENTS
    MR_debug_log_message(
        "restore old det segment: old zone %p, old sp %p old succip: %p",
        MR_CONTEXT(MR_ctxt_detstack_zone), MR_sp, MR_succip);
  #endif

    MR_release_zone(MR_CONTEXT(MR_ctxt_detstack_zone));

    list = MR_CONTEXT(MR_ctxt_prev_detstack_zones);
    MR_CONTEXT(MR_ctxt_detstack_zone) = list->MR_zones_head;
    MR_CONTEXT(MR_ctxt_prev_detstack_zones) = list->MR_zones_tail;
    MR_CONTEXT(MR_ctxt_sp) = orig_sp;
    MR_GC_free_attrib(list);

  #ifdef MR_DEBUG_STACK_SEGMENTS
    MR_debug_log_message(
        "restore old det segment: new zone %p, new sp %p new succip: %p",
        MR_CONTEXT(MR_ctxt_detstack_zone), orig_sp, orig_succip);
  #endif

    MR_sp_word = (MR_Word) orig_sp;
    MR_GOTO(orig_succip);
}
#else   // ! MR_STACK_SEGMENTS
    MR_fatal_error("MR_pop_detstack_segment reached\n");
#endif  // MR_STACK_SEGMENTS

MR_define_entry(MR_pop_nondetstack_segment);
#ifdef MR_STACK_SEGMENTS
{
    // See the big comment before MR_new_nondetstack_segment.
    MR_Word         *sentinel_frame;
    MR_Word         *orig_maxfr;
    MR_Word         *orig_curfr;
    MR_MemoryZone   *orig_zone;
    MR_MemoryZone   *cur_zone;
    MR_MemoryZones  *prev_zones;
    unsigned        num_segments_removed;
    MR_bool         released_orig_zone;

    sentinel_frame = MR_maxfr;
    orig_maxfr = (MR_Word *) MR_prevfr_slot(sentinel_frame);
    orig_curfr = (MR_Word *) MR_succfr_slot(sentinel_frame);
    orig_zone = (MR_MemoryZone *) MR_based_framevar(sentinel_frame, 1);

    cur_zone = MR_CONTEXT(MR_ctxt_nondetstack_zone);
    prev_zones = MR_CONTEXT(MR_ctxt_prev_nondetstack_zones);

#ifdef MR_DEBUG_STACK_SEGMENTS
    printf("\nbefore removing old nondet segment:\n");

    printf("orig maxfr: ");
    MR_print_nondetstackptr(stdout, orig_maxfr);
    printf("\norig curfr: ");
    MR_print_nondetstackptr(stdout, orig_curfr);
    printf("\norig zone:\n");
    MR_print_zone(stdout, orig_zone);

    printf("\ncur zone:\n");
    MR_print_zone(stdout, cur_zone);
    printf("prev zones:\n");
    MR_print_zones(stdout, prev_zones);

    fflush(stdout);
#endif

    // As explained in the big comment above, we do not free the zone
    // of the segment we are leaving. It is very likely that we will need
    // it again, very soon, and reusing it from the list of nondet stack
    // segments in the context is significantly cheaper than reusing it
    // from the general pool.

    num_segments_removed = 0;
    while (cur_zone != orig_zone) {
        MR_MemoryZones  *list_node_to_free;

        num_segments_removed++;
        MR_release_zone(cur_zone);

        list_node_to_free = prev_zones;
        cur_zone = prev_zones->MR_zones_head;
        prev_zones = prev_zones->MR_zones_tail;
        MR_GC_free_attrib(list_node_to_free);
    }

    MR_CONTEXT(MR_ctxt_nondetstack_zone) = cur_zone;
    MR_CONTEXT(MR_ctxt_prev_nondetstack_zones) = prev_zones;

    MR_CONTEXT(MR_ctxt_curfr) = orig_curfr;
    MR_CONTEXT(MR_ctxt_maxfr) = orig_maxfr;
    MR_curfr_word = (MR_Word) orig_curfr;
    MR_maxfr_word = (MR_Word) orig_maxfr;

#ifdef MR_DEBUG_STACK_SEGMENTS
    printf("\nafter removing %d old nondet segment(s):\n",
        num_segments_removed);
    printf("cur zone:\n");
    MR_print_zone(stdout, MR_CONTEXT(MR_ctxt_nondetstack_zone));
    printf("prev zones:\n");
    MR_print_zones(stdout, MR_CONTEXT(MR_ctxt_prev_nondetstack_zones));

    printf("maxfr: ");
    MR_print_nondetstackptr(stdout, MR_maxfr);
    printf("\ncurfr: ");
    MR_print_nondetstackptr(stdout, MR_curfr);
    printf("\n");

    fflush(stdout);
#endif

    MR_redo();
}
#else   // ! MR_STACK_SEGMENTS
    MR_fatal_error("MR_pop_nondetstack_segment reached\n");
#endif  // MR_STACK_SEGMENTS

MR_END_MODULE

#endif // !MR_HIGHLEVEL_CODE

// Forward decls to suppress gcc warnings.
void mercury_sys_init_stacks_init(void);
void mercury_sys_init_stacks_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_stacks_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_stacks_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    stack_segment_module();
#endif
}

void mercury_sys_init_stacks_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_stacks_write_out_proc_statics(FILE *fp)
{
    // No proc_statics to write out.
}
#endif

////////////////////////////////////////////////////////////////////////////

#undef MR_TABLE_DEBUG

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

MR_Integer          MR_gen_next_var;
MR_GenStackFrame    *MR_gen_stack_var;

MR_Integer          MR_cut_next_var;
MR_CutStackFrame    *MR_cut_stack_var;

MR_Integer          MR_pneg_next_var;
MR_PNegStackFrame   *MR_pneg_stack_var;

#ifdef  MR_MINIMAL_MODEL_DEBUG
static  int         MR_pneg_cut_depth = 0;
#endif

static  void    MR_print_gen_stack_entry(FILE *fp, MR_Integer i,
                    MR_GenStackFrame *p);

static  void    MR_cleanup_generator_ptr(MR_SubgoalPtr generator_ptr);
static  void    MR_print_cut_stack_entry(FILE *fp, MR_Integer i,
                    MR_CutStackFrame *p);

static  void    MR_cleanup_consumer_ptr(MR_TrieNode consumer_ptr);
static  void    MR_print_pneg_stack_entry(FILE *fp, MR_Integer i,
                    MR_PNegStackFrame *p);

////////////////////////////////////////////////////////////////////////////

// Record that the nondet stack frame at address frame_addr is now the
// generator for subgoal.

void
MR_push_generator(MR_Word *frame_addr, MR_SubgoalPtr subgoal)
{
    MR_gen_stack[MR_gen_next].MR_gen_frame = frame_addr;
    MR_gen_stack[MR_gen_next].MR_gen_subgoal = subgoal;
    MR_gen_next++;

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("push ");
        MR_print_gen_stack_entry(stdout, MR_gen_next - 1,
            &MR_gen_stack[MR_gen_next - 1]);
    }
#endif
}

// Return the subgoal of the topmost generator on the nondet stack.

MR_Subgoal *
MR_top_generator_table(void)
{
#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("top ");
        MR_print_gen_stack_entry(stdout, MR_gen_next - 1,
            &MR_gen_stack[MR_gen_next - 1]);
    }
#endif

    return MR_gen_stack[MR_gen_next - 1].MR_gen_subgoal;
}

// Record the deletion of the topmost generator on the nondet stack.

void
MR_pop_generator(void)
{
    --MR_gen_next;

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("pop ");
        MR_print_gen_stack_entry(stdout, MR_gen_next,
            &MR_gen_stack[MR_gen_next]);
    }
#endif
}

void
MR_print_gen_stack(FILE *fp)
{
    MR_print_any_gen_stack(fp, MR_gen_next, MR_gen_stack);
}

void
MR_print_any_gen_stack(FILE *fp, MR_Integer gen_next,
    MR_GenStackFrame *gen_block)
{
    MR_Integer  i;

    fprintf(fp, "gen stack size: %d\n", (int) gen_next);
    for (i = gen_next - 1; i >= 0; i--) {
        MR_print_gen_stack_entry(fp, i, &MR_gen_stack[i]);
    }
}

static void
MR_print_gen_stack_entry(FILE *fp, MR_Integer i, MR_GenStackFrame *p)
{
    MR_SubgoalDebug *subgoal_debug;

    fprintf(fp, "gen %ld = <", (long) i);
    MR_print_nondetstackptr(fp, p->MR_gen_frame);
    subgoal_debug = MR_lookup_subgoal_debug_addr(p->MR_gen_subgoal);
    fprintf(fp, ", %s>\n", MR_subgoal_debug_name(subgoal_debug));
}

////////////////////////////////////////////////////////////////////////////

// Record the entering of a committed choice context.

void
MR_commit_mark(void)
{
    MR_restore_transient_registers();

    MR_cut_stack[MR_cut_next].MR_cut_frame = MR_maxfr;
    MR_cut_stack[MR_cut_next].MR_cut_gen_next = MR_gen_next;
    MR_cut_stack[MR_cut_next].MR_cut_generators = NULL;
#ifdef  MR_MINIMAL_MODEL_DEBUG
    MR_cut_stack[MR_cut_next].MR_cut_depth = MR_pneg_cut_depth;
    MR_pneg_cut_depth++;
#endif
    MR_cut_next++;

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("commit stack next up to %ld\n", (long) MR_cut_next);
    }
#endif

    MR_save_transient_registers();
}

// Record the leaving of a committed choice context, and clean up the
// generators that were created within the context that are still active.
// We need to clean them up because otherwise, consumers will be depend on this
// generator to find all the answers to the generator's subgoal, but the
// generation will never compute any more answers, since it will never be
// backtracked into.

void
MR_commit_cut(void)
{
    MR_CutGeneratorList g;

    --MR_cut_next;

#ifdef  MR_MINIMAL_MODEL_DEBUG
    --MR_pneg_cut_depth;
#endif

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("commit stack next down to %ld\n",
            (long) MR_cut_next);
        printf("setting generator stack next back to %ld from %ld\n",
            (long) MR_cut_stack[MR_cut_next].MR_cut_gen_next,
            (long) MR_gen_next);

        if (MR_gen_next < MR_cut_stack[MR_cut_next].MR_cut_gen_next) {
            printf("MR_gen_next %ld, MR_cut_next %ld, "
                "MR_cut_stack[MR_cut_next].gen_next %ld\n",
                (long) MR_gen_next,
                (long) MR_cut_next,
                (long) MR_cut_stack[MR_cut_next].MR_cut_gen_next);
            MR_fatal_error("GEN_NEXT ASSERTION FAILURE");
        }
    }
#endif

    for (g = MR_cut_stack[MR_cut_next].MR_cut_generators; g != NULL;
        g = g->MR_cut_next_generator)
    {
        MR_cleanup_generator_ptr(g->MR_cut_generator_ptr);
    }

    MR_cut_stack[MR_cut_next].MR_cut_generators = NULL;
    MR_gen_next = MR_cut_stack[MR_cut_next].MR_cut_gen_next;
}

// Record the creation of a generator, for possible cleanup later by
// MR_commit_cut.

void
MR_register_generator_ptr(MR_SubgoalPtr subgoal)
{
    struct MR_CutGeneratorListNode  *node;

    if (MR_cut_next <= 0) {
        return;
    }

    node = MR_GC_NEW_ATTRIB(struct MR_CutGeneratorListNode,
        MR_ALLOC_SITE_RUNTIME);
    node->MR_cut_generator_ptr = subgoal;
    node->MR_cut_next_generator =
        MR_cut_stack[MR_cut_next - 1].MR_cut_generators;
    MR_cut_stack[MR_cut_next - 1].MR_cut_generators = node;

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("registering generator %p -> %s at commit stack level %d\n",
            subgoal, MR_subgoal_addr_name(subgoal), MR_cut_next - 1);
    }
#endif
}

static void
MR_cleanup_generator_ptr(MR_SubgoalPtr subgoal)
{
    if (subgoal->MR_sg_status == MR_SUBGOAL_COMPLETE) {
        // There is nothing to do, everything is OK.
#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("no cleanup: generator %p -> %s is complete\n",
                subgoal->MR_sg_back_ptr, MR_subgoal_addr_name(subgoal));
        }
#endif
    } else {
        // This generator will never complete the subgoal.
        MR_ConsumerList consumer_list;

#ifdef  MR_TABLE_DEBUG
        if (MR_tabledebug) {
            printf("cleanup: generator %p -> %s deleted\n",
                subgoal->MR_sg_back_ptr, MR_subgoal_addr_name(subgoal));
        }
#endif

        subgoal->MR_sg_back_ptr->MR_subgoal = NULL;
        subgoal->MR_sg_back_ptr = NULL;

        for (consumer_list = subgoal->MR_sg_consumer_list;
            consumer_list != NULL;
            consumer_list = consumer_list->MR_cl_next)
        {
#ifdef  MR_TABLE_DEBUG
            if (MR_tabledebug) {
                printf("cleanup: consumer %s is deleted",
                    MR_consumer_addr_name(consumer_list->MR_cl_item));
            }
#endif

            consumer_list->MR_cl_item->MR_cns_subgoal = NULL;
        }
    }
}

void
MR_print_cut_stack(FILE *fp)
{
    MR_print_any_cut_stack(fp, MR_cut_next, MR_cut_stack);
}

void
MR_print_any_cut_stack(FILE *fp, MR_Integer cut_next,
    MR_CutStackFrame *cut_block)
{
    MR_Integer  i;

    fprintf(fp, "cut stack size: %d\n", (int) cut_next);
    for (i = cut_next - 1; i >= 0; i--) {
        MR_print_cut_stack_entry(fp, i, &cut_block[i]);
    }
}

static void
MR_print_cut_stack_entry(FILE *fp, MR_Integer i, MR_CutStackFrame *p)
{
    MR_SubgoalDebug     *subgoal_debug;
    MR_CutGeneratorList gen_list;

    fprintf(fp, "cut %ld = <", (long) i);
    MR_print_nondetstackptr(fp, p->MR_cut_frame);
    fprintf(fp, ">");
    fprintf(fp, ", cut_gen_next %d", (int) p->MR_cut_gen_next);
#ifdef  MR_MINIMAL_MODEL_DEBUG
    fprintf(fp, ", pneg+cut stack depth %d", (int) p->MR_cut_depth);
#endif
    fprintf(fp, "\n");

    fprintf(fp, "registered generators:");
    gen_list = p->MR_cut_generators;
    if (gen_list == NULL) {
        fprintf(fp, " none");
    } else {
        while (gen_list != NULL) {
            if (gen_list->MR_cut_generator_ptr == NULL) {
                fprintf(fp, " <NULL>");
            } else {
                subgoal_debug = MR_lookup_subgoal_debug_addr(
                    gen_list->MR_cut_generator_ptr);
                fprintf(fp, " <%s>", MR_subgoal_debug_name(subgoal_debug));
            }

            gen_list = gen_list->MR_cut_next_generator;
        }
    }

    fprintf(fp, "\n");
}

////////////////////////////////////////////////////////////////////////////

void
MR_register_suspension(MR_Consumer *consumer)
{
    MR_PNegConsumerList node_ptr;

    if (MR_pneg_next <= 0) {
        return;
    }

    node_ptr = MR_TABLE_NEW(MR_PNegConsumerListNode);
    node_ptr->MR_pneg_consumer_ptr = consumer;
    node_ptr->MR_pneg_next_consumer =
        MR_pneg_stack[MR_pneg_next - 1].MR_pneg_consumers;
    MR_pneg_stack[MR_pneg_next - 1].MR_pneg_consumers = node_ptr;
}

void
MR_pneg_enter_cond(void)
{
    MR_restore_transient_registers();

    MR_pneg_stack[MR_pneg_next].MR_pneg_frame = MR_maxfr;
    MR_pneg_stack[MR_pneg_next].MR_pneg_consumers = NULL;
#ifdef  MR_MINIMAL_MODEL_DEBUG
    MR_pneg_stack[MR_pneg_next].MR_pneg_gen_next = MR_gen_next;
    MR_pneg_stack[MR_pneg_next].MR_pneg_depth = MR_pneg_cut_depth;
    MR_pneg_cut_depth++;
#endif
    MR_pneg_next++;

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("pneg stack next up to %ld\n", (long) MR_pneg_next);
    }
#endif

    MR_save_transient_registers();
}

void
MR_pneg_enter_then(void)
{
    MR_PNegConsumerList l;
    MR_PNegConsumerList next;

    MR_restore_transient_registers();

    --MR_pneg_next;

#ifdef  MR_MINIMAL_MODEL_DEBUG
    --MR_pneg_cut_depth;
#endif

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("pneg stack down up to %ld (then)\n", (long) MR_pneg_next);
    }
#endif

    for (l = MR_pneg_stack[MR_pneg_next].MR_pneg_consumers; l != NULL;
        l = next)
    {
        next = l->MR_pneg_next_consumer;
        MR_table_free(l);
    }

    MR_save_transient_registers();
}

void
MR_pneg_enter_else(const char *context)
{
    MR_PNegConsumerList l;
    MR_PNegConsumerList next;
    MR_PNegConsumerList consumer_list;

    MR_restore_transient_registers();

    --MR_pneg_next;

#ifdef  MR_MINIMAL_MODEL_DEBUG
    --MR_pneg_cut_depth;
#endif

#ifdef  MR_TABLE_DEBUG
    if (MR_tabledebug) {
        printf("pneg stack down up to %ld (else)\n", (long) MR_pneg_next);
    }
#endif

    consumer_list = MR_pneg_stack[MR_pneg_next].MR_pneg_consumers;
    for (l = consumer_list; l != NULL; l = next) {
        MR_Subgoal  *subgoal;
        MR_Consumer *consumer;

        next = l->MR_pneg_next_consumer;
        consumer = l->MR_pneg_consumer_ptr;
        if (consumer->MR_cns_subgoal == NULL) {
            // This consumer has logically been deleted.
            continue;
        }

        subgoal = consumer->MR_cns_subgoal;
        if (subgoal->MR_sg_status != MR_SUBGOAL_COMPLETE) {
            const char  *msg;
            int         len;
            char        *buf;

            msg = "failing out of negated context with incomplete consumer";
            if (context != NULL) {
                // The 10 accounts for the ": ", the final '\0',
                // and leaves some space to spare.

                len = strlen(context) + strlen(msg) + 10;
                buf = malloc(len);
                if (buf != NULL) {
                    snprintf(buf, len, "%s: %s", context, msg);
                    MR_fatal_error(buf);
                } else {
                    MR_fatal_error(msg);
                }
            } else {
                MR_fatal_error(msg);
            }
        }

        MR_table_free(l);
    }

    MR_save_transient_registers();
}

void
MR_print_pneg_stack(FILE *fp)
{
    MR_print_any_pneg_stack(fp, MR_pneg_next, MR_pneg_stack);
}

void
MR_print_any_pneg_stack(FILE *fp, MR_Integer pneg_next,
    MR_PNegStackFrame *pneg_block)
{
    MR_Integer  i;

    fprintf(fp, "pneg stack size: %d\n", (int) pneg_next);
    for (i = MR_pneg_next - 1; i >= 0; i--) {
        MR_print_pneg_stack_entry(fp, i, &pneg_block[i]);
    }
}

static void
MR_print_pneg_stack_entry(FILE *fp, MR_Integer i, MR_PNegStackFrame *p)
{
    MR_PNegConsumerList l;

    fprintf(fp, "pneg %d = <", (int) i);
    MR_print_nondetstackptr(fp, p->MR_pneg_frame);
    fprintf(fp, ">");
#ifdef  MR_MINIMAL_MODEL_DEBUG
    fprintf(fp, ", pneg_gen_next %d", (int) p->MR_pneg_gen_next);
    fprintf(fp, ", pneg+cut stack depth %d\n", (int) p->MR_pneg_depth);
#endif
    fprintf(fp, "\n");

    fprintf(fp, "registered consumers: ");
    if (p->MR_pneg_consumers == NULL) {
        fprintf(fp, " none");
    } else {
        MR_Consumer *consumer;
        int         n;

        for (n = 1, l = p->MR_pneg_consumers; l != NULL;
            l = l->MR_pneg_next_consumer, n++)
        {
            consumer = l->MR_pneg_consumer_ptr;
            fprintf(fp, " <%d: %s>", n, MR_consumer_addr_name(consumer));
        }
    }

    fprintf(fp, "\n");
}

////////////////////////////////////////////////////////////////////////////

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY
