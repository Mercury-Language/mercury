// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007, 2009, 2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// file: mercury_region.c
// main author: qph

#include "mercury_imp.h"
#include "mercury_region.h"

#ifdef MR_USE_REGIONS

#define word_sizeof(s) (sizeof(s) / sizeof(MR_Word))

MR_RegionPage       *MR_region_free_page_list;
MR_RegionHeader     *MR_live_region_list;

MR_RegionIteFixedFrame      *MR_region_ite_sp = NULL;
MR_RegionDisjFixedFrame     *MR_region_disj_sp = NULL;
MR_RegionCommitFixedFrame   *MR_region_commit_sp = NULL;

MR_Word             MR_region_sequence_number = 1;

#if defined(MR_RBMM_PROFILING)

MR_RegionProfUnit   MR_rbmmp_regions_used = {0, 0, 0};
unsigned int        MR_rbmmp_biggest_region_size = 0;
int                 MR_rbmmp_biggest_region = 0;
unsigned int        MR_rbmmp_regions_instant_reclaimed_at_disj = 0;
unsigned int        MR_rbmmp_regions_instant_reclaimed_at_ite = 0;
unsigned int        MR_rbmmp_regions_reclaimed_by_remove_instr = 0;
unsigned int        MR_rbmmp_regions_reclaimed_at_then_part = 0;
unsigned int        MR_rbmmp_regions_reclaimed_at_commit = 0;

MR_RegionProfUnit   MR_rbmmp_words_used = {0, 0, 0};
unsigned int        MR_rbmmp_words_instant_reclaimed_new_alloc = 0;
unsigned int        MR_rbmmp_words_instant_reclaimed_new_regions = 0;
unsigned int        MR_rbmmp_words_reclaimed_by_remove_instr = 0;
unsigned int        MR_rbmmp_words_reclaimed_at_then_part = 0;
unsigned int        MR_rbmmp_words_reclaimed_at_commit = 0;

MR_RegionProfUnit   MR_rbmmp_pages_used = {0, 0, 0};
unsigned int        MR_rbmmp_pages_requested = 0;
double              MR_rbmmp_page_utilized;
unsigned int        MR_rbmmp_pages_instant_reclaimed_new_alloc = 0;
unsigned int        MR_rbmmp_pages_instant_reclaimed_new_regions = 0;
unsigned int        MR_rbmmp_pages_reclaimed_by_remove_instr = 0;
unsigned int        MR_rbmmp_pages_reclaimed_at_then_part = 0;
unsigned int        MR_rbmmp_pages_reclaimed_at_commit = 0;

MR_RegionProfUnit   MR_rbmmp_num_disj_frames = {0, 0, 0};
MR_RegionProfUnit   MR_rbmmp_words_used_by_disj_frames = {0, 0, 0};
unsigned int        MR_rbmmp_regions_protected_at_semi_disj = 0;
unsigned int        MR_rbmmp_snapshots_saved_at_disj = 0;

MR_RegionProfUnit   MR_rbmmp_num_ite_frames = {0, 0, 0};
MR_RegionProfUnit   MR_rbmmp_words_used_by_ite_frames = {0, 0, 0};
unsigned int        MR_rbmmp_regions_protected_at_ite = 0;
unsigned int        MR_rbmmp_snapshots_saved_at_ite = 0;

MR_RegionProfUnit   MR_rbmmp_num_commit_frames = {0, 0, 0};
MR_RegionProfUnit   MR_rbmmp_words_used_by_commit_frames = {0, 0, 0};
unsigned int        MR_rbmmp_regions_saved_at_commit = 0;

#endif

// Request for more pages from the operating system.
static MR_RegionPage    *MR_region_request_pages(void);

// Take a page from the free page list.
static MR_RegionPage    *MR_region_get_free_page(void);

static void             MR_region_nullify_entries_in_commit_stack(
                            MR_RegionHeader *region);
static void             MR_region_nullify_in_commit_frame(
                            MR_RegionCommitFixedFrame *frame,
                            MR_RegionHeader *region);
static void             MR_region_nullify_in_ite_frame(
                            MR_RegionHeader *region);

static void             MR_region_extend_region(MR_RegionHeader *);

#ifdef  MR_RBMM_DEBUG
static int              MR_region_get_frame_number(MR_Word *);
#endif

#ifdef  MR_RBMM_PROFILING
static void             MR_region_print_profiling_unit(const char *str,
                            MR_RegionProfUnit *profiling_unit);
#endif

static void             MR_region_get_new_pages_and_new_words(
                            MR_RegionSnapshot *snapshot, int *new_pages,
                            int *new_words);

////////////////////////////////////////////////////////////////////////////
// Page operations.

static MR_RegionPage *
MR_region_request_pages(void)
{
    MR_RegionPage   *pages;
    int             bytes_to_request;
    int             i;

    bytes_to_request = MR_REGION_NUM_PAGES_TO_REQUEST * sizeof(MR_RegionPage);
    pages = (MR_RegionPage *) MR_malloc(bytes_to_request);
    if (pages == NULL) {
        MR_fatal_error("Cannot request more memory from the operating system");
    }

    pages[0].MR_regionpage_next = NULL;
    for (i = 1; i < MR_REGION_NUM_PAGES_TO_REQUEST; i++) {
        pages[i].MR_regionpage_next = &pages[i - 1];
    }

#if defined(MR_RBMM_PROFILING)
    MR_rbmmp_pages_requested += MR_REGION_NUM_PAGES_TO_REQUEST;
#endif

    return &(pages[MR_REGION_NUM_PAGES_TO_REQUEST - 1]);
}

static MR_RegionPage *
MR_region_get_free_page(void)
{
    MR_RegionPage   *page;

    if (MR_region_free_page_list == 0) {
        MR_region_free_page_list = MR_region_request_pages();
    }

    page = MR_region_free_page_list;
    MR_region_free_page_list = MR_region_free_page_list->MR_regionpage_next;
    // Disconnect the first free page from the free list.
    page->MR_regionpage_next = NULL;

#if defined(MR_RBMM_PROFILING)
    MR_region_update_profiling_unit(&MR_rbmmp_pages_used, 1);
#endif

    return page;
}

////////////////////////////////////////////////////////////////////////////
// Region operations.

// Create a region.
// The MR_REGION_PAGE_SPACE_SIZE must be larger than the size of
// Region_Struct.

MR_RegionHeader *
MR_region_create_region(void)
{
    MR_RegionPage   *page;
    MR_RegionHeader *region;

    // This is the first page of the region.
    page = MR_region_get_free_page();

    // In the first page, we will store the region header, which occupies
    // word_sizeof(MR_RegionHeader) words from the start of the data area
    // of the page.

    region = MR_regionpage_to_region(page);
    region->MR_region_next_available_word = (MR_Word *)
        (page->MR_regionpage_space + word_sizeof(MR_RegionHeader));
    region->MR_region_last_page = page;
    region->MR_region_removal_counter = 1;
    region->MR_region_sequence_number = MR_region_sequence_number++;
    region->MR_region_logical_removed = 0;
    region->MR_region_ite_protected = NULL;
    region->MR_region_commit_frame = NULL;
    region->MR_region_destroy_at_commit = 0;

    // Add the region to the head of the live region list.
    if (MR_live_region_list != NULL) {
        MR_live_region_list->MR_region_previous_region = region;
    }
    region->MR_region_next_region = MR_live_region_list;
    region->MR_region_previous_region = NULL;
    MR_live_region_list = region;

    MR_region_debug_create_region(region);

#if defined(MR_RBMM_PROFILING)
    MR_region_update_profiling_unit(&MR_rbmmp_regions_used, 1);
    region->MR_region_allocated_size = 0;
#endif

    return region;
}

static void
MR_region_nullify_entries_in_commit_stack(MR_RegionHeader *region)
{
    MR_RegionCommitFixedFrame *frame;

    frame = region->MR_region_commit_frame;
    while (frame != NULL) {
        MR_region_nullify_in_commit_frame(frame, region);
        frame = frame->MR_rcff_previous_commit_frame;
    }
}

static void
MR_region_nullify_in_commit_frame(MR_RegionCommitFixedFrame *commit_frame,
    MR_RegionHeader *region)
{
    MR_RegionCommitSave *commit_save;
    int                 i;

    commit_save = ( MR_RegionCommitSave *) (
        (MR_Word *) commit_frame + MR_REGION_COMMIT_FRAME_FIXED_SIZE);

    // Loop through the saved regions and nullify the entry of the input
    // region if found.

    for (i = 0; i < commit_frame->MR_rcff_num_saved_regions; i++) {
        if (commit_save != NULL &&
            commit_save->MR_commit_save_region == region)
        {
            commit_save->MR_commit_save_region = NULL;
            break;
        } else {
            commit_save += 1;
        }
    }
}

static void
MR_region_nullify_in_ite_frame(MR_RegionHeader *region)
{
    MR_RegionIteFixedFrame      *ite_frame;
    MR_RegionIteProtect         *ite_prot;
    MR_Word                     *protected_region;
    int                         num_protected_regions;
    int                         i;

    ite_frame = region->MR_region_ite_protected;
    ite_prot = (MR_RegionIteProtect *) ( (MR_Word *) ite_frame +
        MR_REGION_ITE_FRAME_FIXED_SIZE );

    // Loop through the protected regions and nullify the entry of the input
    // region if found.

    num_protected_regions = ite_frame->MR_riff_num_prot_regions;
    for (i = 0; i < num_protected_regions; i++) {
        if (ite_prot != NULL && ite_prot->MR_ite_prot_region == region) {
            ite_prot->MR_ite_prot_region = NULL;
            break;
        } else {
            ite_prot += 1;
        }
    }
}

void
MR_region_destroy_region(MR_RegionHeader *region)
{
    if (region->MR_region_commit_frame != NULL) {
        MR_region_nullify_entries_in_commit_stack(region);
    }

    // Break the region from the live region list.
    if (region == MR_live_region_list) {
        // Detach the newest.
        MR_live_region_list = region->MR_region_next_region;
    } else {
        region->MR_region_previous_region->MR_region_next_region =
            region->MR_region_next_region;
        if (region->MR_region_next_region != NULL) {
            // Detach one in the middle.
            region->MR_region_next_region->MR_region_previous_region =
                region->MR_region_previous_region;
        }
    }

    // Return the page list of the region to the free page list.
    MR_region_return_page_list(MR_region_to_first_regionpage(region),
        region->MR_region_last_page);

    // Collect profiling information.
    MR_region_profile_destroyed_region(region);
}

// This method is to be called at the start of the then part of an ite with
// semidet condition (most of the times).
// At this point, we do not have to check whether this ite-protected region is
// disj-protected or not, because we know for sure that it is not:
//
// - The region is protected for this ite only if it has not been protected
//   before the ite, which means that it is not disj-protected before the ite.
//
// - From the start of this ite to this point (i.e. during the condition)
//   it cannot be disj-protected by any disjunctions. This is because this
//   condition is semidet and cannot contain nondet disjunctions.
//   If it contains semidet disjunctions, at this point they must succeed
//   and therefore the region is not disj-protected for them.

void
MR_remove_undisjprotected_region_ite_then_semidet(MR_RegionHeader *region)
{
    MR_region_destroy_region(region);
#if defined(MR_RBMM_PROFILING)
    MR_region_profile_reclaim_region(region,
        &MR_rbmmp_regions_reclaimed_at_then_part,
        &MR_rbmmp_pages_reclaimed_at_then_part,
        &MR_rbmmp_words_reclaimed_at_then_part);
#endif
    MR_region_debug_destroy_region(region);
}

// This method is to be called at the start of the then part of an ite with
// nondet condition.
// We will destroy the region if it is not disj-protected and we will also
// nullify its entry in the ite frame.

void
MR_remove_undisjprotected_region_ite_then_nondet(MR_RegionHeader *region)
{
    MR_region_debug_try_remove_region(region);

    if ( !MR_region_is_disj_protected(region) ) {
        MR_region_nullify_in_ite_frame(region);
        MR_region_destroy_region(region);
#if defined(MR_RBMM_PROFILING)
        MR_region_profile_reclaim_region(region,
            &MR_rbmmp_regions_reclaimed_at_then_part,
            &MR_rbmmp_pages_reclaimed_at_then_part,
            &MR_rbmmp_words_reclaimed_at_then_part);
#endif
        MR_region_debug_destroy_region(region);
    } else {
        region->MR_region_logical_removed = 1;
        MR_region_debug_logically_remove_region(region);
    }
}

void
MR_region_remove_region(MR_RegionHeader *region)
{
    MR_region_debug_try_remove_region(region);

    if ( region->MR_region_ite_protected == NULL &&
        !(MR_region_is_disj_protected(region)) )
    {
        MR_region_destroy_region(region);
#if defined(MR_RBMM_PROFILING)
        MR_region_profile_reclaim_region(region,
            &MR_rbmmp_regions_reclaimed_by_remove_instr,
            &MR_rbmmp_pages_reclaimed_by_remove_instr,
            &MR_rbmmp_words_reclaimed_by_remove_instr);
#endif
    } else {
        region->MR_region_logical_removed = 1;

        // This logical removal happens in a commit operation. If the region
        // is *new* since the start of the commit we mark the region so that
        // it will be destroyed at the commit point.

        if (MR_region_commit_sp != NULL &&
            MR_region_commit_sp->MR_rcff_saved_sequence_number <=
                region->MR_region_sequence_number)
        {
            region->MR_region_destroy_at_commit = 1;
        }

        // if (MR_region_commit_sp != NULL) {
        //     region->MR_region_destroy_at_commit = 1;
        // }

        MR_region_debug_logically_remove_region(region);
    }
}

MR_Word *
MR_region_alloc(MR_RegionHeader *region, unsigned int words)
{
    MR_Word *allocated_cell;

    if (MR_region_available_space(region->MR_region_last_page,
            region->MR_region_next_available_word) < words)
    {
        MR_region_extend_region(region);
    }

    allocated_cell = region->MR_region_next_available_word;
    // Allocate in the increasing direction of address.
    region->MR_region_next_available_word += words;
#if defined(MR_RBMM_PROFILING)
    MR_region_update_profiling_unit(&MR_rbmmp_words_used, words);
    region->MR_region_allocated_size += words;
    if (region->MR_region_allocated_size > MR_rbmmp_biggest_region_size) {
        MR_rbmmp_biggest_region_size = region->MR_region_allocated_size;
        MR_rbmmp_biggest_region = region->MR_region_sequence_number;
    }
#endif

    return allocated_cell;
}

static void
MR_region_extend_region(MR_RegionHeader *region)
{
    MR_RegionPage   *page;

    page = MR_region_get_free_page();
    region->MR_region_last_page->MR_regionpage_next = page;
    region->MR_region_last_page = page;
    region->MR_region_next_available_word = page->MR_regionpage_space;
}

// Destroy any marked regions allocated before the commit.
void
MR_commit_success_destroy_marked_saved_regions(MR_Word num_saved_regions,
    MR_RegionCommitSave *first_commit_save)
{
    MR_RegionCommitSave     *commit_save;
    MR_RegionHeader         *region;
    int                     i;

    commit_save = first_commit_save;
    for (i = 0; i < num_saved_regions; i++, commit_save++) {
        region = commit_save->MR_commit_save_region;
        if (region != NULL) {
            // The region is saved here and has not been destroyed.
            // XXX Because we save only regions that are live at entry, not
            // live at exit, and not protected at entry, at the commit point it
            // must be the case that a logical removal has happened to the
            // region, So just need to destroy it at commit.

            MR_region_destroy_region(region);
#if defined(MR_RBMM_PROFILING)
            MR_region_profile_reclaim_region(region,
                &MR_rbmmp_regions_reclaimed_at_commit,
                &MR_rbmmp_pages_reclaimed_at_commit,
                &MR_rbmmp_words_reclaimed_at_commit);
#endif
        }
    }
}

// Destroy any marked regions allocated since scope entry, i.e. *new* to
// the commit.

void
MR_commit_success_destroy_marked_new_regions(MR_Word saved_region_seq_number)
{
    MR_RegionHeader *region;

    region = MR_live_region_list;
    while (region != NULL &&
        region->MR_region_sequence_number >= saved_region_seq_number)
    {
        if (region->MR_region_destroy_at_commit) {
            MR_region_destroy_region(region);
#if defined(MR_RBMM_PROFILING)
            MR_region_profile_reclaim_region(region,
                &MR_rbmmp_regions_reclaimed_at_commit,
                &MR_rbmmp_pages_reclaimed_at_commit,
                &MR_rbmmp_words_reclaimed_at_commit);
#endif
        }
        // XXX It is fine to destroy the region and then still use it to find
        // the next one because destroying a region is just returning
        // its pages, the "region" is still there in memory.

        region = region->MR_region_next_region;
    }
}

int
MR_region_is_disj_protected(MR_RegionHeader *region)
{
    if (MR_region_disj_sp != NULL &&
        MR_region_disj_sp->MR_rdff_saved_sequence_number >
            region->MR_region_sequence_number)
    {
       return MR_TRUE;
    }
    return MR_FALSE;
}

#if !defined(MR_RBMM_USE_MACROS)

////////////////////////////////////////////////////////////////////////////
// push_region_frame

void
MR_push_region_ite_frame_proc(MR_RegionIteFixedFrame *new_ite_frame)
{
    new_ite_frame->MR_riff_previous_ite_frame = MR_region_ite_sp;
    new_ite_frame->MR_riff_saved_sequence_number = MR_region_sequence_number;
    MR_region_ite_sp = new_ite_frame;
    MR_region_profile_push_ite_frame;
    MR_region_debug_push_ite_frame(new_ite_frame);
}

void
MR_push_region_disj_frame_proc(MR_RegionDisjFixedFrame *new_disj_frame)
{
    new_disj_frame->MR_rdff_previous_disj_frame = MR_region_disj_sp;
    new_disj_frame->MR_rdff_saved_sequence_number = MR_region_sequence_number;
    new_disj_frame->MR_rdff_num_prot_regions = 0;
    MR_region_disj_sp = new_disj_frame;
    MR_region_profile_push_disj_frame;
    MR_region_debug_push_disj_frame(new_disj_frame);
}

void
MR_push_region_commit_frame_proc(MR_RegionCommitFixedFrame *new_commit_frame)
{
    new_commit_frame->MR_rcff_previous_commit_frame = MR_region_commit_sp;
    new_commit_frame->MR_rcff_saved_sequence_number =
        MR_region_sequence_number;
    new_commit_frame->MR_rcff_saved_disj_sp = MR_region_disj_sp;
    MR_region_commit_sp = new_commit_frame;
    MR_region_profile_push_commit_frame;
    MR_region_debug_push_commit_frame(new_commit_frame);
}

////////////////////////////////////////////////////////////////////////////

int
MR_region_fill_ite_protect_func(MR_RegionIteFixedFrame *ite_sp,
    MR_RegionHeader *region, MR_RegionIteProtect *ite_prot)
{
    int incr;

    MR_region_debug_start("fill_ite_protect");
    if (!MR_region_is_disj_protected(region) &&
        region->MR_region_ite_protected == NULL)
    {
        ite_prot->MR_ite_prot_region = region;
        region->MR_region_ite_protected = ite_sp;
        MR_region_profile_fill_ite_protect;
        MR_region_debug_fill_ite_protect(ite_prot, region);
        incr = 1;
    } else {
        MR_region_debug_fill_ite_protect(NULL, region);
        incr = 0;
    }
    MR_region_debug_end("fill_ite_protect");

    return incr;
}

int
MR_region_fill_ite_snapshot_removed_func(MR_RegionHeader *region,
    MR_RegionSnapshot *snapshot)
{
    int incr;

    MR_region_debug_start("fill_ite_snapshot_removed");
    if ((region->MR_region_ite_protected != NULL &&
        region->MR_region_ite_protected != MR_region_ite_sp) ||
        MR_region_is_disj_protected(region))
    {
        MR_save_snapshot(region, snapshot);
        MR_region_profile_fill_ite_snapshot;
        MR_region_debug_fill_ite_snapshot_removed(snapshot, region);
        incr = 1;
    } else {
        // Else the region is not protected.
        MR_region_debug_fill_ite_snapshot_removed(NULL, region);
        incr = 0;
    }
    MR_region_debug_end("fill_ite_snapshot_removed");

    return incr;
}

void
MR_region_fill_ite_snapshot_not_removed_proc(MR_RegionHeader *region,
    MR_RegionSnapshot *snapshot)
{
    MR_region_debug_start("fill_ite_snapshot_not_removed");
    MR_save_snapshot(region, snapshot);
    MR_region_profile_fill_ite_snapshot;
    MR_region_debug_fill_ite_snapshot_not_removed(snapshot, region);
    MR_region_debug_end("fill_ite_snapshot_not_removed");
}

int
MR_region_fill_semi_disj_protect_func(MR_RegionHeader *region,
    MR_RegionSemiDisjProtect *semi_disj_prot)
{
    int incr;

    MR_region_debug_start("fill_semi_disj_protect");
    if (!MR_region_is_disj_protected(region) &&
        region->MR_region_ite_protected == NULL)
    {
        semi_disj_prot->MR_semi_disj_prot_region = region;
        MR_region_profile_fill_semi_disj_protect;
        MR_region_debug_fill_semi_disj_protect(semi_disj_prot, region);
        incr = 1;
    } else {
        MR_region_debug_fill_semi_disj_protect(NULL, region);
        incr = 0;
    }
    MR_region_debug_end("fill_ite_protect");

    return incr;
}

void
MR_region_fill_disj_snapshot_proc(MR_RegionHeader *region,
    MR_RegionSnapshot *snapshot)
{
    MR_region_debug_start("fill_disj_snapshot");
    MR_save_snapshot(region, snapshot);
    MR_region_profile_fill_disj_snapshot;
    MR_region_debug_fill_disj_snapshot(snapshot, region);
    MR_region_debug_end("fill_disj_snapshot");
}

int
MR_region_fill_commit_func(MR_RegionCommitFixedFrame *top_commit_frame,
    MR_RegionHeader *region, MR_RegionCommitSave *commit_save)
{
    int incr;

    MR_region_debug_start("fill_commit");
    if (!MR_region_is_disj_protected(region) &&
        region->MR_region_ite_protected == NULL)
    {
        commit_save->MR_commit_save_region = region;
        region->MR_region_commit_frame = top_commit_frame;
        MR_region_profile_fill_commit;
        MR_region_debug_fill_commit(commit_save, region);
        incr = 1;
    } else {
        MR_region_debug_fill_commit(NULL, region);
        incr = 0;
    }
    MR_region_debug_end("fill_commit");

    return incr;
}

void
MR_use_region_ite_then_semidet_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    int                         i;
    MR_RegionIteProtect         *ite_prot;

    MR_region_debug_start("use_region_ite_then_semidet");
    ite_prot = (MR_RegionIteProtect *) ( ( (MR_Word *) top_ite_frame ) +
        MR_REGION_ITE_FRAME_FIXED_SIZE);
    for (i = 0; i < top_ite_frame->MR_riff_num_prot_regions;
            i++, ite_prot++) {
        MR_remove_undisjprotected_region_ite_then_semidet(
            ite_prot->MR_ite_prot_region);
    }
    MR_pop_region_ite_frame(top_ite_frame);
    MR_region_debug_end("use_region_ite_then_semidet");
}

void
MR_use_region_ite_then_nondet_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    int                         i;
    MR_RegionIteProtect         *ite_prot;

    MR_region_debug_start("use_region_ite_then_nondet");
    ite_prot = (MR_RegionIteProtect *) ( ( (MR_Word *) top_ite_frame ) +
        MR_REGION_ITE_FRAME_FIXED_SIZE);
    for (i = 0; i < top_ite_frame->MR_riff_num_prot_regions;
            i++, ite_prot++) {
        if (ite_prot->MR_ite_prot_region != NULL) {
            MR_remove_undisjprotected_region_ite_then_nondet(
                ite_prot->MR_ite_prot_region);
        }
    }
    MR_region_debug_end("use_region_ite_then_nondet");
}

void
MR_use_region_ite_else_semidet_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    MR_region_debug_start("use_region_ite_else_semidet");
    MR_region_process_at_ite_else_proc(top_ite_frame);
    MR_region_debug_end("use_region_ite_else_semidet");
}

void
MR_use_region_ite_else_nondet_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    MR_region_debug_start("use_region_ite_else_nondet");
    MR_region_process_at_ite_else(top_ite_frame);
    MR_region_debug_end("use_region_ite_else_nondet");
}

void
MR_use_region_ite_nondet_cond_fail_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    MR_region_debug_start("use_region_ite_nondet_cond_fail");
    MR_region_ite_restore_from_snapshots(top_ite_frame);
    MR_pop_region_ite_frame(top_ite_frame);
    MR_region_debug_end("use_region_ite_nondet_cond_fail");
}

void
MR_use_region_disj_later_proc(MR_RegionDisjFixedFrame *top_disj_frame)
{
    MR_region_debug_start("use_region_disj_later");
    MR_region_disj_restore_from_snapshots(top_disj_frame);
    MR_region_disj_destroy_new_regions(top_disj_frame);
    MR_region_debug_end("use_region_disj_later");
}

void
MR_use_region_disj_nonlast_semi_commit_proc(
    MR_RegionDisjFixedFrame *top_disj_frame)
{
    int                         i;
    MR_RegionSemiDisjProtect    *semi_disj_prot;

    MR_region_debug_start("use_region_disj_nonlast_semi_commit");
    // Destroy any regions protected by the disj frame.
    semi_disj_prot = (MR_RegionSemiDisjProtect *) (
        ( (MR_Word *) top_disj_frame ) + MR_REGION_DISJ_FRAME_FIXED_SIZE);
    for (i = 0; i < top_disj_frame->MR_rdff_num_prot_regions;
            i++, semi_disj_prot++) {
            MR_region_destroy_region(
                semi_disj_prot->MR_semi_disj_prot_region);
    }
    MR_pop_region_disj_frame(top_disj_frame);
    MR_region_debug_end("use_region_disj_nonlast_semi_commit");
}

void
MR_use_region_disj_last_proc(MR_RegionDisjFixedFrame *top_disj_frame)
{
    MR_region_debug_start("use_region_disj_last");
    MR_region_disj_restore_from_snapshots(top_disj_frame);
    MR_region_disj_destroy_new_regions(top_disj_frame);
    MR_pop_region_disj_frame(top_disj_frame);
    MR_region_debug_end("use_region_disj_last");
}

void
MR_use_region_commit_success_proc(
    MR_RegionCommitFixedFrame *top_commit_frame)
{
    MR_RegionCommitSave             *first_commit_save;

    MR_region_debug_start("use_region_commit_success");
    first_commit_save = (MR_RegionCommitSave *) (
        ( (MR_Word *) top_commit_frame ) + MR_REGION_COMMIT_FRAME_FIXED_SIZE);
    MR_commit_success_destroy_marked_new_regions(
        top_commit_frame->MR_rcff_saved_sequence_number);
    MR_commit_success_destroy_marked_saved_regions(
        top_commit_frame->MR_rcff_num_saved_regions, first_commit_save);
    MR_region_profile_pop_disj_frame(MR_region_disj_sp,
        top_commit_frame->MR_rcff_saved_disj_sp);
    MR_region_disj_sp = top_commit_frame->MR_rcff_saved_disj_sp;
    MR_pop_region_commit_frame(top_commit_frame);
    MR_region_debug_end("use_region_commit_success");
}

void
MR_use_region_commit_failure_proc(MR_RegionCommitFixedFrame *top_commit_frame)
{
    int                             i;
    MR_RegionCommitSave             *commit_save;
    MR_RegionHeader                 *region;

    MR_region_debug_start("use_region_commit_failure");
    commit_save = (MR_RegionCommitSave *) (
        ( (MR_Word *) top_commit_frame ) + MR_REGION_COMMIT_FRAME_FIXED_SIZE);
    for (i = 0; i < top_commit_frame->MR_rcff_num_saved_regions;
            i++, commit_save++) {
        region = commit_save->MR_commit_save_region;
        if (region != NULL) {
            region->MR_region_commit_frame = NULL;
        }
    }
    MR_pop_region_commit_frame(top_commit_frame);
    MR_region_debug_end("use_region_commit_failure");
}

////////////////////////////////////////////////////////////////////////////
// Helper procedures for ite support.

void
MR_region_process_at_ite_else_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    MR_region_ite_unprotect(top_ite_frame);
    MR_region_ite_restore_from_snapshots(top_ite_frame);
    MR_region_ite_destroy_new_regions(top_ite_frame);
    MR_pop_region_ite_frame(top_ite_frame);
}

void
MR_region_ite_unprotect_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    MR_RegionIteProtect     *ite_prot;
    MR_RegionHeader         *protected_region;
    int                     i;

    MR_region_debug_start("ite_unprotect");
    ite_prot = (MR_RegionIteProtect *) (
        ( (MR_Word *) top_ite_frame ) + MR_REGION_ITE_FRAME_FIXED_SIZE);
    for (i = 0; i < top_ite_frame->MR_riff_num_prot_regions;
            i++, ite_prot++) {
        protected_region = ite_prot->MR_ite_prot_region;
        MR_region_debug_ite_unprotect(protected_region);
        // Try to protect the region by an outer condition.
        protected_region->MR_region_ite_protected = NULL;
    }
    MR_region_debug_end("ite_unprotect");
}

void
MR_region_ite_restore_from_snapshots_proc(
    MR_RegionIteFixedFrame *top_ite_frame)
{
    MR_RegionSnapshot       *first_snapshot;
    MR_Word                 protection_size;

    MR_region_debug_start("ite_restore_from_snapshot");
    protection_size = top_ite_frame->MR_riff_num_prot_regions *
        MR_REGION_ITE_PROT_SIZE;
    first_snapshot = (MR_RegionSnapshot *) (
        ( (MR_Word *) top_ite_frame ) +
        MR_REGION_ITE_FRAME_FIXED_SIZE + protection_size);
    MR_restore_snapshots(top_ite_frame->MR_riff_num_snapshots,
        first_snapshot);
    MR_region_debug_end("ite_restore_from_snapshot");
}

void
MR_region_ite_destroy_new_regions_proc(MR_RegionIteFixedFrame *top_ite_frame)
{
    MR_region_debug_start("ite_destroy_new_regions");
    MR_region_frame_destroy_new_regions(
        top_ite_frame->MR_riff_saved_sequence_number,
        MR_REGION_ITE_FRAME_TYPE);
    MR_region_debug_end("ite_destroy_new_regions");
}

////////////////////////////////////////////////////////////////////////////
// Helpers for nondet disjunction support.

void
MR_region_disj_restore_from_snapshots_proc(
    MR_RegionDisjFixedFrame *top_disj_frame)
{
    MR_RegionSnapshot       *first_snapshot;

    MR_region_debug_start("disj_restore_from_snapshots");
    first_snapshot = (MR_RegionSnapshot *) (
        (MR_Word *) top_disj_frame + MR_REGION_DISJ_FRAME_FIXED_SIZE +
        MR_REGION_SEMI_DISJ_PROT_SIZE *
            top_disj_frame->MR_rdff_num_prot_regions);
    MR_restore_snapshots(top_disj_frame->MR_rdff_num_snapshots,
        first_snapshot);
    MR_region_debug_end("disj_restore_from_snapshots");
}

void
MR_region_disj_destroy_new_regions_proc(
    MR_RegionDisjFixedFrame *top_disj_frame)
{
    MR_region_debug_start("disj_destroy_new_regions");
    MR_region_frame_destroy_new_regions(
        top_disj_frame->MR_rdff_saved_sequence_number,
        MR_REGION_DISJ_FRAME_TYPE);
    MR_region_debug_end("disj_destroy_new_regions");
}

////////////////////////////////////////////////////////////////////////////

void
MR_save_snapshot_proc(MR_RegionHeader *region, MR_RegionSnapshot *snapshot)
{
    snapshot->MR_snapshot_region = region;
    snapshot->MR_snapshot_saved_last_page = region->MR_region_last_page;
    snapshot->MR_snapshot_saved_next_available_word =
        region->MR_region_next_available_word;
}

void
MR_restore_snapshots_proc(int num_snapshots,
    MR_RegionSnapshot *first_snapshot)
{
    MR_RegionSnapshot   *snapshot;
    MR_RegionHeader     *restoring_region;
    MR_RegionPage       *saved_last_page;
    MR_RegionPage       *first_new_page;
    int                 i;

    snapshot = first_snapshot;
    for (i = 0; i < (num_snapshots); i++, snapshot++) {
        restoring_region = snapshot->MR_snapshot_region;
        saved_last_page = snapshot->MR_snapshot_saved_last_page;
        first_new_page = saved_last_page->MR_regionpage_next;
        // Collect profiling information.
        MR_region_profile_restore_from_snapshot(snapshot);
        MR_region_debug_restore_from_snapshot(snapshot);

        if (first_new_page != NULL) {
            MR_region_return_page_list(first_new_page,
                restoring_region->MR_region_last_page);
            restoring_region->MR_region_last_page = saved_last_page;
        } // else no new page added.
        restoring_region->MR_region_next_available_word =
            snapshot->MR_snapshot_saved_next_available_word;
    }
}

void
MR_region_frame_destroy_new_regions_proc(int saved_sequence_number,
    int frame_type)
{
    MR_RegionHeader *region;

    region = MR_live_region_list;
    while (region != NULL &&
           region->MR_region_sequence_number >= saved_sequence_number)
    {
        MR_region_destroy_region(region);
        MR_region_debug_destroy_region(region);
        MR_region_profile_frame_destroy_region(region, frame_type);
        region = region->MR_region_next_region;
    }
    MR_live_region_list = region;
}

#endif  // not defined MR_RBMM_USE_MACROS

////////////////////////////////////////////////////////////////////////////
// Debugging messages for RBMM.

#ifdef MR_RBMM_DEBUG

static int
MR_region_get_frame_number(MR_Word *frame)
{
    int     frame_number;

    frame_number = 0;
    while (frame != NULL) {
        frame_number++;
        frame = (MR_Word *) (*frame);
    }

    return frame_number;
}

void
MR_region_create_region_msg(MR_RegionHeader *region)
{
    printf("Create region #%d (%d).\n", region->MR_region_sequence_number,
        region);
}

void
MR_region_try_remove_region_msg(MR_RegionHeader *region)
{
    printf("Try removing region ");
    MR_region_region_struct_removal_info_msg(region);
}

void
MR_region_destroy_region_msg(MR_RegionHeader *region)
{
    printf("Destroy region #%d.\n", region->MR_region_sequence_number);
}

void
MR_region_logically_remove_region_msg(MR_RegionHeader *region)
{
    printf("Logically remove region #%d.\n",
        region->MR_region_sequence_number);
}

void
MR_region_region_struct_removal_info_msg(MR_RegionHeader *region)
{
    printf("#%d (%d)\n", region->MR_region_sequence_number, region);
    printf("\tLogically removed: %d\n", region->MR_region_logical_removed);
    printf("\tIte-protected by #%d: %d\n",
        MR_region_get_frame_number((MR_Word *)region->MR_region_ite_protected),
        region->MR_region_ite_protected);
    if ( MR_region_is_disj_protected(region) ) {
        printf("\tDisj-protected.\n");
    } else {
        printf("\tNot disj-protected.\n");
    }
    printf("\tSaved in commit frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *)region->MR_region_commit_frame),
        region->MR_region_commit_frame);
    printf("\tBe destroyed at commit: %d\n",
        region->MR_region_destroy_at_commit);
}

void
MR_region_push_ite_frame_msg(MR_RegionIteFixedFrame *ite_frame)
{
    int frame_number;

    frame_number = MR_region_get_frame_number( (MR_Word *) ite_frame);
    printf("Push ite frame #%d: %d\n", frame_number, ite_frame);
    printf("\tPrevious #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (ite_frame->MR_riff_previous_ite_frame)),
        ite_frame->MR_riff_previous_ite_frame);
    printf("\tSaved sequence number: %d\n",
        ite_frame->MR_riff_saved_sequence_number);
}

void
MR_region_ite_frame_msg(MR_RegionIteFixedFrame *ite_frame)
{
    printf("Ite frame #%d: %d\n",
        MR_region_get_frame_number( (MR_Word *) ite_frame), ite_frame);
    printf("\tPrevious #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (ite_frame->MR_riff_previous_ite_frame)),
            ite_frame->MR_riff_previous_ite_frame);
    printf("\tSaved sequence number: %d\n",
        ite_frame->MR_riff_saved_sequence_number);
    MR_region_ite_frame_protected_regions_msg(ite_frame);
    MR_region_ite_frame_snapshots_msg(ite_frame);
}

void
MR_region_ite_frame_protected_regions_msg(MR_RegionIteFixedFrame *ite_frame)
{
    MR_RegionIteProtect     *ite_prot;
    int                     i;

    // This check is for development, when it becomes more stable,
    // the check can be removed. Normally we expect not many regions.

    if (ite_frame->MR_riff_num_prot_regions > 10) {
        printf("Number of protected region: %d\n",
            ite_frame->MR_riff_num_prot_regions);
        MR_fatal_error("Too many protected regions.");
    }

    ite_prot = (MR_RegionIteProtect *) (
        (MR_Word *) ite_frame + MR_REGION_ITE_FRAME_FIXED_SIZE);
    for (i = 0; i < ite_frame->MR_riff_num_prot_regions; i++, ite_prot++) {
        printf("\tAt slot: %d, ite-protect region: %d\n",
            ite_prot, ite_prot->MR_ite_prot_region->MR_region_sequence_number);
    }
}

void
MR_region_ite_frame_snapshots_msg(MR_RegionIteFixedFrame *ite_frame)
{
    MR_RegionSnapshot       *snapshot;
    int                     protection_size;
    int                     i;

    if (ite_frame->MR_riff_num_snapshots > 10) {
        printf("Number of snapshots: %d\n", ite_frame->MR_riff_num_snapshots);
        MR_fatal_error("Too many snapshots");
    }

    protection_size = ite_frame->MR_riff_num_prot_regions *
        MR_REGION_ITE_PROT_SIZE;
    snapshot = (MR_RegionSnapshot *) ((MR_Word *) ite_frame +
        MR_REGION_ITE_FRAME_FIXED_SIZE + protection_size);
    for (i = 0; i < ite_frame->MR_riff_num_snapshots; i++, snapshot++) {
        printf("\tAt slot: %d, snapshot of region: #%d\n", snapshot,
            snapshot->MR_snapshot_region->MR_region_sequence_number);
    }
}

void
MR_region_push_disj_frame_msg(MR_RegionDisjFixedFrame *disj_frame)
{
    printf("Push disj frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *) disj_frame), disj_frame);
    printf("\tPrevious #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (disj_frame->MR_rdff_previous_disj_frame)),
        disj_frame->MR_rdff_previous_disj_frame);
    printf("\tSaved sequence number: %d\n",
        disj_frame->MR_rdff_saved_sequence_number);
}

void
MR_region_disj_frame_msg(MR_RegionDisjFixedFrame *disj_frame)
{
    printf("Disj frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *) disj_frame), disj_frame);
    printf("\tPrevious #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) disj_frame->MR_rdff_previous_disj_frame),
        disj_frame->MR_rdff_previous_disj_frame);
    printf("\tSaved sequence number: %d\n",
        disj_frame->MR_rdff_saved_sequence_number);
}

void
MR_region_disj_frame_snapshots_msg(MR_RegionDisjFixedFrame *disj_frame)
{
    MR_RegionSnapshot   *snapshot;
    int                 i;

    if (disj_frame->MR_rdff_num_snapshots > 10) {
        printf("Number of snapshots: %d\n", disj_frame->MR_rdff_num_snapshots);
        MR_fatal_error("Too many snapshots");
    }

    snapshot = (MR_RegionSnapshot *) (
        (MR_Word *) disj_frame + MR_REGION_DISJ_FRAME_FIXED_SIZE);
    for (i = 0; i < disj_frame->MR_rdff_num_snapshots; i++, snapshot++) {
        printf("\tAt slot: %d, snapshot of region: %d\n", snapshot,
            snapshot->MR_snapshot_region);
    }
}

void
MR_region_push_commit_frame_msg(MR_RegionCommitFixedFrame *commit_frame)
{
    int i;

    printf("Push commit frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *) commit_frame), commit_frame);
    printf("\tPrevious #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) commit_frame->MR_rcff_previous_commit_frame),
        commit_frame->MR_rcff_previous_commit_frame);
    printf("\tSequence number: %d\n",
        commit_frame->MR_rcff_saved_sequence_number);
    printf("\tDisj frame #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (commit_frame->MR_rcff_saved_disj_sp)),
        commit_frame->MR_rcff_saved_disj_sp);
}

void
MR_region_commit_frame_msg(MR_RegionCommitFixedFrame *commit_frame)
{
    MR_RegionCommitSave     *commit_save;
    int                     i;

    printf("Commit frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *)commit_frame), commit_frame);
    printf("\tPrevious frame #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) commit_frame->MR_rcff_previous_commit_frame),
        commit_frame->MR_rcff_previous_commit_frame);
    printf("\tSequence number: %d\n",
        commit_frame->MR_rcff_saved_sequence_number);
    printf("\tDisj frame #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (commit_frame->MR_rcff_saved_disj_sp)),
        commit_frame->MR_rcff_saved_disj_sp);
    printf("\tCurrent disj frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *) MR_region_disj_sp),
        MR_region_disj_sp);

    commit_save = (MR_RegionCommitSave *) (
        (MR_Word *) commit_frame + MR_REGION_COMMIT_FRAME_FIXED_SIZE);

    printf("\tNumber of saved regions: %d\n",
        commit_frame->MR_rcff_num_saved_regions);
    for (i = 0; i < commit_frame->MR_rcff_num_saved_regions;
        i++, commit_save++)
    {
        printf("Slot: %d, region: %d\n", commit_save,
            commit_save->MR_commit_save_region);
    }
}

void
MR_region_commit_frame_saved_regions_msg(
        MR_RegionCommitFixedFrame *commit_frame)
{
    MR_RegionCommitSave *commit_save;
    int                 i;

    // This check is for development, when it becomes more stable,
    // the check can be removed.

    if (commit_frame->MR_rcff_num_saved_regions > 10) {
        printf("Number of saved region: %d\n",
            commit_frame->MR_rcff_num_saved_regions);
        MR_fatal_error("Too many regions were saved.");
    }

    commit_save = (MR_RegionCommitSave *)
        ((MR_Word *) commit_frame + MR_REGION_COMMIT_FRAME_FIXED_SIZE);
    for (i = 0; i < commit_frame->MR_rcff_num_saved_regions;
        i++, commit_save++)
    {
        printf("\tAt slot: %d, saved region: %d\n", commit_save,
            commit_save->MR_commit_save_region);
    }
}

void
MR_region_start_msg(const char *name)
{
    printf("Start %s:\n", name);
}

void
MR_region_end_msg(const char *name)
{
    printf("End %s.\n", name);
}

void
MR_region_fill_ite_protect_msg(MR_RegionIteProtect *ite_prot,
    MR_RegionHeader *region)
{
    if (ite_prot == NULL) {
        printf("\tNot protect region #%d.\n",
            region->MR_region_sequence_number);
    } else {
        printf("\tAt slot: %d, protect region #%d.\n", ite_prot,
            region->MR_region_sequence_number);
    }
}

void
MR_region_fill_ite_snapshot_not_removed_msg(MR_RegionSnapshot *snapshot,
    MR_RegionHeader *region)
{
    printf("\tAt slot: %d, save snapshot of region #%d.\n", snapshot,
        region->MR_region_sequence_number);
}

void
MR_region_fill_ite_snapshot_removed_msg(MR_RegionSnapshot *snapshot,
    MR_RegionHeader *region)
{
    if (snapshot == NULL) {
        printf("\tNot take snapshot of region #%d.\n",
            region->MR_region_sequence_number);
    } else {
        printf("\tAt slot: %d, save snapshot of region #%d.\n", snapshot,
            region->MR_region_sequence_number);
    }
}

void
MR_region_fill_semi_disj_protect_msg(MR_RegionSemiDisjProtect *semi_disj_prot,
    MR_RegionHeader *region)
{
    if (semi_disj_prot == NULL) {
        printf("\tNot protect region #%d.\n",
            region->MR_region_sequence_number);
    } else {
        printf("\tAt slot: %d, protect region #%d.\n", semi_disj_prot,
            region->MR_region_sequence_number);
    }
}

void
MR_region_fill_disj_snapshot_msg(MR_RegionSnapshot *snapshot,
    MR_RegionHeader *region)
{
    printf("\tAt slot: %d, save snapshot of region #%d.\n", snapshot,
        region->MR_region_sequence_number);
}

void
MR_region_fill_commit_msg(MR_RegionCommitSave *commit_save,
    MR_RegionHeader *region)
{
    if (commit_save == NULL) {
        printf("\tNot save region #%d.\n", region->MR_region_sequence_number);
    } else {
        printf("\tAt slot: %d, save region #%d.\n", commit_save,
            region->MR_region_sequence_number);
    }
}

void
MR_region_ite_unprotect_msg(MR_RegionHeader *region)
{
    MR_RegionIteFixedFrame      *old_ite_frame;
    MR_RegionIteFixedFrame      *new_ite_frame;

    old_ite_frame = region->MR_region_ite_protected;
    new_ite_frame = MR_region_ite_sp->MR_riff_previous_ite_frame;
    printf("Ite protected by #%d (%d), now by #%d (%d).\n",
        MR_region_get_frame_number((MR_Word *) old_ite_frame), old_ite_frame,
        MR_region_get_frame_number((MR_Word *) new_ite_frame), new_ite_frame);
}

void
MR_region_restore_from_snapshot_msg(MR_RegionSnapshot *snapshot)
{
    int        new_words;
    int        new_pages;

    MR_region_get_new_pages_and_new_words(snapshot, &new_pages, &new_words);
    printf("Restore region #%d: reclaim %d pages, %d words.\n",
        snapshot->MR_snapshot_region->MR_region_sequence_number,
        new_pages, new_words);
}

#endif // End of MR_RBMM_DEBUG.

////////////////////////////////////////////////////////////////////////////
// Profiling methods for RBMM.

#if defined(MR_RBMM_PROFILING)

void
MR_region_update_profiling_unit(MR_RegionProfUnit *profiling_unit,
    int quantity)
{
    profiling_unit->MR_rbmmpu_current += quantity;
    if (quantity > 0) {
        profiling_unit->MR_rbmmpu_total += quantity;
    }

    if (profiling_unit->MR_rbmmpu_current > profiling_unit->MR_rbmmpu_max) {
        profiling_unit->MR_rbmmpu_max = profiling_unit->MR_rbmmpu_current;
    }
}

void
MR_region_profile_destroyed_region(MR_RegionHeader *region)
{
    int allocated_size_of_region;

    MR_region_update_profiling_unit(&MR_rbmmp_regions_used, -1);
    MR_region_update_profiling_unit(&MR_rbmmp_pages_used,
        -MR_region_get_number_of_pages(MR_region_to_first_regionpage(region),
        region->MR_region_last_page));
    allocated_size_of_region = region->MR_region_allocated_size;
    MR_region_update_profiling_unit(&MR_rbmmp_words_used,
        -allocated_size_of_region);
}

void
MR_region_profile_restore_from_snapshot(MR_RegionSnapshot *snapshot)
{
    int             new_words;
    int             new_pages;
    MR_RegionHeader *restoring_region;

    MR_region_get_new_pages_and_new_words(snapshot, &new_pages, &new_words);
    MR_region_update_profiling_unit(&MR_rbmmp_pages_used, -new_pages);
    restoring_region = snapshot->MR_snapshot_region;
    restoring_region->MR_region_allocated_size
        -= new_words;
    MR_region_update_profiling_unit(&MR_rbmmp_words_used, -new_words);
    MR_rbmmp_pages_instant_reclaimed_new_alloc += new_pages;
    MR_rbmmp_words_instant_reclaimed_new_alloc += new_words;
}

void
MR_region_profile_reclaim_region(MR_RegionHeader *region,
    unsigned int *regions_reclaimed, unsigned int *pages_reclaimed,
    unsigned int *words_reclaimed)
{
    MR_region_profile_increase_counter(regions_reclaimed);
    MR_region_profile_reclaim_pages_and_words(region, pages_reclaimed,
        words_reclaimed);
}

void
MR_region_profile_reclaim_pages_and_words(MR_RegionHeader *region,
    unsigned int *pages_reclaimed, unsigned int *words_reclaimed)
{
    *pages_reclaimed += MR_region_get_number_of_pages(
        MR_region_to_first_regionpage(region),
        region->MR_region_last_page);
    *words_reclaimed += region->MR_region_allocated_size;
}

void
MR_region_profile_increase_counter(unsigned int *counter)
{
    *counter += 1;
}

int
MR_region_get_number_of_pages(MR_RegionPage *from_page, MR_RegionPage *to_page)
{
    MR_RegionPage   *page;
    int             number_of_pages;

    page = from_page;
    number_of_pages = 0;
    while (page != to_page) {
        number_of_pages += 1;
        page = page->MR_regionpage_next;
    }
    // Plus the last page.
    number_of_pages += 1;
    return number_of_pages;
}

int
MR_region_get_ite_frame_size(MR_RegionIteFixedFrame *ite_frame)
{
    int     size;

    size = 0;
    size += MR_REGION_ITE_FRAME_FIXED_SIZE;
    size += (ite_frame->MR_riff_num_prot_regions * MR_REGION_ITE_PROT_SIZE);
    size += (ite_frame->MR_riff_num_snapshots * MR_REGION_ITE_SNAPSHOT_SIZE);
    return size;
}

int
MR_region_get_disj_frame_size(MR_RegionDisjFixedFrame *disj_frame)
{
    int     size;

    size = 0;
    size += MR_REGION_DISJ_FRAME_FIXED_SIZE;
    size += (disj_frame->MR_rdff_num_snapshots * MR_REGION_DISJ_SNAPSHOT_SIZE);
    return size;
};

int
MR_region_get_commit_frame_size(MR_RegionCommitFixedFrame *commit_frame)
{
    int     size;

    size = 0;
    size += MR_REGION_COMMIT_FRAME_FIXED_SIZE;
    size += (commit_frame->MR_rcff_num_saved_regions *
        MR_REGION_COMMIT_FRAME_FIXED_SIZE);
    return size;
}

void
MR_region_print_profiling_unit(const char *str,
    MR_RegionProfUnit *profiling_unit)
{
    printf("%s", str);
    printf("\n");
    printf("\tTotal: %d.\n", profiling_unit->MR_rbmmpu_total);
    printf("\tMaximum: %d.\n", profiling_unit->MR_rbmmpu_max);
    printf("\tCurrent: %d.\n", profiling_unit->MR_rbmmpu_current);
}

void
MR_region_print_profiling_info(void)
{
    printf("\n---------- Profiling information ----------\n");
    // Info about regions.
    MR_region_print_profiling_unit("Regions:", &MR_rbmmp_regions_used);
    printf("Biggest region size: %d.\n", MR_rbmmp_biggest_region_size);
    printf("Biggest region's sequence number: %d.\n", MR_rbmmp_biggest_region);
    printf("Reclaimed as follows:\n");
    printf("\tThanks to reclaiming new regions at ite: %d.\n",
        MR_rbmmp_regions_instant_reclaimed_at_ite);
    printf("\tThanks to reclaiming new regions at disj: %d.\n",
        MR_rbmmp_regions_instant_reclaimed_at_disj);
    printf("\tAt the start of then part: %d.\n",
        MR_rbmmp_regions_reclaimed_at_then_part);
    printf("\tAt commit point: %d.\n",
        MR_rbmmp_regions_reclaimed_at_commit);
    printf("\tBy remove instruction: %d.\n",
        MR_rbmmp_regions_reclaimed_by_remove_instr);

    // Info about words used.
    MR_region_print_profiling_unit("Words:", &MR_rbmmp_words_used);
    printf("Reclaimed as follows:\n");
    printf("\tInstant reclaiming of new allocations: %d.\n",
        MR_rbmmp_words_instant_reclaimed_new_alloc);
    printf("\tInstant reclaiming of new regions: %d.\n",
        MR_rbmmp_words_instant_reclaimed_new_regions);
    printf("\tAt the start of then part: %d.\n",
        MR_rbmmp_words_reclaimed_at_then_part);
    printf("\tAt commit point: %d.\n",
        MR_rbmmp_words_reclaimed_at_commit);
    printf("\tBy remove instruction: %d.\n",
        MR_rbmmp_words_reclaimed_by_remove_instr);

    // Info about pages used.
    MR_region_print_profiling_unit("Pages:", &MR_rbmmp_pages_used);
    printf("Pages requested: %d.\n", MR_rbmmp_pages_requested);
    printf("Pages utilized: %lf.\n",
        MR_rbmmp_pages_used.MR_rbmmpu_total / (double)MR_rbmmp_pages_requested);
    printf("\tInstant reclaiming of new allocations: %d.\n",
        MR_rbmmp_pages_instant_reclaimed_new_alloc);
    printf("\tInstant reclaiming of new regions: %d.\n",
        MR_rbmmp_pages_instant_reclaimed_new_regions);
    printf("\tAt the start of then part: %d.\n",
        MR_rbmmp_pages_reclaimed_at_then_part);
    printf("\tAt commit point: %d.\n",
        MR_rbmmp_pages_reclaimed_at_commit);
    printf("\tBy remove instruction: %d.\n",
        MR_rbmmp_pages_reclaimed_by_remove_instr);

    // Info about embedded frames.
    MR_region_print_profiling_unit("Disj frames used:",
        &MR_rbmmp_num_disj_frames);
    MR_region_print_profiling_unit("Words used by disj frames:",
        &MR_rbmmp_words_used_by_disj_frames);
    printf("Regions protected at (semi) disj frames: %d.\n",
        MR_rbmmp_regions_protected_at_semi_disj);
    printf("Region size records saved at disj frames: %d.\n",
        MR_rbmmp_snapshots_saved_at_disj);
    MR_region_print_profiling_unit("Ite frames used:",
        &MR_rbmmp_num_ite_frames);
    MR_region_print_profiling_unit("Words used by ite frames:",
        &MR_rbmmp_words_used_by_ite_frames);
    printf("Regions protected at ite frames: %d.\n",
        MR_rbmmp_regions_protected_at_ite);
    printf("Region size records saved at ite frames: %d.\n",
        MR_rbmmp_snapshots_saved_at_ite);
    MR_region_print_profiling_unit("Commit frames used:",
        &MR_rbmmp_num_commit_frames);
    MR_region_print_profiling_unit("Words used by commit frames:",
        &MR_rbmmp_words_used_by_commit_frames);
    printf("Regions saved at commit frames: %d.\n",
        MR_rbmmp_regions_saved_at_commit);
}

#else // Not define MR_RBMM_PROFILING.

void
MR_region_update_profiling_unit(MR_RegionProfUnit *pu, int q)
{
    // Do nothing.
}

void
MR_region_profile_destroyed_region(MR_RegionHeader *r)
{
    // Do nothing.
}

void
MR_region_profile_restore_from_snapshot(MR_RegionSnapshot *s)
{
    // Do nothing.
}

void
MR_region_profile_reclaim_region(MR_RegionHeader *region,
    unsigned int *regions_reclaimed, unsigned int *pages_reclaimed,
    unsigned int *words_reclaimed)
{
    // Do nothing.
}

void
MR_region_profile_reclaim_pages_and_words(MR_RegionHeader *region,
    unsigned int *pages_reclaimed, unsigned int *words_reclaimed)
{
    // Do nothing.
}

void
MR_region_profile_increase_counter(unsigned int *counter)
{
    // Do nothing.
}

int
MR_region_get_number_of_pages(MR_RegionPage *fp, MR_RegionPage *tp)
{
    return 0;
}

void
MR_region_print_profiling_info(void)
{
}

#endif // End of Not define MR_RBMM_PROFILING.

static  void
MR_region_get_new_pages_and_new_words(MR_RegionSnapshot *snapshot,
    int *new_pages, int *new_words)
{
    MR_RegionHeader *restoring_region;
    MR_RegionPage   *first_new_page;

    restoring_region = snapshot->MR_snapshot_region;
    first_new_page = snapshot->MR_snapshot_saved_last_page->MR_regionpage_next;

    if (first_new_page != NULL) {
        *new_pages = MR_region_get_number_of_pages(first_new_page,
             restoring_region->MR_region_last_page);
        *new_words = (*new_pages * MR_REGION_PAGE_SPACE_SIZE -
             MR_region_available_space(restoring_region->MR_region_last_page,
                restoring_region->MR_region_next_available_word) +
             MR_region_available_space(snapshot->MR_snapshot_saved_last_page,
                 snapshot->MR_snapshot_saved_next_available_word));
    } else {
        *new_pages = 0;
        *new_words = restoring_region->MR_region_next_available_word -
            snapshot->MR_snapshot_saved_next_available_word;
    }
}

#endif  // MR_USE_REGIONS
