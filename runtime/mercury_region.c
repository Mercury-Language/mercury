/*
** vim:sw=4 ts=4 expandtab
*/
/*
** Copyright (C) 2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** file: mercury_region.c
** main author: qph
*/

#include "mercury_imp.h"
#include "mercury_region.h"

#ifdef MR_USE_REGIONS

#define word_sizeof(s) (sizeof(s) / sizeof(MR_Word))

MR_RegionPage       *MR_region_free_page_list;
MR_Region           *MR_live_region_list;

MR_RegionIteFixedFrame      *MR_region_ite_sp = NULL;
MR_RegionDisjFixedFrame     *MR_region_disj_sp = NULL;
MR_RegionCommitFixedFrame   *MR_region_commit_sp = NULL;

MR_Word             MR_region_sequence_number = 1;

#if defined(MR_RBMM_PROFILING)

MR_RegionProfUnit   MR_rbmmp_words_used = {0, 0, 0};
MR_RegionProfUnit   MR_rbmmp_regions_used = {0, 0, 0};
MR_RegionProfUnit   MR_rbmmp_pages_used = {0, 0, 0};
unsigned int        MR_rbmmp_pages_requested = 0;
unsigned int        MR_rbmmp_biggest_region_size = 0;
MR_RegionProfUnit   MR_rbmmp_regions_saved_at_commit = {0, 0, 0};
unsigned int        MR_rbmmp_regions_protected_at_ite;
unsigned int        MR_rbmmp_snapshots_saved_at_ite;
unsigned int        MR_rbmmp_regions_protected_at_disj;
unsigned int        MR_rbmmp_snapshots_saved_at_disj;
double              MR_rbmmp_page_utilized;

#endif

/* Request for more pages from the operating system. */
static MR_RegionPage    *MR_region_request_pages(void);

/* Take a page from the free page list. */
static MR_RegionPage    *MR_region_get_free_page(void);

static void             MR_region_nullify_entries_in_commit_stack(
                            MR_Region *region);
static void             MR_region_nullify_in_commit_frame(
                            MR_RegionCommitFixedFrame *frame,
                            MR_Region *region);
static void             MR_region_nullify_in_ite_frame(MR_Region *region);

static void             MR_region_extend_region(MR_Region *);

#ifdef  MR_RBMM_DEBUG
static int              MR_region_get_frame_number(MR_Word *);
#endif

#ifdef  MR_RBMM_PROFILING
static void             MR_region_print_profiling_unit(const char *str,
                            MR_RegionProfUnit *profiling_unit);
#endif

/*---------------------------------------------------------------------------*/
/* Page operations. */

static MR_RegionPage *
MR_region_request_pages()
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
    /* Disconnect the first free page from the free list. */
    page->MR_regionpage_next = NULL;

#if defined(MR_RBMM_PROFILING)
    MR_region_update_profiling_unit(&MR_rbmmp_pages_used, 1);
#endif

    return page;
}

/*---------------------------------------------------------------------------*/
/*
** Region operations.
*/

/*
** Create a region.
** The MR_REGION_PAGE_SPACE_SIZE must be larger than the size of
** Region_Struct.
*/

MR_Region *
MR_region_create_region(void)
{
    MR_RegionPage   *page;
    MR_Region       *region;

    /* This is the first page of the region. */
    page = MR_region_get_free_page();

    /*
    ** In the first page we will store region information, which occupies
    ** word_sizeof(MR_Region) words from the start of the first page.
    */
    region = (MR_Region *) (page->MR_regionpage_space);
    region->MR_region_next_available_word = (MR_Word *)
        (page->MR_regionpage_space + word_sizeof(MR_Region));
    region->MR_region_last_page = page;
    region->MR_region_available_space =
        MR_REGION_PAGE_SPACE_SIZE - word_sizeof(MR_Region);
    region->MR_region_removal_counter = 1;
    region->MR_region_sequence_number = MR_region_sequence_number++;
    region->MR_region_logical_removed = 0;
    region->MR_region_ite_protected = NULL;
    region->MR_region_commit_frame = NULL;
    region->MR_region_destroy_at_commit = 0;

    /* Add the region to the head of the live region list. */
    if (MR_live_region_list != NULL) {
        MR_live_region_list->MR_region_previous_region = region;
    }
    region->MR_region_next_region = MR_live_region_list;
    region->MR_region_previous_region = NULL;
    MR_live_region_list = region;

#if defined(MR_RBMM_DEBUG)
    MR_region_debug_create_region(region);
#endif

#if defined(MR_RBMM_PROFILING)
    MR_region_update_profiling_unit(&MR_rbmmp_regions_used, 1);
    ((MR_RegionPage *) region)->MR_regionpage_allocated_size = 0;
#endif

    return region;
}

static void
MR_region_nullify_entries_in_commit_stack(MR_Region *region)
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
    MR_Region *region)
{
    MR_RegionCommitSave     *commit_save;
    int     i;

    commit_save = ( MR_RegionCommitSave *) (
        ( MR_Word *) commit_frame + MR_REGION_COMMIT_FRAME_FIXED_SIZE);

    /*
    ** Loop through the saved regions and nullify the entry of the input
    ** region if found.
    */
    for (i = 0; i < commit_frame->MR_rcff_num_saved_regions; i++) {
        if (commit_save != NULL &&
                commit_save->MR_commit_save_region == region) {
            commit_save->MR_commit_save_region = NULL;
            break;
        } else {
            commit_save += 1;
        }
    }
}

static void
MR_region_nullify_in_ite_frame(MR_Region *region)
{
    MR_RegionIteFixedFrame      *ite_frame;
    MR_RegionIteProtect         *ite_prot;
    MR_Word                     *protected_region;
    int                         num_protected_regions;
    int                         i;

    ite_frame = region->MR_region_ite_protected;
    ite_prot = (MR_RegionIteProtect *) ( (MR_Word *) ite_frame +
        MR_REGION_ITE_FRAME_FIXED_SIZE );

    /*
    ** Loop through the protected regions and nullify the entry of the input
    ** region if found.
    */
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
MR_region_destroy_region(MR_Region *region)
{
    MR_region_debug_destroy_region(region);

    if (region->MR_region_commit_frame != NULL) {
        MR_region_nullify_entries_in_commit_stack(region);
    }

    /* Break the region from the live region list. */
    if (region == MR_live_region_list) {
        /* Detach the newest. */
        MR_live_region_list = region->MR_region_next_region;
    } else {
        region->MR_region_previous_region->MR_region_next_region =
            region->MR_region_next_region;
        if (region->MR_region_next_region != NULL) {
            /* Detach one in the middle. */
            region->MR_region_next_region->MR_region_previous_region =
                region->MR_region_previous_region;
        }
    }

    /* Return the page list of the region to the free page list. */
    MR_region_return_page_list((MR_RegionPage *) region,
        region->MR_region_last_page);

    /* Collect profiling information. */
    MR_region_profile_destroyed_region(region);
}

/*
** This method is to be called at the start of the then part of an ite with
** semidet condition (most of the times).
** At that point we will only check if the region is disj-protected or not.
*/

void
MR_remove_undisjprotected_region_ite_then_semidet(MR_Region *region)
{
    MR_region_debug_try_remove_region(region);
    if ( !MR_region_is_disj_protected(region) ) {
        MR_region_destroy_region(region);
    } else {
        region->MR_region_logical_removed = 1;

#if defined(MR_RBMM_DEBUG)
        MR_region_logically_remove_region_msg(region);
#endif
    }
}

/*
** This method is to be called at the start of the then part of an ite with
** nondet condition.
** We will destroy the region if it is not disj-protected and we will also
** nullify its entry in the ite frame.
*/

void
MR_remove_undisjprotected_region_ite_then_nondet(MR_Region *region)
{
    MR_region_debug_try_remove_region(region);

    if ( !MR_region_is_disj_protected(region) ) {
        MR_region_nullify_in_ite_frame(region);
        MR_region_destroy_region(region);
    } else {
        region->MR_region_logical_removed = 1;

#if defined(MR_RBMM_DEBUG)
        MR_region_logically_remove_region_msg(region);
#endif
    }
}

void
MR_region_remove_region(MR_Region *region)
{
    MR_region_debug_try_remove_region(region);

    if ( region->MR_region_ite_protected == NULL &&
        !(MR_region_is_disj_protected(region)) )
    {
        MR_region_destroy_region(region);
    } else {
        region->MR_region_logical_removed = 1;

        /*
        ** This logical removal happens in a commit operation. Therefore we
        ** mark the region so that it will be destroyed at the commit point.
        */
        if (MR_region_commit_sp != NULL) {
            region->MR_region_destroy_at_commit = 1;
        }

#if defined(MR_RBMM_DEBUG)
        MR_region_logically_remove_region_msg(region);
#endif
    }
}

MR_Word *
MR_region_alloc(MR_Region *region, unsigned int words)
{
    MR_Word *allocated_cell;

    if (region->MR_region_available_space < words) {
        MR_region_extend_region(region);
    }

    allocated_cell = region->MR_region_next_available_word;
    /* Allocate in the increasing direction of address. */
    region->MR_region_next_available_word += words;
    region->MR_region_available_space -= words;
#if defined(MR_RBMM_PROFILING)
    MR_region_update_profiling_unit(&MR_rbmmp_words_used, words);
    ((MR_RegionPage *) region)->MR_regionpage_allocated_size += words;
#endif

    return allocated_cell;
}

static void
MR_region_extend_region(MR_Region *region)
{
    MR_RegionPage   *page;

    page = MR_region_get_free_page();
    region->MR_region_last_page->MR_regionpage_next = page;
    region->MR_region_last_page = page;
    region->MR_region_next_available_word = page->MR_regionpage_space;
    region->MR_region_available_space = MR_REGION_PAGE_SPACE_SIZE;
}

/* Destroy any marked regions allocated before scope entry. */
void
MR_commit_success_destroy_marked_saved_regions(MR_Word num_saved_regions,
    MR_RegionCommitSave *first_commit_save)
{
    MR_RegionCommitSave     *commit_save;
    MR_Region               *region;
    int                     i;

    commit_save = first_commit_save;
    for (i = 0; i < num_saved_regions; i++, commit_save++) {
        region = commit_save->MR_commit_save_region;
        if (region != NULL) {
            /*
            ** The region is saved here and has not been destroyed.
            ** XXX If we save only regions that are live at entry and not live
            ** at exit, at commit it should be the case that a logical removal
            ** has happened to the region, i.e., destroy_at_commit = 1. So just
            ** need to destroy it at commit. The check and the else below are
            ** redundant.
            */
            if (region->MR_region_destroy_at_commit) {
                /*
                ** Logical removal happens to the region in the commit
                ** context. So destroy it at commit.
                */
                MR_region_destroy_region(region);
            } else {
                MR_fatal_error(
                    "MR_commit_success_destroy_marked_saved_regions: "
                    "need to rethink.");
            }
        }
    }
}

/* Destroy any marked regions allocated since scope entry. */
void
MR_commit_success_destroy_marked_new_regions(MR_Word saved_region_seq_number)
{
    MR_Region   *region;

    region = MR_live_region_list;
    while (region != NULL &&
        region->MR_region_sequence_number > saved_region_seq_number)
    {
        if (region->MR_region_destroy_at_commit) {
            MR_region_destroy_region(region);
        }
        /*
        ** XXX It is fine to destroy the region and then still use it to find
        ** the next one because destroying a region is just returning
        ** its pages, the "region" is still there in memory.
        */
        region = region->MR_region_next_region;
    }
}

int
MR_region_is_disj_protected(MR_Region *region) 
{
    MR_RegionDisjFixedFrame     *disj_frame;

    disj_frame = MR_region_disj_sp;
    while (disj_frame != NULL)
    {
        if (disj_frame->MR_rdff_disj_prot_seq_number !=
               MR_REGION_DISJ_FRAME_DUMMY_SEQ_NUMBER &&
            disj_frame->MR_rdff_disj_prot_seq_number <
                region->MR_region_sequence_number) {
           return MR_TRUE;
        } 
        disj_frame = disj_frame->MR_rdff_previous_disj_frame;
    }
    return MR_FALSE;
}


/*---------------------------------------------------------------------------*/
/* Debugging messages for RBMM. */

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
MR_region_create_region_msg(MR_Region *region)
{
    printf("Create region #%d:\n", region->MR_region_sequence_number);
    printf("\tHandle: %d\n", region);
}

void
MR_region_try_remove_region_msg(MR_Region *region)
{
    printf("Try removing region ");
    MR_region_region_struct_removal_info_msg(region);
}

void
MR_region_destroy_region_msg(MR_Region *region)
{
    printf("Destroy region ");
    MR_region_region_struct_removal_info_msg(region);
}

void
MR_region_logically_remove_region_msg(MR_Region *region)
{
    printf("Logically remove region ");
    MR_region_region_struct_removal_info_msg(region);
}

void
MR_region_region_struct_removal_info_msg(MR_Region *region)
{
    printf("#%d\n", region->MR_region_sequence_number);
    printf("\tHandle: %d\n", region);
    printf("\tLogically removed: %d\n", region->MR_region_logical_removed);
    printf("\tProtected by ite frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *)region->MR_region_ite_protected),
        region->MR_region_ite_protected);
    if ( MR_region_is_disj_protected(region) ) {
        printf("\tProtected by a disj frame.\n");
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
    int     frame_number;

    frame_number = MR_region_get_frame_number( (MR_Word *) ite_frame);
    printf("Push ite frame #%d: %d\n", frame_number, ite_frame);
    printf("\tPrevious frame at push #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (ite_frame->MR_riff_previous_ite_frame)),
        ite_frame->MR_riff_previous_ite_frame);
    printf("\tSaved most recent region at push: %d\n",
        ite_frame->MR_riff_saved_region_list);
}

void
MR_region_ite_frame_msg(MR_RegionIteFixedFrame *ite_frame)
{
    printf("Ite frame #%d: %d\n",
        MR_region_get_frame_number( (MR_Word *) ite_frame), ite_frame);
    printf("\tPrevious frame #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (ite_frame->MR_riff_previous_ite_frame)),
            ite_frame->MR_riff_previous_ite_frame);
    printf("\tSaved most recent: %d\n", ite_frame->MR_riff_saved_region_list);
    MR_region_ite_frame_protected_regions_msg(ite_frame);
    MR_region_ite_frame_snapshots_msg(ite_frame);
}

void
MR_region_ite_frame_protected_regions_msg(MR_RegionIteFixedFrame *ite_frame)
{
    MR_RegionIteProtect     *ite_prot;
    int                     i;

    /*
    ** This check is for development, when it becomes more stable,
    ** the check can be removed. Normally we expect not many regions.
    */
    if (ite_frame->MR_riff_num_prot_regions > 10) {
        printf("Number of protected region: %d\n",
            ite_frame->MR_riff_num_prot_regions);
        MR_fatal_error("Too many protected regions.");
    }

    ite_prot = (MR_RegionIteProtect *) (
        (MR_Word *) ite_frame + MR_REGION_ITE_FRAME_FIXED_SIZE);
    for (i = 0; i < ite_frame->MR_riff_num_prot_regions; i++, ite_prot++) {
        printf("\tAt slot: %d, ite-protect region: %d\n",
            ite_prot, ite_prot->MR_ite_prot_region);
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
        printf("\tAt slot: %d, snapshot of region: %d\n", snapshot,
            snapshot->MR_snapshot_region);
    }
}

void
MR_region_push_disj_frame_msg(MR_RegionDisjFixedFrame *disj_frame)
{
    printf("Push disj frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *) disj_frame), disj_frame);
    printf("\tPrevious frame at push #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (disj_frame->MR_rdff_previous_disj_frame)),
        disj_frame->MR_rdff_previous_disj_frame);
    printf("\tSaved most recent region at push: %d\n",
        disj_frame->MR_rdff_saved_region_list);
}

void
MR_region_disj_frame_msg(MR_RegionDisjFixedFrame *disj_frame)
{
    printf("Disj frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *) disj_frame), disj_frame);
    printf("\tPrevious frame #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) disj_frame->MR_rdff_previous_disj_frame),
        disj_frame->MR_rdff_previous_disj_frame);
    printf("\tSaved most recent region: %d\n",
        disj_frame->MR_rdff_saved_region_list);
}

void
MR_region_disj_frame_snapshots_msg(MR_RegionDisjFixedFrame *disj_frame)
{
    MR_RegionSnapshot *snapshot;
    int     i;

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
    printf("\tPrevious frame at push #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) commit_frame->MR_rcff_previous_commit_frame),
        commit_frame->MR_rcff_previous_commit_frame);
    printf("\tSequence number at push: %d\n",
        commit_frame->MR_rcff_saved_sequence_number);
    printf("\tDisj frame at push #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) (commit_frame->MR_rcff_saved_disj_sp)),
        commit_frame->MR_rcff_saved_disj_sp);
}

void
MR_region_commit_frame_msg(MR_RegionCommitFixedFrame *commit_frame)
{
    MR_RegionCommitSave     *commit_save;
    int     i;

    printf("Commit frame #%d: %d\n",
        MR_region_get_frame_number((MR_Word *)commit_frame), commit_frame);
    printf("\tPrevious frame #%d: %d\n",
        MR_region_get_frame_number(
            (MR_Word *) commit_frame->MR_rcff_previous_commit_frame),
        commit_frame->MR_rcff_previous_commit_frame);
    printf("\tSequence number at push: %d\n",
        commit_frame->MR_rcff_saved_sequence_number);
    printf("\tDisj frame at push #%d: %d\n",
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
    for (i = 0; i < commit_frame->MR_rcff_num_saved_regions; i++,
            commit_save++)
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
    int     i;

    /*
    ** This check is for development, when it becomes more stable, the
    ** check can be removed.
    */
    if (commit_frame->MR_rcff_num_saved_regions > 10) {
        printf("Number of saved region: %d\n",
            commit_frame->MR_rcff_num_saved_regions);
        MR_fatal_error("Too many regions were saved.");
    }

    commit_save = (MR_RegionCommitSave *) (
        (MR_Word *) commit_frame + MR_REGION_COMMIT_FRAME_FIXED_SIZE);
    for (i = 0; i < commit_frame->MR_rcff_num_saved_regions;
            i++, commit_save++)
    {
        printf("\tAt slot: %d, saved region: %d\n", commit_save,
            commit_save->MR_commit_save_region);
    }

}

#endif /* End of MR_RBMM_DEBUG. */

/*---------------------------------------------------------------------------*/
/*
** Profiling methods for RBMM.
*/

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
MR_region_profile_destroyed_region(MR_Region *region)
{
    int allocated_size_of_region;

    MR_region_update_profiling_unit(&MR_rbmmp_regions_used, -1);
    MR_region_update_profiling_unit(&MR_rbmmp_pages_used,
        -MR_region_get_number_of_pages((MR_RegionPage *) region,
        region->MR_region_last_page));
    allocated_size_of_region =
        ((MR_RegionPage *) region)->MR_regionpage_allocated_size;
    MR_region_update_profiling_unit(&MR_rbmmp_words_used,
        -allocated_size_of_region);
    if (allocated_size_of_region > MR_rbmmp_biggest_region_size) {
        MR_rbmmp_biggest_region_size = allocated_size_of_region;
    }
}

void
MR_region_profile_restore_from_snapshot(MR_RegionSnapshot *snapshot)
{
    MR_Region       *restoring_region;
    MR_RegionPage   *first_new_page;
    int             new_words;
    int             new_pages;

    restoring_region = snapshot->MR_snapshot_region;
    first_new_page = snapshot->MR_snapshot_saved_last_page->MR_regionpage_next;

    if (first_new_page != NULL) {
        new_pages = MR_region_get_number_of_pages(first_new_page,
             restoring_region->MR_region_last_page);
        MR_region_update_profiling_unit(&MR_rbmmp_pages_used, -new_pages);
        new_words = (new_pages * MR_REGION_PAGE_SPACE_SIZE -
             restoring_region->MR_region_available_space +
             snapshot->MR_snapshot_saved_available_space);
    } else {
        new_words = snapshot->MR_snapshot_saved_available_space -
            restoring_region->MR_region_available_space;
    }
    ((MR_RegionPage *) restoring_region)->MR_regionpage_allocated_size
        -= new_words;
    MR_region_update_profiling_unit(&MR_rbmmp_words_used, -new_words);
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
    /* Plus the last page. */
    number_of_pages += 1;
    return number_of_pages;
}

void
MR_region_print_profiling_unit(const char *str,
    MR_RegionProfUnit *profiling_unit)
{
    printf(str);    /* XXX This is dangerous. */
    printf("\n");
    printf("\tTotal: %d.\n", profiling_unit->MR_rbmmpu_total);
    printf("\tMaximum: %d.\n", profiling_unit->MR_rbmmpu_max);
    printf("\tCurrent: %d.\n", profiling_unit->MR_rbmmpu_current);
}

void
MR_region_print_profiling_info(void)
{
    MR_region_print_profiling_unit("Regions:", &MR_rbmmp_regions_used);
    printf("Biggest region size: %d.\n", MR_rbmmp_biggest_region_size);
    MR_region_print_profiling_unit("Words:", &MR_rbmmp_words_used);
    MR_region_print_profiling_unit("Pages used:", &MR_rbmmp_pages_used);
    printf("Pages requested: %d.\n", MR_rbmmp_pages_requested);
}

#else /* Not define MR_RBMM_PROFILING. */

void
MR_region_update_profiling_unit(MR_RegionProfUnit *pu, int q)
{
    /* do nothing */
}

void
MR_region_profile_destroyed_region(MR_Region *r)
{
    /* do nothing */
}

void
MR_region_profile_restore_from_snapshot(MR_RegionSnapshot *s)
{
    /* do nothing */
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

#endif /* End of Not define MR_RBMM_PROFILING. */

#endif  /* MR_USE_REGIONS */
