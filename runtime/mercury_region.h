/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** File: mercury_region.h
** main author: Quan Phan
*/

#ifndef MERCURY_REGION_H
#define MERCURY_REGION_H

#include "mercury_types.h"

#ifdef MR_USE_REGIONS

/*
** See the documentation of MR_RBMM_DEBUG and MR_RBMM_PROFILING in
** mercury_conf_param.h.
*/

/*
** XXX: Many of these macros would be more efficient if they updated the
** pointer processed in each loop iteration using pointer increment operations
** instead of recomputing it each time from the pointer to the first element,
** and the loop variable  i.
**
** XXX Many of these macros would be more readable if they used array notation.
** For example, stack_pointer[offset] instead *(stack_pointer + offset),
** and &stack_pointer[offset] instead (stack_pointer + offset).
*/

/* XXX This should be made configurable when compiling a program. */
#define     MR_REGION_NUM_PAGES_TO_REQUEST                  100
#define     MR_REGION_PAGE_SPACE_SIZE                       256

/*
** The following constants should match the values of the Mercury compiler
** options with corresponding names.
*/

#define     MR_REGION_FRAME_FIXED_SIZE                      4
#define     MR_REGION_FRAME_PREVIOUS_FRAME                  0
#define     MR_REGION_FRAME_REGION_LIST                     1
#define     MR_REGION_FRAME_NUMBER_PROTECTED_REGIONS        2
#define     MR_REGION_FRAME_NUMBER_SNAPSHOTS                3

#define     MR_REGION_COMMIT_FRAME_FIXED_SIZE               3
#define     MR_REGION_COMMIT_FRAME_PREVIOUS_FRAME           0
#define     MR_REGION_COMMIT_FRAME_SEQUENCE_NUMBER          1
#define     MR_REGION_COMMIT_FRAME_NUMBER_SAVED_REGIONS     2
#define     MR_REGION_COMMIT_FRAME_FIRST_SAVED_REGION       3

#define     MR_REGION_SNAPSHOT_SIZE                         4
#define     MR_REGION_SNAPSHOT_REGION                       0
#define     MR_REGION_SNAPSHOT_LAST_PAGE                    1
#define     MR_REGION_SNAPSHOT_NEXT_AVAILABLE_WORD          2
#define     MR_REGION_SNAPSHOT_AVAILABLE_SPACE              3

#define     MR_REGION_ITE_PROT_SIZE                         1
#define     MR_REGION_DISJ_PROT_SIZE                        1
#define     MR_REGION_COMMIT_ENTRY_SIZE                     1

/*
** A region page contains an array (of MR_Word) to store program data and
** a pointer to the next to form a single-linked-list.  Note: the order of
** fields in this struct is important. We make use of the order for several
** computations (such as the address of a region).
*/

struct MR_RegionPage_Struct {
    /* The space to store program data. */
    MR_Word             MR_regionpage_space[MR_REGION_PAGE_SPACE_SIZE];

    /* Pointer to the next page to form the linked list. */
    MR_RegionPage       *MR_regionpage_next;

#ifdef MR_RBMM_PROFILING
    /*
    ** This is to count the number of words which are currently allocated into
    ** the region. It means that it will be reset at backtracking if the region
    ** is restored from its snapshot.
    */
    int                 MR_regionpage_allocated_size;
#endif
};

/*
** A region is implemented as a single-linked list of (region) pages.
** The information of the region itself (pointer to next available word,
** removal counter, some other operational data) is stored in its first page.
*/

struct MR_Region_Struct {
    /*
    ** Pointer to the last page of the region, i.e., the newest or last added
    ** page. This is needed when we need to enlarge the region.
    */
    MR_RegionPage       *MR_region_last_page;

    /*
    ** Pointer to the next available word for allocation. Allocations into
    ** a region always occur at its last page therefore this pointer always
    ** points into the last page.
    */
    MR_Word             *MR_region_next_available_word;

    /*
    ** The current number of words (in the last page) available for allocation
    ** into the region. When an allocation requires more words than what is
    ** available the region is extended by adding a new page.
    */
    MR_Word             MR_region_available_space;

    /*
    ** Currently not used. To be used for implementing conditional removals
    ** of regions, i.e., the region is only actually removed when the counter
    ** equals to one.
    */
    unsigned int        MR_region_removal_counter;

    /* Sequence number of the region. */
    MR_Word             MR_region_sequence_number;

    /* If the region has been removed in forward execution. */
    MR_Word             MR_region_logical_removed;

    /*
    ** NULL if the region is not protected by any if-then-else. Otherwise
    ** it points to region_ite_stack frame that protects it.
    */
    MR_Word             *MR_region_ite_protected;

    /*
    ** NULL if the region is not protected by any disj goal. If some disj
    ** frames exist and the region is protected from being destroyed by a
    ** disj goal, this field will point to the top region_disj_stack
    ** frame.
    */
    MR_Word             *MR_region_disj_protected;

    /* If the region is saved at this frame in the region_commit_stack. */
    MR_Word             *MR_region_commit_frame;

    /*
    ** True if the region has been removed logically in a commit context and
    ** therefore needs to be destroyed at commit.
    */
    MR_Word             MR_region_destroy_at_commit;

    MR_Region           *MR_region_previous_region;
    MR_Region           *MR_region_next_region;
};

/*
** To save the current size of a region in preparation for instant reclaiming.
*/

struct MR_RegionSnapshot_Struct {
    MR_Region           *region;
    MR_RegionPage       *saved_last_page;
    MR_Word             *saved_next_available_word;
    MR_Word             saved_available_space;
};

/* Protection information in a disj frame. */
struct MR_RegionDisjProtect_Struct {
    MR_Region           *region;
    /* XXX We no longer need to save ite_protected into a disj-frame*/
    /*MR_Word                         *ite_protected;*/
};

/*
** The region runtime maintains a list of free pages, when a program needs
** a page for a region the page is taken from the list.
*/

extern MR_RegionPage    *MR_region_free_page_list;

/* The live regions are linked in a list. */
extern MR_Region        *MR_live_region_list;

extern MR_Word          MR_region_sequence_number;

/* Pointers to the top frames of ite, disj, and commit stacks. */
extern MR_Word          *MR_region_ite_sp;
extern MR_Word          *MR_region_disj_sp;
extern MR_Word          *MR_region_commit_sp;

/*---------------------------------------------------------------------------*/

/* Create a region. */
extern  MR_Region       *MR_region_create_region(void);

/* Destroy a region, i.e., physically deallocate the region. */
extern  void            MR_region_destroy_region(MR_Region *);

/*
** Remove a region.
** If the region is not protected it is destroyed, otherwise it is only
** logically removed, i.e., we mark it as removed but not actually deallocate.
*/

extern  void            MR_region_remove_region(MR_Region *);
extern  void            MR_remove_undisjprotected_region_ite_then_semidet(
                            MR_Region *);
extern  void            MR_remove_undisjprotected_region_ite_then_nondet(
                            MR_Region *);

/* Allocate a number of words into a region. */
extern  MR_Word         *MR_region_alloc(MR_Region *, unsigned int);

#define     MR_alloc_in_region(dest, region, num)                           \
            MR_tag_alloc_in_region(dest, 0, region, num)                    \

#define     MR_tag_alloc_in_region(dest, tag, region, num)                  \
            do {                                                            \
                (dest) = (MR_Word) MR_mkword((tag), (MR_Word)               \
                    MR_region_alloc((MR_Region *) (region), (num)));        \
            } while (0)

/*---------------------------------------------------------------------------*/
/*
** push_region_frame
*/

#define     MR_push_region_ite_frame(new_ite_sp)                            \
            do {                                                            \
                *(new_ite_sp) = (MR_Word) MR_region_ite_sp;                 \
                MR_save_live_region_list(                                   \
                        *((new_ite_sp) + MR_REGION_FRAME_REGION_LIST));     \
                MR_region_ite_sp = (new_ite_sp);                            \
                MR_region_debug_push_ite_frame(new_ite_sp);                 \
            } while (0)

#define     MR_push_region_disj_frame(new_disj_sp)                          \
            do {                                                            \
                *(new_disj_sp) = (MR_Word) MR_region_disj_sp;               \
                MR_save_live_region_list(                                   \
                    *((new_disj_sp) + MR_REGION_FRAME_REGION_LIST));        \
                MR_region_disj_sp = (new_disj_sp);                          \
                MR_region_debug_push_disj_frame(new_disj_sp);               \
                if (MR_live_region_list != NULL) {                          \
                    MR_region_debug_region_struct_removal_info(             \
                        MR_live_region_list);                               \
                }                                                           \
            } while (0)

#define     MR_push_region_commit_frame(new_commit_sp)                      \
            do {                                                            \
                *(new_commit_sp) = (MR_Word) MR_region_commit_sp;           \
                *(new_commit_sp + MR_REGION_COMMIT_FRAME_SEQUENCE_NUMBER) = \
                     MR_region_sequence_number;                             \
                MR_region_commit_sp = (new_commit_sp);                      \
                MR_region_debug_push_commit_frame(new_commit_sp);           \
            } while (0)

/*---------------------------------------------------------------------------*/
/*
** region_fill_frame
*/
/*
** Save the region if it satisfies:
** (a) live before condition
** (c2) aren't already protected (ite_protected or disj_protected).
** If save the region, then set the ite_protected field in these regions
** to point to the frame.
*/
#define     MR_region_fill_ite_protect(ite_sp, region_ptr,                  \
                num_protected_regions, region_slot_reg)                     \
            do {                                                            \
                MR_Region *region;                                          \
                                                                            \
                region = (MR_Region *) (region_ptr);                        \
                if (region->MR_region_disj_protected == NULL &&             \
                    region->MR_region_ite_protected == NULL)                \
                {                                                           \
                    *((MR_Word *) (region_slot_reg)) = (MR_Word) region;    \
                    (num_protected_regions)++;                              \
                    (region_slot_reg) = (MR_Word)                           \
                        (((MR_Word *) (region_slot_reg)) +                  \
                            MR_REGION_ITE_PROT_SIZE);                       \
                    region->MR_region_ite_protected = (ite_sp);             \
                }                                                           \
            } while (0)

/*
** This is to prepare for instant reclaiming at the start of else. For instant
** reclaiming a region we save its current size by taking a snapshot of it. The
** natural question would be for which regions.  The very first
** criterion is whether a region will be destroyed right at the start of the
** else. It is because we need not to reclaim memory for those which will be
** destroyed anyway right after that. To decide if a region will be destroyed
** at the start of the else we need information at both compile-time and
** runtime. That is at compile-time we only know if the region is removed or
** not, and at runtime we will know if the region is protected from being
** destroyed. So,
** 1. Those that are removed and protected need to be saved.
** 2. Those that are not removed (so not destroyed) will need to be saved.
*/
/*
** XXX ite_sp is not used here.
*/
#define     MR_region_fill_ite_snapshot_removed(ite_sp, region_ptr,         \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_Region   *region;                                        \
                                                                            \
                region = (MR_Region *) (region_ptr);                        \
                if (region->MR_region_ite_protected != NULL ||              \
                    region->MR_region_disj_protected != NULL)               \
                {                                                           \
                    MR_save_snapshot(region, snapshot_block);               \
                    MR_next_snapshot_block(snapshot_block);                 \
                    (num_snapshots)++;                                      \
                } /* Else the region is not protected. */                   \
            } while (0)

#define     MR_region_fill_ite_snapshot_not_removed(ite_sp, region_ptr,     \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_Region   *region;                                        \
                                                                            \
                region = (MR_Region *) (region_ptr);                        \
                MR_save_snapshot(region, (snapshot_block));                 \
                MR_next_snapshot_block(snapshot_block);                     \
                (num_snapshots)++;                                          \
            } while (0)

#define     MR_region_fill_disj_protect(disj_sp, region_ptr,                \
                num_protected_regions, protection_block)                    \
            do {                                                            \
                MR_Region   *region;                                        \
                                                                            \
                region = (MR_Region *) (region_ptr);                        \
                if (region->MR_region_disj_protected == NULL) {             \
                    MR_save_disj_protection(region, protection_block);      \
                    (num_protected_regions)++;                              \
                    MR_next_disj_protection_block(protection_block);        \
                    region->MR_region_disj_protected = (disj_sp);           \
                }                                                           \
            } while (0)

#define     MR_region_fill_disj_snapshot(disj_sp, region_ptr,               \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_Region   *region;                                        \
                                                                            \
                region = (MR_Region *) (region_ptr);                        \
                MR_save_snapshot(region, (snapshot_block));                 \
                MR_next_snapshot_block(snapshot_block);                     \
                (num_snapshots)++;                                          \
            } while (0)

/*
** Save the live and unprotected regions which are input to the commit goal
** into the top commit stack frame.
** Set the commit_frame field in these regions to point to the frame.
*/
#define     MR_region_fill_commit(commit_sp, region_ptr,                    \
                    num_saved_region_reg, region_slot_reg)                  \
            do {                                                            \
                MR_Region   *region;                                        \
                                                                            \
                region = (MR_Region *) (region_ptr);                        \
                if (region->MR_region_disj_protected == NULL &&             \
                    region->MR_region_ite_protected == NULL)                \
                {                                                           \
                    *((MR_Word *) (region_slot_reg)) = (MR_Word) region;    \
                    num_saved_region_reg++;                                 \
                    (region_slot_reg) = (MR_Word)                           \
                        (((MR_Word *) (region_slot_reg)) +                  \
                            MR_REGION_COMMIT_ENTRY_SIZE);                   \
                    (region)->MR_region_commit_frame = (commit_sp);         \
                }                                                           \
            } while (0)

/*---------------------------------------------------------------------------*/
/*
** region_set_fixed_slot
*/
#define     MR_region_set_ite_num_protects(ite_sp, num)                     \
            do {                                                            \
                *((ite_sp) + MR_REGION_FRAME_NUMBER_PROTECTED_REGIONS)      \
                    = (num);                                                \
                MR_region_debug_ite_frame_protected_regions(ite_sp);        \
            } while (0)

#define     MR_region_set_ite_num_snapshots(ite_sp, num)                    \
            do {                                                            \
                *((ite_sp) + MR_REGION_FRAME_NUMBER_SNAPSHOTS) = (num);     \
                MR_region_debug_ite_frame_snapshots(ite_sp);                \
            } while (0)

#define     MR_region_set_disj_num_protects(disj_sp, num)                   \
            do {                                                            \
                *((disj_sp) + MR_REGION_FRAME_NUMBER_PROTECTED_REGIONS)     \
                    = (num);                                                \
                MR_region_debug_disj_frame_protected_regions(disj_sp);      \
            } while (0)

#define     MR_region_set_disj_num_snapshots(disj_sp, num)                  \
            do {                                                            \
                *((disj_sp) + MR_REGION_FRAME_NUMBER_SNAPSHOTS) = (num);    \
                MR_region_debug_disj_frame_snapshots(disj_sp);              \
            } while (0)

#define     MR_region_set_commit_num_entries(commit_sp, num)                \
            do {                                                            \
                *((commit_sp) + MR_REGION_COMMIT_FRAME_NUMBER_SAVED_REGIONS)\
                    = (num);                                                \
                MR_region_debug_commit_frame_saved_regions(commit_sp);      \
            } while (0)

/*---------------------------------------------------------------------------*/
/*
** use_and_maybe_pop_region_frame
*/

/*
** The next two macros are to remove each protected region at the start of the
** then part. If the condition is semidet we just need to destroy all the
** protected regions (whose are not disj-protected).
** If the condition is nondet we have to do 2 more things:
**  + 1. check if a protected region has already been destroyed
**  + 2. if we destroy a protected region, we have to nullify its
**  corresponding entry in the ite frame.
*/
#define     MR_use_region_ite_then_semidet(ite_sp)                          \
            do {                                                            \
                MR_Word     num_protected_regions;                          \
                MR_Word     *first_protected_region;                        \
                MR_Region   *protected_region;                              \
                int         i;                                              \
                                                                            \
                num_protected_regions =                                     \
                    MR_region_frame_number_protected_regions(ite_sp);       \
                first_protected_region =                                    \
                    (ite_sp) + MR_REGION_FRAME_FIXED_SIZE;                  \
                for (i = 0; i < num_protected_regions; i++) {               \
                    protected_region = (MR_Region *)                        \
                        (*(first_protected_region + i));                    \
                    MR_remove_undisjprotected_region_ite_then_semidet(      \
                        protected_region);                                  \
                }                                                           \
                MR_pop_region_ite_frame(ite_sp);                            \
            } while (0)

#define     MR_use_region_ite_then_nondet(ite_sp)                           \
            do {                                                            \
                MR_Word     num_protected_regions;                          \
                MR_Word     *first_protected_region;                        \
                MR_Region   *protected_region;                              \
                int         i;                                              \
                                                                            \
                num_protected_regions =                                     \
                    MR_region_frame_number_protected_regions(ite_sp);       \
                first_protected_region =                                    \
                    (ite_sp) + MR_REGION_FRAME_FIXED_SIZE;                  \
                for (i = 0; i < num_protected_regions; i++) {               \
                    protected_region = (MR_Region *)                        \
                        (*(first_protected_region + i));                    \
                    if (protected_region != NULL) {                         \
                        MR_remove_undisjprotected_region_ite_then_nondet(   \
                            protected_region);                              \
                    }                                                       \
                }                                                           \
            } while (0)

#define     MR_use_region_ite_else_semidet(ite_sp)                          \
            do {                                                            \
                MR_region_process_at_ite_else(ite_sp);                      \
            } while (0)

#define     MR_use_region_ite_else_nondet(ite_sp)                           \
            do {                                                            \
                MR_region_process_at_ite_else(ite_sp);                      \
            } while (0)

/*
** XXX  What am I supposed to do here?
** I think it should be exactly the same as the process at the start of
** any else branch, i.e., after the condition fails (to produce any solution).
*/
#define     MR_use_region_ite_nondet_cond_fail(ite_sp)                      \
            do {                                                            \
                MR_region_process_at_ite_else(ite_sp)                       \
            } while (0)

#define     MR_use_region_disj_later(disj_sp)                               \
            do {                                                            \
                MR_region_disj_restore_from_snapshots(disj_sp);             \
                MR_region_disj_destroy_new_regions(disj_sp);                \
            } while (0)

#define     MR_use_region_disj_last(disj_sp)                                \
            do {                                                            \
                MR_region_disj_restore_from_snapshots(disj_sp);             \
                MR_region_disj_destroy_new_regions(disj_sp);                \
                MR_region_disj_unprotect_regions(disj_sp);                  \
                MR_pop_region_disj_frame(disj_sp);                          \
            } while (0)

#define     MR_use_region_commit_success(commit_sp)                         \
            do {                                                            \
                MR_Word saved_region_seq_number;                            \
                MR_Word number_of_saved_regions;                            \
                MR_Word *first_saved_region_slot;                           \
                                                                            \
                saved_region_seq_number =                                   \
                    *((commit_sp) +                                         \
                        MR_REGION_COMMIT_FRAME_SEQUENCE_NUMBER);            \
                number_of_saved_regions =                                   \
                    *((commit_sp) +                                         \
                        MR_REGION_COMMIT_FRAME_NUMBER_SAVED_REGIONS);       \
                first_saved_region_slot =                                   \
                    (commit_sp) + MR_REGION_COMMIT_FRAME_FIRST_SAVED_REGION;\
                MR_region_debug_commit_frame(commit_sp);                    \
                MR_region_debug_destroy_marked_regions_at_commit(           \
                    saved_region_seq_number, number_of_saved_regions,       \
                    first_saved_region_slot);                               \
                MR_destroy_marked_new_regions_at_commit(                    \
                    saved_region_seq_number);                               \
                MR_destroy_marked_old_regions_at_commit(                    \
                    number_of_saved_regions, first_saved_region_slot);      \
                MR_pop_region_commit_frame(commit_sp);                      \
            } while (0)

/*
** Commit failure means that the goal in the commit operation has failed.
** We reset any changes to the commit-related state of saved regions
** i.e., commit_frame and destroy_at_commit, to NULL and false, respectively.
** Then the top commit frame is popped.
*/
#define     MR_use_region_commit_failure(commit_sp)                         \
            do {                                                            \
                MR_Word     number_of_saved_regions;                        \
                MR_Word     *first_saved_region_slot;                       \
                MR_Region   *region;                                        \
                int         i;                                              \
                                                                            \
                number_of_saved_regions =                                   \
                    *((commit_sp) +                                         \
                        MR_REGION_COMMIT_FRAME_NUMBER_SAVED_REGIONS);       \
                first_saved_region_slot =                                   \
                    (commit_sp) + MR_REGION_COMMIT_FRAME_FIRST_SAVED_REGION;\
                for (i = 0; i < number_of_saved_regions; i++) {             \
                    region = (MR_Region *)                                  \
                        (first_saved_region_slot +                          \
                        i * MR_REGION_COMMIT_ENTRY_SIZE);                   \
                    if (region != NULL) {                                   \
                        region->MR_region_commit_frame = NULL;              \
                        region->MR_region_destroy_at_commit = MR_FALSE;     \
                    }                                                       \
                }                                                           \
                MR_pop_region_commit_frame(commit_sp);                      \
            } while (0)

extern  void    MR_destroy_marked_old_regions_at_commit(
                    MR_Word number_of_saved_regions,
                    MR_Word *first_saved_region_slot);

extern  void    MR_destroy_marked_new_regions_at_commit(
                    MR_Word saved_region_seq_number);

/*---------------------------------------------------------------------------*/

#define     MR_pop_region_ite_frame(ite_sp)                                 \
            do {                                                            \
                MR_region_ite_sp = (MR_Word *) (*ite_sp);                   \
            } while (0)

#define     MR_pop_region_disj_frame(disj_sp)                               \
            do {                                                            \
                MR_region_disj_sp = (MR_Word *) (*disj_sp);                 \
            } while (0)

#define     MR_pop_region_commit_frame(commit_sp)                           \
            do {                                                            \
                MR_region_commit_sp = (MR_Word *) (*commit_sp);             \
            } while (0)

/*---------------------------------------------------------------------------*/
/* Helpers for ite support. */

/*
** At the start of else, we
** 1. unprotect the protected regions,
** 2. instant reclaiming using snapshots,
** 3. instant reclaiming by destroying new regions created in the condition,
** 4. pop the current ite frame.
*/
#define     MR_region_process_at_ite_else(ite_sp)                           \
            do {                                                            \
                MR_region_ite_unprotect(ite_sp);                            \
                MR_region_ite_restore_from_snapshots(ite_sp);               \
                MR_region_ite_destroy_new_regions(ite_sp);                  \
                MR_pop_region_ite_frame(ite_sp);                            \
            } while (0)

/*
** Unprotect the protected regions at the beginning of the else part.
*/
#define     MR_region_ite_unprotect(ite_sp)                                 \
            do {                                                            \
                MR_Word     num_protected_regions;                          \
                MR_Region   *protected_region;                              \
                int         i;                                              \
                int         first_protected_region;                         \
                                                                            \
                MR_region_debug_ite_frame(ite_sp);                          \
                num_protected_regions =                                     \
                    MR_region_frame_number_protected_regions(ite_sp);       \
                first_protected_region = MR_REGION_FRAME_FIXED_SIZE;        \
                for (i = 0; i < num_protected_regions; i++) {               \
                    protected_region = (MR_Region *)                        \
                        (*((ite_sp) + first_protected_region + i));         \
                    /* Try to protect the region by an outer condition. */  \
                    protected_region->MR_region_ite_protected = (MR_Word *) \
                        (*(ite_sp));                                        \
                    MR_region_debug_region_struct_removal_info(             \
                        protected_region);                                  \
                }                                                           \
            } while (0)

#define     MR_region_ite_restore_from_snapshots(ite_sp)                    \
            do {                                                            \
                MR_Word num_snapshots;                                      \
                MR_Word first_snapshot_index;                               \
                                                                            \
                num_snapshots = MR_region_frame_number_snapshots(ite_sp);   \
                MR_ite_frame_get_first_snapshot_index(ite_sp,               \
                    first_snapshot_index);                                  \
                MR_restore_snapshots(ite_sp, num_snapshots,                 \
                    first_snapshot_index);                                  \
            } while (0)

#define     MR_region_ite_destroy_new_regions(ite_sp)                       \
            MR_region_frame_destroy_new_regions(ite_sp)

#define     MR_ite_frame_get_first_snapshot_index(ite_frame,                \
                first_snapshot_index)                                       \
            do {                                                            \
                int num_protected_regions;                                  \
                                                                            \
                num_protected_regions =                                     \
                    MR_region_frame_number_protected_regions((ite_frame));  \
                (first_snapshot_index) = MR_REGION_FRAME_FIXED_SIZE +       \
                    num_protected_regions * MR_REGION_ITE_PROT_SIZE;        \
            } while (0)

/*---------------------------------------------------------------------------*/
/* Helpers for nondet disjunction support. */

/* XXX This macro should no long be used.
** At any non-first disjunct, reset the ite_protected to the state before
** the disjunction.
*/
#define     MR_region_disj_restore_ite_protected(disj_sp)                   \
            do {                                                            \
                MR_RegionDisjProtect    *first_block;                       \
                MR_RegionDisjProtect    *block;                             \
                MR_Word                 num_protected_regions;              \
                MR_Word                 i;                                  \
                                                                            \
                first_block = (MR_RegionDisjProtect *)                      \
                    (disj_sp + MR_REGION_FRAME_FIXED_SIZE);                 \
                num_protected_regions =                                     \
                    MR_region_frame_number_protected_regions(disj_sp);      \
                for (i = 0; i < num_protected_regions; i++) {               \
                    block = first_block + i;                                \
                    block->region->MR_region_ite_protected =                \
                        block->MR_region_ite_protected;                     \
                }                                                           \
            } while (0)

/*
** At any non-first disjunct, try instant reclaiming from snapshots.
*/
#define     MR_region_disj_restore_from_snapshots(disj_sp)                  \
            do {                                                            \
                MR_Word num_snapshots;                                      \
                MR_Word first_snapshot_index;                               \
                                                                            \
                num_snapshots = MR_region_frame_number_snapshots(disj_sp);  \
                MR_disj_frame_get_first_snapshot_index(disj_sp,             \
                    first_snapshot_index);                                  \
                MR_restore_snapshots(disj_sp, num_snapshots,                \
                    first_snapshot_index);                                  \
            } while (0)

/*
** At any non-first disjunct, try instant reclaiming by destroying new
** regions.
*/
#define     MR_region_disj_destroy_new_regions(disj_sp)                     \
            MR_region_frame_destroy_new_regions(disj_sp)

/*
** At the last disjunct, we do not disj-protect the regions anymore.
*/
#define     MR_region_disj_unprotect_regions(disj_sp)                       \
            do {                                                            \
                MR_RegionDisjProtect    *first_block;                       \
                MR_RegionDisjProtect    *block;                             \
                MR_Word                 num_protected_regions;              \
                MR_Word                 i;                                  \
                                                                            \
                first_block = (MR_RegionDisjProtect *)                      \
                    (disj_sp + MR_REGION_FRAME_FIXED_SIZE);                 \
                num_protected_regions =                                     \
                    MR_region_frame_number_protected_regions(disj_sp);      \
                for (i = 0; i < num_protected_regions; i++) {               \
                    block = first_block + i;                                \
                    block->region->MR_region_disj_protected = NULL;         \
                }                                                           \
            } while (0)

#define     MR_disj_frame_get_first_snapshot_index(disj_frame,              \
                first_snapshot_index)                                       \
            do {                                                            \
                int num_protected_regions;                                  \
                                                                            \
                num_protected_regions =                                     \
                    MR_region_frame_number_protected_regions(disj_frame);   \
                (first_snapshot_index) = MR_REGION_FRAME_FIXED_SIZE +       \
                    num_protected_regions * MR_REGION_DISJ_PROT_SIZE;       \
            } while (0)

/*---------------------------------------------------------------------------*/

#define     MR_region_frame_number_protected_regions(frame) (               \
                *((frame) + MR_REGION_FRAME_NUMBER_PROTECTED_REGIONS)       \
            )

#define     MR_save_disj_protection(region, protection_block)               \
            do {                                                            \
                MR_RegionDisjProtect *disj_prot;                            \
                                                                            \
                disj_prot = (MR_RegionDisjProtect *) (protection_block);    \
                disj_prot->region = (region);                               \
                /* disj_prot->MR_region_ite_protected =                     \
                    (region)->MR_region_ite_protected; */                   \
            } while (0)

#define     MR_next_disj_protection_block(block) (                          \
                (block) = (MR_Word) (((MR_RegionDisjProtect *) (block)) + 1) \
            )

#define     MR_region_frame_number_snapshots(frame) (                       \
                *((frame) + MR_REGION_FRAME_NUMBER_SNAPSHOTS)               \
            )

#define     MR_save_snapshot(region, snapshot_block)                        \
            do {                                                            \
                MR_RegionSnapshot *snapshot;                                \
                                                                            \
                snapshot = (MR_RegionSnapshot *) (snapshot_block);          \
                snapshot->region = (region);                                \
                snapshot->saved_last_page = (region)->MR_region_last_page;  \
                snapshot->saved_next_available_word =                       \
                    (region)->MR_region_next_available_word;                \
                snapshot->saved_available_space =                           \
                    (region)->MR_region_available_space;                    \
            } while (0)

#define     MR_next_snapshot_block(snapshot_block) (                        \
                (snapshot_block) = (MR_Word)                                \
                    (((MR_RegionSnapshot *) (snapshot_block) ) + 1)         \
            )

/*
** XXX For profiling:
** One correct way to reset the allocated_size is to save it in the snapshot
** so that here we have the old value right away. But having an extra slot
** in the disj frame causes changes at other places. From the snapshot
** information (as it is now) we can only compute the old value correctly if
** there is no wasteful space at the end of the region's pages. Therefore the
** allocated_size sometimes is not realiable.
*/

#define     MR_restore_snapshots(frame, num_snapshots, first_snapshot_index)\
            do {                                                            \
                MR_RegionSnapshot   *first_snapshot;                        \
                MR_RegionSnapshot   *snapshot;                              \
                MR_Region           *restoring_region;                      \
                MR_RegionPage       *saved_last_page;                       \
                MR_RegionPage       *first_new_page;                        \
                int                 i;                                      \
                                                                            \
                first_snapshot = (MR_RegionSnapshot *)                      \
                    ((frame) + (first_snapshot_index));                     \
                for (i = 0; i < (num_snapshots); i++) {                     \
                    snapshot = first_snapshot + i;                          \
                    restoring_region = snapshot->region;                    \
                    saved_last_page = snapshot->saved_last_page;            \
                    first_new_page = saved_last_page->MR_regionpage_next;   \
                    MR_region_profile_restore_from_snapshot(snapshot);      \
                    if (first_new_page != NULL) {                           \
                        MR_region_return_page_list(first_new_page,          \
                            restoring_region->MR_region_last_page);         \
                        restoring_region->MR_region_last_page =             \
                            saved_last_page;                                \
                    } /* else no new page added. */                         \
                    restoring_region->MR_region_next_available_word =       \
                        snapshot->saved_next_available_word;                \
                    restoring_region->MR_region_available_space =           \
                        snapshot->saved_available_space;                    \
                }                                                           \
            } while(0)

#define     MR_region_frame_destroy_new_regions(frame)                      \
            do {                                                            \
                MR_Region   *saved_most_recent_region;                      \
                MR_Region   *region;                                        \
                MR_Region   *next_region;                                   \
                                                                            \
                saved_most_recent_region = (MR_Region *)                    \
                    (*((frame) + MR_REGION_FRAME_REGION_LIST));             \
                region = MR_live_region_list;                               \
                while (region != saved_most_recent_region) {                \
                    next_region = region->MR_region_next_region;            \
                    MR_region_debug_disj_frame(frame);\
                    /* We destroy regions upto before the saved one. */     \
                    MR_region_destroy_region(region);                       \
                    region = next_region;                                   \
                }                                                           \
                MR_live_region_list = saved_most_recent_region;             \
            } while (0)

/* from_page must not be NULL. */
#define     MR_region_return_page_list(from_page, to_page)                  \
            do {                                                            \
                (to_page)->MR_regionpage_next = MR_region_free_page_list;   \
                MR_region_free_page_list = (from_page);                     \
            } while (0)

#define     MR_save_live_region_list(slot) (                                \
                (slot) = (MR_Word) MR_live_region_list                      \
            )

/*---------------------------------------------------------------------------*/
/* Debug RBMM messages. */

#ifdef MR_RBMM_DEBUG
    #define     MR_region_debug_create_region(region)                       \
                MR_region_create_region_msg(region)

    #define     MR_region_debug_try_remove_region(region)                   \
                MR_region_try_remove_region_msg(region)

    #define     MR_region_debug_region_struct_removal_info(region)          \
                MR_region_region_struct_removal_info_msg(region)

    #define     MR_region_debug_destroy_region(region)                      \
                MR_region_destroy_region_msg(region)

/* Debug ite frame messages. */
    #define     MR_region_debug_push_ite_frame(ite_sp)                      \
                MR_region_push_ite_frame_msg(ite_sp)

    #define     MR_region_debug_ite_frame(ite_sp);                          \
                MR_region_ite_frame_msg(ite_sp)

    #define     MR_region_debug_ite_frame_protected_regions(ite_sp);        \
                MR_region_ite_frame_protected_regions_msg(ite_sp)

    #define     MR_region_debug_ite_frame_snapshots(ite_sp);                \
                MR_region_ite_frame_snapshots_msg(ite_sp)

/* Debug disj frame messages. */
    #define     MR_region_debug_push_disj_frame(disj_sp)                    \
                MR_region_push_disj_frame_msg(disj_sp)

    #define     MR_region_debug_disj_frame(disj_sp)                         \
                MR_region_disj_frame_msg(disj_sp)

    #define     MR_region_debug_disj_frame_protected_regions(disj_sp);      \
                MR_region_disj_frame_protected_regions_msg(disj_sp);

    #define     MR_region_debug_disj_frame_snapshots(disj_sp);              \
                MR_region_disj_frame_snapshots_msg(disj_sp);

/* Debug commit frame messages. */
    #define     MR_region_debug_push_commit_frame(frame)                    \
                MR_region_push_commit_frame_msg(frame)

    #define     MR_region_debug_commit_frame(frame)                         \
                MR_region_commit_frame_msg(frame)

    #define     MR_region_debug_commit_frame_saved_regions(commit_sp);      \
                MR_region_commit_frame_saved_regions_msg(commit_sp)

    #define     MR_region_debug_destroy_marked_regions_at_commit(           \
                    saved_region_seq_number, number_of_saved_regions,       \
                    first_saved_region_slot)                                \
                MR_region_destroy_marked_regions_at_commit_msg(             \
                    saved_region_seq_number, number_of_saved_regions,       \
                    first_saved_region_slot)

#else   /* MR_RBMM_DEBUG */
    #define     MR_region_debug_create_region(region)                       \
                ((void) 0)

    #define     MR_region_debug_try_remove_region(region)                   \
                ((void) 0)


    #define     MR_region_debug_region_struct_removal_info(region)          \
                ((void) 0)

    #define     MR_region_debug_destroy_region(region)                      \
                ((void) 0)

    #define     MR_region_debug_push_ite_frame(frame)                       \
                ((void) 0)

    #define     MR_region_debug_ite_frame(ite_sp);                          \
                ((void) 0)

    #define     MR_region_debug_ite_frame_protected_regions(ite_sp);        \
                ((void) 0)

    #define     MR_region_debug_ite_frame_snapshots(ite_sp);                \
                ((void) 0)

    #define     MR_region_debug_push_disj_frame(disj_sp)                    \
                ((void) 0)

    #define     MR_region_debug_disj_frame(frame)                           \
                ((void) 0)

    #define     MR_region_debug_disj_frame_protected_regions(disj_sp);      \
                ((void) 0)

    #define     MR_region_debug_disj_frame_snapshots(disj_sp);              \
                ((void) 0)

    #define     MR_region_debug_push_commit_frame(frame)                    \
                ((void) 0)

    #define     MR_region_debug_commit_frame(frame)                         \
                ((void) 0)

    #define     MR_region_debug_commit_frame_saved_regions(commit_sp)       \
                ((void) 0)

    #define     MR_region_debug_destroy_marked_regions_at_commit(           \
                    saved_region_seq_number, number_of_saved_regions,       \
                    first_saved_region_slot)                                \
                ((void) 0)
#endif /* MR_RBMM_DEBUG */

extern  void    MR_region_create_region_msg(MR_Region *region);
extern  void    MR_region_try_remove_region_msg(MR_Region *region);
extern  void    MR_region_region_struct_removal_info_msg(MR_Region *region);
extern  void    MR_region_destroy_region_msg(MR_Region *region);
extern  void    MR_region_logically_remove_region_msg(MR_Region *region);

extern  void    MR_region_push_ite_frame_msg(MR_Word *ite_frame);
extern  void    MR_region_ite_frame_msg(MR_Word *ite_frame);
extern  void    MR_region_ite_frame_protected_regions_msg(MR_Word *ite_frame);
extern  void    MR_region_ite_frame_snapshots_msg(MR_Word *ite_frame);

extern  void    MR_region_push_disj_frame_msg(MR_Word *disj_frame);
extern  void    MR_region_disj_frame_msg(MR_Word *disj_frame);
extern  void    MR_region_disj_frame_protected_regions_msg(
                    MR_Word *disj_frame);
extern  void    MR_region_disj_frame_snapshots_msg(MR_Word *disj_frame);

extern  void    MR_region_push_commit_frame_msg(MR_Word *commit_frame);
extern  void    MR_region_commit_frame_msg(MR_Word *commit_frame);
extern  void    MR_region_commit_frame_saved_regions_msg(MR_Word *commit_frame);
extern  void    MR_region_destroy_marked_regions_at_commit_msg(
                    int saved_seq_number, int number_of_saved_regions,
                    MR_Word *commit_frame);

/*---------------------------------------------------------------------------*/
/* Profiling RBMM. */

#ifdef MR_RBMM_PROFILING

/*
** This is the profiling wish list, not all of them have been yet collected.
** - How many words are allocated
** - Maximum number of words
** - How many regions are allocated
** - Maximum number of regions
** - Size of the biggest region
** - How many regions are saved at commit entries
** - How many regions are protected at entry to a condition
** - How many snapshots are saved at entry to a condition
** - How many regions are protected at entry to a disj goal
** - How many snapshots are saved at entry to a disj goal
** - How many pages are requested from the OS
** - Time profiling: compile-time and runtime
*/

struct MR_RegionProfUnit_Struct {
    int        MR_rbmmpu_current;
    int        MR_rbmmpu_max;
    int        MR_rbmmpu_total;
};

extern MR_RegionProfUnit    MR_rbmmp_words_used;
extern MR_RegionProfUnit    MR_rbmmp_regions_used;
extern MR_RegionProfUnit    MR_rbmmp_pages_used;
extern unsigned int         MR_rbmmp_page_requested;
extern unsigned int         MR_rbmmp_biggest_region_size;
extern MR_RegionProfUnit    MR_rbmmp_regions_saved_at_commit;
extern unsigned int         MR_rbmmp_regions_protected_at_ite;
extern unsigned int         MR_rbmmp_snapshots_saved_at_ite;
extern unsigned int         MR_rbmmp_regions_protected_at_disj;
extern unsigned int         MR_rbmmp_snapshots_saved_at_disj;
extern double               MR_rbmmp_page_utilized;

#endif /* MR_RBMM_PROFILING. */

extern  void    MR_region_update_profiling_unit(
                    MR_RegionProfUnit *profiling_unit, int quantity);
extern  void    MR_region_profile_destroyed_region(MR_Region *);
extern  void    MR_region_profile_restore_from_snapshot(MR_RegionSnapshot *);
extern  int     MR_region_get_number_of_pages(MR_RegionPage *,
                    MR_RegionPage *);
extern  void    MR_region_print_profiling_info(void);

/*---------------------------------------------------------------------------*/

#endif  /* MR_USE_REGIONS */

#endif  /* MERCURY_REGION_H */
