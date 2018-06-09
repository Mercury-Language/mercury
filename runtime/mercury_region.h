// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007-2009, 2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// File: mercury_region.h
// main author: Quan Phan

#ifndef MERCURY_REGION_H
#define MERCURY_REGION_H

#include "mercury_types.h"

#ifdef MR_USE_REGIONS

// See the documentation of MR_RBMM_DEBUG and MR_RBMM_PROFILING in
// mercury_conf_param.h.

// XXX Many of these macros would be more readable if they used array notation.
// For example, stack_pointer[offset] instead *(stack_pointer + offset),
// and &stack_pointer[offset] instead (stack_pointer + offset).

// XXX This should be made configurable when compiling a program.
#define     MR_REGION_NUM_PAGES_TO_REQUEST                  100
#define     MR_REGION_PAGE_SPACE_SIZE                       2047

// NOTE: The following constants *must match* the values of the Mercury
// compiler options with corresponding names (compiler/options.m). Otherwise,
// runtime errors likely happen.

#define     MR_REGION_ITE_FRAME_FIXED_SIZE                  4
#define     MR_REGION_DISJ_FRAME_FIXED_SIZE                 4
#define     MR_REGION_COMMIT_FRAME_FIXED_SIZE               5
#define     MR_REGION_ITE_PROT_SIZE                         1
#define     MR_REGION_ITE_SNAPSHOT_SIZE                     3
#define     MR_REGION_SEMI_DISJ_PROT_SIZE                   1
#define     MR_REGION_DISJ_SNAPSHOT_SIZE                    3
#define     MR_REGION_COMMIT_SAVE_SIZE                      1

#define     MR_REGION_DISJ_FRAME_TYPE       0
#define     MR_REGION_ITE_FRAME_TYPE        1

// A region page contains an array (of MR_Word) to store program data and
// a pointer to the next to form a single-linked-list.
// Note:
//  We use conversion macros to cast between MR_RegionPage and
//  MR_RegionHeader. We likely will need to update these macros when the
//  structure of MR_RegionPage changes.

struct MR_RegionPage_Struct {
    // Pointer to the next page to form the linked list.
    MR_RegionPage       *MR_regionpage_next;

    // The space to store program data.
    MR_Word             MR_regionpage_space[MR_REGION_PAGE_SPACE_SIZE];
};

// A region is implemented as a singly-linked list of region pages.
// All the information about the region itself is contained in the
// region's header, which is stored at the start of the region's first page.

struct MR_RegionHeader_Struct {
    // Pointer to the last page of the region, i.e. the newest or last added
    // page. This is needed when we need to enlarge the region.

    MR_RegionPage                       *MR_region_last_page;

    // Pointer to the next available word for allocation. Allocations into
    // a region always occur at its last page therefore this pointer always
    // points into the last page.

    MR_Word                             *MR_region_next_available_word;

    // XXX Currently not used. To be used for implementing conditional removals
    // of regions, i.e. the region is only actually removed when the counter
    // equals to one.

    unsigned int                        MR_region_removal_counter;

    // Sequence number of the region.
    MR_Word                             MR_region_sequence_number;

    // If the region has been removed in forward execution.
    // XXX Currently it is not used for anything, we just maintain it
    MR_Word                             MR_region_logical_removed;

    // NULL if the region is not protected by any if-then-else. Otherwise
    // it points to region_ite_stack frame that protects it.

    MR_RegionIteFixedFrame             *MR_region_ite_protected;

    // If the region is saved at this frame in the region_commit_stack.
    MR_RegionCommitFixedFrame          *MR_region_commit_frame;

    // True if the region is *new* to a commit (i.e. created after the entry
    // to the commit), has been removed logically in the commit and
    // therefore needs to be destroyed at commit.

    MR_Word                             MR_region_destroy_at_commit;

    MR_RegionHeader                     *MR_region_previous_region;
    MR_RegionHeader                     *MR_region_next_region;

#ifdef MR_RBMM_PROFILING
    // This is to count the number of words which are currently allocated into
    // the region. It means that it will be reset at backtracking if the region
    // is restored from its snapshot.

    int                                 MR_region_allocated_size;
#endif
};

////////////////////////////////////////////////////////////////////////////
// NOTE: The sizes of the next 7 data structures *must match* the Mercury
// compiler options defined for them in compiler/options.m and also the
// constants defined above, right in this file.
// So changes to these data structures, such as removing or adding fields, must
// be reflected in the other two places.
// Otherwise, runtime errors likely happen.

struct MR_RegionIteFixedFrame_Struct {
    MR_RegionIteFixedFrame          *MR_riff_previous_ite_frame;
    MR_Word                         MR_riff_saved_sequence_number;
    MR_Word                         MR_riff_num_prot_regions;
    MR_Word                         MR_riff_num_snapshots;
};

struct MR_RegionDisjFixedFrame_Struct {
    MR_RegionDisjFixedFrame         *MR_rdff_previous_disj_frame;
    MR_Word                         MR_rdff_saved_sequence_number;
    MR_Word                         MR_rdff_num_snapshots;
    MR_Word                         MR_rdff_num_prot_regions;
};

struct MR_RegionCommitFixedFrame_Struct {
    MR_RegionCommitFixedFrame       *MR_rcff_previous_commit_frame;
    MR_Word                         MR_rcff_saved_sequence_number;
    MR_RegionDisjFixedFrame         *MR_rcff_saved_disj_sp;
    MR_RegionIteFixedFrame          *MR_rcff_saved_ite_sp;
    MR_Word                         MR_rcff_num_saved_regions;
};

// To save the current size of a region in preparation for instant reclaiming.

struct MR_RegionSnapshot_Struct {
    MR_RegionHeader     *MR_snapshot_region;
    MR_RegionPage       *MR_snapshot_saved_last_page;
    MR_Word             *MR_snapshot_saved_next_available_word;
};

// Protection information in an ite frame.
struct MR_RegionIteProtect_Struct {
    MR_RegionHeader     *MR_ite_prot_region;
};

// Protection information in a disj frame.
struct MR_RegionSemiDisjProtect_Struct {
    MR_RegionHeader     *MR_semi_disj_prot_region;
};

// Save information in a commit frame.
struct MR_RegionCommitSave_Struct {
    MR_RegionHeader     *MR_commit_save_region;
};

////////////////////////////////////////////////////////////////////////////

// The region runtime maintains a list of free pages, when a program needs
// a page for a region the page is taken from the list.

extern MR_RegionPage    *MR_region_free_page_list;

// The live regions are linked in a list.
extern MR_RegionHeader  *MR_live_region_list;

extern MR_Word          MR_region_sequence_number;

// Pointers to the top frames of ite, disj, and commit stacks.
extern MR_RegionIteFixedFrame       *MR_region_ite_sp;
extern MR_RegionDisjFixedFrame      *MR_region_disj_sp;
extern MR_RegionCommitFixedFrame    *MR_region_commit_sp;

////////////////////////////////////////////////////////////////////////////

// Create a region.
extern  MR_RegionHeader *MR_region_create_region(void);

// Destroy a region, i.e. physically deallocate the region.
extern  void            MR_region_destroy_region(MR_RegionHeader *);

// Remove a region.
// If the region is not protected it is destroyed, otherwise it is only
// logically removed, i.e. we mark it as removed but not actually deallocate.

extern  void            MR_region_remove_region(MR_RegionHeader *);
extern  void            MR_remove_undisjprotected_region_ite_then_semidet(
                            MR_RegionHeader *);
extern  void            MR_remove_undisjprotected_region_ite_then_nondet(
                            MR_RegionHeader *);

// Allocate a number of words into a region.
extern  MR_Word         *MR_region_alloc(MR_RegionHeader *, unsigned int);

#define     MR_alloc_in_region(dest, region, num)                           \
            MR_tag_alloc_in_region(dest, 0, region, num)                    \

#define     MR_tag_alloc_in_region(dest, tag, region, num)                  \
            do {                                                            \
                (dest) = (MR_Word) MR_mkword((tag), (MR_Word)               \
                    MR_region_alloc((MR_RegionHeader *) (region), (num)));  \
            } while (0)

#define     MR_regionpage_to_region(region_page)                            \
            ( (MR_RegionHeader *) (region_page->MR_regionpage_space) )

// Minus one due to the MR_regionpage_next field
#define     MR_region_to_first_regionpage(region)                           \
            ( (MR_RegionPage *) ( ( (MR_Word *)region ) - 1 ) )

#define     MR_region_page_end(region_page)                                 \
            ((region_page)->MR_regionpage_space + MR_REGION_PAGE_SPACE_SIZE)

#define     MR_region_available_space(region_last_page, next_available_word)\
            (MR_region_page_end(region_last_page) - next_available_word)

extern  int             MR_region_is_disj_protected(MR_RegionHeader *region);

////////////////////////////////////////////////////////////////////////////
// push_region_frame

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_push_region_ite_frame(new_ite_sp)                            \
            do {                                                            \
                MR_RegionIteFixedFrame      *new_ite_frame;                 \
                                                                            \
                new_ite_frame = (MR_RegionIteFixedFrame *) (new_ite_sp);    \
                new_ite_frame->MR_riff_previous_ite_frame =                 \
                    MR_region_ite_sp;                                       \
                new_ite_frame->MR_riff_saved_sequence_number =              \
                    MR_region_sequence_number;                              \
                MR_region_ite_sp = new_ite_frame;                           \
                MR_region_profile_push_ite_frame;                           \
                MR_region_debug_push_ite_frame(new_ite_frame);              \
            } while (0)

  #define   MR_push_region_disj_frame(new_disj_sp)                          \
            do {                                                            \
                MR_RegionDisjFixedFrame     *new_disj_frame;                \
                                                                            \
                new_disj_frame = (MR_RegionDisjFixedFrame *) (new_disj_sp); \
                new_disj_frame->MR_rdff_previous_disj_frame =               \
                    MR_region_disj_sp;                                      \
                new_disj_frame->MR_rdff_saved_sequence_number =             \
                    MR_region_sequence_number;                              \
                new_disj_frame->MR_rdff_num_prot_regions = 0;               \
                MR_region_disj_sp = new_disj_frame;                         \
                MR_region_profile_push_disj_frame;                          \
                MR_region_debug_push_disj_frame(new_disj_frame);            \
            } while (0)

  #define   MR_push_region_commit_frame(new_commit_sp)                      \
            do {                                                            \
                MR_RegionCommitFixedFrame   *new_commit_frame;              \
                                                                            \
                new_commit_frame =                                          \
                    (MR_RegionCommitFixedFrame *) (new_commit_sp);          \
                new_commit_frame->MR_rcff_previous_commit_frame =           \
                    MR_region_commit_sp;                                    \
                new_commit_frame->MR_rcff_saved_sequence_number =           \
                    MR_region_sequence_number;                              \
                new_commit_frame->MR_rcff_saved_disj_sp = MR_region_disj_sp;\
                new_commit_frame->MR_rcff_saved_ite_sp = MR_region_ite_sp;  \
                MR_region_commit_sp = new_commit_frame;                     \
                MR_region_profile_push_commit_frame;                        \
                MR_region_debug_push_commit_frame(new_commit_frame);        \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_push_region_ite_frame(new_ite_sp)                            \
            do {                                                            \
                MR_push_region_ite_frame_proc(                              \
                        (MR_RegionIteFixedFrame *) (new_ite_sp) );          \
            } while (0)

  #define   MR_push_region_disj_frame(new_disj_sp)                          \
            do {                                                            \
                MR_push_region_disj_frame_proc(                             \
                        (MR_RegionDisjFixedFrame *) (new_disj_sp) );        \
            } while (0)

  #define   MR_push_region_commit_frame(new_commit_sp)                      \
            do {                                                            \
                MR_push_region_commit_frame_proc(                           \
                    (MR_RegionCommitFixedFrame *) (new_commit_sp) );        \
            } while (0)

    extern  void    MR_push_region_ite_frame_proc(
                        MR_RegionIteFixedFrame *new_ite_frame);

    extern  void    MR_push_region_disj_frame_proc(
                        MR_RegionDisjFixedFrame *new_disj_frame);

    extern  void    MR_push_region_commit_frame_proc(
                        MR_RegionCommitFixedFrame *new_commit_frame);

#endif      // MR_RBMM_USE_MACROS

////////////////////////////////////////////////////////////////////////////
// region_fill_frame

// Save the region if it satisfies:
// (a) live before condition
// (c2) aren't already protected (ite_protected or disj_protected).
// If save the region, then set the ite_protected field in these regions
// to point to the frame.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_fill_ite_protect(ite_sp, region_ptr,                  \
                num_protected_regions, region_slot_reg)                     \
            do {                                                            \
                MR_RegionHeader         *region;                            \
                MR_RegionIteProtect     *ite_prot;                          \
                                                                            \
                MR_region_debug_start("fill_ite_protect");                  \
                region = (MR_RegionHeader *) (region_ptr);                  \
                if (!MR_region_is_disj_protected(region) &&                 \
                    region->MR_region_ite_protected == NULL)                \
                {                                                           \
                    ite_prot = (MR_RegionIteProtect *) (region_slot_reg);   \
                    ite_prot->MR_ite_prot_region = region;                  \
                    (num_protected_regions)++;                              \
                    (region_slot_reg) = (MR_Word) (ite_prot + 1);           \
                    region->MR_region_ite_protected =                       \
                        (MR_RegionIteFixedFrame *) (ite_sp);                \
                    MR_region_profile_fill_ite_protect;                     \
                    MR_region_debug_fill_ite_protect(ite_prot, region);     \
                } else {                                                    \
                    MR_region_debug_fill_ite_protect(NULL, region);         \
                }                                                           \
                MR_region_debug_end("fill_ite_protect");                    \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_fill_ite_protect(ite_sp, region_ptr,                  \
                num_protected_regions, region_slot_reg)                     \
            do {                                                            \
                int                     incr;                               \
                MR_RegionIteProtect     *ite_prot;                          \
                                                                            \
                ite_prot = (MR_RegionIteProtect *) (region_slot_reg);       \
                incr = MR_region_fill_ite_protect_func(                     \
                    (MR_RegionIteFixedFrame *) (ite_sp),                    \
                    (MR_RegionHeader *) (region_ptr),                       \
                    ite_prot);                                              \
                (num_protected_regions) += incr;                            \
                ite_prot += incr;                                           \
                (region_slot_reg) = ( (MR_Word) ite_prot );                 \
            } while (0)

  extern    int     MR_region_fill_ite_protect_func(
                        MR_RegionIteFixedFrame *ite_sp,
                        MR_RegionHeader *region,
                        MR_RegionIteProtect *ite_prot);

#endif      // MR_RBMM_USE_MACROS

// This is to prepare for instant reclaiming at the start of else. For instant
// reclaiming a region we save its current size by taking a snapshot of it. The
// natural question would be for which regions. The very first criterion is
// whether a region will be destroyed right at the start of the else. It is
// because we need not to reclaim memory for those which will be destroyed
// anyway right after that. To decide if a region will be destroyed at the
// start of the else we need information at both compile-time and runtime. That
// is at compile-time we only know if the region is removed or not, and at
// runtime we will know if the region is protected from being destroyed. So,
// 1. Those that are removed and protected need to be saved.
// 2. Those that are not removed (so not destroyed) will need to be saved.

// XXX ite_sp is not used here.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_fill_ite_snapshot_removed(ite_sp, region_ptr,         \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_RegionHeader         *region;                            \
                MR_RegionSnapshot       *snapshot;                          \
                                                                            \
                MR_region_debug_start("fill_ite_snapshot_removed");         \
                region = (MR_RegionHeader *) (region_ptr);                  \
                snapshot = (MR_RegionSnapshot *) (snapshot_block);          \
                if ((region->MR_region_ite_protected != NULL &&             \
                     region->MR_region_ite_protected != MR_region_ite_sp    \
                    ) || MR_region_is_disj_protected(region))               \
                {                                                           \
                    MR_save_snapshot(region, snapshot);                     \
                    MR_region_profile_fill_ite_snapshot;                    \
                    (snapshot_block) = (MR_Word) (snapshot + 1);            \
                    (num_snapshots)++;                                      \
                    MR_region_debug_fill_ite_snapshot_removed(snapshot,     \
                        region);                                            \
                } else {                                                    \
                    // Else the region is not protected.
                    MR_region_debug_fill_ite_snapshot_removed(NULL, region);\
                }                                                           \
                MR_region_debug_end("fill_ite_snapshot_removed");           \
            } while (0)

#else   // MR_RBMM_USE_MACROS

  #define   MR_region_fill_ite_snapshot_removed(ite_sp, region_ptr,         \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                int                     incr;                               \
                MR_RegionSnapshot       *snapshot;                          \
                                                                            \
                snapshot = (MR_RegionSnapshot *) (snapshot_block);          \
                incr = MR_region_fill_ite_snapshot_removed_func(            \
                        (MR_RegionHeader *) (region_ptr),                   \
                        snapshot);                                          \
                (num_snapshots) += incr;                                    \
                snapshot += incr;                                           \
                (snapshot_block) = ( (MR_Word) snapshot );                  \
            } while (0)

    extern  int     MR_region_fill_ite_snapshot_removed_func(
                        MR_RegionHeader         *region,
                        MR_RegionSnapshot       *snapshot);

#endif  // MR_RBMM_USE_MACROS

// XXX: This can be made more efficient because we will save them all.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_fill_ite_snapshot_not_removed(ite_sp, region_ptr,     \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_RegionHeader         *region;                            \
                MR_RegionSnapshot       *snapshot;                          \
                                                                            \
                MR_region_debug_start("fill_ite_snapshot_not_removed");     \
                region = (MR_RegionHeader *) (region_ptr);                  \
                snapshot = (MR_RegionSnapshot *) (snapshot_block);          \
                MR_save_snapshot(region, snapshot);                         \
                MR_region_profile_fill_ite_snapshot;                        \
                (snapshot_block) = (MR_Word) (snapshot + 1);                \
                (num_snapshots)++;                                          \
                MR_region_debug_fill_ite_snapshot_not_removed(snapshot,     \
                    region);                                                \
                MR_region_debug_end("fill_ite_snapshot_not_removed");       \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_fill_ite_snapshot_not_removed(ite_sp, region_ptr,     \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_RegionSnapshot       *snapshot;                          \
                                                                            \
                snapshot = (MR_RegionSnapshot *) (snapshot_block);          \
                MR_region_fill_ite_snapshot_not_removed_proc(               \
                        (MR_RegionHeader *) (region_ptr),                   \
                        snapshot);                                          \
                (snapshot_block) = (MR_Word) (snapshot + 1);                \
                (num_snapshots)++;                                          \
            } while (0)

    extern  void    MR_region_fill_ite_snapshot_not_removed_proc(
                        MR_RegionHeader *region,
                        MR_RegionSnapshot *snapshot);

#endif      // MR_RBMM_USE_MACROS

// XXX disj_sp is actually not needed here.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_fill_semi_disj_protect(disj_sp, region_ptr,           \
                num_protected_regions, protection_block)                    \
            do {                                                            \
                MR_RegionHeader             *region;                        \
                MR_RegionSemiDisjProtect    *semi_disj_prot;                \
                                                                            \
                MR_region_debug_start("fill_semi_disj_protect");            \
                region = (MR_RegionHeader *) (region_ptr);                  \
                if (!MR_region_is_disj_protected(region) &&                 \
                    region->MR_region_ite_protected == NULL)                \
                {                                                           \
                    semi_disj_prot =                                        \
                        (MR_RegionSemiDisjProtect *) (protection_block);    \
                    semi_disj_prot->MR_semi_disj_prot_region = region;      \
                    (num_protected_regions)++;                              \
                    (protection_block) = (MR_Word) (semi_disj_prot + 1);    \
                    MR_region_profile_fill_semi_disj_protect;               \
                    MR_region_debug_fill_semi_disj_protect(semi_disj_prot,  \
                        region);                                            \
                } else {                                                    \
                    MR_region_debug_fill_semi_disj_protect(NULL, region);   \
                }                                                           \
                MR_region_debug_end("fill_ite_protect");                    \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_fill_semi_disj_protect(disj_sp, region_ptr,           \
                num_protected_regions, protection_block)                    \
            do {                                                            \
                MR_RegionHeader             *region;                        \
                MR_RegionSemiDisjProtect    *semi_disj_prot;                \
                int                         incr;                           \
                                                                            \
                semi_disj_prot =                                            \
                    (MR_RegionSemiDisjProtect *) (protection_block);        \
                incr = MR_region_fill_semi_disj_protect_func(               \
                        (MR_RegionHeader *) (region_ptr),                   \
                        semi_disj_prot);                                    \
                (num_protected_regions) += incr;                            \
                semi_disj_prot += incr;                                     \
                (protection_block) = ( (MR_Word)semi_disj_prot );           \
            } while (0)

    extern  int     MR_region_fill_semi_disj_protect_func(
                        MR_RegionHeader *region,
                        MR_RegionSemiDisjProtect *semi_disj_prot);

#endif      // MR_RBMM_USE_MACROS

// XXX: This can be made more efficient because we will save them all
// XXX: disj_sp is actually not needed.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_fill_disj_snapshot(disj_sp, region_ptr,               \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_RegionHeader         *region;                            \
                MR_RegionSnapshot       *snapshot;                          \
                                                                            \
                MR_region_debug_start("fill_disj_snapshot");                \
                region = (MR_RegionHeader *) (region_ptr);                  \
                snapshot = (MR_RegionSnapshot *) (snapshot_block);          \
                MR_save_snapshot(region, snapshot);                         \
                MR_region_profile_fill_disj_snapshot;                       \
                (snapshot_block) = (MR_Word) (snapshot + 1);                \
                (num_snapshots)++;                                          \
                MR_region_debug_fill_disj_snapshot(snapshot, region);       \
                MR_region_debug_end("fill_disj_snapshot");                  \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_fill_disj_snapshot(disj_sp, region_ptr,               \
                num_snapshots, snapshot_block)                              \
            do {                                                            \
                MR_RegionSnapshot       *snapshot;                          \
                                                                            \
                snapshot = (MR_RegionSnapshot *) (snapshot_block);          \
                MR_region_fill_disj_snapshot_proc(                          \
                        (MR_RegionHeader *) (region_ptr),                   \
                        snapshot);                                          \
                (snapshot_block) = (MR_Word) (snapshot + 1);                \
                (num_snapshots)++;                                          \
            } while (0)

    extern  void    MR_region_fill_disj_snapshot_proc(
                        MR_RegionHeader *region,
                        MR_RegionSnapshot *snapshot);

#endif      // MR_RBMM_USE_MACROS

// Save the live and unprotected regions which are input to the commit goal
// into the top commit stack frame.
// Set the commit_frame field in these regions to point to the frame.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_fill_commit(commit_sp, region_ptr,                    \
                num_saved_region_reg, region_slot_reg)                      \
            do {                                                            \
                MR_RegionHeader         *region;                            \
                MR_RegionCommitSave     *commit_save;                       \
                                                                            \
                MR_region_debug_start("fill_commit");                       \
                region = (MR_RegionHeader *) (region_ptr);                  \
                if (!MR_region_is_disj_protected(region) &&                 \
                    region->MR_region_ite_protected == NULL)                \
                {                                                           \
                    commit_save = (MR_RegionCommitSave *) (region_slot_reg);\
                    commit_save->MR_commit_save_region = region;            \
                    (num_saved_region_reg)++;                               \
                    (region_slot_reg) = (MR_Word) (commit_save + 1);        \
                    region->MR_region_commit_frame =                        \
                        (MR_RegionCommitFixedFrame *) (commit_sp);          \
                    MR_region_profile_fill_commit;                          \
                    MR_region_debug_fill_commit(commit_save, region);       \
                } else {                                                    \
                    MR_region_debug_fill_commit(NULL, region);              \
                }                                                           \
                MR_region_debug_end("fill_commit");                         \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_fill_commit(commit_sp, region_ptr,                    \
                num_saved_region_reg, region_slot_reg)                      \
            do {                                                            \
                int                     incr;                               \
                MR_RegionCommitSave     *commit_save;                       \
                                                                            \
                commit_save = (MR_RegionCommitSave *) (region_slot_reg);    \
                incr = MR_region_fill_commit_func(                          \
                        (MR_RegionCommitFixedFrame *) (commit_sp),          \
                        (MR_RegionHeader *) (region_ptr),                   \
                        commit_save);                                       \
                (num_saved_region_reg) += incr;                             \
                commit_save += incr;                                        \
                (region_slot_reg) = ( (MR_Word) commit_save );              \
            } while (0)

    extern  int     MR_region_fill_commit_func(
                            MR_RegionCommitFixedFrame *commit_sp,
                            MR_RegionHeader *region,
                            MR_RegionCommitSave *commit_save);

#endif      // MR_RBMM_USE_MACROS

////////////////////////////////////////////////////////////////////////////
// region_set_fixed_slot

#define     MR_region_set_ite_num_protects(ite_sp, num)                     \
            do {                                                            \
                MR_RegionIteFixedFrame      *top_ite_frame;                 \
                                                                            \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                top_ite_frame->MR_riff_num_prot_regions = (num);            \
            } while (0)

#define     MR_region_set_ite_num_snapshots(ite_sp, num)                    \
            do {                                                            \
                MR_RegionIteFixedFrame      *top_ite_frame;                 \
                                                                            \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                top_ite_frame->MR_riff_num_snapshots = (num);               \
            } while (0)

#define     MR_region_set_disj_num_protects(disj_sp, num)                   \
            do {                                                            \
                MR_RegionDisjFixedFrame     *top_disj_frame;                \
                                                                            \
                top_disj_frame = (MR_RegionDisjFixedFrame *) (disj_sp);     \
                top_disj_frame->MR_rdff_num_prot_regions = (num);           \
            } while (0)

#define     MR_region_set_disj_num_snapshots(disj_sp, num)                  \
            do {                                                            \
                MR_RegionDisjFixedFrame     *top_disj_frame;                \
                                                                            \
                top_disj_frame = (MR_RegionDisjFixedFrame *) (disj_sp);     \
                top_disj_frame->MR_rdff_num_snapshots = (num);              \
            } while (0)

#define     MR_region_set_commit_num_entries(commit_sp, num)                \
            do {                                                            \
                MR_RegionCommitFixedFrame   *top_commit_frame;              \
                                                                            \
                top_commit_frame =                                          \
                    (MR_RegionCommitFixedFrame *) (commit_sp);              \
                top_commit_frame->MR_rcff_num_saved_regions = (num);        \
            } while (0)

////////////////////////////////////////////////////////////////////////////
// use_and_maybe_pop_region_frame

// The next two macros are to remove each protected region at the start of the
// then part. If the condition is semidet we just need to destroy all the
// protected regions (whose are not disj-protected).
// If the condition is nondet we have to do 2 more things:
//  + 1. check if a protected region has already been destroyed
//  + 2. if we destroy a protected region, we have to nullify its
//  corresponding entry in the ite frame.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_ite_then_semidet(ite_sp)                          \
            do {                                                            \
                MR_RegionIteFixedFrame      *top_ite_frame;                 \
                MR_RegionIteProtect         *ite_prot;                      \
                int                         i;                              \
                                                                            \
                MR_region_debug_start("use_region_ite_then_semidet");       \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                ite_prot = (MR_RegionIteProtect *) ( (ite_sp) +             \
                    MR_REGION_ITE_FRAME_FIXED_SIZE);                        \
                for (i = 0; i < top_ite_frame->MR_riff_num_prot_regions;    \
                        i++, ite_prot++) {                                  \
                    MR_remove_undisjprotected_region_ite_then_semidet(      \
                        ite_prot->MR_ite_prot_region);                      \
                }                                                           \
                MR_pop_region_ite_frame(top_ite_frame);                     \
                MR_region_debug_end("use_region_ite_then_semidet");         \
            } while (0)

  #define   MR_use_region_ite_then_nondet(ite_sp)                           \
            do {                                                            \
                MR_RegionIteFixedFrame      *top_ite_frame;                 \
                MR_RegionIteProtect         *ite_prot;                      \
                int                         i;                              \
                                                                            \
                MR_region_debug_start("use_region_ite_then_nondet");        \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                ite_prot = (MR_RegionIteProtect *) ( (ite_sp) +             \
                    MR_REGION_ITE_FRAME_FIXED_SIZE);                        \
                for (i = 0; i < top_ite_frame->MR_riff_num_prot_regions;    \
                        i++, ite_prot++) {                                  \
                    if (ite_prot->MR_ite_prot_region != NULL) {             \
                        MR_remove_undisjprotected_region_ite_then_nondet(   \
                            ite_prot->MR_ite_prot_region);                  \
                    }                                                       \
                }                                                           \
                MR_region_debug_end("use_region_ite_then_nondet");          \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_ite_then_semidet(ite_sp)                          \
            do {                                                            \
                MR_use_region_ite_then_semidet_proc(                        \
                        (MR_RegionIteFixedFrame *) (ite_sp));               \
            } while (0)

    extern  void    MR_use_region_ite_then_semidet_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

  #define   MR_use_region_ite_then_nondet(ite_sp)                           \
            do {                                                            \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                MR_use_region_ite_then_nondet_proc(                         \
                        (MR_RegionIteFixedFrame *) (ite_sp));               \
            } while (0)

    extern  void    MR_use_region_ite_then_nondet_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_ite_else_semidet(ite_sp)                          \
            do {                                                            \
                MR_RegionIteFixedFrame      *top_ite_frame;                 \
                                                                            \
                MR_region_debug_start("use_region_ite_else_semidet");       \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                MR_region_process_at_ite_else(top_ite_frame);               \
                MR_region_debug_end("use_region_ite_else_semidet");         \
            } while (0)

  #define   MR_use_region_ite_else_nondet(ite_sp)                           \
            do {                                                            \
                MR_RegionIteFixedFrame      *top_ite_frame;                 \
                                                                            \
                MR_region_debug_start("use_region_ite_else_nondet");        \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                MR_region_process_at_ite_else(top_ite_frame);               \
                MR_region_debug_end("use_region_ite_else_nondet");          \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_ite_else_semidet(ite_sp)                          \
            do {                                                            \
                MR_use_region_ite_else_semidet_proc(                        \
                        (MR_RegionIteFixedFrame *) (ite_sp));               \
            } while (0)

    extern  void    MR_use_region_ite_else_semidet_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

  #define   MR_use_region_ite_else_nondet(ite_sp)                           \
            do {                                                            \
                MR_use_region_ite_else_nondet_proc(                         \
                        (MR_RegionIteFixedFrame *) (ite_sp));               \
            } while (0)

    extern  void    MR_use_region_ite_else_nondet_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

#endif      // MR_RBMM_USE_MACROS

// This is to handle the post-success failure of the condition in an
// if-then-else with nondet condition.
// Because the execution will resume at the alternative after the
// if-then-else:
// 1. any ite-protected regions here are new regions w.r.t the alternative
// because old regions w.r.t the alternative is disj-protected and will not
// be ite-protected here;
// 2. any new regions here are certainly also new regions to
// the alternative;
// 3. new allocations (into regions) here may be into old regions
// or into new regions to the alternative (but old to the if-then-else).
// (1) and (2) will be destroyed anyway at the beginning of the alternative.
// So we do not do anything to them here.
// For (3) we may want to reclaim only the new allocations into old regions to
// the alternative, not into the new ones because they will be destroyed
// anyway.
// XXX But the implementation below just reclaims them all. That is
// suboptimal but the cost seems negligible.
// After that the frame is discarded.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_ite_nondet_cond_fail(ite_sp)                      \
            do {                                                            \
                MR_RegionIteFixedFrame      *top_ite_frame;                 \
                                                                            \
                MR_region_debug_start("use_region_ite_nondet_cond_fail");   \
                top_ite_frame = (MR_RegionIteFixedFrame *) (ite_sp);        \
                MR_region_ite_restore_from_snapshots(top_ite_frame);        \
                MR_pop_region_ite_frame(top_ite_frame);                     \
                MR_region_debug_end("use_region_ite_nondet_cond_fail");     \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_ite_nondet_cond_fail(ite_sp)                      \
            do {                                                            \
                MR_use_region_ite_nondet_cond_fail(                         \
                        (MR_RegionIteFixedFrame *) (ite_sp));               \
            } while (0)

    extern  void    MR_use_region_ite_nondet_cond_fail_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_disj_later(disj_sp)                               \
            do {                                                            \
                MR_RegionDisjFixedFrame     *top_disj_frame;                \
                                                                            \
                MR_region_debug_start("use_region_disj_later");             \
                top_disj_frame = (MR_RegionDisjFixedFrame *) (disj_sp);     \
                MR_region_disj_restore_from_snapshots(top_disj_frame);      \
                MR_region_disj_destroy_new_regions(top_disj_frame);         \
                MR_region_debug_end("use_region_disj_later");               \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_disj_later(disj_sp)                               \
            do {                                                            \
                MR_use_region_disj_later_proc(                              \
                        (MR_RegionDisjFixedFrame *) (disj_sp));             \
            } while (0)

    extern  void    MR_use_region_disj_later_proc(
                        MR_RegionDisjFixedFrame *top_disj_frame);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_disj_nonlast_semi_commit(disj_sp)                 \
            do {                                                            \
                MR_RegionDisjFixedFrame     *top_disj_frame;                \
                MR_RegionSemiDisjProtect    *semi_disj_prot;                \
                int                         i;                              \
                                                                            \
                MR_region_debug_start(                                      \
                    "use_region_disj_nonlast_semi_commit");                 \
                top_disj_frame = (MR_RegionDisjFixedFrame *) (disj_sp);     \
                /* Destroy any regions protected by the disj frame. */      \
                semi_disj_prot = (MR_RegionSemiDisjProtect *) ( (disj_sp) + \
                    MR_REGION_DISJ_FRAME_FIXED_SIZE);                       \
                for (i = 0; i < top_disj_frame->MR_rdff_num_prot_regions;   \
                        i++, semi_disj_prot++) {                            \
                        MR_region_destroy_region(                           \
                            semi_disj_prot->MR_semi_disj_prot_region);      \
                }                                                           \
                MR_pop_region_disj_frame(top_disj_frame);                   \
                MR_region_debug_end("use_region_disj_nonlast_semi_commit"); \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_disj_nonlast_semi_commit(disj_sp)                 \
            do {                                                            \
                MR_RegionDisjFixedFrame     *top_disj_frame;                \
                MR_RegionSemiDisjProtect    *semi_disj_prot;                \
                int                         i;                              \
                                                                            \
                MR_use_region_disj_nonlast_semi_commit_proc(                \
                        (MR_RegionDisjFixedFrame *) (disj_sp));             \
            } while (0)

    extern  void    MR_use_region_disj_nonlast_semi_commit_proc(
                        MR_RegionDisjFixedFrame *top_disj_frame);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_disj_last(disj_sp)                                \
            do {                                                            \
                MR_RegionDisjFixedFrame     *top_disj_frame;                \
                                                                            \
                MR_region_debug_start("use_region_disj_last");              \
                top_disj_frame = (MR_RegionDisjFixedFrame *) (disj_sp);     \
                MR_region_disj_restore_from_snapshots(top_disj_frame);      \
                MR_region_disj_destroy_new_regions(top_disj_frame);         \
                MR_pop_region_disj_frame(top_disj_frame);                   \
                MR_region_debug_end("use_region_disj_last");                \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_disj_last(disj_sp)                                \
            do {                                                            \
                MR_use_region_disj_last_proc(                               \
                        (MR_RegionDisjFixedFrame *) (disj_sp) );            \
            } while (0)

    extern  void    MR_use_region_disj_last_proc(
                        MR_RegionDisjFixedFrame *top_disj_frame);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_commit_success(commit_sp)                         \
            do {                                                            \
                MR_RegionCommitFixedFrame       *top_commit_frame;          \
                MR_RegionCommitSave             *first_commit_save;         \
                                                                            \
                MR_region_debug_start("use_region_commit_success");         \
                top_commit_frame =                                          \
                    (MR_RegionCommitFixedFrame *) (commit_sp);              \
                first_commit_save = (MR_RegionCommitSave *) (               \
                    (commit_sp) + MR_REGION_COMMIT_FRAME_FIXED_SIZE);       \
                MR_commit_success_destroy_marked_new_regions(               \
                    top_commit_frame->MR_rcff_saved_sequence_number);       \
                MR_commit_success_destroy_marked_saved_regions(             \
                    top_commit_frame->MR_rcff_num_saved_regions,            \
                    first_commit_save);                                     \
                MR_region_profile_pop_disj_frame(MR_region_disj_sp,         \
                    top_commit_frame->MR_rcff_saved_disj_sp);               \
                MR_region_disj_sp = top_commit_frame->MR_rcff_saved_disj_sp;\
                MR_region_profile_pop_ite_frame(MR_region_ite_sp,           \
                    top_commit_frame->MR_rcff_saved_ite_sp);                \
                MR_region_ite_sp = top_commit_frame->MR_rcff_saved_ite_sp;  \
                MR_pop_region_commit_frame(top_commit_frame);               \
                MR_region_debug_end("use_region_commit_success");           \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_commit_success(commit_sp)                         \
            do {                                                            \
                MR_use_region_commit_success_proc(                          \
                    (MR_RegionCommitFixedFrame *) (commit_sp) );            \
            } while (0)

    extern  void    MR_use_region_commit_success_proc(
                        MR_RegionCommitFixedFrame *top_commit_frame);

#endif      // MR_RBMM_USE_MACROS

// Commit failure means that the goal in the commit operation has failed.
// We reset the commit_frame field of the saved regions at the commit frame to
// NULL. Then the top commit frame is popped.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_use_region_commit_failure(commit_sp)                         \
            do {                                                            \
                MR_RegionCommitFixedFrame       *top_commit_frame;          \
                MR_RegionCommitSave             *commit_save;               \
                MR_RegionHeader                 *region;                    \
                int                             i;                          \
                                                                            \
                MR_region_debug_start("use_region_commit_failure");         \
                top_commit_frame =                                          \
                    (MR_RegionCommitFixedFrame *) (commit_sp);              \
                commit_save = (MR_RegionCommitSave *) ( (commit_sp) +       \
                    MR_REGION_COMMIT_FRAME_FIXED_SIZE);                     \
                for (i = 0; i < top_commit_frame->MR_rcff_num_saved_regions;\
                        i++, commit_save++) {                               \
                    region = commit_save->MR_commit_save_region;            \
                    if (region != NULL) {                                   \
                        region->MR_region_commit_frame = NULL;              \
                    }                                                       \
                }                                                           \
                MR_pop_region_commit_frame(top_commit_frame);               \
                MR_region_debug_end("use_region_commit_failure");           \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_use_region_commit_failure(commit_sp)                         \
            do {                                                            \
                MR_use_region_commit_failure_proc(                          \
                        (MR_RegionCommitFixedFrame *) (commit_sp) );        \
            } while (0)

    extern  void    MR_use_region_commit_failure_proc(
                        MR_RegionCommitFixedFrame *top_commit_frame);

#endif      // MR_RBMM_USE_MACROS

extern  void    MR_commit_success_destroy_marked_saved_regions(
                    MR_Word number_of_saved_regions,
                    MR_RegionCommitSave *first_commit_save);

extern  void    MR_commit_success_destroy_marked_new_regions(
                    MR_Word saved_region_seq_number);

////////////////////////////////////////////////////////////////////////////

#define     MR_pop_region_ite_frame(top_ite_frame)                          \
            do {                                                            \
                MR_region_ite_sp =                                          \
                    top_ite_frame->MR_riff_previous_ite_frame;              \
                MR_region_profile_pop_ite_frame(top_ite_frame);             \
            } while (0)

#define     MR_pop_region_disj_frame(top_disj_frame)                        \
            do {                                                            \
                MR_region_disj_sp =                                         \
                    top_disj_frame->MR_rdff_previous_disj_frame;            \
                MR_region_profile_pop_disj_frame(top_disj_frame,            \
                    top_disj_frame->MR_rdff_previous_disj_frame);           \
            } while (0)

#define     MR_pop_region_commit_frame(top_commit_frame)                    \
            do {                                                            \
                MR_region_commit_sp =                                       \
                    top_commit_frame->MR_rcff_previous_commit_frame;        \
                MR_region_profile_pop_commit_frame(top_commit_frame);       \
            } while (0)

////////////////////////////////////////////////////////////////////////////
// Helpers for ite support.

// At the start of else, we
// 1. unprotect the protected regions,
// 2. instant reclaiming using snapshots,
// 3. instant reclaiming by destroying new regions created in the condition,
// 4. pop the current ite frame.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_process_at_ite_else(top_ite_frame)                    \
            do {                                                            \
                MR_region_ite_unprotect(top_ite_frame);                     \
                MR_region_ite_restore_from_snapshots(top_ite_frame);        \
                MR_region_ite_destroy_new_regions(top_ite_frame);           \
                MR_pop_region_ite_frame(top_ite_frame);                     \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_process_at_ite_else(top_ite_frame)                    \
            do {                                                            \
                MR_region_process_at_ite_else_proc(top_ite_frame);          \
            } while (0)

    extern  void    MR_region_process_at_ite_else_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

#endif      // MR_RBMM_USE_MACROS

// Unprotect the protected regions at the beginning of the else part.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_ite_unprotect(top_ite_frame)                          \
            do {                                                            \
                MR_RegionIteProtect     *ite_prot;                          \
                MR_RegionHeader         *protected_region;                  \
                int                     i;                                  \
                                                                            \
                MR_region_debug_start("ite_unprotect");                     \
                ite_prot = (MR_RegionIteProtect *) (                        \
                    ( (MR_Word *) (top_ite_frame) ) +                       \
                    MR_REGION_ITE_FRAME_FIXED_SIZE);                        \
                for (i = 0; i < top_ite_frame->MR_riff_num_prot_regions;    \
                        i++, ite_prot++) {                                  \
                    protected_region = ite_prot->MR_ite_prot_region;        \
                    MR_region_debug_ite_unprotect(protected_region);        \
                    /* Try to protect the region by an outer condition. */  \
                    protected_region->MR_region_ite_protected = NULL;       \
                }                                                           \
                MR_region_debug_end("ite_unprotect");                       \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_ite_unprotect(top_ite_frame)                          \
            do {                                                            \
                MR_region_ite_unprotect_proc(top_ite_frame);                \
            } while (0)

    extern  void    MR_region_ite_unprotect_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_ite_restore_from_snapshots(top_ite_frame)             \
            do {                                                            \
                MR_RegionSnapshot       *first_snapshot;                    \
                MR_Word                 protection_size;                    \
                                                                            \
                MR_region_debug_start("ite_restore_from_snapshot");         \
                protection_size = top_ite_frame->MR_riff_num_prot_regions * \
                    MR_REGION_ITE_PROT_SIZE;                                \
                first_snapshot = (MR_RegionSnapshot *) (                    \
                    ( (MR_Word *) (top_ite_frame) ) +                       \
                    MR_REGION_ITE_FRAME_FIXED_SIZE + protection_size);      \
                MR_restore_snapshots(top_ite_frame->MR_riff_num_snapshots,  \
                    first_snapshot);                                        \
                MR_region_debug_end("ite_restore_from_snapshot");           \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_ite_restore_from_snapshots(top_ite_frame)             \
            do {                                                            \
                MR_region_ite_restore_from_snapshots_proc(top_ite_frame);   \
            } while (0)

    extern  void    MR_region_ite_restore_from_snapshots_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_ite_destroy_new_regions(top_ite_frame)                \
            do {                                                            \
                MR_region_debug_start("ite_destroy_new_regions");           \
                MR_region_frame_destroy_new_regions(                        \
                    top_ite_frame->MR_riff_saved_sequence_number,           \
                    MR_REGION_ITE_FRAME_TYPE);                              \
                MR_region_debug_end("ite_destroy_new_regions");             \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_ite_destroy_new_regions(top_ite_frame)                \
            do {                                                            \
                MR_region_ite_destroy_new_regions_proc(top_ite_frame);      \
            } while (0)

    extern  void    MR_region_ite_destroy_new_regions_proc(
                        MR_RegionIteFixedFrame *top_ite_frame);

#endif      // MR_RBMM_USE_MACROS

////////////////////////////////////////////////////////////////////////////
// Helpers for nondet disjunction support.

// At any non-first disjunct, try instant reclaiming from snapshots.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_disj_restore_from_snapshots(top_disj_frame)           \
            do {                                                            \
                MR_RegionSnapshot       *first_snapshot;                    \
                                                                            \
                MR_region_debug_start("disj_restore_from_snapshots");       \
                first_snapshot = (MR_RegionSnapshot *) (                    \
                    (MR_Word *) (top_disj_frame) +                          \
                    MR_REGION_DISJ_FRAME_FIXED_SIZE +                       \
                    MR_REGION_SEMI_DISJ_PROT_SIZE *                         \
                        top_disj_frame->MR_rdff_num_prot_regions);          \
                MR_restore_snapshots(top_disj_frame->MR_rdff_num_snapshots, \
                    first_snapshot);                                        \
                MR_region_debug_end("disj_restore_from_snapshots");         \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_disj_restore_from_snapshots(top_disj_frame)           \
            do {                                                            \
                MR_region_disj_restore_from_snapshots_proc(top_disj_frame); \
            } while (0)

    extern  void    MR_region_disj_restore_from_snapshots_proc(
                        MR_RegionDisjFixedFrame *top_disj_frame);

#endif      // MR_RBMM_USE_MACROS

// At any non-first disjunct, try instant reclaiming by destroying new
// regions.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_disj_destroy_new_regions(top_disj_frame)              \
            do {                                                            \
                MR_region_debug_start("disj_destroy_new_regions");          \
                MR_region_frame_destroy_new_regions(                        \
                    top_disj_frame->MR_rdff_saved_sequence_number,          \
                    MR_REGION_DISJ_FRAME_TYPE);                             \
                MR_region_debug_end("disj_destroy_new_regions");            \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_disj_destroy_new_regions(top_disj_frame)              \
            do {                                                            \
                MR_region_disj_destroy_new_regions_proc(top_disj_frame);    \
            } while (0)

    extern  void    MR_region_disj_destroy_new_regions_proc(
                        MR_RegionDisjFixedFrame *top_disj_frame);

#endif      // MR_RBMM_USE_MACROS

////////////////////////////////////////////////////////////////////////////

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_save_snapshot(region, snapshot)                              \
            do {                                                            \
                (snapshot)->MR_snapshot_region = (region);                  \
                (snapshot)->MR_snapshot_saved_last_page =                   \
                    (region)->MR_region_last_page;                          \
                (snapshot)->MR_snapshot_saved_next_available_word =         \
                    (region)->MR_region_next_available_word;                \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_save_snapshot(region, snapshot)                              \
            do {                                                            \
                MR_save_snapshot_proc(region,snapshot);                     \
            } while (0)

    extern  void    MR_save_snapshot_proc(MR_RegionHeader *region,
                        MR_RegionSnapshot *snapshot);

#endif      // MR_RBMM_USE_MACROS

// XXX For profiling:
// One correct way to reset the allocated_size is to save it in the snapshot
// so that here we have the old value right away. But having an extra slot
// in the disj frame causes changes at other places. From the snapshot
// information (as it is now) we can only compute the old value correctly if
// there is no wasteful space at the end of the region's pages. Therefore the
// allocated_size sometimes is not reliable.

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_restore_snapshots(num_snapshots, first_snapshot)             \
            do {                                                            \
                MR_RegionSnapshot   *snapshot;                              \
                MR_RegionHeader     *restoring_region;                      \
                MR_RegionPage       *saved_last_page;                       \
                MR_RegionPage       *first_new_page;                        \
                int                 i;                                      \
                                                                            \
                snapshot = first_snapshot;                                  \
                for (i = 0; i < (num_snapshots); i++, snapshot++) {         \
                    restoring_region = snapshot->MR_snapshot_region;        \
                    saved_last_page = snapshot->MR_snapshot_saved_last_page;\
                    first_new_page = saved_last_page->MR_regionpage_next;   \
                    /* Collect profiling information. */                    \
                    MR_region_profile_restore_from_snapshot(snapshot);      \
                    MR_region_debug_restore_from_snapshot(snapshot);        \
                                                                            \
                    if (first_new_page != NULL) {                           \
                        MR_region_return_page_list(first_new_page,          \
                            restoring_region->MR_region_last_page);         \
                        restoring_region->MR_region_last_page =             \
                            saved_last_page;                                \
                    } /* Else no new page added. */                         \
                    restoring_region->MR_region_next_available_word =       \
                        snapshot->MR_snapshot_saved_next_available_word;    \
                }                                                           \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_restore_snapshots(num_snapshots, first_snapshot)             \
            do {                                                            \
                MR_restore_snapshots_proc(num_snapshots, first_snapshot);   \
            } while (0)

    extern  void    MR_restore_snapshots_proc(int num_snapshots,
                        MR_RegionSnapshot *first_snapshot);

#endif      // MR_RBMM_USE_MACROS

#ifdef      MR_RBMM_USE_MACROS

  #define   MR_region_frame_destroy_new_regions(saved_sequence_number,      \
                frame_type)                                                 \
            do {                                                            \
                MR_RegionHeader *region;                                    \
                                                                            \
                region = MR_live_region_list;                               \
                while (region != NULL &&                                    \
                        region->MR_region_sequence_number >=                \
                        saved_sequence_number)                              \
                {                                                           \
                    MR_region_destroy_region(region);                       \
                    MR_region_debug_destroy_region(region);                 \
                    MR_region_profile_frame_destroy_region(region,          \
                        frame_type);                                        \
                    region = region->MR_region_next_region;                 \
                }                                                           \
                MR_live_region_list = region;                               \
            } while (0)

#else       // MR_RBMM_USE_MACROS

  #define   MR_region_frame_destroy_new_regions(saved_sequence_number,      \
                frame_type)                                                 \
            do {                                                            \
                MR_region_frame_destroy_new_regions_proc(                   \
                        saved_sequence_number, frame_type);                 \
            } while (0)

    extern  void    MR_region_frame_destroy_new_regions_proc(
                        int saved_sequence_number, int frame_type);

#endif     // MR_RBMM_USE_MACROS

// from_page must not be NULL.
#define     MR_region_return_page_list(from_page, to_page)                  \
            do {                                                            \
                (to_page)->MR_regionpage_next = MR_region_free_page_list;   \
                MR_region_free_page_list = (from_page);                     \
            } while (0)

////////////////////////////////////////////////////////////////////////////
// Debug RBMM messages.

#ifdef MR_RBMM_DEBUG
    #define     MR_region_debug_create_region(region)                       \
                MR_region_create_region_msg(region)

    #define     MR_region_debug_try_remove_region(region)                   \
                MR_region_try_remove_region_msg(region)

    #define     MR_region_debug_region_struct_removal_info(region)          \
                MR_region_region_struct_removal_info_msg(region)

    #define     MR_region_debug_destroy_region(region)                      \
                MR_region_destroy_region_msg(region)

    #define     MR_region_debug_logically_remove_region(region)             \
                MR_region_logically_remove_region_msg(region)

// Debug ite frame messages.
    #define     MR_region_debug_push_ite_frame(ite_sp)                      \
                MR_region_push_ite_frame_msg(ite_sp)

    #define     MR_region_debug_ite_frame(ite_sp);                          \
                MR_region_ite_frame_msg(ite_sp)

    #define     MR_region_debug_ite_frame_protected_regions(ite_sp)         \
                MR_region_ite_frame_protected_regions_msg(ite_sp)

    #define     MR_region_debug_ite_frame_snapshots(ite_sp)                 \
                MR_region_ite_frame_snapshots_msg(ite_sp)

// Debug disj frame messages.
    #define     MR_region_debug_push_disj_frame(disj_sp)                    \
                MR_region_push_disj_frame_msg(disj_sp)

    #define     MR_region_debug_disj_frame(disj_sp)                         \
                MR_region_disj_frame_msg(disj_sp)

    #define     MR_region_debug_disj_frame_protected_regions(disj_sp)       \
                MR_region_disj_frame_protected_regions_msg(disj_sp)

    #define     MR_region_debug_disj_frame_snapshots(disj_sp)               \
                MR_region_disj_frame_snapshots_msg(disj_sp)

// Debug commit frame messages.
    #define     MR_region_debug_push_commit_frame(frame)                    \
                MR_region_push_commit_frame_msg(frame)

    #define     MR_region_debug_commit_frame(frame)                         \
                MR_region_commit_frame_msg(frame)

    #define     MR_region_debug_commit_frame_saved_regions(commit_sp)       \
                MR_region_commit_frame_saved_regions_msg(commit_sp)

// Some other helpers
    #define     MR_region_debug_start(name)                                 \
                MR_region_start_msg(name)

    #define     MR_region_debug_end(name)                                   \
                MR_region_end_msg(name)

    #define     MR_region_debug_fill_ite_protect(ite_prot, region)          \
                MR_region_fill_ite_protect_msg(ite_prot, region)

    #define     MR_region_debug_fill_ite_snapshot_not_removed(snapshot,     \
                    region)                                                 \
                MR_region_fill_ite_snapshot_not_removed_msg(snapshot,       \
                    region)

    #define     MR_region_debug_fill_ite_snapshot_removed(snapshot, region) \
                MR_region_fill_ite_snapshot_removed_msg(snapshot, region)

    #define     MR_region_debug_fill_semi_disj_protect(semi_disj_prot,      \
                    region)                                                 \
                MR_region_fill_semi_disj_protect_msg(semi_disj_prot, region)

    #define     MR_region_debug_fill_disj_snapshot(snapshot, region)    \
                MR_region_fill_disj_snapshot_msg(snapshot, region)

    #define     MR_region_debug_fill_commit(commit_save, region)            \
                MR_region_fill_commit_msg(commit_save, region)

    #define     MR_region_debug_ite_unprotect(protected_region)             \
                MR_region_ite_unprotect_msg(protected_region)

    #define     MR_region_debug_restore_from_snapshot(snapshot)             \
                MR_region_restore_from_snapshot_msg(snapshot)

#else   // Not define MR_RBMM_DEBUG
    #define     MR_region_debug_create_region(region)                       \
                ((void) 0)

    #define     MR_region_debug_try_remove_region(region)                   \
                ((void) 0)

    #define     MR_region_debug_region_struct_removal_info(region)          \
                ((void) 0)

    #define     MR_region_debug_destroy_region(region)                      \
                ((void) 0)

    #define     MR_region_debug_logically_remove_region(region)             \
                ((void) 0)

    #define     MR_region_debug_push_ite_frame(frame)                       \
                ((void) 0)

    #define     MR_region_debug_ite_frame(ite_sp);                          \
                ((void) 0)

    #define     MR_region_debug_ite_frame_protected_regions(ite_sp)         \
                ((void) 0)

    #define     MR_region_debug_ite_frame_snapshots(ite_sp)                 \
                ((void) 0)

    #define     MR_region_debug_push_disj_frame(disj_sp)                    \
                ((void) 0)

    #define     MR_region_debug_disj_frame(frame)                           \
                ((void) 0)

    #define     MR_region_debug_disj_frame_snapshots(disj_sp)               \
                ((void) 0)

    #define     MR_region_debug_push_commit_frame(frame)                    \
                ((void) 0)

    #define     MR_region_debug_commit_frame(frame)                         \
                ((void) 0)

    #define     MR_region_debug_commit_frame_saved_regions(commit_sp)       \
                ((void) 0)

    #define     MR_region_debug_start(name)                                 \
                ((void) 0)

    #define     MR_region_debug_end(name)                                   \
                ((void) 0)

    #define     MR_region_debug_fill_ite_protect(ite_prot, region)          \
                ((void) 0)

    #define     MR_region_debug_fill_ite_snapshot_not_removed(snapshot,     \
                    region)                                                 \
                ((void) 0)

    #define     MR_region_debug_fill_ite_snapshot_removed(snapshot, region) \
                ((void) 0)

    #define     MR_region_debug_fill_semi_disj_protect(semi_disj_prot,      \
                    region)                                                 \
                ((void) 0)

    #define     MR_region_debug_fill_disj_snapshot(snapshot, region)        \
                ((void) 0)

    #define     MR_region_debug_fill_commit(commit_save, region)            \
                ((void) 0)

    #define     MR_region_debug_ite_unprotect(protected_region)             \
                ((void) 0)

    #define     MR_region_debug_restore_from_snapshot(snapshot)             \
                ((void) 0)

#endif // End of not define MR_RBMM_DEBUG

extern  void    MR_region_create_region_msg(MR_RegionHeader *region);
extern  void    MR_region_try_remove_region_msg(MR_RegionHeader *region);
extern  void    MR_region_region_struct_removal_info_msg(MR_RegionHeader *region);
extern  void    MR_region_destroy_region_msg(MR_RegionHeader *region);
extern  void    MR_region_logically_remove_region_msg(MR_RegionHeader *region);

extern  void    MR_region_push_ite_frame_msg(MR_RegionIteFixedFrame *ite_frame);
extern  void    MR_region_ite_frame_msg(MR_RegionIteFixedFrame *ite_frame);
extern  void    MR_region_ite_frame_protected_regions_msg(
                    MR_RegionIteFixedFrame *ite_frame);
extern  void    MR_region_ite_frame_snapshots_msg(
                    MR_RegionIteFixedFrame *ite_frame);

extern  void    MR_region_push_disj_frame_msg(
                    MR_RegionDisjFixedFrame *disj_frame);
extern  void    MR_region_disj_frame_msg(MR_RegionDisjFixedFrame *disj_frame);
extern  void    MR_region_disj_frame_snapshots_msg(
                    MR_RegionDisjFixedFrame *disj_frame);

extern  void    MR_region_push_commit_frame_msg(
                    MR_RegionCommitFixedFrame *commit_frame);
extern  void    MR_region_commit_frame_msg(
                    MR_RegionCommitFixedFrame *commit_frame);
extern  void    MR_region_commit_frame_saved_regions_msg(
                    MR_RegionCommitFixedFrame *commit_frame);
extern  void    MR_region_commit_success_destroy_marked_regions_msg(
                    int saved_seq_number, int number_of_saved_regions,
                    MR_RegionCommitFixedFrame *commit_frame);

extern  void    MR_region_start_msg(const char *);
extern  void    MR_region_end_msg(const char *);

extern  void    MR_region_fill_ite_protect_msg(MR_RegionIteProtect *,
                    MR_RegionHeader *);
extern  void    MR_region_fill_ite_snapshot_not_removed_msg(
                    MR_RegionSnapshot *, MR_RegionHeader *);
extern  void    MR_region_fill_ite_snapshot_removed_msg(MR_RegionSnapshot *,
                    MR_RegionHeader *);
extern  void    MR_region_fill_semi_disj_protect_msg(
                    MR_RegionSemiDisjProtect *, MR_RegionHeader *);
extern  void    MR_region_fill_disj_snapshot_msg(MR_RegionSnapshot *,
                    MR_RegionHeader *);
extern  void    MR_region_fill_commit_msg(MR_RegionCommitSave *,
                    MR_RegionHeader *);
extern  void    MR_region_ite_unprotect_msg(MR_RegionHeader *);
extern  void    MR_region_restore_from_snapshot_msg(MR_RegionSnapshot *);

////////////////////////////////////////////////////////////////////////////
// Profiling RBMM.

#ifdef MR_RBMM_PROFILING

// This is the profiling wish list, not all of them have been yet collected.
// - How many words are allocated
// - Maximum number of words
// - How many regions are allocated
// - Maximum number of regions
// - Size of the biggest region
// - How many regions are saved at commit entries
// - How many regions are protected at entry to a condition
// - How many snapshots are saved at entry to a condition
// - How many regions are protected at entry to a disj goal
// - How many snapshots are saved at entry to a disj goal
// - How many pages are requested from the OS
// - Time profiling: compile-time and runtime

struct MR_RegionProfUnit_Struct {
    int        MR_rbmmpu_current;
    int        MR_rbmmpu_max;
    int        MR_rbmmpu_total;
};

extern MR_RegionProfUnit    MR_rbmmp_regions_used;
extern unsigned int         MR_rbmmp_biggest_region_size;
extern int                  MR_rbmmp_biggest_region;
extern unsigned int         MR_rbmmp_regions_instant_reclaimed_at_disj;
extern unsigned int         MR_rbmmp_regions_instant_reclaimed_at_ite;
extern unsigned int         MR_rbmmp_regions_reclaimed_by_remove_instr;
extern unsigned int         MR_rbmmp_regions_reclaimed_at_then_part;
extern unsigned int         MR_rbmmp_regions_reclaimed_at_commit;

extern MR_RegionProfUnit    MR_rbmmp_words_used;
extern unsigned int         MR_rbmmp_words_instant_reclaimed_new_alloc;
extern unsigned int         MR_rbmmp_words_instant_reclaimed_new_regions;
extern unsigned int         MR_rbmmp_words_reclaimed_by_remove_instr;
extern unsigned int         MR_rbmmp_words_reclaimed_at_then_part;
extern unsigned int         MR_rbmmp_words_reclaimed_at_commit;

extern MR_RegionProfUnit    MR_rbmmp_pages_used;
extern unsigned int         MR_rbmmp_page_requested;
extern double               MR_rbmmp_page_utilized;
extern unsigned int         MR_rbmmp_pages_instant_reclaimed_new_alloc;
extern unsigned int         MR_rbmmp_pages_instant_reclaimed_new_regions;
extern unsigned int         MR_rbmmp_pages_reclaimed_by_remove_instr;
extern unsigned int         MR_rbmmp_pages_reclaimed_at_then_part;
extern unsigned int         MR_rbmmp_pages_reclaimed_at_commit;

extern MR_RegionProfUnit    MR_rbmmp_num_disj_frames;
extern MR_RegionProfUnit    MR_rbmmp_words_used_by_disj_frames;
extern unsigned int         MR_rbmmp_regions_protected_at_semi_disj;
extern unsigned int         MR_rbmmp_snapshots_saved_at_disj;

extern MR_RegionProfUnit    MR_rbmmp_num_ite_frames;
extern MR_RegionProfUnit    MR_rbmmp_num_commit_frames;
extern unsigned int         MR_rbmmp_regions_protected_at_ite;
extern unsigned int         MR_rbmmp_snapshots_saved_at_ite;

extern MR_RegionProfUnit    MR_rbmmp_words_used_by_ite_frames;
extern MR_RegionProfUnit    MR_rbmmp_words_used_by_commit_frames;
extern unsigned int         MR_rbmmp_regions_saved_at_commit;

extern  int     MR_region_get_ite_frame_size(MR_RegionIteFixedFrame *);
extern  int     MR_region_get_disj_frame_size(MR_RegionDisjFixedFrame *);
extern  int     MR_region_get_commit_frame_size(MR_RegionCommitFixedFrame *);

#define     MR_region_profile_push_ite_frame                                \
            do {                                                            \
                MR_region_update_profiling_unit(&MR_rbmmp_num_ite_frames,   \
                    1);                                                     \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_ite_frames,                     \
                    MR_REGION_ITE_FRAME_FIXED_SIZE);                        \
            } while (0)

#define     MR_region_profile_push_disj_frame                               \
            do {                                                            \
                MR_region_update_profiling_unit(&MR_rbmmp_num_disj_frames,  \
                    1);                                                     \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_disj_frames,                    \
                    MR_REGION_DISJ_FRAME_FIXED_SIZE);                       \
            } while (0)

#define     MR_region_profile_push_commit_frame                             \
            do {                                                            \
                MR_region_update_profiling_unit(&MR_rbmmp_num_commit_frames,\
                    1);                                                     \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_commit_frames,                  \
                    MR_REGION_COMMIT_FRAME_FIXED_SIZE);                     \
            } while (0)

#define     MR_region_profile_pop_ite_frame(top_ite_frame)                  \
            do {                                                            \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_ite_frames,                     \
                    -MR_region_get_ite_frame_size(top_ite_frame));          \
                MR_region_update_profiling_unit(&MR_rbmmp_num_ite_frames,   \
                    -1);                                                    \
            } while (0)

#define     MR_region_profile_pop_disj_frame(top_disj_frame,                \
                new_top_disj_frame)                                         \
            do {                                                            \
                int                         num_popped_frames;              \
                MR_RegionDisjFixedFrame     *disj_frame;                    \
                                                                            \
                num_popped_frames = 0;                                      \
                disj_frame = top_disj_frame;                                \
                while (disj_frame != new_top_disj_frame) {                  \
                    num_popped_frames += 1;                                 \
                    MR_region_update_profiling_unit(                        \
                        &MR_rbmmp_words_used_by_disj_frames,                \
                        -MR_region_get_disj_frame_size(disj_frame));        \
                    disj_frame = disj_frame->MR_rdff_previous_disj_frame;   \
                }                                                           \
                MR_region_update_profiling_unit(&MR_rbmmp_num_disj_frames,  \
                    -num_popped_frames);                                    \
            } while (0)

#define     MR_region_profile_pop_commit_frame(top_commit_frame)            \
            do {                                                            \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_commit_frames,                  \
                    -MR_region_get_commit_frame_size(top_commit_frame));    \
                MR_region_update_profiling_unit(&MR_rbmmp_num_commit_frames,\
                    -1);                                                    \
            } while (0)

#define     MR_region_profile_fill_ite_protect                              \
            do {                                                            \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_ite_frames,                     \
                    MR_REGION_ITE_PROT_SIZE);                               \
                MR_region_profile_increase_counter(                         \
                    &MR_rbmmp_regions_protected_at_ite);                    \
            } while (0)

#define     MR_region_profile_fill_commit                                   \
            do {                                                            \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_commit_frames,                  \
                    MR_REGION_COMMIT_SAVE_SIZE);                            \
                MR_region_profile_increase_counter(                         \
                    &MR_rbmmp_regions_saved_at_commit);                     \
            } while (0)

#define     MR_region_profile_fill_ite_snapshot                             \
            do {                                                            \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_ite_frames,                     \
                    MR_REGION_ITE_SNAPSHOT_SIZE);                           \
                MR_region_profile_increase_counter(                         \
                    &MR_rbmmp_snapshots_saved_at_ite);                      \
            } while (0)

#define     MR_region_profile_fill_semi_disj_protect                        \
            do {                                                            \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_disj_frames,                    \
                    MR_REGION_SEMI_DISJ_PROT_SIZE);                         \
                MR_region_profile_increase_counter(                         \
                    &MR_rbmmp_regions_protected_at_semi_disj);              \
            } while (0)

#define     MR_region_profile_fill_disj_snapshot                            \
            do {                                                            \
                MR_region_update_profiling_unit(                            \
                    &MR_rbmmp_words_used_by_disj_frames,                    \
                    MR_REGION_DISJ_SNAPSHOT_SIZE);                          \
                MR_region_profile_increase_counter(                         \
                    &MR_rbmmp_snapshots_saved_at_disj);                     \
            } while (0)

#define     MR_region_profile_frame_destroy_region(region, frame_type)      \
            do {                                                            \
                switch (frame_type) {                                       \
                    case MR_REGION_DISJ_FRAME_TYPE:                         \
                        MR_region_profile_increase_counter(                 \
                            &MR_rbmmp_regions_instant_reclaimed_at_disj);   \
                        break;                                              \
                    case MR_REGION_ITE_FRAME_TYPE:                          \
                        MR_region_profile_increase_counter(                 \
                            &MR_rbmmp_regions_instant_reclaimed_at_ite);    \
                        break;                                              \
                }                                                           \
                MR_region_profile_reclaim_pages_and_words(region,           \
                    &MR_rbmmp_pages_instant_reclaimed_new_regions,          \
                    &MR_rbmmp_words_instant_reclaimed_new_regions);         \
            } while (0)

#else   // Not define MR_RBMM_PROFILING

#define     MR_region_profile_push_ite_frame                                \
            ((void) 0)

#define     MR_region_profile_push_disj_frame                               \
            ((void) 0)

#define     MR_region_profile_push_commit_frame                             \
            ((void) 0)

#define     MR_region_profile_pop_ite_frame(top_ite_frame);                 \
            ((void) 0)

#define     MR_region_profile_pop_disj_frame(top_disj_frame,                \
                new_top_disj_frame)                                         \
            ((void) 0)

#define     MR_region_profile_pop_commit_frame(top_commit_frame);           \
            ((void) 0)

#define     MR_region_profile_fill_ite_protect                              \
            ((void) 0)

#define     MR_region_profile_fill_semi_disj_protect                        \
            ((void) 0)

#define     MR_region_profile_fill_commit                                   \
            ((void) 0)

#define     MR_region_profile_fill_ite_snapshot                             \
            ((void) 0)

#define     MR_region_profile_fill_disj_snapshot                            \
            ((void) 0)

#define     MR_region_profile_frame_destroy_region(region, frame_type)      \
            ((void) 0)
#endif  // End of not define MR_RBMM_PROFILING.

extern  void    MR_region_update_profiling_unit(
                    MR_RegionProfUnit *profiling_unit, int quantity);
extern  void    MR_region_profile_destroyed_region(MR_RegionHeader *);
extern  void    MR_region_profile_restore_from_snapshot(MR_RegionSnapshot *);
extern  void    MR_region_profile_reclaim_region(MR_RegionHeader *,
                    unsigned int *, unsigned int *, unsigned int*);
extern  void    MR_region_profile_reclaim_pages_and_words(MR_RegionHeader *,
                    unsigned int *, unsigned int*);
extern  void    MR_region_profile_increase_counter(unsigned int *);
extern  int     MR_region_get_number_of_pages(MR_RegionPage *,
                    MR_RegionPage *);

extern  void    MR_region_print_profiling_info(void);

////////////////////////////////////////////////////////////////////////////

#endif  // MR_USE_REGIONS

#endif  // MERCURY_REGION_H
