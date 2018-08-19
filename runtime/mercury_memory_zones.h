// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2002, 2004-2006, 2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_memory_zones.h
//
// This module defines a generic memory zone handler, which can be used for
// stacks and heaps in the Mercury runtime. It provides functions for
// generating offsets so that different memory zones begin at different
// offsets (improves performance with direct mapped caches). It also
// handles the fake_reg array for holding Mercury virtual registers.

#ifndef MERCURY_MEMORY_ZONES_H
#define MERCURY_MEMORY_ZONES_H

#include "mercury_regs.h"           // for MR_NUM_REAL_R_REGS, etc

#include <stdio.h>                  // for FILE
#include <stdlib.h>                 // for size_t

#include "mercury_types.h"          // for MR_Word
#include "mercury_std.h"            // for MR_bool
#include "mercury_atomic_ops.h"     // for MR_THREADSAFE_VOLATILE

typedef struct      MR_MemoryZone_Struct MR_MemoryZone;

typedef MR_bool     MR_ZoneHandler(MR_Word *addr, MR_MemoryZone *zone,
                        void *context);

// The Mercury runtime uses a number of memory areas or *zones*. These
// hold the detstack, the nondetstack, the heap, and potentially other
// areas such as a trail, a "solutions"-heap, and so on.
// These memory areas are each represented by an MR_MemoryZone structure
// that contains the following fields:
//
// next     The memory zones are organized as a linked list of free zones
//          and a linked list of used zones. The next field, if not NULL,
//          points to the next memory zone in the list.
//
// name     A string constant used to name the allocated area.
//
// id       An integer which together with the name should uniquely
//          identify the allocated area.
//
// lru_token
//          This field is filled with a token each time a zone is freed.
//          We use it to track the least-recently-used zones in the free
//          list so that they can be handed back to the garbage collector/OS.
//          Zones with larger tokens have been used more recently than
//          zones with smaller tokens.
//
// desired_size
//          The desired size of the zone in kilobytes. The actual size
//          may be larger due to roundups.
//
// redzone_size
//          The desired size of the redzone in kilobytes. The actual size
//          may be larger due to roundups.
//
// bottom   The address of the bottom of the allocated area
//          (should be on a page boundary).
//
// top      The address one word past the top of the allocated area
//          (should be on a page boundary).
//
// min      The address of the lowest part of the allocated that will be used.
//          This may be different to `bottom' so that the use of different
//          memory zones doesn't beat the cache.
//
// max      The highest address in this memory area that has been used so far.
//          This is only computed if MR_LOWLEVEL_DEBUG is enabled.
//
// hardmax  The address of the bottom of the last page of the allocated area.
//          This is one higher than the highest address that can be used
//          in this zone. We never unprotect the last page of a zone so that
//          we retain protection against overrunning the end of the zone.
//          This is obviously only available on platforms that have mprotect.
//          (Should be on a page boundary.)
//
// redzone  The address of the start of the region that has been mprotected
//          as a redzone. Redzone is only available on platforms where
//          MR_CHECK_OVERFLOW_VIA_MPROTECT is defined.
//          (Should be on a page boundary.)
//
// redzone_base
//          The original redzone.
//
// handler  The address of a function to handle accesses in the redzone
//          of this allocated area. This is only available with
//          MR_CHECK_OVERFLOW_VIA_MPROTECT.
//
// gc_threshold
//          This field, which is only used for heap zones, points to
//          MR_heap_margin_size bytes before MR_zone_end (which is defined
//          as one the fields above by the macros below). It is used to decide
//          when to do garbage collection without incurring the expense of
//          a subtraction on every allocation.
//
// extend_threshold
//          This field, which is only used for stack zones, points to
//          MR_stack_margin_size bytes before MR_zone_end.

struct MR_MemoryZone_Struct {
    MR_MemoryZone           *MR_THREADSAFE_VOLATILE  MR_zone_next;
    const char              *MR_zone_name;
    MR_Unsigned             MR_zone_id;
#ifndef MR_DO_NOT_CACHE_FREE_MEMORY_ZONES
    MR_Unsigned             MR_zone_lru_token;
#endif
    size_t                  MR_zone_desired_size;
    size_t                  MR_zone_redzone_size;
    MR_Word                 *MR_zone_bottom;
    MR_Word                 *MR_zone_top;
    MR_Word                 *MR_zone_min;
    MR_Word                 *MR_zone_max;
#ifdef MR_PROTECTPAGE
    MR_Word                 *MR_zone_hardmax;
#endif  // MR_PROTECTPAGE

#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    MR_Word                 *MR_zone_redzone_base;
    MR_Word                 *MR_zone_redzone;
    MR_ZoneHandler          *MR_zone_handler;
#endif // MR_CHECK_OVERFLOW_VIA_MPROTECT

#if defined(MR_NATIVE_GC) && defined(MR_HIGHLEVEL_CODE)
    // set to MR_zone_end - MR_heap_margin_size_bytes
    char                    *MR_zone_gc_threshold;
#endif
#if defined(MR_STACK_SEGMENTS) && !defined(MR_HIGHLEVEL_CODE)
    // set to MR_zone_end - MR_stack_margin_size_words
    MR_Word                 *MR_zone_extend_threshold;
#endif
};

typedef struct MR_MemoryZones_Struct    MR_MemoryZones;

struct MR_MemoryZones_Struct {
    MR_MemoryZone           *MR_zones_head;
    MR_MemoryZones          *MR_zones_tail;
};

#ifndef MR_DO_NOT_CACHE_FREE_MEMORY_ZONES
// Free memory zones are arranged in a list of lists. The outer list (below)
// associates a size of the zones in each inner list. It is to be kept in
// sorted order from smallest to largest. So that a traversal of this list
// returns the zones that are the 'best fit' as the 'first fit'. The inner
// lists (using the MR_zone_next field of the zones) contain zones of all the
// same size.

typedef struct MR_MemoryZonesFree_Struct MR_MemoryZonesFree;

struct MR_MemoryZonesFree_Struct {
    size_t                  MR_zonesfree_size;
    MR_MemoryZonesFree      *MR_zonesfree_major_next;
    MR_MemoryZonesFree      *MR_zonesfree_major_prev;
    MR_MemoryZone           *MR_zonesfree_minor_head;
    MR_MemoryZone           *MR_zonesfree_minor_tail;
};
#endif

// MR_zone_end specifies the end of the area accessible without a page fault.
// It is used by MR_clear_zone_for_GC().

#ifdef MR_CHECK_OVERFLOW_VIA_MPROTECT
    #define MR_zone_end     MR_zone_redzone
#elif defined(MR_PROTECTPAGE)
    #define MR_zone_end     MR_zone_hardmax
#else
    #define MR_zone_end     MR_zone_top
#endif

// MR_clear_zone_for_GC(MR_MemoryZone *zone, void *start_address):
// Zero out the (hopefully unused) portion of the zone from the specified
// `start_address' to the end of the zone. This is used to avoid unwanted
// memory retention due to false hits in the conservative garbage collector.

#define MR_clear_zone_for_GC(zone, start_address)                       \
    ((void) MR_memset((start_address), 0,                               \
        (char*)((zone)->MR_zone_end) - (char *)(start_address)))

// Rather then using mprotect directly, we call MR_protect_pages which
// is OS independent.

#ifdef MR_PROTECTPAGE

  #ifdef MR_WIN32
    #ifndef PROT_NONE
      #define PROT_NONE  0x0000
    #endif
    #ifndef PROT_READ
      #define PROT_READ  0x0001
    #endif
    #ifndef PROT_WRITE
      #define PROT_WRITE 0x0002
    #endif
  #endif

// MR_protect_pages() is currently only a wrapper around mprotect() so callers
// may expect errno to be set appropriately on error to provide better error
// messages. If MR_protect_pages() gains another implementation then it may be
// necessary to update callers to not depend on errno being set.
extern int      MR_protect_pages(void *addr, size_t size, int prot_flags);

#endif

// MR_init_zones() initializes the memory zone pool and the offset generator.
// It should be used before any zones are created or offsets requested.

extern void     MR_init_zones(void);

// MR_create_zone(Name, Id, Size, Offset, RedZoneSize, FaultHandler)
// allocates a new memory zone with name Name, and number Id, size Size
// (in bytes - which gets rounded up to the nearest multiple of the page size),
// an offset Offset from the page boundary at which to start using the
// memory region (used to help avoid beating the cache), the amount Redzone
// of memory (in bytes) to be protected as a redzone (must be less than Size),
// and the address of a function to handle memory references in the redzone.
// If it fails to allocate or protect the zone, then it exits.
// If MR_CHECK_OVERFLOW_VIA_MPROTECT is unavailable, then the last two
// arguments are ignored.
//
// This may reuse previously allocated memory but will re-configure the
// name, redzone and handler.

extern MR_MemoryZone    *MR_create_or_reuse_zone(const char *name,
                            size_t size, size_t offset, size_t redsize,
                            MR_ZoneHandler *handler);

extern void             MR_release_zone(MR_MemoryZone *zone);

// MR_extend_zone(Zone, NewSize) extends Zone to increase its size to NewSize,
// and returns the difference between the new address of the memory area of
// the zone and the old.

extern MR_Integer       MR_extend_zone(MR_MemoryZone *zone,
                            size_t new_size);

// MR_reset_redzone(Zone) resets the redzone on the given MR_MemoryZone to the
// original zone specified in the call to create_or_reuse_zone() if
// MR_CHECK_OVERFLOW_VIA_MPROTECT is defined. Otherwise it does nothing.

extern void             MR_reset_redzone(MR_MemoryZone *zone);

// MR_get_used_memory_zones_readonly() returns a pointer to the linked list of
// used memory zones. The list should be considered read only and may not be
// complete. This is suitable for use where locking is impossible but
// incomplete data is acceptable.

extern MR_MemoryZone    *MR_get_used_memory_zones_readonly(void);

// Returns true iff ptr is the given zone.

extern MR_bool          MR_in_zone(const MR_Word *ptr,
                            const MR_MemoryZone *zone);

// MR_debug_memory(fp) prints out debugging information about the current
// memory zones to fp.

extern void             MR_debug_memory(FILE *fp);

// MR_debug_memory_zone(fp, zone) prints out debugging information about zone
// to fp.

extern void             MR_debug_memory_zone(FILE *fp, MR_MemoryZone *zone);

// MR_next_offset() returns successive offsets across the primary cache.
// Useful when calling {create,construct}_zone().

extern size_t           MR_next_offset(void);

// MR_print_zone_stats() print statistics about memory zone allocation.

#ifdef MR_PROFILE_ZONES
extern void             MR_print_zone_stats(void);
#endif

#endif // not MERCURY_MEMORY_ZONES_H
